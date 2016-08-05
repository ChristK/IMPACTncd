#cmpfile("./initialisation.R")
## IMPACTncd: A decision support tool for primary prevention of NCDs
## Copyright (C) 2015  Chris Kypridemos

## IMPACTncd is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program; if not, see <http://www.gnu.org/licenses/>
## or write to the Free Software Foundation, Inc., 51 Franklin Street,
## Fifth Floor, Boston, MA 02110-1301  USA.


# preample
dependencies <- function(x) {
  for (j in x) {
    # require returns T invisibly if it was able to load package
    if (!require(j, character.only = T)) {
      # If package was not able to be loaded then re-install
      install.packages(j, dependencies = T)
      # Load package after installing
      require(j, character.only = T)
    }
  }
}

# Then try/install packages...
dependencies(c("demography", 
               #"truncnorm", 
               "stringr", 
               "nnet",
               "reshape2", 
               "compiler",
               "survey",
               "ggplot2",
               "randtoolbox",
               "mc2d",
               "doParallel",
               "doRNG",
               "foreach",
               "quantreg",
               "Rcpp",
               "data.table", 
               "dplyr"))

enableJIT(2) #set to 1, 2 or 3 to enable different precompiling levels
# options(datatable.optimize = 1)
# options(datatable.verbose = T)
options(survey.lonely.psu = "adjust") #Lonely PSU (center any single-PSU strata around the sample grand mean rather than the stratum mean)
# require(devtools)
# install_github("Rdatatable/data.table",  build_vignettes = F)
if (packageVersion("data.table") != "1.9.6") setthreads(1L) # deactivate implicit parallelisation 

# max projection horizon (limited by fertility)
if (init.year + yearstoproject > 2061) yearstoproject <- 2061 - init.year

# Define end() function to beep end print a message
end <- function(...) {
  cat("All done! \a\n")
  sink(file = "./Output/simulation parameters.txt",
       append = T, 
       type = "output",
       split = F)
  cat(paste0("Simulation ended successfully at: ", Sys.time(), "\n"))
  sink()
  if (Sys.info()[1] == "Windows") {
    system("rundll32 user32.dll,MessageBeep -1")
    Sys.sleep(.5)
  }
}

# Function for timing log
time.mark <- function(x) {
  sink(file = "./Output/times.txt",
       append = T, 
       type = "output",
       split = F)
  cat(paste0(x, " at: ", Sys.time(), "\n"))
  sink()
}

# # Create a vector of random numbers, using the SIMD-oriented Fast Mersenne Twister algorithms by
# # Matsumoto & Saito (2008)
# dice <- function(n = .N) {
#     rand <- SFMT(n, dim = 1, mexp = 19937, usepset = T, withtorus = F, usetime = T)
#     return(rand)
# }

# Define RNG for parallel use with doRNG
RNGkind("L'Ecuyer-CMRG")
dice <- cmpfun(function(n = .N) runif(n))

# truncated normal sampler that doesn't require you to throw out observations. 
# 3x faster than truncnorm package.
# but truncnorm::rtruncnorm(1, 0, Inf, mean=0, sd = 0) gives NaN while this Inf
# because qnorn(1, x, x) = Inf
rtruncnorm <- 
  cmpfun(
    function(n, a = -Inf, b = Inf, mean, sd){
      qnorm(runif(n, pnorm(a, mean, sd), pnorm(b, mean, sd)), mean, sd)
    })

# define function for stochastic RR
stochRRabov1 <-
  cmpfun(
    function(n = .N, m, ci) { # lognormal
      rr <- exp(rtruncnorm(n, 0, Inf, log(m), abs(log(m) - log(ci))/1.96))
      rr[!is.finite(rr)] <- 1 # fix for rtruncnorm above
      return(rr)  
    }
  )

stochRRtabl <-
  cmpfun(
    function(m, ci, id = .id) { # lognormal
      kk <- runif(ifelse(paired, numberofiterations, it))
      rr <- exp(qnorm(kk[[id]], log(m), abs(log(m) - log(ci))/1.96))
      rr[!is.finite(rr)] <- 1 # fix for rtruncnorm above
      return(rr)  
    }
  )

stochRRbelow1 <-
  cmpfun(
    function(n = .N, m, ci) { # lognormal
      rr <- exp(rtruncnorm(n, -Inf, 0, log(m), abs(log(m) - log(ci))/1.96))
      rr[!is.finite(rr)] <- 1 # fix for rtruncnorm above
      return(rr)  
    }
  )

stochRRnorm <-
  cmpfun(
    function(n = .N, m, ci) { # normal distr
      if (m < 1) {
        a = 0
        b = 1
      } else {
        a = 1
        b = Inf
      }
      ifelse(m == ci, rr <- rep(m, n),
             rr <- rtruncnorm(n = n, a = a, b = b, mean = m, sd = abs(m - ci)/1.96))
      rr[!is.finite(rr)] <- 1 # fix for rtruncnorm above
      return(rr)  
    }
  )

# function to calculate mortality based on 1st and 5th year survival
hyperbola <-
  cmpfun(
    function(y1, y5, x) {
      b = (5 * y5 - y1)/4
      a = y1 - b
      y = b + a/x
      return(y)
    }
  )

# function to clone dt for 2dmc
clonedt <-
  cmpfun(
    function(DT) {
      xx <- key(DT)
      l <- sample(list(DT), ifelse(paired, numberofiterations, it), T)
      return(setkeyv(rbindlist(l, idcol = T), xx))
    }
  )

# function to calculate mortality based on 1st and 5th year survival based on Weibull distribution
# hazard h(t) = l*g*t^(g-1)
# cummulative hazard H(t) = l*t^g
# survival function S(t) = exp(-l*t^g)
# from ! and 5 year net survival rates I solve S(t) to find l and g
survival.fn <-
  cmpfun(
    function(y1, y5, t) {
      l = -log(y1)
      g = log(-log(y5)/l ,5)
      return(exp(-l*t^g)) # returns pct of survivors at time t
      #return(l*g*t^(g-1)) # returns the rate of death at time t
      #return(l*t^g) # returns the cumulative hazard of death at time t
    }
  )

fatality.rt <-
  cmpfun(
    function(y1, y5, t=1) {
      l = -log(y1)
      g = log(-log(y5)/l ,5)
      #return(exp(-l*t^g)) # returns pct of survivors at time t
      return(l*g*t^(g-1)) # returns the rate of death at time t
      #return(l*t^g) # returns the cumulative hazard of death at time t
    }
  )
#plot(survival.fn(0.55675983428955, 0.35348030090332, 0:20), x=0:20, ylim=c(0,1))

# Define function for sampling. Taken from sample man pages 
resample <-
  cmpfun(
    function(x, ...) {
      x <- na.omit(x)
      x[sample.int(length(x), ...)]
    }
  )

# Define operator %!in%, meaning !%in%
'%!in%' <- cmpfun(function(x,y)!('%in%'(x,y)))

# Define outersect. Like setdiff but symmetrical. I.e. setdiff(a,b) is not the same as setdiff(b,a). outersect solve this by calculating both
outersect <- 
  cmpfun(
    function(x, y, ...) {
      big.vec <- c(x, y, ...)
      duplicates <- big.vec[duplicated(big.vec)]
      setdiff(big.vec, unique(duplicates))
    }
  )

# Define cbind for datatables. 2 orders of magnitude faster than original
cbind.dt = function(...) {
  x = c(...)
  setattr(x, "class", c("data.table", "data.frame"))
  ans = .Call(data.table:::Calloccolwrapper, x, max(100L, ncol(x) + 64L), FALSE)
  .Call(data.table:::Csetnamed, ans, 0L)
}

# Define function to split agegroups and create groups
agegroup.fn <- 
  cmpfun(
    function(x, lagtime = 0) {
      breaks <- c(0, 1, seq(5, 85, 5), Inf)
      labels <- c("<1   ", "01-04", "05-09",
                  "10-14", "15-19", "20-24", 
                  "25-29", "30-34", "35-39", 
                  "40-44", "45-49", "50-54",
                  "55-59", "60-64", "65-69",
                  "70-74", "75-79", "80-84", 
                  "85+")
      if (is.numeric(x)) { 
        agegroup = cut(x + lagtime, 
                       breaks = breaks, 
                       labels = labels, 
                       include.lowest = T, 
                       right = F, 
                       ordered_result = T)
        return(invisible(agegroup))    
      } else {
        if (is.data.table(x)) {
          x[, agegroup := cut(age + lagtime, 
                              breaks = breaks, 
                              labels = labels, 
                              include.lowest = T, 
                              right = F, 
                              ordered_result = T)]
          setorder(x, qimd, sex, agegroup)
          x[, group := rleid(qimd, sex, agegroup)]
          return(invisible(x))
        } else return(print("only datatables and vectors are eligible inputs"))
      }
    }
  )

# Define function to split agegroups and create groups
agegroup.part <- 
  cmpfun(
    function(x, lagtime = 0) {
      breaks <- c(seq(20, 85, 5), Inf)
      labels <- c("20-24", "25-29", "30-34", 
                  "35-39", "40-44", "45-49",
                  "50-54", "55-59", "60-64",
                  "65-69", "70-74", "75-79",
                  "80-84", "85+")
      if (is.numeric(x)) { 
        agegroup = cut(x + lagtime, 
                       breaks = breaks, 
                       labels = labels, 
                       include.lowest = T, 
                       right = F, 
                       ordered_result = T)
        return(invisible(agegroup))    
      } else {
        if (is.data.table(x)) {
          x[, agegroup := cut(age + lagtime, 
                              breaks = breaks, 
                              labels = labels, 
                              include.lowest = T, 
                              right = F, 
                              ordered_result = T)]
          return(invisible(x))
        } else return(print("only datatables and vectors are eligible inputs"))
      }
    }
  )

# Define function to clear labels form SPSS labelled imports
clear.labels <- 
  function(x) {
    if(is.list(x)) {
      for(i in 1 : length(x)) class(x[[i]]) <- setdiff(class(x[[i]]), 'labelled') 
      for(i in 1 : length(x)) attr(x[[i]],"label") <- NULL
    }
    else {
      class(x) <- setdiff(class(x), "labelled")
      attr(x, "label") <- NULL
    }
    return(invisible(x))
  }


# from plyr. relevel vector
mapvalues <- function (x, from, to, warn_missing = TRUE) 
{
  if (length(from) != length(to)) {
    stop("`from` and `to` vectors are not the same length.")
  }
  if (!is.atomic(x)) {
    stop("`x` must be an atomic vector.")
  }
  if (is.factor(x)) {
    levels(x) <- mapvalues(levels(x), from, to, warn_missing)
    return(x)
  }
  mapidx <- match(x, from)
  mapidxNA <- is.na(mapidx)
  from_found <- sort(unique(mapidx))
  if (warn_missing && length(from_found) != length(from)) {
    message("The following `from` values were not present in `x`: ", 
            paste(from[!(1:length(from) %in% from_found)], collapse = ", "))
  }
  x[!mapidxNA] <- to[mapidx[!mapidxNA]]
  x
}

# Define function for percentile rank (dplyr provides similar functions)
# perc.rank <- function(x) rank(x,  ties.method = "random")/length(x)

# Define function to bound a vector (numeric)
bound <- cmpfun(
  function(x, a=0, b=1) {
    x[x < a] <- a
    x[x > b] <- b
    return(x)
  }
)

# ASFR for 2010 is the observed from ONS(same for all fertility projections), for 2011 is copy of 2012
if (Fertility.Assumption == "N") {
  Fertility <- fread("./Fertility/Principal fertility ONS projections.csv", header = T, 
                     colClasses = "numeric")
} else if (Fertility.Assumption == "H") {
  Fertility <- fread("./Fertility/High fertility ONS projections.csv", header = T, 
                     colClasses = "numeric")
} else if (Fertility.Assumption == "L") {
  Fertility <- fread("./Fertility/Low fertility ONS projections.csv", header = T, 
                     colClasses = "numeric")
} else stop("Fertility.Assumption was set incorrectly. Please specify fertility scenario")

setnames(Fertility, c("age", 2000:2061))
setkey(Fertility, age)

# Find and load scenarios -------------------------------------------------
if (!exists("scenarios.list")) { # not if defined by GUI
  scenarios.list <- list.files(path = "./Scenarios", pattern = glob2rx("*.Rc"), full.names = F, recursive = F)
}

n.scenarios <- length(scenarios.list)
#scenarios.list <- rep(scenarios.list, each = numberofiterations)
scenarios.list <- rep(scenarios.list, numberofiterations)
it <- numberofiterations * n.scenarios
#if (paired == T) counter <- rep(1:numberofiterations, n.scenarios)
if (paired == T) counter <- rep(1:numberofiterations, each = n.scenarios)
if (paired == F) counter <- 1:it

if (!exists("paired.mem") | (exists("paired.mem") && paired.mem == 0)) { #priority to gui input
  paired.mem <-
    suppressWarnings(
      max(
        as.integer(
          list.dirs(
            path = "./Output/current trends", 
            #pattern = "riskfactors.rds", 
            full.names = F, 
            recursive = F
          )
        )
      )
    )
  paired.mem <- ifelse(is.infinite(paired.mem), 0, paired.mem)
}

# Specify output.txt file for simulation parameters -----------------------
dir.create(path = "./Output/", recursive = T, showWarnings = F)
fileOut <- file(paste0("./Output/simulation parameters temp.txt"))
writeLines(c("IMPACTncd\nA dynamic microsimulation, by Dr Chris Kypridemos", "\n", 
             paste0("Simulation started at: ", Sys.time(), "\n"),
             "Simulation parameters:\n",
             paste0("Paired = ", paired),
             paste0("First year of the simulation = ", init.year),
             paste0("Years to project = ", yearstoproject),
             paste0("Fertility assumption = ", Fertility.Assumption),
             paste0("ageL = ", ageL),
             paste0("ageH = ", ageH),
             paste0("cvd.lag = ", cvd.lag),
             paste0("cancer.lag = ", cancer.lag),
             paste0("diseases = ", diseasestoexclude),
             paste0("CHD annual fatality improvement = ", fatality.annual.improvement.chd/100),
             paste0("Stroke annual fatality improvement = ", fatality.annual.improvement.stroke/100),
             paste0("Gastric cancer annual fatality improvement = ", fatality.annual.improvement.c16/100),
             paste0("Lung cancer annual fatality improvement = ", fatality.annual.improvement.c34/100),
             paste0("CHD fatality gradient = ", fatality.sec.gradient.chd /100),
             paste0("Stroke fatality gradient = ", fatality.sec.gradient.stroke/100),
             paste0("Gastric cancer fatality gradient = ", fatality.sec.gradient.c16/100),
             paste0("Lung cancer fatality gradient = ", fatality.sec.gradient.c34/100),
             paste0("Sample size = ", format(n, scientific = F)),
             paste0("Number of iterations = ", numberofiterations),
             paste0("Number of scenarios = ", n.scenarios) 
), 
fileOut)
close(fileOut)


# Load C++ function to summarise riskfactors ------------------------------
sourceCpp("functions.cpp")

# Sample for parameter distributions --------------------------------------
cvd.lag <- (cvd.lag - 1)/9 # calculate p of binom for mean = user input cvd.lag
cancer.lag <- (cancer.lag - 1)/9 # because n*p is integer, mean = median
cvd.lag.l    <- 1L + rbinom(ifelse(paired, numberofiterations, it), 9, cvd.lag)
cancer.lag.l <- 1L + rbinom(ifelse(paired, numberofiterations, it), 9, cancer.lag)

lagtimes.dt <- data.table("cvd.lag" = cvd.lag.l, "cancer.lag" = cancer.lag.l)
if (file.exists("./Output/lagtimes.csv")) {
  write.table(lagtimes.dt, file = "./Output/lagtimes.csv", append = T, 
              row.names=F, col.names=F,  sep=",")
} else write.csv(lagtimes.dt, file = "./Output/lagtimes.csv", row.names=F)
# TODO Above will not work if paired = F. I will have to move later after  


load(file="./Lagtimes/salt.rq.coef.rda")
if (paired == T) { 
  salt.rq.coef <- sample(salt.rq.coef, numberofiterations, T)
} else {
  salt.rq.coef <- sample(salt.rq.coef, it, T)
}

# Load RA incid
RAincid.rr.l <- setkey(
  fread(
    "./Lagtimes/RAincid.csv", 
    stringsAsFactors = F, 
    colClasses = c("numeric", "factor", 
                   "numeric")
  ),
  age, sex, rr
)


# Scenario specific uncertainties ------------------------------------------

# atorvastatin effect
# from Law MR, et al. Quantifying effect of statins on low density lipoprotein cholesterol, 
# ischaemic heart disease, and stroke: systematic review and meta-analysis. BMJ 2003;326:1423. 
# table 2. 43% (0.3958 - 0.46875) reduction of ldl. to convert to tc, tc/ldl = 0.27/0.36 from
# Edwards JE, et al. Statins in hypercholesterolaemia: A dose-specific meta-analysis of lipid 
# changes in randomised, double blind trials. BMC Family Practice 2003;4:18. 

atorv.eff.l <- 
  rnorm(ifelse(paired, numberofiterations, it),
        0.27*0.43/0.36,
        (0.27*0.46875/0.36 - 0.27*0.3958/0.36)/(2*1.96))

smoking.decr.l <- 
  rpert(ifelse(paired, numberofiterations, it),
        0.05, 0.13, 0.14, 4)

bmi.rate.decr.l <- 
  rpert(ifelse(paired, numberofiterations, it),
        0.98, 0.99, 1, 4)
sbp.decr.l <- 
  rpert(ifelse(paired, numberofiterations, it),
        0.18, 0.81, 1.10, 4)
fv.decr.l <- 
  rpert(ifelse(paired, numberofiterations, it),
        0.2, 0.5, 0.8, 4)

persistence.l <- 
  rpert(ifelse(paired, numberofiterations, it),
        0.5, 0.8, 1, 4)

p1.l <- 
  rpert(ifelse(paired, numberofiterations, it), 
        0.2, 0.25, 0.3, 4) # proportion of 10% to 20% risk
p2.l <- 
  rpert(ifelse(paired, numberofiterations, it),
        0.04, 0.05, 0.10, 4)# proportion of more than 20% risk

# CHD parameters ----------------------------------------------------------
if ("CHD" %in% diseasestoexclude) {
  chd.ets.rr.l <-  stochRRabov1(ifelse(paired, numberofiterations, it), 1.26, 1.38)
  chd.fv.rr.l <- stochRRbelow1(ifelse(paired, numberofiterations, it), 0.96, 0.99)
  
  chd.tobacco.rr.l  <- setkey(
    fread(
      "./CVD Statistics/tobacco.rrchd.csv", 
      stringsAsFactors = F, 
      colClasses = c("factor", "factor",
                     "factor", "numeric",
                     "numeric")
    ),
    agegroup, sex, cigst1.cvdlag
  )
  
  chd.tobacco.rr.l <- clonedt(chd.tobacco.rr.l)
  chd.tobacco.rr.l[, rr := stochRRtabl(mean.rr, ci.rr, .id), by = .id]
  chd.tobacco.rr.l[is.na(rr) | rr < 1, rr := 1]
  tt <- setDT(expand.grid(age = ageL:100, sex = 1:2, cigst1.cvdlag = 3:4))
  tt[, `:=` (agegroup = as.character(agegroup.fn(age)),
             sex = as.character(sex), 
             cigst1.cvdlag = as.character(cigst1.cvdlag))]
  chd.tobacco.rr.l <- tt[chd.tobacco.rr.l, on = c("agegroup", "sex", "cigst1.cvdlag"), allow.cartesian = T]
  chd.tobacco.rr.l[age > 69 & cigst1.cvdlag == "3",
                   rr := rr * (1-(age-69)/100)] # decrease risk for elderly
  chd.tobacco.rr.l[, rr := predict(loess(rr~age, span=0.75)), by = .(.id, sex, cigst1.cvdlag)]
  chd.tobacco.rr.l[rr < 1, rr := 1]
  #chd.tobacco.rr.l[sex == "1" & cigst1.cvdlag == "3", plot(age, rr)]
  #chd.tobacco.rr.l[sex == "1" & cigst1.cvdlag == "3" & .id == 1, lines(age, rr.sm)]
  
  chd.sbp.rr.l <- setkey(
    fread(
      "./CVD Statistics/sbp.rrchd.csv", 
      stringsAsFactors = F, 
      colClasses = c("factor", "factor",
                     "numeric", "numeric")
    ),
    agegroup, sex
  )
  
  chd.sbp.rr.l <- clonedt(chd.sbp.rr.l)
  chd.sbp.rr.l[, rr := stochRRtabl(mean.rr, ci.rr, .id), by = .id]
  chd.sbp.rr.l[is.na(rr) | rr > 1, rr := 1]
  tt <- setDT(expand.grid(age = ageL:ageH, sex = 1:2))
  tt[, `:=` (agegroup = as.character(agegroup.fn(age)),
             sex = as.character(sex))]
  chd.sbp.rr.l <- tt[chd.sbp.rr.l, on = c("agegroup", "sex"), allow.cartesian = T]
  chd.sbp.rr.l[, rr := predict(loess(rr~age, span=0.75)), by = .(.id, sex)]
  chd.sbp.rr.l[rr > 1, rr := 1]
  #chd.sbp.rr.l[sex == "1", plot(age, rr)]
  #chd.sbp.rr.l[sex == "1" & .id == 1, lines(age, rr.sm)]
  
  chd.chol.rr.l <- setkey(
    fread(
      "./CVD Statistics/chol.rrchd.csv", 
      stringsAsFactors = F, 
      colClasses = c("factor", "numeric",
                     "numeric")
    ),
    agegroup
  )
  
  chd.chol.rr.l <- clonedt(chd.chol.rr.l)
  chd.chol.rr.l[, rr := stochRRtabl(mean.rr, ci.rr, .id), by = .id]
  chd.chol.rr.l[is.na(rr) | rr > 1, rr := 1]
  tt <- setDT(expand.grid(age = ageL:ageH))
  tt[, `:=` (agegroup = as.character(agegroup.fn(age)))]
  chd.chol.rr.l <- tt[chd.chol.rr.l, on = c("agegroup"), allow.cartesian = T]
  chd.chol.rr.l[, rr := predict(loess(rr~age, span=0.75)), by = .(.id)]
  chd.sbp.rr.l[rr > 1, rr := 1]
  #chd.chol.rr.l[, plot(age, rr)]
  #chd.chol.rr.l[.id == 1, lines(age, rr.sm)]
  
  chd.bmi.rr.l <-setkey(
    fread(
      "./CVD Statistics/bmi.rrchd.csv", 
      stringsAsFactors = F, 
      colClasses = c("factor",
                     "numeric", "numeric")
    ),
    agegroup
  )
  
  chd.bmi.rr.l <- clonedt(chd.bmi.rr.l)
  chd.bmi.rr.l[, rr := stochRRtabl(mean.rr, ci.rr, .id), by = .id]
  chd.bmi.rr.l[is.na(rr) | rr < 1, rr := 1]
  tt <- setDT(expand.grid(age = 0:100))
  tt[, `:=` (agegroup = as.character(agegroup.fn(age)))]
  chd.bmi.rr.l <- tt[chd.bmi.rr.l, on = c("agegroup"), allow.cartesian = T]
  chd.bmi.rr.l[, rr := predict(loess(rr~age, span=0.75)), by = .(.id)]
  chd.bmi.rr.l[rr < 1, rr := 1]
  #chd.bmi.rr.l[, plot(age, rr)]
  #chd.bmi.rr.l[.id == 1, lines(age, rr.sm)]
  
  chd.diab.rr.l <-setkey(
    fread(
      "./CVD Statistics/diab.rrchd.csv", 
      stringsAsFactors = F, 
      colClasses = c("factor", "factor",
                     "numeric", "numeric")
    ),
    agegroup, diabtotr.cvdlag
  )
  
  chd.diab.rr.l <- clonedt(chd.diab.rr.l)
  chd.diab.rr.l[, rr := stochRRtabl(mean.rr, ci.rr, .id), by = .id]
  chd.diab.rr.l[is.na(rr) | rr < 1, rr := 1]
  tt <- setDT(expand.grid(age = 0:100, diabtotr.cvdlag = 1:2))
  tt[, `:=` (agegroup = as.character(agegroup.fn(age)),
             diabtotr.cvdlag = as.character(diabtotr.cvdlag))]
  chd.diab.rr.l <- tt[chd.diab.rr.l, on = c("agegroup", "diabtotr.cvdlag"), allow.cartesian = T]
  chd.diab.rr.l[diabtotr.cvdlag == "2", rr := predict(loess(rr~age, span=0.75)), by = .(.id)]
  chd.diab.rr.l[rr < 1, rr := 1]
  #chd.diab.rr.l[, plot(age, rr)]
  #chd.diab.rr.l[.id == 1, lines(age, rr.sm)]
  
  chd.pa.rr.l <- setkey(
    fread(
      "./CVD Statistics/pa.rrchd.csv", 
      stringsAsFactors = F, 
      colClasses = c("factor", "integer",
                     "numeric", "numeric")
    ),
    agegroup, a30to06m.cvdlag
  )
  
  chd.pa.rr.l <- clonedt(chd.pa.rr.l)
  chd.pa.rr.l[, rr := stochRRtabl(mean.rr, ci.rr, .id), by = .id]
  chd.pa.rr.l[is.na(rr) | rr < 1, rr := 1]
  tt <- setDT(expand.grid(age = 0:100, a30to06m.cvdlag = 0:4))
  tt[, `:=` (agegroup = as.character(agegroup.fn(age)))]
  chd.pa.rr.l  <- tt[ chd.pa.rr.l , on = c("agegroup", "a30to06m.cvdlag"), allow.cartesian = T]
  chd.pa.rr.l [, rr := predict(loess(rr~age, span=0.75)), by = .(.id, a30to06m.cvdlag)]
  chd.pa.rr.l [rr < 1, rr := 1]
  # chd.pa.rr.l[a30to06m.cvdlag == 0, plot(age, rr)]
  # chd.pa.rr.l [.id == 1 & a30to06m.cvdlag == 0, lines(age, rr.sm)]
}


# Stroke parameters -------------------------------------------------------
if ("stroke" %in% diseasestoexclude) {
  stroke.ets.rr.l <- stochRRabov1(ifelse(paired, numberofiterations, it), 1.25, 1.38)
  stroke.fv.rr.l <- stochRRbelow1(ifelse(paired, numberofiterations, it), 0.95, 0.97)
  
  stroke.tobacco.rr.l <- setkey(
    fread(
      "./CVD Statistics/tobacco.rrstroke.csv", 
      stringsAsFactors = F, 
      colClasses = c("factor", "factor", 
                     "factor", "numeric",
                     "numeric")
    ),
    agegroup, sex, cigst1.cvdlag
  )
  stroke.tobacco.rr.l <- clonedt(stroke.tobacco.rr.l)
  stroke.tobacco.rr.l[, rr := stochRRtabl(mean.rr, ci.rr, .id), by = .id]
  stroke.tobacco.rr.l[is.na(rr) | rr < 1, rr := 1]
  tt <- setDT(expand.grid(age = ageL:100, sex = 1:2, cigst1.cvdlag = 3:4))
  tt[, `:=` (agegroup = as.character(agegroup.fn(age)),
             sex = as.character(sex), 
             cigst1.cvdlag = as.character(cigst1.cvdlag))]
  stroke.tobacco.rr.l <- tt[stroke.tobacco.rr.l, on = c("agegroup", "sex", "cigst1.cvdlag"), allow.cartesian = T]
  stroke.tobacco.rr.l[age > 69 & cigst1.cvdlag == "3",
                      rr := rr * (1-(age-69)/100)] # decrease risk for elderly
  stroke.tobacco.rr.l[, rr := predict(loess(rr~age, span=0.75)), by = .(.id, sex, cigst1.cvdlag)]
  stroke.tobacco.rr.l[rr < 1, rr := 1]
  #stroke.tobacco.rr.l[sex == "1" & cigst1.cvdlag == "4", plot(age, rr)]
  #stroke.tobacco.rr.l[sex == "1" & cigst1.cvdlag == "4" & .id == 1, lines(age, rr.sm)]
  
  stroke.sbp.rr.l <- setkey(
    fread(
      "./CVD Statistics/sbp.rrstroke.csv", 
      stringsAsFactors = F, 
      colClasses = c("factor", "factor",
                     "numeric", "numeric")
    ),
    agegroup, sex
  )
  stroke.sbp.rr.l <- clonedt(stroke.sbp.rr.l)
  stroke.sbp.rr.l[, rr := stochRRtabl(mean.rr, ci.rr, .id), by = .id]
  stroke.sbp.rr.l[is.na(rr) | rr > 1, rr := 1]
  tt <- setDT(expand.grid(age = ageL:ageH, sex = 1:2))
  tt[, `:=` (agegroup = as.character(agegroup.fn(age)),
             sex = as.character(sex))]
  stroke.sbp.rr.l <- tt[stroke.sbp.rr.l, on = c("agegroup", "sex"), allow.cartesian = T]
  stroke.sbp.rr.l[, rr := predict(loess(rr~age, span=0.75)), by = .(.id, sex)]
  stroke.sbp.rr.l[rr > 1, rr := 1]
  #stroke.sbp.rr.l[sex == "1", plot(age, rr)]
  #stroke.sbp.rr.l[sex == "1" & .id == 1, lines(age, rr.sm)]
  
  
  stroke.chol.rr.l <- setkey(
    fread(
      "./CVD Statistics/chol.rrstroke.csv", 
      stringsAsFactors = F, 
      colClasses = c("factor", "numeric", 
                     "numeric")
    ),
    agegroup
  )
  stroke.chol.rr.l <- clonedt(stroke.chol.rr.l)
  stroke.chol.rr.l[, rr := stochRRtabl(mean.rr, ci.rr, .id), by = .id]
  stroke.chol.rr.l[is.na(rr) | rr > 1, rr := 1]
  tt <- setDT(expand.grid(age = ageL:ageH))
  tt[, `:=` (agegroup = as.character(agegroup.fn(age)))]
  stroke.chol.rr.l <- tt[stroke.chol.rr.l, on = c("agegroup"), allow.cartesian = T]
  stroke.chol.rr.l[, rr := predict(loess(rr~age, span=0.75)), by = .(.id)]
  stroke.sbp.rr.l[rr > 1, rr := 1]
  #stroke.chol.rr.l[, plot(age, rr)]
  #stroke.chol.rr.l[.id == 1, lines(age, rr.sm)]
  
  stroke.pa.rr.l <- setkey(
    fread(
      "./CVD Statistics/pa.rrstroke.csv", 
      stringsAsFactors = F, 
      colClasses = c("factor", "integer",
                     "numeric", "numeric")
    ),
    agegroup, a30to06m.cvdlag
  )
  stroke.pa.rr.l <- clonedt(stroke.pa.rr.l)
  stroke.pa.rr.l[, rr := stochRRtabl(mean.rr, ci.rr, .id), by = .id]
  stroke.pa.rr.l[is.na(rr) | rr < 1, rr := 1]
  tt <- setDT(expand.grid(age = 0:100, a30to06m.cvdlag = 0:4))
  tt[, `:=` (agegroup = as.character(agegroup.fn(age)))]
  stroke.pa.rr.l  <- tt[ stroke.pa.rr.l , on = c("agegroup", "a30to06m.cvdlag"), allow.cartesian = T]
  stroke.pa.rr.l [, rr := predict(loess(rr~age, span=0.75)), by = .(.id, a30to06m.cvdlag)]
  stroke.pa.rr.l [rr < 1, rr := 1]
  # stroke.pa.rr.l[a30to06m.cvdlag == 0, plot(age, rr)]
  # stroke.pa.rr.l [.id == 1 & a30to06m.cvdlag == 0, lines(age, rr.sm)]
  
  stroke.bmi.rr.l <- setkey(
    fread(
      "./CVD Statistics/bmi.rrstroke.csv", 
      stringsAsFactors = F, 
      colClasses = c("factor",
                     "numeric", "numeric")
    ),
    agegroup
  )
  
  stroke.bmi.rr.l <- clonedt(stroke.bmi.rr.l)
  stroke.bmi.rr.l[, rr := stochRRtabl(mean.rr, ci.rr, .id), by = .id]
  stroke.bmi.rr.l[is.na(rr) | rr < 1, rr := 1]
  tt <- setDT(expand.grid(age = 0:100))
  tt[, `:=` (agegroup = as.character(agegroup.fn(age)))]
  stroke.bmi.rr.l <- tt[stroke.bmi.rr.l, on = c("agegroup"), allow.cartesian = T]
  stroke.bmi.rr.l[, rr := predict(loess(rr~age, span=0.75)), by = .(.id)]
  stroke.bmi.rr.l[rr < 1, rr := 1]
  #stroke.bmi.rr.l[, plot(age, rr)]
  #stroke.bmi.rr.l[.id == 1, lines(age, rr.sm)]
  
  stroke.diab.rr.l <- setkey(
    fread(
      "./CVD Statistics/diab.rrstroke.csv", 
      stringsAsFactors = F, 
      colClasses = c("factor", "factor",
                     "numeric", "numeric")
    ),
    agegroup, diabtotr.cvdlag
  )
  
  stroke.diab.rr.l <- clonedt(stroke.diab.rr.l)
  stroke.diab.rr.l[, rr := stochRRtabl(mean.rr, ci.rr, .id), by = .id]
  stroke.diab.rr.l[is.na(rr) | rr < 1, rr := 1]
  tt <- setDT(expand.grid(age = 0:100, diabtotr.cvdlag = 1:2))
  tt[, `:=` (agegroup = as.character(agegroup.fn(age)),
             diabtotr.cvdlag = as.character(diabtotr.cvdlag))]
  stroke.diab.rr.l <- tt[stroke.diab.rr.l, on = c("agegroup", "diabtotr.cvdlag"), allow.cartesian = T]
  stroke.diab.rr.l[diabtotr.cvdlag == "2", rr := predict(loess(rr~age, span=0.75)), by = .(.id)]
  stroke.diab.rr.l[rr < 1, rr := 1]
  #stroke.diab.rr.l[, plot(age, rr)]
  #stroke.diab.rr.l[.id == 1, lines(age, rr.sm)]
}

# Lung cancer parameters -----------------------------------------------
if ("C34" %in% diseasestoexclude) {
  c34.ets.rr.mc.l <- stochRRabov1(ifelse(paired, numberofiterations, it), 1.33, 1.54)
  c34.fv.rr.mc.l <- stochRRbelow1(ifelse(paired, numberofiterations, it), 0.96^0.8, 0.98^0.8)
  # ^0.8 to adjust risk of reported 100gr servings to 80gr UK servings
}

# Gastric cancer parameters -----------------------------------------------
if ("C16" %in% diseasestoexclude) {
  c16.salt.optim.l <- rpert(ifelse(paired, numberofiterations, it), 614*2.5/1000, 1500*2.5/1000, 2391*2.5/1000, 4) # optimal level for salt around 4 g/day. Under which no risk from mozaffarian NEJM
  c16.salt.mr.l <- rpert(ifelse(paired, numberofiterations, it), 5.8, 6, 7, 4) # optimal level for salt around 4 g/day. Under which no risk
  #c16.tob.rr.mc.l <- stochRRabov1(ifelse(paired, numberofiterations, it), 1.04, 1.01)
  c16.tob.rr.mc.l <-rtruncnorm(ifelse(paired, numberofiterations, it),
                               0, Inf, 1.017144, 0.0009138235) # see gastric model.R for explanation
  c16.extob.rr.mc.l <- stochRRbelow1(ifelse(paired, numberofiterations, it), 0.961, 1)
  c16.bmi.rr.mc.l <- runif(ifelse(paired, numberofiterations, it))
  
  # From Continuous Update Project Report: Diet, 
  # Nutrition, Physical Activity and Stomach Cancer. 2016
  # figure/table 8, p38
  # bmi <- c(21.7, 23.45, 26.2, 28.7, 32)
  # rr <- c(1, 1.02, 1.13, 1.32, 1.68)
  # rr.lci <- c(1, 1, 1.08, 1.24, 1.54)
  # rr.uci <- c(1, 1.04, 1.18, 1.4, 1.84)
  
  # Fit exp regression to get a function of rr by bmi
  # mm <- glm(rr~exp(bmi/5))
  # Fit polynom to get a function of se of rr, by bmi
  # mm.se <- lm(I(rr.uci-rr.lci)~I(bmi^4))
  
  # gives the rr by bmi. The uncertainty comes from the differnt quantiles that are 
  # used in each iteration.
  c16.bmi.rr.fn <- cmpfun(
    function(bmi, quantil) {
      bmi[bmi > 35] <- 35
      rr <- 0.889359 + 0.001323 * exp(bmi/5) # gives the mean from mm
      se <- (-7.253e-02 + 3.536e-07 * bmi^4)/3.92 # gives the se from mm.se
      return(qnorm(quantil, rr, se))
    }
  )
  
  c16.fv.rr.mc.l <- setkey(
    fread(
      "./Cancer Statistics/fv.rrc16.csv", 
      stringsAsFactors = F, 
      colClasses = c("factor", "numeric",
                     "numeric")
    ),
    agegroup
  )
  c16.fv.rr.mc.l <- clonedt(c16.fv.rr.mc.l)
  c16.fv.rr.mc.l[, rr := stochRRtabl(mean.rr, ci.rr, .id), by = .id]
  c16.fv.rr.mc.l[is.na(rr) | rr > 1, rr := 1]
  tt <- setDT(expand.grid(age = 0:100))
  tt[, `:=` (agegroup = as.character(agegroup.fn(age)))]
  c16.fv.rr.mc.l <- tt[c16.fv.rr.mc.l, on = c("agegroup"), allow.cartesian = T]
  c16.fv.rr.mc.l[, rr := predict(loess(rr~age, span=0.75)), by = .(.id)]
  c16.fv.rr.mc.l[rr > 1, rr := 1]
  #c16.fv.rr.mc.l[, plot(age, rr)]
  #c16.fv.rr.mc.l[.id == 1, lines(age, rr.sm)]
  
  c16.salt.rr.mc.l <- setkey(
    fread(
      "./Cancer Statistics/salt.rrc16.csv", 
      stringsAsFactors = F, 
      colClasses = c("factor", "numeric",
                     "numeric")
    ),
    agegroup
  )
  c16.salt.rr.mc.l <- clonedt(c16.salt.rr.mc.l)
  c16.salt.rr.mc.l[, rr := stochRRtabl(mean.rr, ci.rr, .id), by = .id]
  c16.salt.rr.mc.l[is.na(rr) | rr < 1, rr := 1]
  tt <- setDT(expand.grid(age = 0:100))
  tt[, `:=` (agegroup = as.character(agegroup.fn(age)))]
  c16.salt.rr.mc.l <- tt[c16.salt.rr.mc.l, on = c("agegroup"), allow.cartesian = T]
  c16.salt.rr.mc.l[, rr := predict(loess(rr~age, span=0.75)), by = .(.id)]
  c16.salt.rr.mc.l[is.na(rr) | rr < 1, rr := 1]
  #c16.salt.rr.mc.l[, plot(age, rr)]
  #c16.salt.rr.mc.l[.id == 1, lines(age, rr.sm)]
}


# SPOP load order --------------------------------------------------------------
if (init.year == 2006) {
  random.spop.file <- list.files("./SynthPop", 
                                 pattern = glob2rx("spop050607*.rds"), 
                                 full.names = T) # pick a random file from the available population files
} 
# if (init.year == 2006 & widesynthpop == F) {
#   random.spop.file <- list.files("./SynthPop", 
#                                  pattern = glob2rx("spop2006*.rds"), 
#                                  full.names = T) # pick a random file from the available population files
# }
if (init.year == 2011) {
  random.spop.file <- list.files("./SynthPop", 
                                 pattern = glob2rx("spop2011*.rds"), 
                                 full.names = T) # pick a random file from the available population files
}

random.spop.file <- sample(random.spop.file, ifelse(paired, numberofiterations, it), T)

# Set seed to ensure same sample and rng streams per scenario per iterations
seed <- sample(1e6:1e7, ifelse(paired, numberofiterations, it), F) 

cat(paste0(seed), "\n", 
    file = "./Output/seed temp.txt",
    append = F)

rm(n.scenarios, lagtimes.dt)


