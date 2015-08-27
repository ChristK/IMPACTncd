#cmpfile("./initialisation.R")
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
               "truncnorm", 
               "stringr", 
               "reshape2", 
               "compiler",
               "survey",
               "ggplot2",
               "randtoolbox",
               "doParallel",
               "doRNG",
               "foreach",
               "quantreg",
               "Rcpp",
               "data.table", 
               "dplyr"))

enableJIT(0) #set to 1,2 or 3 to enable different precompiling levels

options(survey.lonely.psu = "adjust") #Lonely PSU (center any single-PSU strata around the sample grand mean rather than the stratum mean)
# require(devtools)
# install_github("Rdatatable/data.table",  build_vignettes = F)

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

# define function for stochastic RR
stochRRabov1 <- cmpfun(function(n = .N, m, ci) { # lognormal
  rr <- exp(rtruncnorm(n, 0, Inf, log(m), abs(log(m) - log(ci))/1.96))
  return(rr)  
}
)

stochRRbelow1 <- cmpfun(function(n = .N, m, ci) { # lognormal
  rr <- exp(rtruncnorm(n, -Inf, 0, log(m), abs(log(m) - log(ci))/1.96))
  return(rr)  
}
)

stochRRnorm <- cmpfun(function(n = .N, m, ci) { # normal distr
  if (m < 1) {
    a = 0
    b = 1
  } else {
    a = 1
    b = Inf
  }
  ifelse(m == ci, rr <- rep(m, n), rr <- rtruncnorm(n = n, a = a, b = b, mean = m, sd = abs(m - ci)/1.96))
  return(rr)  
}
)

# function to calculate mortality based on 1st and 5th year survival
hyperbola <- cmpfun(function(y1, y5, x) {
  b = (5 * y5 - y1)/4
  a = y1 - b
  y = b + a/x
  return(y)
}
)

# function to calculate mortality based on 1st and 5th year survival based on Weibull distribution
# hazard h(t) = l*g*t^(g-1)
# cummulative hazard H(t) = l*t^g
# survival function S(t) = exp(-l*t^g)
# from ! and 5 year net survival rates I solve S(t) to find l and g
survival.fn <- cmpfun(function(y1, y5, t) {
  l = -log(y1)
  g = log(-log(y5)/l ,5)
  return(exp(-l*t^g)) # returns pct of survivors at time t
  #return(l*g*t^(g-1)) # returns the rate of death at time t
  #return(l*t^g) # returns the cumulative hazard of death at time t
}
)

fatality.rt <- cmpfun(function(y1, y5, t=1) {
  l = -log(y1)
  g = log(-log(y5)/l ,5)
  #return(exp(-l*t^g)) # returns pct of survivors at time t
  return(l*g*t^(g-1)) # returns the rate of death at time t
  #return(l*t^g) # returns the cumulative hazard of death at time t
}
)
#plot(survival.fn(0.55675983428955, 0.35348030090332, 0:20), x=0:20, ylim=c(0,1))

# Define function for sampling. Taken from sample man pages 
resample <- cmpfun(function(x, ...) {
  x <- na.omit(x)
  x[sample.int(length(x), ...)]
}
)

# Define operator %!in%, meaning !%in%
'%!in%' <- cmpfun(function(x,y)!('%in%'(x,y)))

# Define outersect. Like setdiff but symmetrical. I.e. setdiff(a,b) is not the same as setdiff(b,a). outersect solve this by calculating both
outersect <- cmpfun(function(x, y, ...) {
  big.vec <- c(x, y, ...)
  duplicates <- big.vec[duplicated(big.vec)]
  setdiff(big.vec, unique(duplicates))
}
)

# Define function to split agegroups and create groups
agegroup.fn <- cmpfun(
  function(x, lagtime = 0) {
    breaks                   <- c(0, 1, seq(5, 85, 5), Inf)
    labels                   <- c("<1   ", "01-04", "05-09",
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
agegroup.part <- cmpfun(function(x, lagtime = 0) {
  breaks                   <- c(seq(20, 85, 5), Inf)
  labels                   <- c("20-24", "25-29", "30-34", 
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
clear.labels <- function(x) {
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

# truncate a distr (from mc2d)
rtrunc <- function (distr = runif, n, linf = -Inf, lsup = Inf, ...) 
{
  # linf <- as.vector(linf)
  # lsup <- as.vector(lsup)
  if (!is.character(distr)) 
    distr <- as.character(match.call()$distr)
  distr <- substr(distr, 2, 1000)
  if (any(linf >= lsup)) 
    stop("linf should be < lsup")
  pfun <- get(paste("p", distr, sep = ""), mode = "function")
  pinf <- as.vector(pfun(q = linf, ...))
  psup <- as.vector(pfun(q = lsup, ...))
  p <- runif(n, min = pinf, max = psup)
  qfun <- get(paste("q", distr, sep = ""), mode = "function")
  res <- as.vector(qfun(p, ...))
  res[pinf <= 0 & res > lsup] <- NaN
  res[psup >= 1 & res < linf] <- NaN
  res[is.na(linf) | is.na(lsup)] <- NaN
  if (any(res <= linf | res > lsup, na.rm = TRUE)) 
    stop("Error in rtrunc: some values are not in the expected range (maybe due to rounding errors)")
  if (isTRUE(all.equal(pinf, 1)) | isTRUE(all.equal(psup, 0))) 
    warning("Warning: check the results from rtrunc. It may have reached rounding errors")
  return(res)
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
# hist(rtrunc(rnorm, 1000, mean=0, sd=1, -10, 10))

# Define function for percentile rank (dplyr provides similar functions)
# perc.rank <- function(x) rank(x,  ties.method = "random")/length(x)



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


# Find and load scenarios
if (!exists("scenarios.list")) { # not if defined by GUI
  scenarios.list <- list.files(path = "./Scenarios", pattern = glob2rx("*.R"), full.names = F, recursive = F)
}

n.scenarios <- length(scenarios.list)
scenarios.list <- rep(scenarios.list, each = numberofiterations)

it <- numberofiterations * n.scenarios

# specify output.txt file for simulation parameters
dir.create(path = "./Output/", recursive = T, showWarnings = F)
fileOut <- file(paste0("./Output/simulation parameters temp.txt"))
writeLines(c("IMPACTncd\nA dynamic microsimulation, by Dr Chris Kypridemos", "\n", 
             paste0("Simulation started at: ", Sys.time(), "\n"),
             "Simulation parameters:\n",
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
             paste0("CHD fatality gradient = ", fatality.sec.gradient.chd /100),
             paste0("Stroke fatality gradient = ", fatality.sec.gradient.stroke/100),
             paste0("Gastric cancer fatality gradient = ", fatality.sec.gradient.c16/100),
             paste0("Sample size = ", format(n, scientific = F)),
             paste0("Number of iterations = ", numberofiterations),
             paste0("Number of scenarios = ", n.scenarios) 
             ), 
           fileOut)
close(fileOut)
# Load C++ function to summarise riskfactors
sourceCpp("functions.cpp")

rm(Fertility.Assumption, n.scenarios)


