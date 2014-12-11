#!/opt/gridware/apps/gcc/R/3.1.0/bin/Rscript
#!/usr/bin/Rscript
# User input

require("RGtk2")
# needs sudo apt-get install libgtk2.0-dev before install in linux
require("RGtk2Extras")

input.fn <- function(yearstoproject=10,
                     ageL= 30, 
                     ageH = 84,
                     diseasestoexclude, 
                     init.year = 2011, 
                     n = 1000,
                     cvd.lag = 5, 
                     cancer.lag = 10, 
                     clusternumber = 4)
 {
  if (ageL > ageH) {
    tt <- ageL
    ageH <- ageL
    ageH <- tt 
  }
  return(list(yearstoproject = yearstoproject, 
              ageL = ageL, 
              ageH = ageH, 
              init.year = init.year,
              diseasestoexclude = diseasestoexclude,
              cvd.lag = cvd.lag,
              cancer.lag = cancer.lag,
              n = n,
              clusternumber = clusternumber)
  )
}


.input.fn.dialog = list(
  title = "IMPACTncd by Chris Kypridemos",
  label = "Input Parameters for Existing Results",
  yearstoproject.rangeItem = c(value=30, from=1, to=60, by=1), 
  label = "Forecast horizon",
  ageL.rangeItem = c(value=30, from=30, to=84, by=1), 
  label = "Define lower age limit for the diseases-model simulation",
  ageH.rangeItem = c(value=84, from=30, to=84, by=1), 
  label = "Define upper age limit for the diseases-model simulation",
  n.integerItem = c(value=1000000, from=100000, to=2000000, by=100000),
  label = "Define the sample size",
  cvd.lag.rangeItem = c(value=5, from=1, to=10, by=1),
  label = "Define time lag for CHD and stroke (in years)",
  cancer.lag.rangeItem = c(value=10, from=1, to=10, by=1),
  label = "Define time lag for cancers (in years)",
  
  BREAK = T,
  
  diseasestoexclude.variableSelectorItem =  c("CHD", "stroke", "lung cancer"),
  label = "Define diseases to be included in the simulation",
  

  BREAK = T,
  
  init.year.integerItem = c(value=2011, from=2001, to=2020, by=1),
  label = "Define year to start the simulation",
  tooltip = "Use years other than 2011 with caution",
  clusternumber.integerItem = c(value=70, from=1, to=80, by=1),
  label = "Define number of cores", 
  tooltip = "Each core needs about 3Gb of ram"
)

run.dialog(input.fn)   

list2env(input_fn_output, envir = .GlobalEnv)
# *************************************************************************************************

cat("Initialising IMPACTncd...\n\n")
options(warn = 1)

if (Sys.info()[1] == "Linux") {
  if (system("whoami", T )== "mdxasck2") {
    setwd("~/IMPACTncd/")
    clusternumber <- ifelse (clusternumber<70, 70, clusternumber)  # overwrites previous if <60
  } else {
    setwd(paste("/home/", 
                system("whoami", T), 
                "/Dropbox/PhD/Models/IMPACTncd/", 
                sep = "", 
                collapse = ""))
  }
} else {
  get.dropbox.folder <- function() {
    if (!require(RCurl)) 
      stop("You need to install RCurl package.")
    if (Sys.info()["sysname"] != "Windows") 
      stop("Currently, 'get.dropbox.folder' works for Windows and Linux only. Sorry.")
    db.file <- paste(Sys.getenv("APPDATA"), "\\Dropbox\\host.db", sep = "")
    base64coded <- readLines(db.file, warn = F)[2]
    base64(base64coded, encode = F)
  }
  setwd(paste0(get.dropbox.folder(), "/PhD/Models/IMPACTncd/"))
}

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
dependencies(c("data.table", 
               "dplyr",
               "demography", 
               "truncnorm", 
               "stringr", 
               "reshape2", 
               "compiler",
               "survey",
               "ggplot2",
               "randtoolbox",
               "doParallel",
               "doRNG",
               "foreach"))

enableJIT(1) #set to 1,2 or 3 to enable different precompiling levels

options(survey.lonely.psu = "adjust") #Lonely PSU (center any single-PSU strata around the sample grand mean rather than the stratum mean)
#require(devtools)
#install_github("Rdatatable/data.table",  build_vignettes = F)
# OR install_local("~/R/data.table-master.zip") #after manually download from github

# max projection horizon (limited by fertility)
if (init.year + yearstoproject > 2061) yearstoproject <- 2061 - init.year

# Define end() function to beep end print a message
end <- function(...) {
  cat("All done! \a\n")
  sink(file = "./Output/simulation parameters.txt",
       append = T, 
       type = "output",
       split = F)
  cat(paste0("Simulation ended succesfuly at: ", Sys.time(), "\n"))
  sink()
  if (Sys.info()[1] == "Windows") {
    system("rundll32 user32.dll,MessageBeep -1")
    Sys.sleep(.5)
  }
}

# Function for timing log
time.mark <- function(x) {
  sink(file = "./Output/simulation parameters.txt",
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
dice <- function(n = .N) runif(n)

# define function for stochastic RR
stochRR <- function(n = .N, m, ci) { # lognormal
  if (m < 1) {
    a = -Inf
    b = 0
  } else {
    a = 0
    b = Inf
  }
  ifelse(m == ci, rr <- rep(log(m), n), rr <- rtruncnorm(n = n, a = a, b = b, mean = log(m), sd = abs(log(m) - log(ci))/1.96))
  return(exp(rr))  
}

stochRRnorm <- function(n = .N, m, ci) { # normal distr
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

# function to calculate mortality based on 1st and 5th year survival
hyperbola <- function(y1, y5, x) {
  b = (5 * y5 - y1)/4
  a = y1 - b
  y = b + a/x
  return(y)
}
hyperbola <- cmpfun(hyperbola) # compiled version

# Define function for sampling. Taken from sample man pages 
resample <- function(x, ...) {
  x <- na.omit(x)
  x[sample.int(length(x), ...)]
}

# Define operator %!in%, meaning !%in%
'%!in%' <- function(x,y)!('%in%'(x,y))

# Define outersect. Like setdiff but symmetrical. I.e. setdiff(a,b) is not the same as setdiff(b,a). outersect solve this by calculating both
outersect <- function(x, y, ...) {
  big.vec <- c(x, y, ...)
  duplicates <- big.vec[duplicated(big.vec)]
  setdiff(big.vec, unique(duplicates))
}

# Define function to split agegroups and create groups
agegroup.fn <- function(x, lagtime = 0) {
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
      x[, group := as.factor(paste0(qimd, sex, agegroup))]
      return(invisible(x))
    } else return(print("only datatables and vectors are eligible inputs"))
  }
}

# Define function to split agegroups and create groups
agegroup.part <- function(x, lagtime = 0) {
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

# Define function for percentile rank (dplyr provides similar functions)
perc.rank <- function(x) rank(x,  ties.method = "random")/length(x)

# Define function to match continuous distributions of each group with the one in SPOP2011 to simulate ageing 
ageing.distr <- function(risk.factor) {
  temp <- SPOP2011[, c(risk.factor, "group"), with = F]
  nam <- paste0(risk.factor, ".rank")
  temp[, (nam) := percent_rank(get(risk.factor)), by = group]
  setkeyv(temp, c("group", nam))
  
  POP[, (nam) := percent_rank(get(risk.factor)), by = group]
  POP[, (risk.factor) := NULL]
  setkeyv(POP, c("group", nam))
  return(temp[POP, roll = "nearest"])
}
#example POP <- ageing.distr("bmival")


# Define function to export annual summaries of RF
pop.summ <-  function(N, ...) {
  return(list("year" = 2011 + i,
              "scenario" = gsub(".R", "", scenarios.list[[iterations]]),
              "mc" = haha,
              "pop" = N))
}

cont.summ <- function(rf, name, ...) {
  mylist <- list()
  mylist[[paste0(name, ".mean")]] <- mean(rf, na.rm = T)
  mylist[[paste0(name, ".sd")]] <- sd(rf, na.rm = T)
  #mylist[[paste0(name, ".median")]] <- median(rf, na.rm=T) # disabled to improve spead
  #mylist[[paste0(name, ".mad")]] <- mad(rf, na.rm=T)
  #mylist[[paste0(name, ".iqr")]] <- IQR(rf, na.rm=T)
  return(mylist)
}

cat.summ <- function(rf, name, ...) {
  absol <-summary(factor(rf, exclude = c(NA, NaN, "99"), ...))
  #pct <- prop.table(absol)
  absol <- absol[names(absol)!="NA's"]
  #ct <- pct[names(pct)!="NA's"]
  names(absol) <- paste0(name, ".", names(absol))
  #names(pct) <- paste0(name, ".", names(pct), ".pct")
  #return(as.list(c(absol, pct)))
  return(as.list(absol))
}

output.rf  <- function(x, ...) {
  with(x, return(c(pop.summ(nrow(x)),
                   cont.summ(bmival, "bmi"),
                   cont.summ(bmival.cvdlag, "bmi.cvd"),
                   cont.summ(bmival.calag, "bmi.ca"),
                   cont.summ(omsysval, "sbp"),
                   cont.summ(omsysval.cvdlag, "sbp.cvd"),
                   cont.summ(cholval, "tc"),
                   cont.summ(cholval.cvdlag, "tc.cvd"),
                   cat.summ(cigst1.cvdlag, "smok.cvd",
                            levels = 1:4, 
                            labels = c("never", "ex.2", "ex.3", "active")),
                   cat.summ(cigst1.calag, "smok.ca", 
                            levels = 1:4, 
                            labels = c("never", "ex.2", "ex.3", "active")),
                   cat.summ(porftvg.cvdlag, "fv.cvd", levels = 0:9),
                   cat.summ(porftvg.calag, "fv.ca", levels = 0:9),
                   cat.summ(frtpor.cvdlag, "fruit.cvd", levels = 0:9),
                   cat.summ(frtpor.calag, "fruit.ca", levels = 0:9),
                   cat.summ(diabtotr.cvdlag, "diab.cvd",
                            levels = 1:2, 
                            labels = c("no", "yes")),
                   cat.summ(expsmokCat, "ets",
                            levels = 0:1))))
}

output.chd  <- function(x, ...) {
  O1 <- pop.summ(nrow(x))
  O2 <- with(x, cat.summ(chd.incidence, "chd",levels = init.year + i, labels="incidence"))
  O3 <- with(x, sum(table(factor(chd.incidence, exclude = c(0, NA)))))
  names(O3) <- "chd.prevalence"
  O4 <- with(x, sum(table(dead, exclude=c(F, NA, NaN))))
  names(O4) <- "chd.mortality"
  return(c(O1, O2, O3, O4))
}

output.stroke  <- function(x, ...) {
  O1 <- pop.summ(nrow(x))
  O2 <- with(x, cat.summ(stroke.incidence, "stroke",levels = 2011 + i, labels="incidence"))
  O3 <- with(x, sum(table(factor(stroke.incidence, exclude = c(0, NA)))))
  names(O3) <- "stroke.prevalence"
  O4 <- with(x, sum(table(dead, exclude=c(F, NA, NaN))))
  names(O4) <- "stroke.mortality"
  return(c(O1, O2, O3, O4))
}

output.other  <- function(x, ...) {
  O1 <- pop.summ(nrow(x))
  O2 <- with(x, sum(table(dead, exclude=c(F, NA, NaN))))
  names(O2) <- "other.mortality"
  return(c(O1, O2))
}

# Match the sex and age structure of the initial year
population.actual <- fread("./Population/population.struct.csv",  header = T)[year == init.year, ]
population.actual[, pct := round(as.numeric(n) * pop / sum(pop))]

# Calculate the exact fraction of the mid 2010 population this sample represents
pop.fraction <- n / population.actual[, sum(pop)] # 53107200 is the total mid 2011 population of England (52642600 for 2010)

source(file = "./post simulation functions.R")
source(file = "./output.R")
