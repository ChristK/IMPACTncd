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

enableJIT(0) #set to 1,2 or 3 to enable different precompiling levels

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
dice <- cmpfun(function(n = .N) runif(n))

# define function for stochastic RR
stochRR <- cmpfun(function(n = .N, m, ci) { # lognormal
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
agegroup.fn <- cmpfun(function(x, lagtime = 0) {
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

# Define function for percentile rank (dplyr provides similar functions)
perc.rank <- function(x) rank(x,  ties.method = "random")/length(x)



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
fileOut <- file(paste0("./Output/simulation parameters.txt"))
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
             paste0("Sample size = ", format(n, scientific = F)),
             paste0("Number of iterations = ", numberofiterations),
             paste0("Number of scenarios = ", n.scenarios), "\n"), fileOut)
close(fileOut)

# Match the sex and age structure of the initial year
population.actual <- fread("./Population/population.struct.csv",  header = T)[year == init.year, ]
population.actual[, pct := round(as.numeric(n) * pop / sum(pop))]

# Calculate the exact fraction of the mid 2010 population this sample represents
pop.fraction <- n / population.actual[, sum(pop)] # 53107200 is the total mid 2011 population of England (52642600 for 2010)

rm(Fertility.Assumption)

# Import (or create) Synthetic Population
if (length(list.files("./SynthPop")) == 0) {
  cat("Building synthetic population...\nThis might take some time...\nThank you for your patience :)\n\n")
  source(file = paste0(get.dropbox.folder(), "/PhD/Models/SynthPop/Synthetic Population Script.R"))
}

