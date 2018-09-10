#!/opt/microsoft/ropen/3.4.3/lib64/R/bin/Rscript
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


# Preample -----------------------------------------
gc()
options(datatable.verbose = F)

# User input
init.year <- 2006
n <- 5e5  # Define the sample size
yearstoproject <- 30L  # NEED TO force >=1 and up to 50
numberofiterations <- 500L
clusternumber <- 14L # Change to your number of CPU cores 
process.output <- TRUE

ageL <- 30  # Define lower age limit to diseases-model simulation (min = 30)

ageH <- 84  # Define lower age limit to diseases-model simulation (max = 84)

qdrisk <- T # Use QDrisk score for diabetes incidence

Fertility.Assumption <- "N"  # Select (N)ormal, (H)igh or (L)ow fertility rate asumptions based on ONS scenarios. They do matter for accurate population statistics

cvd.lag <- 5L 
cancer.lag <- 8L

fatality.annual.improvement.chd    <- 3L # 3 means 3% annual improvement in fatality
fatality.annual.improvement.stroke <- 3L # 3 means 3% annual improvement in fatality
fatality.annual.improvement.c16    <- 2L
fatality.annual.improvement.c34    <- 2L

fatality.sec.gradient.chd    <- 40L # Percentage of difference in fatality between qimd 1 and 5. Positive values mean the poorest are experincing higher fatality 
fatality.sec.gradient.stroke <- 40L # Percentage of difference in fatality between qimd 1 and 5. Positive values mean the poorest are experincing higher fatality 
fatality.sec.gradient.c16    <- 30L
fatality.sec.gradient.c34    <- 30L

paired <- T 

cleardirectories <- F # If T delete auxiliary output directories when simulation finish

export.graphs <- F

diseasestoexclude <- c("CHD", "stroke", "C16")  # Define disease to be excluded from lifetables
# ICD10 code reminder for disease coding (http://apps.who.int/classifications/icd10/browse/2010/en#/I20-I25)


# *******************************************************
# IMPACT NCD Prototype 09 ----                           
# *******************************************************

cat("Initialising IMPACTncd...\n\n")
options(warn = 1)

if (Sys.info()[1] == "Linux") {
  if (system("whoami", T) == "mdxasck2") {
    setwd("~/IMPACTncd/")
    # all.files <- list.files('./SynthPop', pattern = glob2rx('spop2011*.rds'),
    # full.names = T) spop.l <- lapply(all.files, readRDS) rm(all.files)
  } else {
    setwd(paste("/home/", system("whoami", T), "/pCloudDrive/My Models/Responsibility deal/", 
      sep = "", collapse = ""))
  }
} else {
  get.dropbox.folder <- function() {
    if (!require(RCurl)) 
      stop("You need to install RCurl package.")
    if (Sys.info()["sysname"] != "Windows") 
      stop("Currently, 'get.dropbox.folder' works for Windows and Linux only. Sorry...")
    db.file <- paste(Sys.getenv("APPDATA"), "\\Dropbox\\host.db", sep = "")
    base64coded <- readLines(db.file, warn = F)[2]
    base64(base64coded, encode = F)
  }
  setwd(paste0(get.dropbox.folder(), "/PhD/Models/IMPACTncd/"))
}

require(compiler)

#loadcmp("./GUI.Rc")
source("./initialisation.R")

lagtimes.dt <- data.table("cvd.lag" = cvd.lag.l, "cancer.lag" = cancer.lag.l)
if (file.exists("/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/lagtimes.csv")) {
  fwrite(lagtimes.dt, file = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/lagtimes.csv", append = T)
} else fwrite(lagtimes.dt, file = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/lagtimes.csv", append = F)
# TODO Above will not work if paired = F. I will have to move later after  

# Create lifetable without the disease(s) to be modelled. Lifetables were
# calculated using data from England and Wales not just England. Minimal bias
# since we use probabilities.
cat("Generating life table...\n\n")
setMKLthreads(1L)
setDTthreads(1L)
source("./life table engine.R")

# cl <- makeCluster(clusternumber) # used for clustering. win compatible
# registerDoParallel(cl)
registerDoParallel(clusternumber)  # used for forking. only linux
time.mark("start parallelisation")

foreach(iterations = 1:it, .inorder = F, .verbose = T,
        .packages = c("compiler", "mc2d", "gamlss","gamlss.tr", "nnet", 
                      "dplyr", "stringr", "data.table"),
  .export = ls(), .noexport = c("scenarios.list", "time.mark")) %dorng% 
  {
    setMKLthreads(1L)
    setDTthreads(1L)
    if (paired) 
      set.seed(seed[[counter[[iterations]]]])
    cat(paste0("iteration: ", iterations, " counter: ", counter[[iterations]], 
      " seed: ", seed[[counter[[iterations]]]], "\n"), 
      file = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/seed temp.txt", 
      append = T)
    
    print(seed[[counter[[iterations]]]])
    
    my.env <- environment()  # get environment of this branch
    
    # time.mark('Define functions in foreach loop')
    sys.source("./cluster functions.R", my.env)
    
    sys.source("./diseases epidemiology.R", my.env)
    
    # time.mark('Load synthetic population')
    if (paired) set.seed(seed[[counter[[iterations]]]])
    sys.source(file = "./load synthetic population.R", my.env)
    
    # Load RF trajectoy functions cmpfile('./risk factor trajectories.R')
    # sys.source(file = './risk factor trajectories.R', my.env)
    sys.source(file = "./risk factor trajectories.R", my.env)
    sys.source(file = "./2dmc.R", my.env)  # sample a value for each parameter 
    
    time.mark("start simulation")
    sys.source(file = "./simulation.R", my.env)
    rm(my.env)  # BE CAREFULL. DTs altered with in my.env, change universaly. 
    # You need copies of DTs to by handled within my.env
  }

if (exists("cl")) stopCluster(cl)
time.mark("End of parallelisation")

# Output
# setMKLthreads(10L)
# setDTthreads(10L)
file.rename("/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/simulation parameters temp.txt", "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/simulation parameters.txt")
file.rename("/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/seed temp.txt", "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/seed.txt")

source(file = "./post simulation functions.R")
if (process.output == TRUE) source(file = "./output.R")
end()

# compile scenarios
# lapply(list.files(path = './Scenarios', pattern = glob2rx('*.R'), full.names = T, recursive = F), cmpfile) 
# lapply(list.files(path = './', pattern = glob2rx('*.R'), full.names = T, recursive = F), cmpfile)
