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


# Preample ----------------------------------------------------------------
gc()
options(datatable.verbose = F)

# User input
init.year <- 2006

widesynthpop <- F

n <- 1e5  # Define the sample size

yearstoproject <- 3  # NEED TO force >=1 and up to 50

numberofiterations <- 5

ageL <- 30  # Define lower age limit to diseases-model simulation (min = 30)

ageH <- 84  # Define lower age limit to diseases-model simulation (max = 84)

alignment <- F # T or F (apply correction factor to counterpoise levin's and exposure error)

qdrisk <- T # Use QDrisk score for diabetes incidence

Fertility.Assumption <- "N"  # Select (N)ormal, (H)igh or (L)ow fertility rate asumptions based on ONS scenarios. They do matter for accurate population statistics

cvd.lag <- 5 
fatality.annual.improvement.chd    <- 3L # 3 means 3% annual improvement in fatality
fatality.annual.improvement.stroke <- 3L # 3 means 3% annual improvement in fatality
fatality.annual.improvement.c16    <- 2L
fatality.annual.improvement.c34    <- 2L

fatality.sec.gradient.chd    <- 40L # Percentage of difference in fatality between qimd 1 and 5. Positive values mean the poorest are experincing higher fatality 
fatality.sec.gradient.stroke <- 40L # Percentage of difference in fatality between qimd 1 and 5. Positive values mean the poorest are experincing higher fatality 
fatality.sec.gradient.c16    <- 30L
fatality.sec.gradient.c34    <- 30L

cancer.lag <- 8 

clusternumber <- 4L # Change to your number of CPU cores 

paired <- T 

cleardirectories <- F # If T delete auxiliary output directories when simulation finish

export.graphs <- F

diseasestoexclude <- c("CHD", "stroke", "C34", "C16")  # Define disease to be excluded from lifetables
# ICD10 code reminder for disease coding (http://apps.who.int/classifications/icd10/browse/2010/en#/I20-I25)

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
} else if (Sys.info()[1] == "Darwin") {
    setwd("/Volumes/home/dropbox/PhD/Models/IMPACTncd/")
} else {
  get.dropbox.folder <- function() {
    if (!require(RCurl)) 
      stop("You need to install RCurl package.")
    if (Sys.info()["sysname"] != "Windows") 
      stop("Currently, 'get.dropbox.folder' works for Windows and Linux only. Sorry.")
    db.file <- paste(Sys.getenv("LOCALAPPDATA"), "\\Dropbox\\host.db", sep = "")
    base64coded <- readLines(db.file, warn = F)[2]
    base64(base64coded, encode = F)
  }
  setwd(paste0(get.dropbox.folder(), "/PhD/Models/IMPACTncd/"))
}

# Main --------------------------------------------------------------------
source(file = "./initialisation.R")
#setthreads(1L)
# Create lifetable without the disease(s) to be modelled. Lifetables were calculated using data from
# England and Wales not just England. Minimal bias since we use probabilities.
cat("Generating life table...\n\n")
source(file = "./life table engine.R")

iterations = 1 
my.env <- environment() # get environment of this branch

# Define functions in foreach loop
sys.source(file = "./cluster functions.R", my.env)
sys.source(file = "./diseases epidemiology.R", my.env)
# Load synthetic population
sys.source(file = "./load synthetic population.R", my.env)


# Actual simulation
i = init.year - 2011
loadcmp(file = "./risk factor trajectories.Rc", my.env)
loadcmp(file = "./2dmc.Rc", my.env)
loadcmp(file = paste0("./Scenarios/", scenarios.list[[iterations]]), my.env)
loadcmp(file = "./birth engine.Rc", my.env)
loadcmp(file = "./ageing engine.Rc", my.env)


# Estimating incidence and mortality of modelled NCDs
indiv.mort <- vector("list", length(diseasestoexclude) + 1) # to store individual deaths
indiv.incid <- vector("list", length(diseasestoexclude))  # to store individual incidence
diseases <- sample(diseases) # randomly change the order of diseases each year
lapply(diseases, function(f) f()) # run all functions in the list

# Summarising individual outputs
loadcmp(file = "./individual summary.Rc", my.env)

cat("Advance age\n")
POP[, `:=`(age = age + 1)]  # make pop older
agegroup.fn(POP)

for (i in (init.year - 2010) : (init.year - 2012 + yearstoproject)) {
  loadcmp(file = "./birth engine.Rc", my.env)
  loadcmp(file = "./ageing engine.Rc", my.env)
  diseases <- sample(diseases) # randomly change the order of diseases each year
  lapply(diseases, function(f) f()) # run all functions in the list
  loadcmp(file = "./individual summary.Rc", my.env)
  POP[, `:=`(age = age + 1)]  # make pop older
  agegroup.fn(POP)
}

# Output
source(file = "./post simulation functions.R")
source(file = "./output.R")
end()

# compile scenarios
#lapply(list.files(path = "./Scenarios", pattern = glob2rx("*.R"), full.names = T, recursive = F), cmpfile)
#lapply(list.files(path = "./", pattern = glob2rx("*.R"), full.names = T, recursive = F), cmpfile)
