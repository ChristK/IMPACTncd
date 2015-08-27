#!/opt/gridware/apps/gcc/R/3.2.0/bin/Rscript
#!/usr/bin/Rscript
# ********************************************************************************
#
# IMPACT NCD Prototype 08
#
# ********************************************************************************

# Preample ----------------------------------------------------------------


gc()
options(datatable.verbose = F)

# User input
init.year <- 2011

n <- 2e5  # Define the sample size

yearstoproject <- 5  # NEED TO force >=1 and up to 50

numberofiterations <- 1

ageL <- 30  # Define lower age limit to diseases-model simulation (min = 30)

ageH <- 84  # Define lower age limit to diseases-model simulation (max = 84)

alignment <- F # T or F (apply correction factor to counterpoise levin's and exposure error)

qdrisk <- T # Use QDrisk score for diabetes incidence

Fertility.Assumption <- "N"  # Select (N)ormal, (H)igh or (L)ow fertility rate asumptions based on ONS scenarios. They do matter for accurate population statistics

cvd.lag <- 5L # Avoid 0
fatality.annual.improvement.chd <- 3 # 3 means 3% annual improvement in fatality
fatality.annual.improvement.stroke <- 3 # 3 means 3% annual improvement in fatality
fatality.annual.improvement.c16 <- 2

fatality.sec.gradient.chd    <- 40 # Percentage of difference in fatality between qimd 1 and 5. Positive values mean the poorest are experincing higher fatality 
fatality.sec.gradient.stroke <- 40 # Percentage of difference in fatality between qimd 1 and 5. Positive values mean the poorest are experincing higher fatality 
fatality.sec.gradient.c16    <- 30

cancer.lag <- 10L # Needs to be longer than cvd.lag to work properly (smoking histories)

clusternumber <- 4 # Change to your number of CPU cores 

cleardirectories <- F # If T delete auxiliary output directories when simulation finish

diseasestoexclude <- c("CHD", "stroke", "C16")  # Define disease to be excluded from lifetables
{
  # Ischaemic heart diseases (I20-I25)
  # Stroke (I60-I69), 
  # Malignant neoplasms of lip, oral cavity and pharynx (C00-C14) 
  # Malignant neoplasm of oesophagus (C15) 
  # Malignant neoplasm of stomach (C16)
  # Malignant neoplasm of colon (C18) 
  # Malignant neoplasm of rectosigmoid junction (C19) 
  # Malignant neoplasm of rectum (C20)
  # Malignant neoplasm of liver and intrahepatic bile ducts (C22)
  # Malignant neoplasm of pancreas (C25) 
  # Malignant neoplasm of larynx (C32) 
  # Malignant neoplasm of trachea (C33)
  # Malignant neoplasm of bronchus and lung (C34) 
  # Malignant neoplasm of breast (C50) 
}  # ICD10 code reminder for disease coding (http://apps.who.int/classifications/icd10/browse/2010/en#/I20-I25)

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
    db.file <- paste(Sys.getenv("APPDATA"), "\\Dropbox\\host.db", sep = "")
    base64coded <- readLines(db.file, warn = F)[2]
    base64(base64coded, encode = F)
  }
  setwd(paste0(get.dropbox.folder(), "/PhD/Models/IMPACTncd/"))
}

# Main --------------------------------------------------------------------
source(file = "./initialisation.R")

# Create lifetable without the disease(s) to be modelled. Lifetables were calculated using data from
# England and Wales not just England. Minimal bias since we use probabilities.
cat("Generating life table...\n\n")
source(file = "./life table engine.R")
iterations = 1 
my.env <- environment() # get environment of this branch

# Define functions in foreach loop
sys.source(file = "./cluster functions.R", my.env)

# Load synthetic population
sys.source(file = "./load synthetic population.R", my.env)

# Generating Incidence tables
sys.source(file = "./cancer statistics.R", my.env) # for cancer
sys.source(file = "./CVD statistics.R", my.env) # for cvd

# Actual simulation
i = 0
loadcmp(file = paste0("./Scenarios/", scenarios.list[[iterations]],"c"), my.env)
loadcmp(file = "./2dmc.Rc", my.env)
loadcmp(file = "./birth engine.Rc", my.env)
loadcmp(file = "./ageing engine.Rc", my.env)


# Estimating incidence and mortality of modelled NCDs
indiv.mort <- vector("list", length(diseasestoexclude)+1) # to store individual deaths
indiv.incid <- vector("list", length(diseasestoexclude))  # to store individual incidence
diseases <- sample(diseases) # randomly change the order of diseases each year
lapply(diseases, function(f) f()) # run all functions in the list

# Summarising individual outputs
loadcmp(file = "./individual summary.Rc", my.env)

cat("Advance age\n")
POP[, `:=`(age = age + 1)]  # make pop older
agegroup.fn(POP)

#rm(my.env)


time.mark("End of parallelisation")

# Output
source(file = "./post simulation functions.R")
source(file = "./output.R")
end()
