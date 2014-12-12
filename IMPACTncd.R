#!/opt/gridware/apps/gcc/R/3.1.0/bin/Rscript
#!/usr/bin/Rscript
# ************************************************************************************************
#
# IMPACT NCD Prototype 06
#
# ************************************************************************************************
gc()

# User input
init.year <- 2011

n <- 100000  # Define the sample size

yearstoproject <- 2  # NEED TO force >=1 and up to 50

numberofiterations <- 1

ageL <- 30  # Define lower age limit to diseases-model simulation (min = 30)

ageH <- 84  # Define lower age limit to diseases-model simulation (max = 84)

alignment <- F # T or F (apply correction factor to counterpoise levin's and exposure error)

Fertility.Assumption <- "N"  # Select (N)ormal, (H)igh or (L)ow fertility rate asumptions based on ONS scenarios. They do matter for accurate population statistics

cvd.lag <- 5 # Avoid 0
fatality.annual.improvement.chd <- 3 # 3 means 3% annual improvement in fatality
fatality.annual.improvement.stroke <- 3 # 3 means 3% annual improvement in fatality

fatality.sec.gradient.chd <-40 # Percentage of difference in fatality between qimd 1 and 5. Positive values mean the poorest are experincing higher fatality 
fatality.sec.gradient.stroke <-40 # Percentage of difference in fatality between qimd 1 and 5. Positive values mean the poorest are experincing higher fatality 

cancer.lag <- 10 # Needs to be longer than cvd.lag to work properly (smoking histories)

clusternumber <- 4 # Change to your number of CPU cores 

cleardirectories <- F # If T delete auxiliary output directories when simulation finish

diseasestoexclude <- c("CHD", "stroke")  # Define disease to be excluded from lifetables
{
    # Ischaemic heart diseases (I20-I25) Cardiac arrest (I46) Heart failure (I50) Cerebral infarction
    # (I63) Stroke, not specified as haemorrhage or infarction (I64) Malignant neoplasms of lip, oral
    # cavity and pharynx (C00-C14) Malignant neoplasm of oesophagus (C15) Malignant neoplasm of stomach
    # (C16) Malignant neoplasm of colon (C18) Malignant neoplasm of rectosigmoid junction (C19) Malignant
    # neoplasm of rectum (C20) Malignant neoplasm of liver and intrahepatic bile ducts (C22) Malignant
    # neoplasm of pancreas (C25) Malignant neoplasm of larynx (C32) Malignant neoplasm of trachea (C33)
    # Malignant neoplasm of bronchus and lung (C34) Malignant neoplasm of breast (C50) ??D05 Malignant
    # neoplasm of corpus uteri (Endometrium) (C541) Malignant neoplasm of ovary (C56) Malignant neoplasm
    # of prostate (C61) Malignant neoplasm of bladder (C67)
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

source(file = "./initialisation.R")

# Create lifetable without the disease(s) to be modelled. Lifetables were calculated using data from
# England and Wales not just England. Minimal bias since we use probabilities.
cat("Generating life table...\n\n")
source(file = "./life table engine.R")

cl <- makeCluster(clusternumber) 
registerDoParallel(cl)

cat("Monte Carlo simulation...\n\n")
time.mark("start parallelisation")
foreach(iterations = 1 : it,
        .inorder = F,
        .verbose = T,
        .packages = c("data.table",
                      "dplyr",
                      "randtoolbox", 
                      "truncnorm", 
                      "stringr",
                      "compiler"),
        .export = ls(),
        .noexport = c("scenarios.list", "time.mark")) %dorng% {
        
        my.env <- environment() # get environment of this branch
            
        # Define functions in foreach loop
        sys.source(file = "./cluster functions.R", my.env)
        
        # Load synthetic population
        sys.source(file = "./load synthetic population.R", my.env)
            
        # Generating Incidence tables
        sys.source(file = "./cancer statistics.R", my.env) # for cancer
        sys.source(file = "./CVD statistics.R", my.env) # for cvd
        
        # Actual simulation
        sys.source(file = "./simulation.R", my.env)
        rm(my.env)
}

stopCluster(cl)
time.mark("End of parallelisation")

# Output
source(file = "./post simulation functions.R")
source(file = "./output.R")
end()
