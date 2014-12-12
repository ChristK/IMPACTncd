# User input
init.year <- 2011

n <- 1000000  # Define the sample size

yearstoproject <- 30  # NEED TO force >=1 and up to 50

n <- 200000  # Define the sample size

yearstoproject <- 50  # NEED TO force >=1 and up to 50

numberofiterations <- 100

ageL <- 30  # Define lower age limit to diseases-model simulation (min = 30)

ageH <- 84  # Define lower age limit to diseases-model simulation (max = 84)

alignment <- F # T or F (apply correction factor to counterpoise levin's and exposure error)

Fertility.Assumption <- "N"  # Select (N)ormal, (H)igh or (L)ow fertility rate asumptions based on ONS scenarios. They do matter for accurate population statistics

cvd.lag <- 5 # Avoid 0
fatality.annual.improvement.chd <- 3 # 3 means 3% annual improvement in fatality
fatality.annual.improvement.stroke <- 3 # 3 means 3% annual improvement in fatality



fatality.sec.gradient.chd <- 40 # Percentage of difference in fatality between qimd 1 and 5. Positive values mean the poorest are experincing higher fatality 
fatality.sec.gradient.stroke <- 40 # Percentage of difference in fatality between qimd 1 and 5. Positive values mean the poorest are experincing higher fatality 


fatality.sec.gradient.chd <-40 # Percentage of difference in fatality between qimd 1 and 5. Positive values mean the poorest are experincing higher fatality 
fatality.sec.gradient.stroke <-40 # Percentage of difference in fatality between qimd 1 and 5. Positive values mean the poorest are experincing higher fatality 
 
cancer.lag <- 10 # Needs to be longer than cvd.lag to work properly (smoking histories)

clusternumber <- 1 # Change to your number of CPU cores 

 
cleardirectories <- F # If T delete auxiliary output directories when simulation finish

cleardirectories <- T # If T delete auxiliary output directories when simulation finish


diseasestoexclude <- c("CHD", "stroke")  # Define disease to be excluded from lifetables

cat("Initialising IMPACTncd...\n\n")
options(warn = 1)

if (Sys.info()[1] == "Linux") {
  if (system("whoami", T )== "mdxasck2") {
    setwd("~/IMPACTncd/")
    ifelse (clusternumber<100, 100,clusternumber)  # overwrites previous if <100
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


# Load RF trajectoy functions
source(file = "./risk factor trajectories.R")

# Create lifetable without the disease(s) to be modelled. Lifetables were calculated using data from
# England and Wales not just England. Minimal bias since we use probabilities.
cat("Generating life table...\n\n")
source(file = "./life table engine.R")


# Generating Incidence tables
cat("Generating incidence tables...\n\n")
source(file = "./cancer statistics.R") # for cancer
source(file = "./CVD statistics.R") # for cvd


i = 0


source(file = "./post simulation functions.R")
load("./POP.RData")


POP[, stroke.incidence := chd.incidence]
my.env <- new.env()
iterations <- 1
sys.source(file = "./cluster functions.R", my.env)
