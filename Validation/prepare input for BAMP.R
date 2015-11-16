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

#!/opt/gridware/apps/gcc/R/3.2.0/bin/Rscript
#!/usr/bin/Rscript
# ************************************************************************************************
#
# IMPACT NCD Prototype 07
#
# ************************************************************************************************

# Preample ----------------------------------------------------------------


gc()
options(datatable.verbose=TRUE)

# User input
init.year <- 2011

n <- 200000  # Define the sample size

yearstoproject <- 20  # NEED TO force >=1 and up to 50

numberofiterations <- 1

ageL <- 30  # Define lower age limit to diseases-model simulation (min = 30)

ageH <- 84  # Define lower age limit to diseases-model simulation (max = 84)

alignment <- F # T or F (apply correction factor to counterpoise levin's and exposure error)

Fertility.Assumption <- "N"  # Select (N)ormal, (H)igh or (L)ow fertility rate asumptions based on ONS scenarios. They do matter for accurate population statistics

cvd.lag <- 5L # Avoid 0
fatality.annual.improvement.chd <- 1.5 # 3 means 3% annual improvement in fatality
fatality.annual.improvement.stroke <- 1.5 # 3 means 3% annual improvement in fatality
fatality.annual.improvement.c16 <- 0.1

fatality.sec.gradient.chd    <- 40 # Percentage of difference in fatality between qimd 1 and 5. Positive values mean the poorest are experincing higher fatality 
fatality.sec.gradient.stroke <- 40 # Percentage of difference in fatality between qimd 1 and 5. Positive values mean the poorest are experincing higher fatality 
fatality.sec.gradient.c16    <- 10

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


source(file = "./initialisation.R")

# Population --------------------------------------------------------------

population <-
  fread(
    "./Population/Population by adjusted IMD 2010 quintile_final.csv",
    header = T, 
    skip = 0
  )
setnames(population, names(population), str_trim(names(population)))
population <-
  melt(
    population, id.vars = c("year", "sex", "qimd"), 
    variable.name = "agegroup",
    value.name = "pop"
  )
population <- population[agegroup %in% c(as.character(unique(agegroup.fn(35:84))), "85-89", "90+"),]
population[agegroup  %in% unique(agegroup.fn(35:44)), agegroup := "35-44"]
population[agegroup  %in% unique(agegroup.fn(45:54)), agegroup := "45-54"]
population[agegroup  %in% unique(agegroup.fn(55:64)), agegroup := "55-64"]
population[agegroup  %in% unique(agegroup.fn(65:74)), agegroup := "65-74"]
population[agegroup  %in% unique(agegroup.fn(75:84)), agegroup := "75-84"]
population[agegroup  %in% c("85-89", "90+"),          agegroup := "85+"]
population[, agegroup := ordered(agegroup)]

foreach(k = 1:2, .inorder = F, .combine = 'c') %:% # sex
  foreach(
    l = 1:5,
    .inorder = F,
    .packages = c("demography", "reshape2", "data.table")
  ) %do% {
    ss <- ifelse(k==1, "M", "F")
    xx <- acast(population[sex == k & qimd == l,],
                year ~ agegroup, sum, value.var = "pop")
    dir.create(
      path = paste0("./Validation/C16/", ss,l), 
      recursive = T, 
      showWarnings = F
    )
    
    write.table(xx,
                file = paste0("./Validation/C16/", ss, l, "/Pop.txt"),
                append = F, sep = "\t",  row.names = F, col.names = F)
  }

acast(population[sex == k & qimd == l,],
      year ~ agegroup, sum, value.var = "pop")
# Gastric cancer --------------------------------------------------------------------

deaths.causes <-
  fread(
    "./LifeTables/deaths from selected causes by quintile_final_tcm77-388639.csv",
    header = T,
    skip = 0
  )
setnames(deaths.causes, names(deaths.causes), str_trim(names(deaths.causes)))
deaths.causes <- deaths.causes[cause %in% "Malignant neoplasm of stomach",]
deaths.causes <-
  melt(
    deaths.causes, id.vars = c("year", "sex", "qimd", "cause"), 
    variable.name = "agegroup", 
    value.name = "disease"
  )
deaths.causes <- deaths.causes[agegroup %in% c(as.character(unique(agegroup.fn(35:84))), "85-89", "90+"),]
deaths.causes[agegroup  %in% unique(agegroup.fn(35:44)), agegroup := "35-44"]
deaths.causes[agegroup  %in% unique(agegroup.fn(45:54)), agegroup := "45-54"]
deaths.causes[agegroup  %in% unique(agegroup.fn(55:64)), agegroup := "55-64"]
deaths.causes[agegroup  %in% unique(agegroup.fn(65:74)), agegroup := "65-74"]
deaths.causes[agegroup  %in% unique(agegroup.fn(75:84)), agegroup := "75-84"]
deaths.causes[agegroup  %in% c("85-89", "90+"),          agegroup := "85+"]
deaths.causes[, agegroup := ordered(agegroup)]

foreach(k = 1:2, .inorder = F, .combine = 'c') %:% # sex
  foreach(
    l = 1:5,
    .inorder = F,
    .packages = c("demography", "reshape2", "data.table")
  ) %do% {
    ss <- ifelse(k==1, "M", "F")
      xx <- acast(deaths.causes[sex == k & qimd == l,],
                  year ~ agegroup, sum, value.var = "disease")
      dir.create(
        path = paste0("./Validation/C16/", ss,l), 
        recursive = T, 
        showWarnings = F
      )
      
    write.table(xx,
                file = paste0("./Validation/C16/", ss,l, "/Deaths.txt"),
                append = F, sep = "\t",  row.names = F, col.names = F)
  }

incid <- fread("./Cancer Statistics/2011 cases.csv", header = T)




