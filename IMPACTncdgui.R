#!/opt/gridware/apps/gcc/R/3.1.0/lib64/R/bin/Rscript
#!/usr/bin/Rscript
# ************************************************************************************************
#
# IMPACT NCD Prototype 06
#
# ************************************************************************************************

cat("Initialising IMPACTncd...\n\n")
options(warn = 1)

if (Sys.info()[1] == "Linux") {
  if (system("whoami", T )== "mdxasck2") {
    setwd("~/IMPACTncd/")
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

source(file = "./GUI.R")
source(file = "./initialisation.R")

# Create lifetable without the disease(s) to be modelled. Lifetables were calculated using data from
# England and Wales not just England. Minimal bias since we use probabilities.
cat("Generating life table...\n\n")
source(file = "./life table engine.R")


# Generating Incidence tables
cat("Generating incidence tables...\n\n")
source(file = "./cancer statistics.R") # for cancer
source(file = "./CVD statistics.R") # for cvd

cl <- makeCluster(clusternumber) 
registerDoParallel(cl)
# registerDoSNOW(cl)
time.mark("start parallelisation")
foreach(iterations = 1 : it,
        .inorder = F,
        .verbose = T,
        .packages = c("data.table",
                      "dplyr",
                      "randtoolbox", 
                      "truncnorm", 
                      # "reshape2", 
                      "compiler"),
        .export = ls(),
        .noexport = c("scenarios.list")) %dorng% {
          
          my.env <- environment(function(){}) # trick to get environment of this branch
          
          #time.mark("Define functions in foreach loop")
          sys.source(file = "./cluster functions.R", my.env)
          
          #time.mark("Load synthetic population")
          sys.source(file = "./load synthetic population.R", my.env)
          
          time.mark("Actual simulation start")
          sys.source(file = "./simulation.R", my.env)
          rm(my.env)
        }

stopCluster(cl)
time.mark("End of parallelisation")

# Output
source(file = "./post simulation functions.R")
source(file = "./output.R")
end()

