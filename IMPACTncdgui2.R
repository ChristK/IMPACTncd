#!/opt/gridware/apps/gcc/R/3.1.0/lib64/R/bin/Rscript
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
      stop("Currently, 'get.dropbox.folder' works for Windows and Linux only. Sorry...")
    db.file <- paste(Sys.getenv("APPDATA"), "\\Dropbox\\host.db", sep = "")
    base64coded <- readLines(db.file, warn = F)[2]
    base64(base64coded, encode = F)
  }
  setwd(paste0(get.dropbox.folder(), "/PhD/Models/IMPACTncd/"))
}

source(file = "./GUI.R")
#cmpfile(infile = "./initialisation.R")
source(file = "./initialisation.R")
#loadcmp(file = "./initialisation.Rc")

# Create lifetable without the disease(s) to be modelled. Lifetables were calculated using data from
# England and Wales not just England. Minimal bias since we use probabilities.
cat("Generating life table...\n\n")
loadcmp("./life table engine.Rc")


# cl <- makeCluster(clusternumber) # used for clustering. win compatible
# registerDoParallel(cl) 
registerDoParallel(clusternumber) # used for forking. only linux
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
          
          #time.mark("Define functions in foreach loop")
          loadcmp(file = "./cluster functions.Rc", my.env)
          
          #time.mark("Load synthetic population")
          loadcmp(file = "./load synthetic population.Rc", my.env)
          
          # Generating Incidence tables
          loadcmp(file = "./cancer statistics.Rc", my.env) # for cancer
          loadcmp(file = "./CVD statistics.Rc", my.env) # for cvd
          
          time.mark("start simulation")
          loadcmp(file = "./simulation.Rc", my.env)
          rm(my.env)
        }

if (exists("cl")) stopCluster(cl)
time.mark("End of parallelisation")

# Output
#cmpfile("./post simulation functions.R")
#source(file = "./post simulation functions.R")
loadcmp(file = "./post simulation functions.Rc")

#cmpfile("./output.R")
#source(file = "./output.R")
loadcmp(file = "./output.Rc")
end()

# compile scenarios
#lapply(list.files(path = "./Scenarios", pattern = glob2rx("*.R"), full.names = T, recursive = F), cmpfile)
