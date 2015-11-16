#!/opt/gridware/apps/gcc/R/3.2.0/lib64/R/bin/Rscript

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
# ************************************************************************************************
#
# IMPACTncd Prototype 08
#
# ************************************************************************************************

cat("Initialising IMPACTncd...\n\n")
options(warn = 1)

if (Sys.info()[1] == "Linux") {
  if (system("whoami", T )== "mdxasck2") {
    setwd("~/IMPACTncd/")
    #     all.files <- list.files("./SynthPop", 
    #                             pattern = glob2rx("spop2011*.rds"), 
    #                             full.names = T)
    #     
    #     spop.l <- lapply(all.files, readRDS)
    #     rm(all.files)
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

require(compiler)

loadcmp("./GUI.Rc")
loadcmp("./initialisation.Rc")

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
                      "compiler",
                      "mc2d",
                      "quantreg"),
        .export = ls(),
        .noexport = c("scenarios.list", "time.mark")) %dorng% {
          
          if (paired) set.seed(seed[[counter[[iterations]]]])
          cat(paste0("iteration: ", iterations,
                     " counter: ",counter[[iterations]],
                     " seed: ",seed[[counter[[iterations]]]], "\n"), 
              file = "./Output/seed temp.txt",
              append = T)
          
          print(seed[[counter[[iterations]]]])
          
          my.env <- environment() # get environment of this branch
          
          #time.mark("Define functions in foreach loop")
          loadcmp(file = "./cluster functions.Rc", my.env)
          
          loadcmp(file = "./diseases epidemiology.Rc", my.env)
          
          #time.mark("Load synthetic population")
          loadcmp(file = "./load synthetic population.Rc", my.env)
          
          # Load RF trajectoy functions
          #cmpfile("./risk factor trajectories.R")
          #sys.source(file = "./risk factor trajectories.R", my.env)
          loadcmp(file = "./risk factor trajectories.Rc", my.env)
          loadcmp(file = "./2dmc.Rc", my.env) # sample a value for each parameter 
          
          time.mark("start simulation")
          loadcmp(file = "./simulation.Rc", my.env)
          rm(my.env) # BE CAREFULL. DTs altered with in my.env, change universaly. 
          # You need copies of DTs to by handled within my.env
        }

if (exists("cl")) stopCluster(cl)
time.mark("End of parallelisation")

# Output
file.rename("./Output/simulation parameters temp.txt", "./Output/simulation parameters.txt")
file.rename("./Output/seed temp.txt", "./Output/seed.txt")
loadcmp(file = "./post simulation functions.Rc")
if (process.output == T) {
  loadcmp(file = "./output.Rc")
}
end()

# compile scenarios
#lapply(list.files(path = "./Scenarios", pattern = glob2rx("*.R"), full.names = T, recursive = F), cmpfile)
#lapply(list.files(path = "./", pattern = glob2rx("*.R"), full.names = T, recursive = F), cmpfile)
