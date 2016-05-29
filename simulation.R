#cmpfile("./simulation.R")
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


# Year 0 is mid-year 2011 (or any other init.year) Dates of Fieldwork for HSE 2011: January 2011 - March 2012
sink(file = paste0(output.dir(), "log.txt"),
     append = T, 
     type = "output",
     split = F)

for (i in (init.year - 2011):(yearstoproject + init.year - 2012)) {
  cat(paste0(rep("* ", 20)), "\n\n")
  cat(paste0("Simulating mid ", 2011 + i, " to mid ", 2012 + i, "...\n")) 
  cat(paste0(Sys.time(), "\n"))
  # cat(paste0("Lags:", cvd.lag, " ",cancer.lag, "\n\n"))
  cat(paste0(rep("* ", 20)), "\n\n")    
  
  # Load scenario
  loadcmp(file = paste0("./Scenarios/", scenarios.list[[iterations]]), my.env)
  
  # Start births engine
  #sys.source(file = "./birth engine.R", my.env)
  loadcmp(file = "./birth engine.Rc", my.env)
  
  # Start ageing engine
  loadcmp(file = "./ageing engine.Rc", my.env)
  
  # Estimating incidence and mortality of modelled NCDs
  indiv.mort <- vector("list", length(diseasestoexclude) + 1) # to store individual deaths
  indiv.incid <- vector("list", length(diseasestoexclude))  # to store individual incidence
  if (paired) set.seed(seed[[counter[[iterations]]]] + i)
  lapply(sample(diseases), function(f) f()) # randomly change the order of diseases each year and run all functions in the list
  
  # Summarising individual outputs
  loadcmp(file = "./individual summary.Rc", my.env)
  
  #cat("Advance age\n")
  POP[, `:=`(age = age + 1)]  # make pop older
  agegroup.fn(POP)
}

sink()
gc()
