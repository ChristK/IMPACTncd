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

# This scenario assumes exposure stable at 2011 levels
cat("no trends scenario (2011 exposures)\n\n")

intervention.year <- 2016

# Load prediction equations
if (i == (init.year-2011)) {
  #     load(file="./Lagtimes/bmi.svylm.rda")
  #     load(file="./Lagtimes/chol.svylm.rda")
  #     load(file="./Lagtimes/sbp.svylm.rda")
  #     load(file="./Lagtimes/diab.svylr.rda")
  #     load(file="./Lagtimes/smok.active.svylr.rda")
  #     load(file="./Lagtimes/smok.cess.svylr.rda")
  #     load(file="./Lagtimes/smok.cess.success.rda")
  #     load(file="./Lagtimes/smok.start.svylr.rda")
  #     load(file="./Lagtimes/fv.svylr.rda")
  #     load(file="./Lagtimes/fvrate.svylr.rda")
  #     
  #     # Function to apply after ageing
  post.ageing.scenario.fn <- function() {
    cat("Post ageing scenario function")
  }
}

if (i == init.year-2011 + cvd.lag) {
  cat("alter regressions")
  #bmi
  yr <- grep("year", names(bmi.svylm$coefficients)) # get coefficients containing year
  bmi.svylm$coefficients[yr] <- 0 # replace them with 0
  #plot(pred.bmi(0:50, 30, 2, 1, 0), ylim=c(0,40))
  
  #sbp
  yr <- grep("year", names(sbp.svylm$coefficients)) # get coefficients containing year
  sbp.svylm$coefficients[yr] <- 0 # replace them with 0
  #plot(pred.sbp(0:50, 30, 2, 1, 25, 0), ylim=c(0,140))
  
  #chol
  yr <- grep("year", names(chol.svylm$coefficients)) # get coefficients containing year
  chol.svylm$coefficients[yr] <- 0 # replace them with 0
  #plot(pred.chol(0:50, 30, 2, 1, 25, 0), ylim=c(3,7))
  
  #diabetes
  yr <- grep("year", names(diab.svylr$coefficients)) # get coefficients containing year
  diab.svylr$coefficients[yr] <- 0 # replace them with 0
  #plot(pred.diab(0:50, 40, 2, 5, 25, 0), ylim=c(0,0.4))
  
  # Smoking active
  yr <- grep("year", names(smok.active.svylr$coefficients)) # get coefficients containing year
  smok.active.svylr$coefficients[yr] <- 0 # replace them with 0
  
  # Smoking cessation
  yr <- grep("year", names(smok.cess.svylr$coefficients)) # get coefficients containing year
  smok.cess.svylr$coefficients[yr] <- 0 # replace them with 0
  
  # Smoking start
  yr <- grep("year", names(smok.start.svylr$coefficients)) # get coefficients containing year
  smok.start.svylr$coefficients[yr] <- 0 # replace them with 0
  
  # F&V
  yr <- grep("year", names(fv.svylr$coefficients)) # get coefficients containing year
  fv.svylr$coefficients[yr] <- 0 # replace them with 0
  
  yr <- grep("year", names(fvrate.svylr$coefficients)) # get coefficients containing year
  fvrate.svylr$coefficients[yr] <- 0 # replace them with 0
}

#bmi.svylm$coefficients["year"] <- bmi.svylm$coefficients["year"]*2 # Double the rate
#(bmi.svylm$coefficients["(Intercept)"])^(-1/2)

