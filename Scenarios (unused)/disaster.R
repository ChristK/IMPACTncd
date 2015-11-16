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

# This scenario is the disaster one
# Assumes that the all trends except (bmi and diabetes) will backward at 2016 
cat("disaster scenario\n\n")

intervention.year <- 2016

# Load prediction equations
if (i == (init.year - 2011)) {
  
  #sys.source(file = "./risk factor trajectories.R", my.env)
  loadcmp(file = "./risk factor trajectories.Rc", my.env)
  
  # Function to apply after ageing
    post.ageing.scenario.fn <- function(i) {
    cat("Post ageing scenario function")
  }
}

if (i == intervention.year - 2011 + cvd.lag) {
  cat("alter i to reverse trends")
  body(pred.sbp)[[6]][[3]][[2]][[3]][[2]][[2]] <- 
    substitute((intervention.year - 2011 + cvd.lag) * 2 - year)
  
  body(pred.chol)[[5]][[3]][[2]][[3]][[2]][[2]] <- 
    substitute((intervention.year - 2011 + cvd.lag) * 2 - year)
  
  body(pred.nev0sm1)[[4]][[3]][[2]][[3]][[2]]  <- 
    substitute((intervention.year - 2011 + cvd.lag) * 2 - year)
  
  body(pred.sm0ex1)[[4]][[3]][[2]][[3]][[2]][[2]] <- 
    substitute((intervention.year - 2011 + cvd.lag) * 2 - year)
  
  body(pred.sm0prev)[[4]][[3]][[2]][[3]][[2]] <- 
    substitute((intervention.year - 2011 + cvd.lag) * 2 - year)
  
  body(pred.fv)[[4]][[3]][[2]][[3]][[2]][[2]] <- 
    substitute((intervention.year - 2011 + cvd.lag) * 2 - year)
  
  body(pred.fvrate)[[4]][[3]][[2]][[3]][[2]][[2]] <- 
    substitute((intervention.year - 2011 + cvd.lag) * 2 - year)
}

if (i == (intervention.year - 2011 + cvd.lag) * 2 + 10 - cvd.lag) {
  cat("alter i to fix at 2001")
  body(pred.sbp)[[6]][[3]][[2]][[3]][[2]][[2]] <- 
    substitute(lag - 10*year/(year + 1))
  
  body(pred.chol)[[5]][[3]][[2]][[3]][[2]][[2]] <- 
    substitute(lag - 10*year/(year + 1))
  
  body(pred.nev0sm1)[[4]][[3]][[2]][[3]][[2]][[2]]  <- 
    substitute(lag - 10*year/(year + 1))
  
  body(pred.sm0ex1)[[4]][[3]][[2]][[3]][[2]][[2]] <- 
    substitute(lag - 10*year/(year + 1))
  
  body(pred.sm0prev)[[4]][[3]][[2]][[3]][[2]] <- 
    substitute(- 10*year/(year + 1))
  
  body(pred.fv)[[4]][[3]][[2]][[3]][[2]][[2]] <- 
    substitute(lag - 10*year/(year + 1))
  
  body(pred.fvrate)[[4]][[3]][[2]][[3]][[2]][[2]] <- 
    substitute(lag - 10*year/(year + 1))
}
