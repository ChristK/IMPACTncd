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

#cmpfile("./Scenarios/current trends.R")
# This scenario is the fundamental one
# Assumes that the trends that where observed since 2001 will continue in the future
cat("current trends scenario\n\n")

# Load prediction equations
if (i == (init.year-2011)) {
  
  # Load RF trajectoy functions
  #cmpfile("./risk factor trajectories.R")
  #sys.source(file = "./risk factor trajectories.R", my.env)
  loadcmp(file = "./risk factor trajectories.Rc", my.env)
  
  # coefficients for salt model from the MC simulation
  load(file="./Lagtimes/salt.rq.coef.rda")
  salt.rq$coefficients <- sample(salt.rq.coef,1)[[1]] 
  #salt.rq$coefficients <- apply(simplify2array(salt.rq.coef), 1:2, mean) # mean of MC
  
  # Function to apply after ageing
  post.ageing.scenario.fn <- function(i) {
    cat("Post ageing scenario function\n")
  }
}
