#cmpfile("./Scenarios/smoking ban 90.R")
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


# Smoking ban scenario
# Assumes total ban on tobacco products sales
cat("total tobacco ban 90\n\n")
intervention.year <- 2016
diffusion.time <- 5

if (i == (init.year - 2011)) {
  policy.effect.cigdyal  <- 0
  policy.effect.cigst1   <- 0
  millenia               <- FALSE
  # Function to apply after ageing
  post.ageing.scenario.fn <- function(i, env = my.env) {
    cat("Post ageing scenario function\n")
  }
}

# diffusion period 
if (i > (intervention.year - 2011) & i <= (intervention.year - 2011 + diffusion.time)) {
  policy.effect.cigst1 <- 0.9 * (i - (intervention.year - 2011))/diffusion.time# effect of policy on smoking prevelence (80% reduction)
}

if (i > (intervention.year - 2011 + cvd.lag) & i <= (intervention.year - 2011 + diffusion.time + cvd.lag)) {
  policy.effect.cigdyal <- 0.9 * (i - (intervention.year - 2011 + cvd.lag))/diffusion.time # effect of policy on cigarette consumption (50% reduction)
}

# full effect
if (i > (intervention.year - 2011 + diffusion.time)) {
  policy.effect.cigst1 <- 0.9 # effect of policy on smoking prevelence (80% reduction)
}

if (i > (intervention.year - 2011 + diffusion.time + cvd.lag)) {
  policy.effect.cigdyal <- 0.9 # effect of policy on cigarette consumption (50% reduction)
}
