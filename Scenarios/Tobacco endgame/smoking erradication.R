#cmpfile("./Scenarios/smoking erradication.R")
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
cat("Smoking erradication\n\n")
intervention.year <- 2016


if (i == (init.year - 2011)) {
  policy.effect.cigdyal  <- 0
  policy.effect.cigst1   <- 0
  millenia               <- FALSE
  # Function to apply after ageing
  post.ageing.scenario.fn <- function(i, env = my.env) {
    cat("Post ageing scenario function\n")
    if ((i + 2011) > intervention.year) {
      POP[, `:=` (
        cigst1   = factor(1, 1:4),
        cigdyal  = 0,
        numsmok  = 0,
        endsmoke = 0L,
        smokyrs  = 0L,
        cigst1.cvdlag   = factor(1, 1:4),
        cigdyal.cvdlag  = 0,
        numsmok.cvdlag  = 0,
        endsmoke.cvdlag = 0L,
        smokyrs.cvdlag  = 0L,
        cigst1.calag   = factor(1, 1:4),
        cigdyal.calag  = 0,
        numsmok.calag  = 0,
        endsmoke.calag = 0L,
        smokyrs.calag  = 0L
      )]
      #assign("POP", POP, envir = env)
    } 
  }
}

