#cmpfile("./Scenarios/responsibility deal.R")
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


# This scenario is the fundamental one
# Assumes that the trends stops in 2011 as a result of the responsibility deal
cat("Responsibility deal scenario\n\n")
intervention.year <- 2011

if (i == (init.year - 2011)) {

  
  # Function to apply after ageing
  post.ageing.scenario.fn <- function(i, env = my.env) {
    cat("Post ageing scenario function\n")
    if (i - cvd.lag >= intervention.year - init.year) {
      POP[, salt24h.baseline := salt24h.cvdlag]
      POP[, salt24h.cvdlag := pred.salt.rd(i, age, sex, salt_rank, cvd.lag, salt_data)]
      set(POP, NULL, "salt_diff", 0)
      POP[salt24h.cvdlag > c16.salt.optim, salt_diff := salt24h.cvdlag - 
            salt24h.baseline]
      POP[between(age, ageL, ageH), omsysval.cvdlag := omsysval.cvdlag +
            salt.sbp.diff(salt_diff, age, omsysval.cvdlag, .N)]
    }
    
    if (i - cancer.lag >= intervention.year - init.year) {
      POP[, salt24h.calag.rd := pred.salt.rd(i, age, sex, salt_rank, cancer.lag, salt_data)]
      POP[salt24h.calag > c16.salt.optim, salt24h.calag := salt24h.calag.rd]
    }
  }
}


