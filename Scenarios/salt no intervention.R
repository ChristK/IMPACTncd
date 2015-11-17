#cmpfile("./Scenarios/salt no intervention.R")
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


# This scenario is the no intervention for salt scenario
cat("salt no intervention scenario\n\n")

# Load prediction equations
if (i == (init.year - 2011)) {

  # Function to apply after ageing
  post.ageing.scenario.fn <- function(i, env = my.env) {
    cat("Post ageing scenario function\n")
    
    cat("translate salt to sbp change\n")
    # translate to sbp change 
    set(POP, NULL, "salt.diff",  0)
    POP[between(age, ageL, ageH) & 
          salt24h.cvdlag > c16.salt.optim &
          salt24h.cvdlag.alt > c16.salt.optim, 
        salt.diff := salt24h.cvdlag.alt - salt24h.cvdlag]
    POP[between(age, ageL, ageH) & 
          salt24h.cvdlag > c16.salt.optim &
          salt24h.cvdlag.alt <= c16.salt.optim, 
        salt.diff := c16.salt.optim - salt24h.cvdlag]
    POP[between(age, ageL, ageH) & 
          salt24h.cvdlag <= c16.salt.optim &
          salt24h.cvdlag.alt > c16.salt.optim, 
        salt.diff := salt24h.cvdlag.alt - c16.salt.optim]
    POP[between(age, ageL, ageH), 
        omsysval.cvdlag := omsysval.cvdlag + 
          salt.sbp.reduct(salt.diff, 
                          age, omsysval.cvdlag, .N)]
    
    cat("delete salt24h.cvdlag.alt\n")
    POP[, `:=`(salt24h.cvdlag.alt = NULL,
               salt.diff          = NULL)]
    
    assign("POP", POP, envir = env)
    
  }
}

