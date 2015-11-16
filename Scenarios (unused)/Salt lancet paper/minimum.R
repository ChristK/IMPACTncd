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

#cmpfile("./Scenarios/minimum.R")
# This scenario is the theoretical minimum risk scenario
# Assumes that the salt consumption will reach target 3gr/d
cat("Salt theoretical minimum risk\n\n")

intervention.year <- 2003
diffusion.period <- 0 # years till reaching target

# Function to apply after ageing
post.ageing.scenario.fn <- 
  cmpfun(
    function(i, env = my.env) {
      if ((i + 2011 - cvd.lag) >= 2003) {
      POP[between(age, ageL, ageH), 
          salt24h.cvdlag.mr := c16.salt.optim]
      
      cat("translate to sbp change\n")
      # translate to sbp change 
      POP[between(age, ageL, ageH), 
          omsysval.cvdlag := omsysval.cvdlag + 
            salt.sbp.reduct(salt24h.cvdlag - salt24h.cvdlag.mr, 
                            age, omsysval.cvdlag, .N)]
      
      cat("delete salt24h.cvdlag.mr\n")
      POP[, `:=` (salt24h.cvdlag = salt24h.cvdlag.mr,  salt24h.cvdlag.mr = NULL)]
    }

    # For salt with cancer lag
    if ((i + 2011 - cancer.lag) >= 2003) { #20
      cat("Salt for cancer\n")

      POP[between(age, ageL, ageH), 
          salt24h.calag :=  c16.salt.optim]
    }
    
    assign("POP", POP, envir = env)
    }
  )

