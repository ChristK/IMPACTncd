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

#cmpfile("./Scenarios/ideal 2015.R")
# This scenario is the ideal probably non feasible scenario
# Assumes that the salt consumption will reach target 3gr/d
cat("Salt ideal 2015\n\n")

intervention.year <- 2015
diffusion.period <- 12 # years till reaching target

# Function to apply after ageing
post.ageing.scenario.fn <- 
  cmpfun(
    function(i, env = my.env) {
      if (i >= (intervention.year - 2011 + cvd.lag) && #10
          i < (intervention.year - 2011 + diffusion.period + cvd.lag)) { #15
        cat("Post ageing scenario function\n")
        if (POP[between(age, 20 + cvd.lag, 64 + cvd.lag) & sex == "1",
                mean(salt24h.cvdlag)] > c16.salt.optim) {
          xx <- POP[between(age, 20 + cvd.lag, 64 + cvd.lag) & sex == "1", mean(salt24h.cvdlag)]
          if (xx > c16.salt.optim) {
            POP[between(age, ageL, ageH) & sex == "1", 
                salt24h.cvdlag.mr := 
                  salt24h.cvdlag * (
                    c16.salt.optim + (diffusion.period + intervention.year - 2012 - i + cvd.lag) * 
                      ((xx - c16.salt.optim) / diffusion.period)) / xx]
          } else {
            POP[between(age, ageL, ageH) & sex == "1", 
                salt24h.cvdlag.mr := salt24h.cvdlag]
          }
        }
        if (POP[between(age, 20 + cvd.lag, 64 + cvd.lag) & sex == "2",
                mean(salt24h.cvdlag)] > c16.salt.optim) {
          xx <- POP[between(age, 20 + cvd.lag, 64 + cvd.lag) & sex == "2", mean(salt24h.cvdlag)]
          if (xx > c16.salt.optim) {
            POP[between(age, ageL, ageH) & sex == "2", 
                salt24h.cvdlag.mr := 
                  salt24h.cvdlag * (
                    c16.salt.optim + (diffusion.period + intervention.year - 2012 - i + cvd.lag) * 
                      ((xx - c16.salt.optim) / diffusion.period)) / xx]
          } else {
            POP[between(age, ageL, ageH) & sex == "2", 
                salt24h.cvdlag.mr := salt24h.cvdlag]
          }
        }
        cat("translate to sbp change\n")
        # translate to sbp change 
        set(POP, NULL, "salt.diff",  0)
        POP[between(age, ageL, ageH) & 
              salt24h.cvdlag > c16.salt.optim &
              salt24h.cvdlag.mr > c16.salt.optim, 
            salt.diff := salt24h.cvdlag - salt24h.cvdlag.mr]
        POP[between(age, ageL, ageH) & 
              salt24h.cvdlag > c16.salt.optim &
              salt24h.cvdlag.mr <= c16.salt.optim, 
            salt.diff := salt24h.cvdlag - c16.salt.optim]
        POP[between(age, ageL, ageH) & 
              salt24h.cvdlag <= c16.salt.optim &
              salt24h.cvdlag.mr > c16.salt.optim, 
            salt.diff := c16.salt.optim - salt24h.cvdlag.mr]
        
        POP[between(age, ageL, ageH), 
            omsysval.cvdlag := omsysval.cvdlag + 
              salt.sbp.reduct(salt.diff, 
                              age, omsysval.cvdlag, .N)]
        
        cat("delete salt24h.cvdlag.mr\n")
        POP[, `:=` (salt24h.cvdlag    = salt24h.cvdlag.mr,
                    salt24h.cvdlag.mr = NULL,
                    salt.diff         = NULL)]
      }
      
      if (i >= (intervention.year - 2011 + diffusion.period + cvd.lag)) { #15
        cat("Post ageing scenario function\n")
        cat(paste0("fix salt to ", c16.salt.optim,"g\n"))
        xx <- POP[between(age, 20 + cvd.lag, 64 + cvd.lag) & sex == "1", mean(salt24h.cvdlag)]
        if (xx > c16.salt.optim) {
          POP[between(age, ageL, ageH) & sex == "1", salt24h.cvdlag.mr :=
                salt24h.cvdlag * c16.salt.optim / xx]
        } else {
          POP[between(age, ageL, ageH) & sex == "1", 
              salt24h.cvdlag.mr := salt24h.cvdlag]
        }
        xx <- POP[between(age, 20 + cvd.lag, 64 + cvd.lag) & sex == "2", mean(salt24h.cvdlag)]
        if (xx > c16.salt.optim) {
          POP[between(age, ageL, ageH) & sex == "2", salt24h.cvdlag.mr :=
                salt24h.cvdlag * c16.salt.optim / xx]
        }else {
          POP[between(age, ageL, ageH) & sex == "2", 
              salt24h.cvdlag.mr := salt24h.cvdlag]
        }
        cat("translate to sbp change\n")
        
        # translate to sbp change 
        set(POP, NULL, "salt.diff",  0)
        POP[between(age, ageL, ageH) & 
              salt24h.cvdlag > c16.salt.optim &
              salt24h.cvdlag.mr > c16.salt.optim, 
            salt.diff := salt24h.cvdlag - salt24h.cvdlag.mr]
        POP[between(age, ageL, ageH) & 
              salt24h.cvdlag > c16.salt.optim &
              salt24h.cvdlag.mr <= c16.salt.optim, 
            salt.diff := salt24h.cvdlag - c16.salt.optim]
        POP[between(age, ageL, ageH) & 
              salt24h.cvdlag <= c16.salt.optim &
              salt24h.cvdlag.mr > c16.salt.optim, 
            salt.diff := c16.salt.optim - salt24h.cvdlag.mr]
        
        POP[between(age, ageL, ageH), 
            omsysval.cvdlag := omsysval.cvdlag + 
              salt.sbp.reduct(salt.diff, 
                              age, omsysval.cvdlag, .N)]
        
        POP[, `:=` (salt24h.cvdlag    = salt24h.cvdlag.mr,
                    salt24h.cvdlag.mr = NULL,
                    salt.diff         = NULL)]
      }
      
      # For salt with cancer lag
      if (i >= (intervention.year - 2011 + cancer.lag) && #15
          i <  (intervention.year - 2011 + diffusion.period + cancer.lag)) { #20
        cat("Salt for cancer\n")
        
        if (POP[between(age, 20 + cancer.lag, 64 + cancer.lag) & sex == "1", mean(salt24h.calag)] > c16.salt.optim) {
          # store the mean salt for ages 30-74 (with lag 20-64)
          xx <- POP[between(age, 20 + cancer.lag, 64 + cancer.lag) & sex == "1", mean(salt24h.calag)]
          if (xx > c16.salt.optim) {
            POP[between(age, ageL, ageH) & sex == "1", salt24h.calag := 
                  salt24h.calag * (c16.salt.optim + (diffusion.period + intervention.year - 
                                                       2012 - i + cancer.lag) * 
                                     ((xx - c16.salt.optim) / diffusion.period)) / xx]
          }
        }
        
        if (POP[between(age, 20 + cancer.lag, 64 + cancer.lag) & sex == "2", mean(salt24h.calag)] > c16.salt.optim) {
          xx <- POP[between(age, 20 + cancer.lag, 64 + cancer.lag) & sex == "2", mean(salt24h.calag)]
          if (xx > c16.salt.optim) {
            POP[between(age, ageL, ageH) & sex == "2", salt24h.calag := 
                  salt24h.calag * (c16.salt.optim + (diffusion.period + intervention.year -
                                                       2012 - i + cancer.lag) * 
                                     ((xx - c16.salt.optim) / diffusion.period)) / xx]
          }
        }
      }
      
      if (i >= (intervention.year - 2011 + diffusion.period + cancer.lag)) { #20
        cat("Salt for cancer\n")
        xx <- POP[between(age, 20 + cancer.lag, 64 + cancer.lag) & sex == "1", mean(salt24h.calag)]
        if (xx > c16.salt.optim) {
          POP[between(age, ageL, ageH) & sex == "1", 
              salt24h.calag :=  salt24h.calag * c16.salt.optim / xx]
        }
        xx <- POP[between(age, 20 + cancer.lag, 64 + cancer.lag) & sex == "2", mean(salt24h.calag)]
        if (xx > c16.salt.optim) {
          POP[between(age, ageL, ageH) & sex == "2", 
              salt24h.calag :=  salt24h.calag * c16.salt.optim / xx]
        }
      }
      
      assign("POP", POP, envir = env)
    }
  )

