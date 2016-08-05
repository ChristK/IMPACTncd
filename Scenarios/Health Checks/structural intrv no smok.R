#cmpfile("./Scenarios/structural intrv no smok.R")
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

# This scenario is the absolute population level interventions one
# Assumes that the SBP, TC, BMI will drop by a specific amount from the estimated one in the baseline scenario, 
# every year

cat("population scenario\n\n")

intervention.year <- 2011

# Load prediction equations
if (i == (init.year - 2011)) {
  
  # Function to apply after ageing
  post.ageing.scenario.fn <- cmpfun(
    function(i
    ) {
      cat("Post ageing scenario function\n"
      ) 
    }
  )
}

# model bmi plateau and then reverse trend 
if (i > intervention.year - 2011 + cvd.lag & i <= intervention.year - 2011 + cvd.lag + 5) {
  cat("alter bmi intercept to reverse trends\n")
  # model bmi plateau
  bmi.svylm$coefficients[grep("year", names(bmi.svylm$coefficients))] <- 
    bmi.svylm$coefficients[grep("year", names(bmi.svylm$coefficients))]  * bmi.rate.decr
  
  cat("reduce sbp intercept by 0.026 mmHg per year for 5 years\n")
  sbp.svylm$coefficients["(Intercept)"] <- sbp.svylm$coefficients["(Intercept)"] - sbp.decr/5 # 1.3 in 5 years (salt mandatory reformulation)
  
  # cat("reduce tc intercept by 0.05 mmol/l per year for 5 years\n")
  # chol.svylm$coefficients["(Intercept)"] <-
  #   chol.svylm$coefficients["(Intercept)"] - 2.4*0.052/5
  
  FV.intervention <- 1L
  FV.intervention.success <- (i - cvd.lag) * fv.decr/5 # 0.8 in 5 years
}

# if (i > intervention.year - 2011 + cvd.lag + 5 & i <= intervention.year - 2011 + cvd.lag + 10) {
#   cat("alter bmi intercept to reverse trends\n")
#   bmi.svylm$coefficients[grep("year", names(bmi.svylm$coefficients))] <- 
#     bmi.svylm$coefficients[grep("year", names(bmi.svylm$coefficients))] * 0.96
#   
#   #   cat("reduce sbp trend \n")
#   #   sbp.svylm$coefficients["year"] <- 
#   #     sbp.svylm$coefficients["year"] * 0.95
#   #   
#   #   cat("half tc trends after intervention\n")
#   #   chol.svylm$coefficients["year"] <- 
#   #     chol.svylm$coefficients["year"] * 0.95
# }
# 
# if (i > intervention.year - 2011 + cvd.lag + 10 & i <= intervention.year - 2011 + cvd.lag + 20) {
#   cat("alter bmi intercept to reverse trends\n")
#   bmi.svylm$coefficients[grep("year", names(bmi.svylm$coefficients))] <- 
#     bmi.svylm$coefficients[grep("year", names(bmi.svylm$coefficients))]  * 0.99
# }
