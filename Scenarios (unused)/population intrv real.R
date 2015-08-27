#cmpfile("./Scenarios/population intrv real.R")
# This scenario is the absolute population level interventions one
# Assumes that the SBP, TC, BMI will drop by a specific amount from the estimated one in the baseline scenario, 
# every year

cat("population scenario\n\n")

intervention.year <- 2011

# Load prediction equations
if (i == (init.year - 2011)) {
  
  # Load RF trajectoy functions
  #sys.source(file = "./risk factor trajectories.R", my.env)
  loadcmp(file = "./risk factor trajectories.Rc", my.env)
  
  # Function to apply after ageing
  post.ageing.scenario.fn <- cmpfun(function(i) {
    cat("Post ageing scenario function\n") 
    if (i > intervention.year - 2011  & i <= intervention.year - 2011 + 5) {
      POP[cigst1 == "4", cigst1.temp := ifelse(dice(.N) < 0.1 + (i-(intervention.year - 2011 + cvd.lag))/50, T, F)] 
      POP[cigst1.temp == T, `:=`(cigst1 = "3", endsmoke.curr = 0, numsmok.curr = cigdyalCat.curr)]
      POP[, cigst1.temp := NULL]
    }
  }
  )
}

# model bmi plateau and then reverse trend 
if (i > intervention.year - 2011 + cvd.lag & i <= intervention.year - 2011 + cvd.lag + 5) {
  cat("alter bmi intercept to reverse trends\n")
  # model bmi plateau
  bmi.svylm$coefficients[grep("year", names(bmi.svylm$coefficients))] <- 
    bmi.svylm$coefficients[grep("year", names(bmi.svylm$coefficients))]  * 0.98
  
  cat("reduce sbp intercept by 1 mmHg per year for 5 years\n")
  sbp.svylm$coefficients["(Intercept)"] <- sbp.svylm$coefficients["(Intercept)"] - 1
  
  cat("reduce tc intercept by 0.05 mmol/l per year for 5 years\n")
  chol.svylm$coefficients["(Intercept)"] <- chol.svylm$coefficients["(Intercept)"] - 0.05
  
  FV.intervention <- 1 * (i-(intervention.year - 2011 + cvd.lag))/5
  PA.intervention <- 2.5 * (i-(intervention.year - 2011 + cvd.lag))/5 # should be 1 instead of 2.5, but
  # because I use a negbinom distr instead of a poisson, a adding 2.5 to mean, translates to increase of ~1 day in the population 
}

if (i > intervention.year - 2011 + cvd.lag + 5 & i <= intervention.year - 2011 + cvd.lag + 10) {
  cat("alter bmi intercept to reverse trends\n")
  bmi.svylm$coefficients[grep("year", names(bmi.svylm$coefficients))] <- 
    bmi.svylm$coefficients[grep("year", names(bmi.svylm$coefficients))] * 0.96
  
  cat("reduce sbp trend \n")
  sbp.svylm$coefficients["year"] <- 
    sbp.svylm$coefficients["year"] * 0.95
#   
#   cat("half tc trends after intervention\n")
#   chol.svylm$coefficients["year"] <- 
#     chol.svylm$coefficients["year"] * 0.95
}

if (i > intervention.year - 2011 + cvd.lag + 10 & i <= intervention.year - 2011 + cvd.lag + 20) {
  cat("alter bmi intercept to reverse trends\n")
  bmi.svylm$coefficients[grep("year", names(bmi.svylm$coefficients))] <- 
    bmi.svylm$coefficients[grep("year", names(bmi.svylm$coefficients))]  * 0.99
}
