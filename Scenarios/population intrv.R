# This scenario is the absolute population level interventions one
# Assumes that the SBP, TC, BMI will drop by a specific amount from the estimated one in the baseline scenario, 
# every year

cat("population scenario\n\n")

intervention.year <- 2016

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
    bmi.svylm$coefficients[grep("year", names(bmi.svylm$coefficients))]  * 0.9
  
  cat("reduce sbp intercept by 2mmhg per year for 5 years\n")
  sbp.svylm$coefficients["(Intercept)"] <- sbp.svylm$coefficients["(Intercept)"] - 2
  
  cat("reduce tc intercept by 0.1 mmol/l per year for 5 years\n")
  chol.svylm$coefficients["(Intercept)"] <- chol.svylm$coefficients["(Intercept)"] - 0.1
  
  FV.intervention <- 0.2 + (i-(intervention.year - 2011 + cvd.lag))/5
}

if (i > intervention.year - 2011 + cvd.lag + 5 & i <= intervention.year - 2011 + cvd.lag + 10) {
  cat("alter bmi intercept to reverse trends\n")
  bmi.svylm$coefficients[grep("year", names(bmi.svylm$coefficients))] <- 
    bmi.svylm$coefficients[grep("year", names(bmi.svylm$coefficients))]  * 0.8
  
  cat("reduce sbp trend \n")
  sbp.svylm$coefficients["year"] <- 
    sbp.svylm$coefficients["year"] * 0.99
  
  cat("half tc trends after intervention\n")
  chol.svylm$coefficients["year"] <- 
    chol.svylm$coefficients["year"] * 0.5
}

if (i > intervention.year - 2011 + cvd.lag + 10 & i <= intervention.year - 2011 + cvd.lag + 20) {
  cat("alter bmi intercept to reverse trends\n")
  bmi.svylm$coefficients[grep("year", names(bmi.svylm$coefficients))] <- 
    bmi.svylm$coefficients[grep("year", names(bmi.svylm$coefficients))]  * 0.7
}
