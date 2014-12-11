# This scenario is the absolute population level interventions one
# Assumes that the SBP, TC, BMI will drop by a specific amount from the estimated one in the baseline scenario, 
# every year

cat("Rose absolute scenario\n\n")

intervention.year <- 2016

# Load prediction equations
if (i == (init.year - 2011)) {
#   load(file="./Lagtimes/bmi.svylm.rda")
#   load(file="./Lagtimes/chol.svylm.rda")
#   load(file="./Lagtimes/sbp.svylm.rda")
#   load(file="./Lagtimes/diab.svylr.rda")
#   load(file="./Lagtimes/smok.active.svylr.rda")
#   load(file="./Lagtimes/smok.cess.svylr.rda")
#   load(file="./Lagtimes/smok.cess.success.rda")
#   load(file="./Lagtimes/smok.start.svylr.rda")
#   load(file="./Lagtimes/fv.svylr.rda")
#   load(file="./Lagtimes/fvrate.svylr.rda")
  
  # Function to apply after ageing
  post.ageing.scenario.fn <- function() {
    cat("Post ageing scenario function")
    if (i >= (intervention.year - 2011 + cvd.lag)) {
      cat("pop intervention")
      POP[, `:=` (bmival.cvdlag = bmival.cvdlag - 2,
                  cholval.cvdlag = cholval.cvdlag - 0.4,
                  omsysval.cvdlag = omsysval.cvdlag - 10,
                  porftvg.cvdlag = porftvg.cvdlag + 1)]
      
      cat("smoking intervention")
      POP[cigst1.cvdlag == "4", cigst1.cvdlag := ifelse(dice(.N) < 0.20, "3", "4")] 
    }
  }
}
