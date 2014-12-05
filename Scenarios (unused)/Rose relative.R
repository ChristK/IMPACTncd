# This scenario is the relative population level interventions one
# Assumes that the SBP, TC, BMI will drop by a 5% from the estimated one in the baseline scenario, 
# every year

cat("Rose relative scenario\n\n")

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
  
  post.ageing.scenario.fn <- function() {
    if (i >= (intervention.year - 2011 + cvd.lag)) {
      cat("Post ageing scenario function")
      POP[, bmival.cvdlag := bmival.cvdlag * 0.95]
      POP[, cholval.cvdlag := cholval.cvdlag * 0.95]
      POP[, omsysval.cvdlag := omsysval.cvdlag * 0.95]
    }
  }
}




