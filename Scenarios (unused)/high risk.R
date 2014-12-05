# This scenario is the high risk intervention one
# Assumes that 80% of those with SBP above 140mmHg, TC above 5 mmol/l and BMI above 35 kgr/m2 have a
# reduction of 30% on their estimated values
cat("highrisk scenario\n\n")

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
    if (i >= (intervention.year - 2011 + cvd.lag)) {
      
      setkey(POP, id)
      cat("Post ageing scenario function")
      POP[sample_frac(POP[bmival.cvdlag >= 35, .(id)], 0.8), bmival.cvdlag := bmival.cvdlag * 0.7]
      POP[sample_frac(POP[omsysval.cvdlag >= 140, .(id)], 0.8), omsysval.cvdlag := omsysval.cvdlag * 0.7]
      POP[sample_frac(POP[cholval.cvdlag >= 5, .(id)], 0.8), cholval.cvdlag := cholval.cvdlag * 0.7]
      return()
    }
  }
}


