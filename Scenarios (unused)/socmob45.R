# This scenario is the conservative social mobility one
# Assumes that , 10% of the QIMD 5 population moves permanently to QIMD 4 risk exposures every year. 
# Also, 20% of the QIMD 4 population moves permanently to QIMD 3 risk exposures every year.
cat("socmob45 scenario\n\n")

intervention.year <- 2016

# Load prediction equations
if (i == (init.year - 2011)) {
  #     load(file="./Lagtimes/bmi.svylm.rda")
  #     load(file="./Lagtimes/chol.svylm.rda")
  #     load(file="./Lagtimes/sbp.svylm.rda")
  #     load(file="./Lagtimes/diab.svylr.rda")
  #     load(file="./Lagtimes/smok.active.svylr.rda")
  #     load(file="./Lagtimes/smok.cess.svylr.rda")
  #     load(file="./Lagtimes/smok.cess.success.rda")
  #     load(file="./Lagtimes/smok.start.svylr.rda")
  #     load(file="./Lagtimes/fv.svylr.rda")
  #     load(file="./Lagtimes/fvrate.svylr.rda")
  #    
  #     # Function to apply after ageing
  post.ageing.scenario.fn <- function() {
    cat("Post ageing scenario function")
  }
}

if (i >= (intervention.year - 2011 + cvd.lag)) {
  cat("upgrade qimd")
  setkey(POP, id)
  POP[sample_frac(POP[qimd == "5", .(id)], 0.1), qimd := "4"]
  POP[sample_frac(POP[qimd == "4", .(id)], 0.1), qimd := "3"]
}

