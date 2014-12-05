# Tris scenario assumes exposure stable at 2011 levels
cat("no SEC gradient scenario (2011 exposures)\n\n")

intervention.year <- 2016

# Load prediction equations
if (i == (init.year-2011)) {
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
  # Function to apply after ageing
  post.ageing.scenario.fn <- function() {
    cat("Post ageing scenario function")
  }
}

if (i == intervention.year - 2011 + cvd.lag) {
  cat("alter regressions")
  #bmi
  yr <- grep("qimd", names(bmi.svylm$coefficients)) # get coefficients containing qimd
  bmi.svylm$coefficients[yr] <- 0 # replace them with 0
  #plot(pred.bmi(0:50, 30, 2, 1, 0), ylim=c(0,40))
  
  #sbp
  yr <- grep("qimd", names(sbp.svylm$coefficients)) # get coefficients containing qimd
  sbp.svylm$coefficients[yr] <- 0 # replace them with 0
  #plot(pred.sbp(0:50, 30, 2, 1, 25, 0), ylim=c(0,140))
  
  #chol
  yr <- grep("qimd", names(chol.svylm$coefficients)) # get coefficients containing qimd
  chol.svylm$coefficients[yr] <- 0 # replace them with 0
  #plot(pred.chol(0:50, 30, 2, 1, 25, 0), ylim=c(3,7))
  
  #diabetes
  yr <- grep("qimd", names(diab.svylr$coefficients)) # get coefficients containing qimd
  diab.svylr$coefficients[yr] <- 0 # replace them with 0
  #plot(pred.diab(0:50, 40, 2, 5, 25, 0), ylim=c(0,0.4))
  
  # Smoking active
  yr <- grep("qimd", names(smok.active.svylr$coefficients)) # get coefficients containing qimd
  smok.active.svylr$coefficients[yr] <- 0 # replace them with 0
  
  # Smoking cessation
  yr <- grep("qimd", names(smok.cess.svylr$coefficients)) # get coefficients containing qimd
  smok.cess.svylr$coefficients[yr] <- 0 # replace them with 0
  
  # Smoking start
  yr <- grep("qimd", names(smok.start.svylr$coefficients)) # get coefficients containing qimd
  smok.start.svylr$coefficients[yr] <- 0 # replace them with 0
  
  # F&V
  yr <- grep("qimd", names(fv.svylr$coefficients)) # get coefficients containing qimd
  fv.svylr$coefficients[yr] <- 0 # replace them with 0
  
  yr <- grep("qimd", names(fvrate.svylr$coefficients)) # get coefficients containing qimd
  fvrate.svylr$coefficients[yr] <- 0 # replace them with 0
}

#bmi.svylm$coefficients["year"] <- bmi.svylm$coefficients["year"]*2 # Double the rate
#(bmi.svylm$coefficients["(Intercept)"])^(-1/2)

