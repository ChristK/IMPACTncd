# This scenario is the disaster one
# Assumes that the all trends except (bmi and diabetes) will backward at 2016 
cat("disaster scenario\n\n")

intervention.year <- 2016

# Load prediction equations
if (i == (init.year - 2011)) {
    load(file="./Lagtimes/bmi.svylm.rda")
    load(file="./Lagtimes/chol.svylm.rda")
    load(file="./Lagtimes/sbp.svylm.rda")
    load(file="./Lagtimes/diab.svylr.rda")
    load(file="./Lagtimes/smok.active.svylr.rda")
    load(file="./Lagtimes/smok.cess.svylr.rda")
    load(file="./Lagtimes/smok.cess.success.rda")
    load(file="./Lagtimes/smok.start.svylr.rda")
    load(file="./Lagtimes/fv.svylr.rda")
    load(file="./Lagtimes/fvrate.svylr.rda")
}

if (i == (intervention.year - 2011 + cvd.lag)) {
  body(pred.sbp)[[5]][[3]][[2]][[3]][[2]][[2]] <- 
    substitute((intervention.year - 2011) * 2 - year)
  
  body(pred.chol)[[5]][[3]][[2]][[3]][[2]][[2]] <- 
    substitute((intervention.year - 2011) * 2 - year)
  
  body(pred.nev0sm1)[[4]][[3]][[2]][[3]][[2]]  <- 
    substitute((intervention.year - 2011) * 2 - year)
  
  body(pred.sm0ex1)[[4]][[3]][[2]][[3]][[2]] <- 
    substitute((intervention.year - 2011) * 2 - year)
  
  body(pred.sm0prev)[[4]][[3]][[2]][[3]][[2]] <- 
    substitute((intervention.year - 2011) * 2 - year)
  
  body(pred.fv)[[5]][[3]][[2]][[3]][[2]][[2]] <- 
    substitute((intervention.year - 2011) * 2 - year)
  
  body(pred.fvrate)[[4]][[3]][[2]][[3]][[2]][[2]] <- 
    substitute((intervention.year - 2011) * 2 - year)
  
#   chol.svylm$coefficients["year"] <- chol.svylm$coefficients[
#     grep("year", names(chol.svylm$coefficients))] * (-1)
#   
#   sbp.svylm$coefficients["year"] <- sbp.svylm$coefficients[
#     grep("year", names(sbp.svylm$coefficients))] * (-1)
#   
#   smok.active.svylr$coefficients["year"] <- smok.active.svylr$coefficients[
#     grep("year", names(smok.active.svylr$coefficients))] * (-1)
#   
#   smok.cess.svylr$coefficients["year"] <- smok.cess.svylr$coefficients[
#     grep("year", names(smok.cess.svylr$coefficients))] * (-1)
#   
#   smok.cess.success$coefficients["year"] <- smok.cess.success$coefficients[
#     grep("year", names(smok.cess.success$coefficients))] * (-1)
#   
#   smok.start.svylr$coefficients["year"] <- smok.start.svylr$coefficients[
#     grep("year", names(smok.start.svylr$coefficients))] * (-1)
#   
#   fv.svylr$coefficients["year"] <- fv.svylr$coefficients[
#     grep("year", names(fv.svylr$coefficients))] * (-1)
#   
#   fvrate$coefficients["year"] <- fvrate$coefficients[
#     grep("year", names(fvrate$coefficients))] * (-1)
}
