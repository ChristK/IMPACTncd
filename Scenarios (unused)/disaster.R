# This scenario is the disaster one
# Assumes that the all trends except (bmi and diabetes) will backward at 2016 
cat("disaster scenario\n\n")

intervention.year <- 2016

# Load prediction equations
if (i == (init.year - 2011)) {
  
  #sys.source(file = "./risk factor trajectories.R", my.env)
  loadcmp(file = "./risk factor trajectories.Rc", my.env)
  
  # Function to apply after ageing
    post.ageing.scenario.fn <- function(i) {
    cat("Post ageing scenario function")
  }
}

if (i == intervention.year - 2011 + cvd.lag) {
  cat("alter i to reverse trends")
  body(pred.sbp)[[6]][[3]][[2]][[3]][[2]][[2]] <- 
    substitute((intervention.year - 2011 + cvd.lag) * 2 - year)
  
  body(pred.chol)[[5]][[3]][[2]][[3]][[2]][[2]] <- 
    substitute((intervention.year - 2011 + cvd.lag) * 2 - year)
  
  body(pred.nev0sm1)[[4]][[3]][[2]][[3]][[2]]  <- 
    substitute((intervention.year - 2011 + cvd.lag) * 2 - year)
  
  body(pred.sm0ex1)[[4]][[3]][[2]][[3]][[2]][[2]] <- 
    substitute((intervention.year - 2011 + cvd.lag) * 2 - year)
  
  body(pred.sm0prev)[[4]][[3]][[2]][[3]][[2]] <- 
    substitute((intervention.year - 2011 + cvd.lag) * 2 - year)
  
  body(pred.fv)[[4]][[3]][[2]][[3]][[2]][[2]] <- 
    substitute((intervention.year - 2011 + cvd.lag) * 2 - year)
  
  body(pred.fvrate)[[4]][[3]][[2]][[3]][[2]][[2]] <- 
    substitute((intervention.year - 2011 + cvd.lag) * 2 - year)
}

if (i == (intervention.year - 2011 + cvd.lag) * 2 + 10 - cvd.lag) {
  cat("alter i to fix at 2001")
  body(pred.sbp)[[6]][[3]][[2]][[3]][[2]][[2]] <- 
    substitute(lag - 10*year/(year + 1))
  
  body(pred.chol)[[5]][[3]][[2]][[3]][[2]][[2]] <- 
    substitute(lag - 10*year/(year + 1))
  
  body(pred.nev0sm1)[[4]][[3]][[2]][[3]][[2]][[2]]  <- 
    substitute(lag - 10*year/(year + 1))
  
  body(pred.sm0ex1)[[4]][[3]][[2]][[3]][[2]][[2]] <- 
    substitute(lag - 10*year/(year + 1))
  
  body(pred.sm0prev)[[4]][[3]][[2]][[3]][[2]] <- 
    substitute(- 10*year/(year + 1))
  
  body(pred.fv)[[4]][[3]][[2]][[3]][[2]][[2]] <- 
    substitute(lag - 10*year/(year + 1))
  
  body(pred.fvrate)[[4]][[3]][[2]][[3]][[2]][[2]] <- 
    substitute(lag - 10*year/(year + 1))
}
