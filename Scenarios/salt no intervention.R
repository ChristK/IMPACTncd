#cmpfile("./Scenarios/salt no intervention.R")
# This scenario is the no intervention for salt scenario
cat("salt no intervention scenario\n\n")

# Load prediction equations
if (i == (init.year - 2011)) {
  
  # Load RF trajectoy functions
  #cmpfile("./risk factor trajectories.R")
  #sys.source(file = "./risk factor trajectories.R", my.env)
  loadcmp(file = "./risk factor trajectories.Rc", my.env)
  
  # coefficients for salt model from the MC simulation
  load(file = "./Lagtimes/salt.rq.coef.rda")
  salt.rq$coefficients <- sample(salt.rq.coef,1)[[1]] 
  #salt.rq$coefficients <- apply(simplify2array(salt.rq.coef), 1:2, mean) # mean of MC
  
  # Function to apply after ageing
  post.ageing.scenario.fn <- function(i, env = my.env) {
    cat("Post ageing scenario function\n")
    
    cat("translate salt to sbp change\n")
    
    # translate to sbp change for normotensives
    POP[between(age, ageL, ageH) & omsysval.cvdlag <= 130, 
        omsysval.cvdlag := omsysval.cvdlag - 
          salt.sbp.norm(salt24h.cvdlag.alt - salt24h.cvdlag)]
    # translate to sbp change for hypertensives
    POP[between(age, ageL, ageH) & omsysval.cvdlag > 130, 
        omsysval.cvdlag := omsysval.cvdlag - 
          salt.sbp.htn(salt24h.cvdlag.alt - salt24h.cvdlag)]
    
    cat("delete salt24h.cvdlag.mr\n")
    POP[, `:=`(salt24h.cvdlag.alt = NULL)]
    
    assign("POP", POP, envir = env)
    
  }
}

