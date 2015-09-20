#cmpfile("./Scenarios/salt no intervention.R")
# This scenario is the no intervention for salt scenario
cat("salt no intervention scenario\n\n")

# Load prediction equations
if (i == (init.year - 2011)) {

  # Function to apply after ageing
  post.ageing.scenario.fn <- function(i, env = my.env) {
    cat("Post ageing scenario function\n")
    
    cat("translate salt to sbp change\n")
    # translate to sbp change 
    POP[between(age, ageL, ageH), 
        omsysval.cvdlag := omsysval.cvdlag + 
          salt.sbp.reduct(salt24h.cvdlag.alt - salt24h.cvdlag, 
                          age, omsysval.cvdlag, .N)]
    
    cat("delete salt24h.cvdlag.alt\n")
    POP[, `:=`(salt24h.cvdlag.alt = NULL)]
    
    assign("POP", POP, envir = env)
    
  }
}

