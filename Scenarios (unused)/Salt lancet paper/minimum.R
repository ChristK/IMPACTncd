#cmpfile("./Scenarios/minimum.R")
# This scenario is the theoretical minimum risk scenario
# Assumes that the salt consumption will reach target 3gr/d
cat("Salt theoretical minimum risk\n\n")

intervention.year <- 2003
diffusion.period <- 0 # years till reaching target

# Function to apply after ageing
post.ageing.scenario.fn <- 
  cmpfun(
    function(i, env = my.env) {
      if ((i + 2011 - cvd.lag) >= 2003) {
      POP[between(age, ageL, ageH), 
          salt24h.cvdlag.mr := c16.salt.optim]
      
      cat("translate to sbp change\n")
      # translate to sbp change 
      POP[between(age, ageL, ageH), 
          omsysval.cvdlag := omsysval.cvdlag + 
            salt.sbp.reduct(salt24h.cvdlag - salt24h.cvdlag.mr, 
                            age, omsysval.cvdlag, .N)]
      
      cat("delete salt24h.cvdlag.mr\n")
      POP[, `:=` (salt24h.cvdlag = salt24h.cvdlag.mr,  salt24h.cvdlag.mr = NULL)]
    }

    # For salt with cancer lag
    if ((i + 2011 - cancer.lag) >= 2003) { #20
      cat("Salt for cancer\n")

      POP[between(age, ageL, ageH), 
          salt24h.calag :=  c16.salt.optim]
    }
    
    assign("POP", POP, envir = env)
    }
  )

