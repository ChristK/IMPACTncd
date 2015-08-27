#cmpfile("./Scenarios/salt mandatory reformulation.R")
# This scenario is the salt mandatory reformulation
# Assumes that the salt consumption will reach target of 
cat("Salt mandatory reformulation\n\n")

intervention.year <- 2016
diffusion.period <- 5 # years till reaching target

# Load prediction equations
if (i == (init.year - 2011)) {
  loadcmp(file = "./risk factor trajectories.Rc", my.env)
}

# Function to apply after ageing
post.ageing.scenario.fn <- cmpfun(
  function(i, env = my.env) {
    if (i >= (intervention.year - 2011 + cvd.lag) && #10
        i < (intervention.year - 2011 + diffusion.period + cvd.lag)) { #15
      cat("Post ageing scenario function\n")
      if (POP[between(age, ageL, ageH) & sex == "1", mean(salt24h.cvdlag)] > 6) {
        POP[between(age, ageL, ageH) & sex == "1", salt24h.cvdlag.mr := 
              salt24h.cvdlag * (6 + (diffusion.period + intervention.year-2012-i+ cvd.lag) * 
                                  ((mean(salt24h.cvdlag)- 6) / diffusion.period)) / 
              mean(salt24h.cvdlag)]
      }
      if (POP[between(age, ageL, ageH) & sex == "2", mean(salt24h.cvdlag)] > 6) {
        POP[between(age, ageL, ageH) & sex == "2", salt24h.cvdlag.mr := 
              salt24h.cvdlag * (6 + (diffusion.period + intervention.year-2012 - i + cvd.lag) * 
                                  ((mean(salt24h.cvdlag)- 6) / diffusion.period)) / 
              mean(salt24h.cvdlag)]
      }
      cat("translate to sbp change\n")
      # translate to sbp change for normotensives
      POP[between(age, ageL, ageH) & omsysval.cvdlag <= 130, 
          omsysval.cvdlag := omsysval.cvdlag - 
            salt.sbp.norm(salt24h.cvdlag - salt24h.cvdlag.mr)]
      # translate to sbp change for hypertensives
      POP[between(age, ageL, ageH) & omsysval.cvdlag > 130, 
          omsysval.cvdlag := omsysval.cvdlag - 
            salt.sbp.htn(salt24h.cvdlag - salt24h.cvdlag.mr)]
      cat("delete salt24h.cvdlag.mr\n")
      POP[, `:=` (salt24h.cvdlag = salt24h.cvdlag.mr,  salt24h.cvdlag.mr = NULL)]
    }
    
    if (i >= (intervention.year-2011 + diffusion.period + cvd.lag)) { #15
      cat("Post ageing scenario function\n")
      cat("fix salt to 6g\n")
      POP[between(age, ageL, ageH), salt24h.cvdlag.mr :=  salt24h.cvdlag * 
            6 / mean(salt24h.cvdlag), by = sex]
      cat("translate to sbp change\n")
      
      # translate to sbp change for normotensives
      POP[between(age, ageL, ageH) & omsysval.cvdlag <= 130, 
          omsysval.cvdlag := omsysval.cvdlag - 
            salt.sbp.norm(salt24h.cvdlag - salt24h.cvdlag.mr)]
      # translate to sbp change for hypertensives
      POP[between(age, ageL, ageH) & omsysval.cvdlag > 130, 
          omsysval.cvdlag := omsysval.cvdlag - 
            salt.sbp.htn(salt24h.cvdlag - salt24h.cvdlag.mr)]
      
      POP[, `:=` (salt24h.cvdlag = salt24h.cvdlag.mr,  salt24h.cvdlag.mr = NULL)]
    }
    
    # For salt with cancer lag
    if (i >= (intervention.year - 2011 + cancer.lag) && #15
        i < (intervention.year - 2011 + diffusion.period + cancer.lag)) { #20
      cat("Salt for cancer\n")
      
      if (POP[between(age, ageL, ageH) & sex == "1", mean(salt24h.calag)] > 6) {
        POP[between(age, ageL, ageH) & sex == "1", salt24h.calag := 
              salt24h.calag * (6 + (diffusion.period + intervention.year-2012-i +
                                      cancer.lag) * 
                                 ((mean(salt24h.calag)- 6) / diffusion.period)) / 
              mean(salt24h.calag)]
      }
      
      if (POP[between(age, ageL, ageH) & sex == "2", mean(salt24h.calag)] > 6) {
        POP[between(age, ageL, ageH) & sex == "2", salt24h.calag := 
              salt24h.calag * (6 + (diffusion.period + intervention.year-2012-i + 
                                      cancer.lag) * 
                                 ((mean(salt24h.calag)- 6) / diffusion.period)) / 
              mean(salt24h.calag)]
      }
    }
    
    if (i >= (intervention.year-2011 + diffusion.period + cancer.lag)) { #20
      cat("Salt for cancer\n")
      POP[between(age, ageL, ageH), salt24h.calag :=  salt24h.calag * 
            6 / mean(salt24h.calag), by = sex]
    }
    
    assign("POP", POP, envir = env)
  }
)

