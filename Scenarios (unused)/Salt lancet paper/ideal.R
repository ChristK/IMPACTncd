#cmpfile("./Scenarios/ideal.R")
# This scenario is the ideal probably non feasible scenario
# Assumes that the salt consumption will reach target 3gr/d
cat("Salt ideal\n\n")

intervention.year <- 2003
diffusion.period <- 7 # years till reaching target

# Load prediction equations
if (i == (init.year - 2011)) loadcmp("./risk factor trajectories.Rc", my.env)

# Function to apply after ageing
post.ageing.scenario.fn <- cmpfun(
  function(i, env = my.env) {
    if (i >= (intervention.year - 2011 + cvd.lag) && #10
        i < (intervention.year - 2011 + diffusion.period + cvd.lag)) { #15
      cat("Post ageing scenario function\n")
      if (POP[between(age, 25, 69) & sex == "1",
              mean(salt24h.cvdlag)] > 3) {
        xx <- POP[between(age, 25, 69) & sex == "1", mean(salt24h.cvdlag)]
        
        POP[between(age, ageL, ageH) & sex == "1", 
            salt24h.cvdlag.mr := 
              salt24h.cvdlag * (
                3 + (diffusion.period + intervention.year - 2012 - i + cvd.lag) * 
                  ((xx - 3) / diffusion.period)) / xx]
      }
      if (POP[between(age, 25, 69) & sex == "2",
              mean(salt24h.cvdlag)] > 3) {
        xx <- POP[between(age, 25, 69) & sex == "2", mean(salt24h.cvdlag)]
        
        POP[between(age, ageL, ageH), 
            salt24h.cvdlag.mr := 
              salt24h.cvdlag * (
                3 + (diffusion.period + intervention.year - 2012 - i + cvd.lag) * 
                  ((xx - 3) / diffusion.period)) / xx]
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
    
    if (i >= (intervention.year - 2011 + diffusion.period + cvd.lag)) { #15
      cat("Post ageing scenario function\n")
      cat("fix salt to 3g\n")
      xx <- POP[between(age, 25, 69) & sex == "1", mean(salt24h.cvdlag)]
      
      POP[between(age, ageL, ageH) & sex == "1", salt24h.cvdlag.mr :=
            salt24h.cvdlag * 3 / xx]
      
      xx <- POP[between(age, 25, 69) & sex == "2", mean(salt24h.cvdlag)]
      
      POP[between(age, ageL, ageH) & sex == "2", salt24h.cvdlag.mr :=
            salt24h.cvdlag * 3 / xx]
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
        i <  (intervention.year - 2011 + diffusion.period + cancer.lag)) { #20
      cat("Salt for cancer\n")
      
      if (POP[between(age, 30, 74) & sex == "1", mean(salt24h.calag)] > 3) {
        # store the mean salt for ages 30-74 (with lag 20-64)
        xx <- POP[between(age, 30, 74) & sex == "1", mean(salt24h.calag)]
        POP[between(age, ageL, ageH) & sex == "1", salt24h.calag := 
              salt24h.calag * (3 + (diffusion.period + intervention.year - 
                                      2012 - i + cancer.lag) * 
                                 ((xx - 3) / diffusion.period)) / xx]
      }
      
      if (POP[between(age, 30, 74) & sex == "2", mean(salt24h.calag)] > 3) {
        xx <- POP[between(age, 30, 74) & sex == "2", mean(salt24h.calag)]
        POP[between(age, ageL, ageH) & sex == "2", salt24h.calag := 
              salt24h.calag * (3 + (diffusion.period + intervention.year -
                                      2012 - i + cancer.lag) * 
                                 ((xx - 3) / diffusion.period)) / xx]
      }
    }
    
    if (i >= (intervention.year - 2011 + diffusion.period + cancer.lag)) { #20
      cat("Salt for cancer\n")
      xx <- POP[between(age, 30, 74) & sex == "1", mean(salt24h.calag)]
      
      POP[between(age, ageL, ageH) & sex == "1", 
          salt24h.calag :=  salt24h.calag * 3 / xx]
      
      xx <- POP[between(age, 30, 74) & sex == "2", mean(salt24h.calag)]
      
      POP[between(age, ageL, ageH) & sex == "2", 
          salt24h.calag :=  salt24h.calag * 3 / xx]
    }
    
    assign("POP", POP, envir = env)
  }
)

