#cmpfile("./Scenarios/ideal.R")
# This scenario is the ideal probably non feasible scenario
# Assumes that the salt consumption will reach target 3gr/d
cat("Salt ideal\n\n")

intervention.year <- 2003
diffusion.period <- 14 # years till reaching target

# Function to apply after ageing
post.ageing.scenario.fn <- 
  cmpfun(
    function(i, env = my.env) {
      if (i >= (intervention.year - 2011 + cvd.lag) && #10
          i < (intervention.year - 2011 + diffusion.period + cvd.lag)) { #15
        cat("Post ageing scenario function\n")
        if (POP[between(age, 20 + cvd.lag, 64 + cvd.lag) & sex == "1",
                mean(salt24h.cvdlag)] > c16.salt.optim) {
          xx <- POP[between(age, 20 + cvd.lag, 64 + cvd.lag) & sex == "1", mean(salt24h.cvdlag)]
          
          POP[between(age, ageL, ageH) & sex == "1", 
              salt24h.cvdlag.mr := 
                salt24h.cvdlag * (
                  c16.salt.optim + (diffusion.period + intervention.year - 2012 - i + cvd.lag) * 
                    ((xx - c16.salt.optim) / diffusion.period)) / xx]
        }
        if (POP[between(age, 20 + cvd.lag, 64 + cvd.lag) & sex == "2",
                mean(salt24h.cvdlag)] > c16.salt.optim) {
          xx <- POP[between(age, 20 + cvd.lag, 64 + cvd.lag) & sex == "2", mean(salt24h.cvdlag)]
          
          POP[between(age, ageL, ageH) & sex == "2", 
              salt24h.cvdlag.mr := 
                salt24h.cvdlag * (
                  c16.salt.optim + (diffusion.period + intervention.year - 2012 - i + cvd.lag) * 
                    ((xx - c16.salt.optim) / diffusion.period)) / xx]
        }
        cat("translate to sbp change\n")
        # translate to sbp change 
        POP[between(age, ageL, ageH), 
            omsysval.cvdlag := omsysval.cvdlag + 
              salt.sbp.reduct(salt24h.cvdlag - salt24h.cvdlag.mr, 
                              age, omsysval.cvdlag, .N)]
        
        cat("delete salt24h.cvdlag.mr\n")
        POP[, `:=` (salt24h.cvdlag = salt24h.cvdlag.mr,  salt24h.cvdlag.mr = NULL)]
      }
      
      if (i >= (intervention.year - 2011 + diffusion.period + cvd.lag)) { #15
        cat("Post ageing scenario function\n")
        cat(paste0("fix salt to ", c16.salt.optim,"g\n"))
        xx <- POP[between(age, 20 + cvd.lag, 64 + cvd.lag) & sex == "1", mean(salt24h.cvdlag)]
        
        POP[between(age, ageL, ageH) & sex == "1", salt24h.cvdlag.mr :=
              salt24h.cvdlag * c16.salt.optim / xx]
        
        xx <- POP[between(age, 20 + cvd.lag, 64 + cvd.lag) & sex == "2", mean(salt24h.cvdlag)]
        
        POP[between(age, ageL, ageH) & sex == "2", salt24h.cvdlag.mr :=
              salt24h.cvdlag * c16.salt.optim / xx]
        cat("translate to sbp change\n")
        
        # translate to sbp change 
        POP[between(age, ageL, ageH), 
            omsysval.cvdlag := omsysval.cvdlag + 
              salt.sbp.reduct(salt24h.cvdlag - salt24h.cvdlag.mr, 
                              age, omsysval.cvdlag, .N)]
        
        POP[, `:=` (salt24h.cvdlag = salt24h.cvdlag.mr,
                    salt24h.cvdlag.mr = NULL)]
      }
      
      # For salt with cancer lag
      if (i >= (intervention.year - 2011 + cancer.lag) && #15
          i <  (intervention.year - 2011 + diffusion.period + cancer.lag)) { #20
        cat("Salt for cancer\n")
        
        if (POP[between(age, 20 + cancer.lag, 64 + cancer.lag) & sex == "1", mean(salt24h.calag)] > c16.salt.optim) {
          # store the mean salt for ages 30-74 (with lag 20-64)
          xx <- POP[between(age, 20 + cancer.lag, 64 + cancer.lag) & sex == "1", mean(salt24h.calag)]
          POP[between(age, ageL, ageH) & sex == "1", salt24h.calag := 
                salt24h.calag * (c16.salt.optim + (diffusion.period + intervention.year - 
                                        2012 - i + cancer.lag) * 
                                   ((xx - c16.salt.optim) / diffusion.period)) / xx]
        }
        
        if (POP[between(age, 20 + cancer.lag, 64 + cancer.lag) & sex == "2", mean(salt24h.calag)] > c16.salt.optim) {
          xx <- POP[between(age, 20 + cancer.lag, 64 + cancer.lag) & sex == "2", mean(salt24h.calag)]
          POP[between(age, ageL, ageH) & sex == "2", salt24h.calag := 
                salt24h.calag * (c16.salt.optim + (diffusion.period + intervention.year -
                                        2012 - i + cancer.lag) * 
                                   ((xx - c16.salt.optim) / diffusion.period)) / xx]
        }
      }
      
      if (i >= (intervention.year - 2011 + diffusion.period + cancer.lag)) { #20
        cat("Salt for cancer\n")
        xx <- POP[between(age, 20 + cancer.lag, 64 + cancer.lag) & sex == "1", mean(salt24h.calag)]
        
        POP[between(age, ageL, ageH) & sex == "1", 
            salt24h.calag :=  salt24h.calag * c16.salt.optim / xx]
        
        xx <- POP[between(age, 20 + cancer.lag, 64 + cancer.lag) & sex == "2", mean(salt24h.calag)]
        
        POP[between(age, ageL, ageH) & sex == "2", 
            salt24h.calag :=  salt24h.calag * c16.salt.optim / xx]
      }
      
      assign("POP", POP, envir = env)
    }
  )

