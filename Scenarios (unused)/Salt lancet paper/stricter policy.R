#cmpfile("./Scenarios/stricter policy.R")
# This scenario is the salt mandatory reformulation
# Assumes that the salt consumption will reach target of 
cat("Salt stricter policy 2015\n\n")

intervention.year <- 2015
diffusion.period <- 5 # years till reaching target

# Load prediction equations
if (i == (init.year - 2011)) loadcmp("./risk factor trajectories.Rc", my.env)

# Function to apply after ageing
post.ageing.scenario.fn <- cmpfun(
  function(i, env = my.env) {
    # For salt with cancer lag
    if (i >= (intervention.year - 2011 + cancer.lag) && #15
        i <  (intervention.year - 2011 + diffusion.period + cancer.lag)) { #20
      cat("Salt for cancer\n")
      
      if (POP[between(age, 20 + cancer.lag, 64 + cancer.lag), mean(salt24h.calag)] > c16.salt.mr) {
        xx <- POP[between(age, 20 + cancer.lag, 64 + cancer.lag), mean(salt24h.calag)]
        
        POP[between(age, ageL, ageH), salt24h.calag := 
              salt24h.calag * (c16.salt.mr + (diffusion.period + intervention.year -
                                      2012 - i + cancer.lag) * 
                                 ((xx - c16.salt.mr) / diffusion.period)) / xx]
      }
      
      # uncomment to enable mean 6gr separately for sexes
      #       if (POP[between(age, 30, 74) & sex == "1", mean(salt24h.calag)] > 6) {
      #         # store the mean salt for ages 30-74 (with lag 20-64)
      #         xx <- POP[between(age, 30, 74) & sex == "1", mean(salt24h.calag)]
      #         POP[between(age, ageL, ageH) & sex == "1", salt24h.calag := 
      #               salt24h.calag * (6 + (diffusion.period + intervention.year - 
      #                                       2012 - i + cancer.lag) * 
      #                                  ((xx - 6) / diffusion.period)) / xx]
      #       }
      #       
      #       if (POP[between(age, 30, 74) & sex == "2", mean(salt24h.calag)] > 6) {
      #         xx <- POP[between(age, 30, 74) & sex == "2", mean(salt24h.calag)]
      #         POP[between(age, ageL, ageH) & sex == "2", salt24h.calag := 
      #               salt24h.calag * (6 + (diffusion.period + intervention.year -
      #                                       2012 - i + cancer.lag) * 
      #                                  ((xx - 6) / diffusion.period)) / xx]
      #       }
    }
    
    if (i >= (intervention.year - 2011 + diffusion.period + cancer.lag)) { #20
      cat("Salt for cancer\n")
      xx <- POP[between(age, 20 + cancer.lag, 64 + cancer.lag), mean(salt24h.calag)]
      
      POP[between(age, ageL, ageH), salt24h.calag :=  salt24h.calag * c16.salt.mr / xx]
    }
    
    # for cvd
    if (i >= (intervention.year - 2011 + cvd.lag) && #10
        i < (intervention.year - 2011 + diffusion.period + cvd.lag)) { #15
      cat("Post ageing scenario function\n")
      if (POP[between(age, 20 + cvd.lag, 64 + cvd.lag),
              mean(salt24h.cvdlag)] > c16.salt.mr) {
        xx <- POP[between(age, 20 + cvd.lag, 64 + cvd.lag), mean(salt24h.cvdlag)]
        
        POP[between(age, ageL, ageH), 
            salt24h.cvdlag.mr := 
              salt24h.cvdlag * (
                c16.salt.mr + (diffusion.period + intervention.year - 2012 - i + cvd.lag) * 
                  ((xx - c16.salt.mr) / diffusion.period)) / xx]
      }
      cat("translate to sbp change\n")
      # translate to sbp change 
      POP[between(age, ageL, ageH), 
          omsysval.cvdlag := omsysval.cvdlag + 
            salt.sbp.reduct(salt24h.cvdlag - salt24h.cvdlag.mr, 
                            age, omsysval.cvdlag, .N)]
      
      cat("delete salt24h.cvdlag.mr\n")
      POP[, `:=` (salt24h.cvdlag = salt24h.cvdlag.mr,
                  salt24h.cvdlag.mr = NULL)]
    }
    
    if (i >= (intervention.year - 2011 + diffusion.period + cvd.lag)) { #15
      cat("Post ageing scenario function\n")
      cat("fix salt to 6g\n")
      xx <- POP[between(age, 20 + cvd.lag, 64 + cvd.lag), mean(salt24h.cvdlag)]
      
      POP[between(age, ageL, ageH), salt24h.cvdlag.mr :=  salt24h.cvdlag * 
            c16.salt.mr / xx]
      cat("translate to sbp change\n")
      
      # translate to sbp change 
      POP[between(age, ageL, ageH), 
          omsysval.cvdlag := omsysval.cvdlag + 
            salt.sbp.reduct(salt24h.cvdlag - salt24h.cvdlag.mr, 
                            age, omsysval.cvdlag, .N)]
      
      POP[, `:=` (salt24h.cvdlag = salt24h.cvdlag.mr,  salt24h.cvdlag.mr = NULL)]
    }
    assign("POP", POP, envir = env)
  }
)

