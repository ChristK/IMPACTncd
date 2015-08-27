#cmpfile("./Scenarios/health checks20+.R")
# This scenario is the high risk intervention one
# Assumes that 40% of those with SBP above 140mmHg, TC above 5 mmol/l and BMI above 35 kgr/m2 have a (based on dh)
# reduction of 30% on their estimated values
cat("health check scenario\n\n")

intervention.year <- 2011

risk.cutoff <- 0.10

if (i == (init.year - 2011)) {
  
  # Load RF trajectoy functions
  loadcmp(file = "./risk factor trajectories.Rc", my.env)
  
  bnf.risk <- cmpfun(function(age, sex, sbp, tc, smoker, diabetes, period = 10, lvh = 0, hdl = 1.4) {
    cvdrisk <- 1-exp(-exp((log(period)-(15.5305+(28.4441*(1-sex))+(-1.4792*log(age))+(0*log(age)*log(age))+(-14.4588*log(age)*(1-sex))+(1.8515*log(age)*log(age)*(1-sex))+(-0.9119*log(sbp))+(-0.2767*smoker)+(-0.7181*log(tc/hdl))+(-0.1759*diabetes)+(-0.1999*diabetes*(1-sex))+(-0.5865*lvh)+(0*lvh*sex)))/(exp(0.9145)*exp(-0.2784*(15.5305+(28.4441*(1-sex))+(-1.4792*log(age))+(0*log(age)*log(age))+(-14.4588*log(age)*(1-sex))+(1.8515*log(age)*log(age)*(1-sex))+(-0.9119*log(sbp))+(-0.2767*smoker)+(-0.7181*log(tc/hdl))+(-0.1759*diabetes)+(-0.1999*diabetes*(1-sex))+(-0.5865*lvh)+(0*lvh*sex)))))) +
      1-exp(-exp((log(period)-(26.5116+(0.2019*(1-sex))+(-2.3741*log(age))+(0*log(age)*log(age))+(0*log(age)*(1-sex))+(0*log(age)*log(age)*(1-sex))+(-2.4643*log(sbp))+(-0.3914*smoker)+(-0.0229*log(tc/hdl))+(-0.3087*diabetes)+(-0.2627*diabetes*(1-sex))+(-0.2355*lvh)+(0*lvh*sex)))/(exp(-0.4312)*exp(0*(26.5116+(0.2019*(1-sex))+(-2.3741*log(age))+(0*log(age)*log(age))+(0*log(age)*(1-sex))+(0*log(age)*log(age)*(1-sex))+(-2.4643*log(sbp))+(-0.3914*smoker)+(-0.0229*log(tc/hdl))+(-0.3087*diabetes)+(-0.2627*diabetes*(1-sex))+(-0.2355*lvh)+(0*lvh*sex))))))
    
    return(cvdrisk)
  }
  )
  
  # Function to apply after ageing
  post.ageing.scenario.fn <- cmpfun(function(i, env = my.env) {
    cat("Post ageing scenario function\n")
    if (i > intervention.year - 2011) {
      
      POP[between(age, 40, 74) &
            chd.incidence == 0 & 
            stroke.incidence == 0,
          high.risk := risk.cutoff < bnf.risk(age, 
                                              sex == "1",
                                              omsysval.cvdlag,
                                              cholval.cvdlag,
                                              cigst1 == "4",
                                              diabtotr == "2")]
      
#       POP[between(age, ageL, ageH) &
#             (age >=75 | 
#                chd.incidence > 0 |
#                stroke.incidence > 0 |
#                (diabtotr.cvdlag == "2" & age >=40)), 
#           high.risk := T]
      
          # setkey(POP, id) # breaks diabetes. WHY?
      # Social gradient
      if (i == intervention.year - 2011 + 1) {
        diffusion <- sample_frac(POP[ high.risk == T & qimd == "1", .(id)], 0.07)
        POP[id %in% diffusion,
            `:=` (high.risk2 = T,
                  high.risk6 = T)]
        diffusion <- sample_frac(POP[ high.risk == T & qimd == "2", .(id)], 0.06)
        POP[id %in% diffusion,
            `:=` (high.risk2 = T,
                  high.risk6 = T)]
        diffusion <- sample_frac(POP[ high.risk == T & qimd == "3", .(id)], 0.05)
        POP[id %in% diffusion,
            `:=` (high.risk2 = T,
                  high.risk6 = T)]
        diffusion <- sample_frac(POP[ high.risk == T & qimd == "4", .(id)], 0.04)
        POP[id %in% diffusion,
            `:=` (high.risk2 = T,
                  high.risk6 = T)]
        diffusion <- sample_frac(POP[ high.risk == T & qimd == "5", .(id)], 0.03)
        POP[id %in% diffusion,
            `:=` (high.risk2 = T,
                  high.risk6 = T)]
      }
      
      if (i == intervention.year - 2011 + 2) {
        diffusion <- sample_frac(POP[ high.risk == T & qimd == "1", .(id)], 0.12)
        POP[id %in% diffusion,
            `:=` (high.risk2 = T,
                  high.risk5 = T)]
        diffusion <- sample_frac(POP[ high.risk == T & qimd == "2", .(id)], 0.11)
        POP[id %in% diffusion,
            `:=` (high.risk2 = T,
                  high.risk5 = T)]
        diffusion <- sample_frac(POP[ high.risk == T & qimd == "3", .(id)], 0.1)
        POP[id %in% diffusion,
            `:=` (high.risk2 = T,
                  high.risk5 = T)]
        diffusion <- sample_frac(POP[ high.risk == T & qimd == "4", .(id)], 0.09)
        POP[id %in% diffusion,
            `:=` (high.risk2 = T,
                  high.risk5 = T)]
        diffusion <- sample_frac(POP[ high.risk == T & qimd == "5", .(id)], 0.08)
        POP[id %in% diffusion,
            `:=` (high.risk2 = T,
                  high.risk5 = T)]
      }
      
      if (i == intervention.year - 2011 + 3) {
        diffusion <- sample_frac(POP[ high.risk == T & qimd == "1", .(id)], 0.19)
        POP[id %in% diffusion,
            `:=` (high.risk2 = T,
                  high.risk4 = T)]
        diffusion <- sample_frac(POP[ high.risk == T & qimd == "2", .(id)], 0.17)
        POP[id %in% diffusion,
            `:=` (high.risk2 = T,
                  high.risk4 = T)]
        diffusion <- sample_frac(POP[ high.risk == T & qimd == "3", .(id)], 0.15)
        POP[id %in% diffusion,
            `:=` (high.risk2 = T,
                  high.risk4 = T)]
        diffusion <- sample_frac(POP[ high.risk == T & qimd == "4", .(id)], 0.13)
        POP[id %in% diffusion,
            `:=` (high.risk2 = T,
                  high.risk4 = T)]
        diffusion <- sample_frac(POP[ high.risk == T & qimd == "5", .(id)], 0.11)
        POP[id %in% diffusion,
            `:=` (high.risk2 = T,
                  high.risk4 = T)]
      }
      
      if (i == intervention.year - 2011 + 4) {
        diffusion <- sample_frac(POP[ high.risk == T & qimd == "1", .(id)], 0.23)
        POP[id %in% diffusion,
            `:=` (high.risk2 = T,
                  high.risk3 = T)]
        diffusion <- sample_frac(POP[ high.risk == T & qimd == "2", .(id)], 0.20)
        POP[id %in% diffusion,
            `:=` (high.risk2 = T,
                  high.risk3 = T)]
        diffusion <- sample_frac(POP[ high.risk == T & qimd == "3", .(id)], 0.17) #0.18
        POP[id %in% diffusion,
            `:=` (high.risk2 = T,
                  high.risk3 = T)]
        diffusion <- sample_frac(POP[ high.risk == T & qimd == "4", .(id)], 0.15)
        POP[id %in% diffusion,
            `:=` (high.risk2 = T,
                  high.risk3 = T)]
        diffusion <- sample_frac(POP[ high.risk == T & qimd == "5", .(id)], 0.13)
        POP[id %in% diffusion,
            `:=` (high.risk2 = T,
                  high.risk3 = T)]
      }
      
      if (i >= intervention.year - 2011 + 5) {
        diffusion <- sample_frac(POP[ high.risk == T & qimd == "1", .(id)], 0.25)
        POP[id %in% diffusion,
            `:=` (high.risk2 = T)]
        diffusion <- sample_frac(POP[ high.risk == T & qimd == "2", .(id)], 0.225)
        POP[id %in% diffusion,
            `:=` (high.risk2 = T)]
        diffusion <- sample_frac(POP[ high.risk == T & qimd == "3", .(id)], 0.2)
        POP[id %in% diffusion,
            `:=` (high.risk2 = T)]
        diffusion <- sample_frac(POP[ high.risk == T & qimd == "4", .(id)], 0.165) # 0.175
        POP[id %in% diffusion,
            `:=` (high.risk2 = T)]
        diffusion <- sample_frac(POP[ high.risk == T & qimd == "5", .(id)], 0.15)
        POP[id %in% diffusion,
            `:=` (high.risk2 = T)]
      }
      
      cat("apply smoking treatment")
      POP[cigst1 == "4" & high.risk2 == T, 
          cigst1.temp := ifelse(dice(.N) < 0.1, T, F)] 
      
      POP[cigst1.temp == T, `:=`(cigst1 = "3", endsmoke.curr = 0, numsmok.curr = cigdyalCat.curr)]
      
      POP[, cigst1.temp := NULL]
      
      output <- vector("list", 2)
      
      if (exists("highrisk.rds")) output[[1]] <- highrisk.rds
      
      output[[2]] <- merge(POP[between(age, ageL, ageH), 
                               pop.summ(.N),
                               by = c("sex", "agegroup", "qimd")], 
                           POP[between(age, ageL, ageH) & high.risk == T,
                               list(high.risk = .N), 
                               by = c("sex", "agegroup", "qimd")],
                           by = c("sex", "agegroup", "qimd"),
                           all.x = T)
      
      
      
      assign("highrisk.rds", rbindlist(output, fill = T), env)
      
      rm(output)
      
      if (i == yearstoproject + init.year - 2012) {
        saveRDS(highrisk.rds, file = paste0(output.dir(), "highrisk.rds"))
      }
    }
    
    if (i > intervention.year - 2011 + cvd.lag) {
      
      if (i == intervention.year - 2011 + cvd.lag + 1) {
        POP[, high.risk2 := high.risk6]
        POP[, high.risk6 := NULL]
      }
      
      if (i == intervention.year - 2011 + cvd.lag + 2) {
        POP[, high.risk2 := high.risk5]
        POP[, high.risk5 := NULL]
      }
      
      if (i == intervention.year - 2011 + cvd.lag + 3) {
        POP[, high.risk2 := high.risk4]
        POP[, high.risk4 := NULL]
      }
      
      if (i == intervention.year - 2011 + cvd.lag + 4) {
        POP[, high.risk2 := high.risk3]
        POP[, high.risk3 := NULL]
      }
      
      cat("apply treatment")
      POP[high.risk2 == T,
          `:=` (bmival.cvdlag = bmival.cvdlag * 0.99)]
      
      POP[high.risk2 == T & porftvg.cvdlag < 8,
          `:=` (porftvg.cvdlag = porftvg.cvdlag + 1)]
      
      POP[high.risk2 == T & a30to06m.cvdlag < 7,
          `:=` (a30to06m.cvdlag = a30to06m.cvdlag + 1)]
      
      POP[high.risk2 == T & omsysval.cvdlag > 135 & age < 80,
          `:=` (omsysval.cvdlag = 135)]
      
      POP[high.risk2 == T & omsysval.cvdlag > 145 & age >= 80,
          `:=` (omsysval.cvdlag = 145)]
      
      POP[high.risk2 == T & cholval.cvdlag > 4,
          `:=` (cholval.cvdlag = 4)]
      
      # surgery for morbid obesity
      POP[high.risk2 == T & bmival.cvdlag > 50, 
          bmival.cvdlag := 30]
      
    }
    
    if (i > intervention.year - 2011) {       
      POP[, `:=` (high.risk = NULL,
                  high.risk2 = NULL)]
    }
    assign("POP", POP, envir = env)
  }
  )
}




