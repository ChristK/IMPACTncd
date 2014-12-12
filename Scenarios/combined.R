#cmpfile("./Scenarios/combined.R")
# This scenario is the combination of the population intervention with the pragmatic high risk one
cat("Combined scenario\n\n")

intervention.year <- 2016
risk.cutoff <- 0.10

# Load prediction equations
if (i == (init.year - 2011)) {
  
  
  # Load RF trajectoy functions
  loadcmp(file = "./risk factor trajectories.Rc", my.env)
  
  bnf.risk <- cmpfun(function(age, 
                              sex,
                              sbp,
                              tc,
                              smoker,
                              diabetes,
                              period = 10,
                              lvh = 0,
                              hdl = 1.4) {
    cvdrisk <- 1-exp(-exp((log(period)-(15.5305+(28.4441*(1-sex))+(-1.4792*log(age))+(0*log(age)*log(age))+(-14.4588*log(age)*(1-sex))+(1.8515*log(age)*log(age)*(1-sex))+(-0.9119*log(sbp))+(-0.2767*smoker)+(-0.7181*log(tc/hdl))+(-0.1759*diabetes)+(-0.1999*diabetes*(1-sex))+(-0.5865*lvh)+(0*lvh*sex)))/(exp(0.9145)*exp(-0.2784*(15.5305+(28.4441*(1-sex))+(-1.4792*log(age))+(0*log(age)*log(age))+(-14.4588*log(age)*(1-sex))+(1.8515*log(age)*log(age)*(1-sex))+(-0.9119*log(sbp))+(-0.2767*smoker)+(-0.7181*log(tc/hdl))+(-0.1759*diabetes)+(-0.1999*diabetes*(1-sex))+(-0.5865*lvh)+(0*lvh*sex)))))) +
      1-exp(-exp((log(period)-(26.5116+(0.2019*(1-sex))+(-2.3741*log(age))+(0*log(age)*log(age))+(0*log(age)*(1-sex))+(0*log(age)*log(age)*(1-sex))+(-2.4643*log(sbp))+(-0.3914*smoker)+(-0.0229*log(tc/hdl))+(-0.3087*diabetes)+(-0.2627*diabetes*(1-sex))+(-0.2355*lvh)+(0*lvh*sex)))/(exp(-0.4312)*exp(0*(26.5116+(0.2019*(1-sex))+(-2.3741*log(age))+(0*log(age)*log(age))+(0*log(age)*(1-sex))+(0*log(age)*log(age)*(1-sex))+(-2.4643*log(sbp))+(-0.3914*smoker)+(-0.0229*log(tc/hdl))+(-0.3087*diabetes)+(-0.2627*diabetes*(1-sex))+(-0.2355*lvh)+(0*lvh*sex))))))
    
    return(cvdrisk)
  }
  )
  
  
  # Function to apply after ageing
  post.ageing.scenario.fn <- cmpfun(function(i) {
    cat("Post ageing scenario function\n") 
    if (i > intervention.year - 2011  & i <= intervention.year - 2011 + 5) {
      POP[cigst1 == "4", cigst1.temp := ifelse(dice(.N) < 0.1 + (i-(intervention.year - 2011 + cvd.lag))/50, T, F)] 
      POP[cigst1.temp == T, `:=`(cigst1 = "3", endsmoke.curr = 0, numsmok.curr = cigdyalCat.curr)]
      POP[, cigst1.temp := NULL]
    }
    
    if (i > intervention.year - 2011) {
      
    
      POP[between(age, ageL, ageH),
          high.risk := risk.cutoff < bnf.risk(age, 
                                              sex == "1",
                                              omsysval.cvdlag,
                                              cholval.cvdlag,
                                              cigst1.cvdlag == "4",
                                              diabtotr.cvdlag == "2")]
      
      POP[between(age, ageL, ageH) &
            (age >=75 | 
               chd.incidence > 0 |
               stroke.incidence > 0 |
               (diabtotr.cvdlag == "2" & age >=40)), 
          high.risk := T]
      
      setkey(POP, id)
      
      # Social gradient
      POP[sample_frac(POP[ high.risk == T & qimd == "1", .(id)], 0.7),
          `:=` (high.risk2 = T)]
      POP[sample_frac(POP[ high.risk == T & qimd == "2", .(id)], 0.65),
          `:=` (high.risk2 = T)]
      POP[sample_frac(POP[ high.risk == T & qimd == "3", .(id)], 0.6),
          `:=` (high.risk2 = T)]
      POP[sample_frac(POP[ high.risk == T & qimd == "4", .(id)], 0.55),
          `:=` (high.risk2 = T)]
      POP[sample_frac(POP[ high.risk == T & qimd == "5", .(id)], 0.5),
          `:=` (high.risk2 = T)]
      
      cat("apply smoking treatment")
      POP[cigst1.cvdlag == "4" & high.risk2 == T, 
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
      
      
      
      assign("highrisk.rds", rbindlist(output, fill = T), my.env)
      
      rm(output)
      
      if (i == yearstoproject + init.year - 2012) {
        saveRDS(highrisk.rds, file = paste0(output.dir(), "highrisk.rds"))
      }
    }
    
    if (i > intervention.year - 2011 + cvd.lag) {
      
      cat("apply treatment")
      POP[high.risk2 == T,
          `:=` (bmival.cvdlag = bmival.cvdlag * 0.99,
                omsysval.cvdlag = omsysval.cvdlag * 0.7,
                cholval.cvdlag = cholval.cvdlag * 0.7,
                porftvg.cvdlag = porftvg.cvdlag + 1)]
      
      # surgery for morbid obesity
      POP[high.risk2 == T & bmival.cvdlag > 50, 
          bmival.cvdlag := 25]
      
    }
    
    if (i > intervention.year - 2011) {       
      POP[, `:=` (high.risk = NULL,
                  high.risk2 = NULL)]
      return()
    }
    
  }
  )
}


# model bmi plateau and then reverse trend 
if (i > intervention.year - 2011 + cvd.lag & i <= intervention.year - 2011 + cvd.lag + 5) {
  cat("alter bmi intercept to reverse trends\n")
  # model bmi plateau
  bmi.svylm$coefficients[grep("year", names(bmi.svylm$coefficients))] <- 
    bmi.svylm$coefficients[grep("year", names(bmi.svylm$coefficients))]  * 0.9
  
  cat("reduce sbp intercept by 2mmhg per year for 5 years\n")
  sbp.svylm$coefficients["(Intercept)"] <- sbp.svylm$coefficients["(Intercept)"] - 2
  
  cat("reduce tc intercept by 0.1 mmol/l per year for 5 years\n")
  chol.svylm$coefficients["(Intercept)"] <- chol.svylm$coefficients["(Intercept)"] - 0.1
  
  FV.intervention <- 0.2 + (i-(intervention.year - 2011 + cvd.lag))/5
}

if (i > intervention.year - 2011 + cvd.lag + 5 & i <= intervention.year - 2011 + cvd.lag + 10) {
  cat("alter bmi intercept to reverse trends\n")
  bmi.svylm$coefficients[grep("year", names(bmi.svylm$coefficients))] <- 
    bmi.svylm$coefficients[grep("year", names(bmi.svylm$coefficients))]  * 0.8
  
  cat("reduce sbp trend \n")
  sbp.svylm$coefficients["year"] <- 
    sbp.svylm$coefficients["year"] * 0.99
  
  cat("half tc trends after intervention\n")
  chol.svylm$coefficients["year"] <- 
    chol.svylm$coefficients["year"] * 0.5
}

if (i > intervention.year - 2011 + cvd.lag + 10 & i <= intervention.year - 2011 + cvd.lag + 20) {
  cat("alter bmi intercept to reverse trends\n")
  bmi.svylm$coefficients[grep("year", names(bmi.svylm$coefficients))] <- 
    bmi.svylm$coefficients[grep("year", names(bmi.svylm$coefficients))]  * 0.7
}

