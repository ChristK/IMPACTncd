#cmpfile("./Scenarios/high risk max.R")
# This scenario is the high risk intervention one
# Assumes that 80% of those with SBP above 140mmHg, TC above 5 mmol/l and BMI above 35 kgr/m2 have a
# reduction of 30% on their estimated values
cat("highrisk scenario\n\n")

intervention.year <- 2016

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
  post.ageing.scenario.fn <- function(i) {
    cat("Post ageing scenario function\n")
    if (i > intervention.year - 2011 + cvd.lag) {
      
      POP[, high.risk:=NULL]
      
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
      
      cat("apply treatment")
      POP[sample_frac(POP[ high.risk == T, .(id)], 0.8),
          `:=` (bmival.cvdlag = bmival.cvdlag * 0.99,
                omsysval.cvdlag = omsysval.cvdlag * 0.7,
                cholval.cvdlag = cholval.cvdlag * 0.7,
                porftvg.cvdlag = porftvg.cvdlag + 1,
                high.risk2 = T)]
      
      # surgery for morbid obesity
      POP[high.risk2 == T & bmival.cvdlag > 50, 
          bmival.cvdlag := 25]
      
      
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
      
      POP[, high.risk := NULL]
      POP[, high.risk2 := NULL]
      return()
    }
  }
}




