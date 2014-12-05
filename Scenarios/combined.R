# This scenario is the absolute population level interventions one
# Assumes that the SBP, TC, BMI will drop by a specific amount from the estimated one in the baseline scenario, 
# every year

cat("Combined scenario\n\n")

intervention.year <- 2016
risk.cutoff <- 0.10

# Load prediction equations
if (i == (init.year - 2011)) {
  #   load(file="./Lagtimes/bmi.svylm.rda")
  #   load(file="./Lagtimes/chol.svylm.rda")
  #   load(file="./Lagtimes/sbp.svylm.rda")
  #   load(file="./Lagtimes/diab.svylr.rda")
  #   load(file="./Lagtimes/smok.active.svylr.rda")
  #   load(file="./Lagtimes/smok.cess.svylr.rda")
  #   load(file="./Lagtimes/smok.cess.success.rda")
  #   load(file="./Lagtimes/smok.start.svylr.rda")
  #   load(file="./Lagtimes/fv.svylr.rda")
  #   load(file="./Lagtimes/fvrate.svylr.rda")
  #   
  bnf.risk <- function(age, 
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
  
  # Function to apply after ageing
  post.ageing.scenario.fn <- function() {
    cat("Post ageing scenario function")
    if (i >= (intervention.year - 2011 + cvd.lag)) {
      cat("population part")
      POP[, `:=` (bmival.cvdlag = bmival.cvdlag - 2,
                  cholval.cvdlag = cholval.cvdlag - 0.3,
                  omsysval.cvdlag = omsysval.cvdlag - 10,
                  porftvg.cvdlag = porftvg.cvdlag + 1)]
      POP[cigst1.cvdlag == "4", cigst1.cvdlag := ifelse(dice(.N) < 0.10, "3", "4")] 
      
      POP[, high.risk:=NULL]
      
      cat("high risk estimation")
      POP[between(age, ageL, ageH),
          high.risk := risk.cutoff < bnf.risk(age, 
                                              sex=="1",
                                              omsysval.cvdlag,
                                              cholval.cvdlag,
                                              cigst1.cvdlag == "4",
                                              diabtotr.cvdlag == "2")]
      POP[between(age, ageL, ageH) &
               (age >=75 | 
               chd.incidence > 0 |
               stroke.incidence > 0 |
               (diabtotr.cvdlag == "2" &
                  age >=40)), 
          high.risk := T]
      
      setkey(POP, id)
      
      cat("treatment part")
      POP[sample_frac(POP[ high.risk == T, .(id)], 0.8),
          `:=` (bmival.cvdlag = bmival.cvdlag * 0.9,
                omsysval.cvdlag = omsysval.cvdlag * 0.7,
                cholval.cvdlag = cholval.cvdlag * 0.7,
                porftvg.cvdlag = porftvg.cvdlag + 1,
                high.risk2 = T)]
      
      cat("smoking treatment part")
      POP[cigst1.cvdlag == "4" & high.risk2 == T, 
          cigst1.cvdlag := ifelse(dice(.N) < 0.1, "3", "4")] 
      
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
      
      
      
      assign("highrisk.rds", rbindlist(output, fill = T, my.env))
      
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
