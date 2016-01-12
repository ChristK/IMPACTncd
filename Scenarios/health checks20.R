#cmpfile("./Scenarios/health checks20.R")
## IMPACTncd: A decision support tool for primary prevention of NCDs
## Copyright (C) 2015  Chris Kypridemos

## IMPACTncd is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program; if not, see <http://www.gnu.org/licenses/>
## or write to the Free Software Foundation, Inc., 51 Franklin Street,
## Fifth Floor, Boston, MA 02110-1301  USA.

# This scenario is the high risk intervention one

cat("health check scenario\n\n")

intervention.year <- 2011
uptake <- 0.5
risk.cutoff <- 0.20



if (i == (init.year - 2011)) {
  
  # Load RF trajectoy functions
  # bnf.risk <- cmpfun(
  #   function(age, sex, sbp, tctohdl, smoker, diabetes, period = 10, lvh = 0) {
  #     cvdrisk <- 1-exp(-exp((log(period)-(15.5305+(28.4441*(1-sex))+(-1.4792*log(age))+(0*log(age)*log(age))+(-14.4588*log(age)*(1-sex))+(1.8515*log(age)*log(age)*(1-sex))+(-0.9119*log(sbp))+(-0.2767*smoker)+(-0.7181*log(tctohdl))+(-0.1759*diabetes)+(-0.1999*diabetes*(1-sex))+(-0.5865*lvh)+(0*lvh*sex)))/(exp(0.9145)*exp(-0.2784*(15.5305+(28.4441*(1-sex))+(-1.4792*log(age))+(0*log(age)*log(age))+(-14.4588*log(age)*(1-sex))+(1.8515*log(age)*log(age)*(1-sex))+(-0.9119*log(sbp))+(-0.2767*smoker)+(-0.7181*log(tctohdl))+(-0.1759*diabetes)+(-0.1999*diabetes*(1-sex))+(-0.5865*lvh)+(0*lvh*sex)))))) +
  #       1-exp(-exp((log(period)-(26.5116+(0.2019*(1-sex))+(-2.3741*log(age))+(0*log(age)*log(age))+(0*log(age)*(1-sex))+(0*log(age)*log(age)*(1-sex))+(-2.4643*log(sbp))+(-0.3914*smoker)+(-0.0229*log(tctohdl))+(-0.3087*diabetes)+(-0.2627*diabetes*(1-sex))+(-0.2355*lvh)+(0*lvh*sex)))/(exp(-0.4312)*exp(0*(26.5116+(0.2019*(1-sex))+(-2.3741*log(age))+(0*log(age)*log(age))+(0*log(age)*(1-sex))+(0*log(age)*log(age)*(1-sex))+(-2.4643*log(sbp))+(-0.3914*smoker)+(-0.0229*log(tctohdl))+(-0.3087*diabetes)+(-0.2627*diabetes*(1-sex))+(-0.2355*lvh)+(0*lvh*sex))))))
  # 
  #     return(cvdrisk)
  #   }
  # )
  
  # Function to apply after ageing
  post.ageing.scenario.fn <- cmpfun(
    function(i, env = my.env) {
      cat("Post ageing scenario function\n"
      )
      if (i > intervention.year - 2011) {
        # select eligible population
        POP[between(age, 40, 74) &
              chd.incidence == 0 & 
              stroke.incidence == 0 &
              undiag.diab == 0 &
              af == 0 &
              ra == 0 &
              kiddiag == 0,
            eligible := 1L]
        
        POP[eligible == 1,
            qrisk2 := QRisk(age, sex, af, ra, kiddiag, bpmed,
                            rbinom(.N, 1, 0.005), diabtotr.cvdlag, bmival.cvdlag, 
                            origin, famcvd, tctohdl, omsysval.cvdlag,
                            smoke_cat, townsend)]
        # POP[eligible ==1,
        #     jbs2 := bnf.risk(age,
        #                      sex == "1",
        #                      omsysval.cvdlag,
        #                      tctohdl,
        #                      cigst1 == "4",
        #                      diabtotr == "2")]
        # 
        # select those who participate by risk
        POP[eligible == 1 & age<50, wt := 1]
        POP[eligible == 1 & between(age, 50, 59), wt := 1.6]
        POP[eligible == 1 & between(age, 60, 69), wt := 2.5]
        POP[eligible == 1 & age>69, wt := 2.9]
        
        if (POP[qrisk2 < 0.1 & eligible == 1, .N] > 
            (1 - p1 - p2) * POP[eligible == 1, .N] * uptake) {
          tt1 <- POP[qrisk2 < 0.1 & eligible == 1, 
                     sample(id, (1 - p1 - p2) * POP[eligible == 1, .N] * uptake, F, wt)]
        } else {
          tt1 <- POP[qrisk2 < 0.1 & eligible == 1, id]
        }
        if (POP[between(qrisk2, 0.1, 0.2) & eligible == 1, .N] > 
            p1 * POP[eligible == 1, .N] * uptake) {
          tt2 <- POP[between(qrisk2, 0.1, 0.2) & eligible == 1,
                     sample(id, p1 * POP[eligible == 1, .N] * uptake, F, wt)]
        } else {
          tt2 <- POP[between(qrisk2, 0.1, 0.2) & eligible == 1, id]
        }
        if (POP[qrisk2 > 0.2 & eligible == 1,.N] >
            p2 * POP[eligible == 1, .N] * uptake) {
          tt3 <- POP[qrisk2 > 0.2 & eligible == 1,
                     sample(id, p2 * POP[eligible == 1, .N] * uptake, F, wt)]
        } else {
          tt3 <- POP[qrisk2 > 0.2 & eligible == 1, id]
        }
        POP[id %in% c(tt1, tt2, tt3), invited := 1L]
        
        POP[invited == 1 & qrisk2 >= risk.cutoff, high.risk := 1L]
        
        # Difusse intervention in the population
        if (i == intervention.year - 2011 + 1) {
          diffusion <- POP[high.risk == 1, sample(id, 0.2*.N)] 
          POP[id %in% diffusion,
              `:=` (high.risk6 = 1L,
                    high.risk2 = 1L)]
        }
        
        if (i == intervention.year - 2011 + 2) {
          diffusion <- POP[high.risk == 1, sample(id, 0.4*.N)] 
          POP[id %in% diffusion,
              `:=` (high.risk5 = 1L,
                    high.risk2 = 1L)]
        }
        
        if (i == intervention.year - 2011 + 3) {
          diffusion <- POP[high.risk == 1, sample(id, 0.6*.N)] 
          POP[id %in% diffusion,
              `:=` (high.risk4 = 1L,
                    high.risk2 = 1L)]
        }
        
        if (i == intervention.year - 2011 + 4) {
          diffusion <- POP[high.risk == 1, sample(id, 0.8*.N)] 
          POP[id %in% diffusion,
              `:=` (high.risk3 = 1L,
                    high.risk2 = 1L)]
        }
        
        if (i > intervention.year - 2011 + 4) {
          POP[, `:=` (high.risk2 = high.risk)]
        }
        
        cat("apply smoking treatment")
        POP[cigst1 == "4" & high.risk2 == 1 & rbinom(.N, 1, 0.2) * rbinom(.N, 1, 0.1) == 1, 
            `:=`(cigst1 = "3", endsmoke.curr = 0, numsmok.curr = cigdyalCat.curr)] # first binom is the healthcheck 
        #every 5 years the 2nd is the success rate of smoking cessation
        
        
        output <- vector("list", 2)
        
        if (exists("highrisk.rds")) output[[1]] <- highrisk.rds
        
        output[[2]] <- merge(POP[eligible == 1 , 
                                 pop.summ(.N),
                                 by = c("sex", "agegroup", "qimd")], 
                             POP[eligible == 1 & qrisk2 >= risk.cutoff,
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
        POP[high.risk2 == 1,
            `:=` (bmival.cvdlag = bmival.cvdlag * rpert(.N, 0.97, 0.99, 1, 4))] # forster 2014 table 4
        
        POP[high.risk2 == 1 & porftvg.cvdlag < 5,
            `:=` (porftvg.cvdlag = porftvg.cvdlag + rbinom(.N, 1, 0.5))]
        
        POP[high.risk2 == 1 & a30to06m.cvdlag < 5,
            `:=` (a30to06m.cvdlag = a30to06m.cvdlag + rbinom(.N, 1, 0.5))]
        
        POP[high.risk2 == 1 & between(qrisk2, 0.1, 0.2) & omsysval.cvdlag >= 135,
            `:=` (omsysval.cvdlag = omsysval.cvdlag -
                    (omsysval.cvdlag - 115) *
                    rbinom(.N, 1, rpert(.N, 0.05, 0.13, 0.20, 4)) * # uptake *
                    rbinom(.N, 1, persistence) * # persistence
                    rpert(.N, 0.3, 0.7, 1, 4))] # adherence
        
        POP[high.risk2 == 1 & qrisk2 > 0.2 & omsysval.cvdlag >= 135,
            `:=` (omsysval.cvdlag = omsysval.cvdlag -
                    (omsysval.cvdlag - 115) *
                    rbinom(.N, 1, rpert(.N, 0.15, 0.23, 0.30, 4)) * # uptake *
                    rbinom(.N, 1, persistence) * # persistence
                    rpert(.N, 0.3, 0.7, 1, 4))] # adherence
        
        POP[high.risk2 == 1 & between(qrisk2, 0.1, 0.2) & cholval.cvdlag >= 5,
            `:=` (cholval.cvdlag = cholval.cvdlag -
                    cholval.cvdlag * atorv.eff *
                    rbinom(.N, 1, rpert(.N, 0.01, 0.07, 0.10, 4)) * # uptake
                    rbinom(.N, 1, persistence) * # persistence
                    rpert(.N, 0.3, 0.7, 1, 4))] # adherence
        
        POP[high.risk2 == 1 & qrisk2 > 0.2 & cholval.cvdlag >= 5,
            `:=` (cholval.cvdlag = cholval.cvdlag -
                    cholval.cvdlag * atorv.eff *
                    rbinom(.N, 1, rpert(.N, 0.2, 0.24, 0.3, 4)) * # uptake
                    rbinom(.N, 1, persistence) * # persistence
                    rpert(.N, 0.3, 0.7, 1, 4))] # adherence
        
        # surgery for morbid obesity
        POP[high.risk2 == 1 & bmival.cvdlag > 50 & rbinom(.N, 1, 0.2) == 1, 
            bmival.cvdlag := 30]
        
      }
      
      if (i > intervention.year - 2011) {       
        POP[, `:=` (high.risk  = NULL,
                    high.risk2 = NULL,
                    eligible   = NULL, 
                    invited    = NULL,
                    wt         = NULL)]
      }
      assign("POP", POP, envir = env)
    }
  )
}




