#cmpfile("./Scenarios/health checks10poor.R")
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
# Assumes that 40% of those with SBP above 140mmHg, TC above 5 mmol/l and BMI above 35 kgr/m2 have a (based on dh)
# reduction of 30% on their estimated values
cat("targeted health check scenario\n\n")
intervention.year <- 2011
uptake <- 0.5
risk.cutoff <- 0.10

if (i == (init.year - 2011)
    ) {
  
  load(file = "./Lagtimes/tctohdl.svylm.rda")
  load(file = "./Lagtimes/famcvd.svylr.rda")
  load(file = "./Lagtimes/af.svylr.rda")
  load(file = "./Lagtimes/kiddiag.svylr.rda")
  
  # TC to HDL prediction ---------------------------------------------------------
  # Define function for hdl estimation
  pred.tctohdl <-
    cmpfun(function(cholval1,
                    age,
                    sex,
                    qimd,
                    bmival,
                    a30to06m,
                    cigst1,
                    lag = cvd.lag) {
      if (is.factor(sex) == F) {
        sex <-  factor(sex,
                       levels = c(1, 2),
                       ordered = F)
      }
      if (is.ordered(qimd) == F) {
        qimd <- factor(qimd,
                       levels = c(1, 2, 3, 4, 5),
                       ordered = T)
      }
      bmival[bmival > 40] <- 40 # otherwise predicts NAN values
      cigst2 <- mapvalues(cigst1,  c(4:1), c(1, 0, 0, 0))
      pr <- data.frame(predict(
        tctohdl.svylm,
        data.table(
          cholval1     = cholval1,
          age          = age - lag,
          sex          = sex,
          qimd         = qimd,
          bmival       = bmival,
          a30to06m.imp = a30to06m,
          cigst1       = cigst2
        ),
        type = "response",
        se.fit = T
      ))
      #return(pr[[1]])
      #return(rtruncnorm(nrow(pr), a = 2.5, b = 12,  pr[[1]], pr[[2]]))
      return(rnorm(nrow(pr), pr[[1]], pr[[2]]))
    })
  
  # FamCVD prediction ---------------------------------------------
  pred.famcvd <- cmpfun(function(n, age, qimd) {
    newdata <-
      data.table(age          = age,
                 qimd         = qimd)
    
    type <- "response"
    total <- NULL
    tt <- delete.response(terms(formula(famcvd.svylr)))
    mf <- model.frame(tt, data = newdata)
    mm <- model.matrix(tt, mf)
    if (!is.null(total) && attr(tt, "intercept")) {
      mm[, attr(tt, "intercept")] <- mm[, attr(tt, "intercept")] *
        total
    }
    eta <- drop(mm %*% coef(famcvd.svylr))
    eta <-
      switch(type,
             link = eta,
             response = famcvd.svylr$family$linkinv(eta))
    rbinom(n, 1, eta)
    #return(rtruncnorm(nrow(pr), a = 0, b = 1, mean=pr[[1]], sd=pr[[2]]))
  })
  
  # AF prevalence prediction ---------------------------------------------
  pred.af <- cmpfun(function(n, age, qimd, cigst1) {
    newdata <-
      data.table(age          = age,
                 qimd         = qimd,
                 cigst1       = cigst1)
    
    type <- "response"
    total <- NULL
    tt <- delete.response(terms(formula(af.svylr)))
    mf <- model.frame(tt, data = newdata)
    mm <- model.matrix(tt, mf)
    if (!is.null(total) && attr(tt, "intercept")) {
      mm[, attr(tt, "intercept")] <- mm[, attr(tt, "intercept")] *
        total
    }
    eta <- drop(mm %*% coef(af.svylr))
    eta <-
      switch(type,
             link = eta,
             response = af.svylr$family$linkinv(eta))
    rbinom(n, 1, eta)
    #return(rtruncnorm(nrow(pr), a = 0, b = 1, mean=pr[[1]], sd=pr[[2]]))
  })
  
  # Kidney disease prevalence prediction ---------------------------------------------
  pred.kiddiag <- cmpfun(function(n, age, sex, qimd) {
    newdata <-
      data.table(age          = age,
                 sex          = sex,
                 qimd         = qimd)
    
    type <- "response"
    total <- NULL
    tt <- delete.response(terms(formula(kiddiag.svylr)))
    mf <- model.frame(tt, data = newdata)
    mm <- model.matrix(tt, mf)
    if (!is.null(total) && attr(tt, "intercept")) {
      mm[, attr(tt, "intercept")] <- mm[, attr(tt, "intercept")] *
        total
    }
    eta <- drop(mm %*% coef(kiddiag.svylr))
    eta <-
      switch(type,
             link = eta,
             response = kiddiag.svylr$family$linkinv(eta))
    rbinom(n, 1, eta)
    #return(rtruncnorm(nrow(pr), a = 0, b = 1, mean=pr[[1]], sd=pr[[2]]))
  })
  
  # Function to apply after ageing
  post.ageing.scenario.fn <- cmpfun(
    function(i, env = my.env
    ) {
      cat("Post ageing scenario function\n"
      )
      if (i > intervention.year - 2011
          ) {
        # TC to HDL estimation ----------------------------------------------------------
        POP[between(age, ageL, ageH),
            tctohdl := pred.tctohdl(
              cholval.cvdlag,
              age,
              sex,
              qimd,
              bmival.cvdlag,
              a30to06m.cvdlag,
              cigst1.cvdlag,
              cvd.lag
            )]
        
        # FamCVD ------------------------------------------------------------------
        POP[between(age, ageL, ageH), famcvd := pred.famcvd(.N, age, qimd)]
        
        # AF prevalence ------------------------------------------------------------------
        POP[between(age, ageL, ageH), af := pred.af(.N, age, qimd, cigst1.cvdlag)]
        
        # Kidney prevalence ------------------------------------------------------------------
        POP[between(age, ageL, ageH), kiddiag := pred.kiddiag(.N, age, sex, qimd)]
        
        # Rheum arthr prevalence --------------------------------------------------
        POP[RAincid.rr.l, on = c("age", "sex"), ra := rbinom(.N, 1, rr)]
        
        POP[qimd %in% c("4", "5")   &
              between(age, 40, 74)  &
              chd.incidence == 0    & 
              stroke.incidence == 0 &
              undiag.diab == 0      &
              af == 0               &
              ra == 0               &
              kiddiag == 0,
            eligible := 1L]
        
        POP[eligible == 1,
            qrisk2 := QRisk(age, sex, af, ra, kiddiag, bpmed,
                            rbinom(.N, 1, 0.005), diabtotr.cvdlag, bmival.cvdlag, 
                            origin, famcvd, tctohdl, omsysval.cvdlag,
                            smoke_cat, townsend)]
        
        POP[eligible == 1 & rbinom(.N, 1, uptake) == 1,
            invited := 1L]
        
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
            `:=`(cigst1 = "3", endsmoke.curr = 0, numsmok.curr = cigdyal.curr)] # first binom is the healthcheck 
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
                             all.x = T
                             )
        assign("highrisk.rds", rbindlist(output, fill = T), env
               )
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
                    rbinom(.N, 1, rpert(.N, 0.07, 0.17, 0.24, 4)) * # uptake
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
                    invited    = NULL)]
      }
      assign("POP", POP, envir = env)
    }
  )
}




