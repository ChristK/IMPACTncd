#cmpfile("./ageing engine.R")
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


#************************* Lagtimes/Ageing implementation *******************#

# salt estimation ---------------------------------------------------------
cat("Initiating lag/ageing engine...\n")
cat(paste0(Sys.time(), "\n\n"))

cat("salt estimation\n")
if (i == init.year - 2011) {
  POP[between(age, 19, ageH),
      percentile := (frank(salt.intersalt,
                           na.last = F,
                           ties.method = "random") - 1)/(.N - 1), 
      by = .(age, sex, qimd)]
  
  POP[, `:=`(salt.intersalt = NULL)]
} else {
  POP[between(age, 19, ageH),
      percentile := (frank(salt24h.calag,
                           na.last = F, 
                           ties.method = "random") - 1)/(.N - 1), 
      by = .(age, sex, qimd)]
}

setkey(POP, age, sex, qimd, percentile)

if (scenarios.list[[iterations]] == "salt no intervention.Rc") {
  if ((i + 2011 - cancer.lag) < 2003 && (i + 2011 - cvd.lag) < 2003) {
    tmp.cvd  <- pred.salt(i, cvd.lag) 
    tmp.ca  <- pred.salt(i, cancer.lag)
    setkey(tmp.cvd,  age, sex, qimd, percentile)
    setkey(tmp.ca,  age, sex, qimd, percentile)
    
    POP <- tmp.cvd[POP, roll = "nearest"]
    POP[between(age, 19, ageH), salt24h.cvdlag := runif(.N, salt.l, salt.u)]
    POP[, `:=`(salt.l = NULL, salt.u = NULL)]
    
    POP[between(age, 19, ageH), salt24h.cvdlag.alt := salt24h.cvdlag] # so the difference is 0 and doesn't affect sbp
    
    POP <- tmp.ca[POP, roll = "nearest"]
    POP[between(age, 19, ageH), salt24h.calag := runif(.N, salt.l, salt.u)]
    POP[, `:=`(percentile = NULL, salt.l = NULL, salt.u = NULL)]
    rm(tmp.ca, tmp.cvd)
  } else if ((i + 2011 - cancer.lag) < 2003 && (i + 2011 - cvd.lag) >= 2003) {
    tmp.cvd0 <- pred.salt(cvd.lag - 7.5, cvd.lag) # salt exposure remains as of 2003
    tmp.cvd  <- pred.salt(i, cvd.lag) # alternative current policy
    tmp.ca0  <- pred.salt(i, cancer.lag) 
    setkey(tmp.cvd0, age, sex, qimd, percentile)
    setkey(tmp.cvd,  age, sex, qimd, percentile)
    setkey(tmp.ca0,  age, sex, qimd, percentile)
    
    POP <- tmp.cvd0[POP, roll = "nearest"]
    POP[between(age, 19, ageH), salt24h.cvdlag := runif(.N, salt.l, salt.u)]
    POP[, `:=`(salt.l = NULL, salt.u = NULL)]
    
    POP <- tmp.cvd[POP, roll = "nearest"]
    POP[between(age, 19, ageH), salt24h.cvdlag.alt := runif(.N, salt.l, salt.u)]
    POP[, `:=`(salt.l = NULL, salt.u = NULL)]
    
    POP <- tmp.ca0[POP, roll = "nearest"]
    POP[between(age, 19, ageH), salt24h.calag := runif(.N, salt.l, salt.u)]
    POP[, `:=`(percentile = NULL, salt.l = NULL, salt.u = NULL)]
    rm(tmp.ca0, tmp.cvd0, tmp.cvd)
  } else {
    tmp.cvd0 <- pred.salt(-7.5 + cvd.lag, cvd.lag) # salt exposure remains as of 2003
    tmp.cvd <- pred.salt(i, cvd.lag) # alternative current policy
    tmp.ca0  <- pred.salt(-7.5 + cancer.lag, cancer.lag) # as of 2003
    setkey(tmp.cvd0, age, sex, qimd, percentile)
    setkey(tmp.cvd, age, sex, qimd, percentile)
    setkey(tmp.ca0,  age, sex, qimd, percentile)
    
    POP <- tmp.cvd0[POP, roll = "nearest"]
    POP[between(age, 19, ageH), salt24h.cvdlag := runif(.N, salt.l, salt.u)]
    POP[, `:=`(salt.l = NULL, salt.u = NULL)]
    
    POP <- tmp.cvd[POP, roll = "nearest"]
    POP[between(age, 19, ageH), salt24h.cvdlag.alt := runif(.N, salt.l, salt.u)]
    POP[, `:=`(salt.l = NULL, salt.u = NULL)]
    
    POP <- tmp.ca0[POP, roll = "nearest"]
    POP[between(age, 19, ageH), salt24h.calag := runif(.N, salt.l, salt.u)]
    POP[, `:=`(percentile = NULL, salt.l = NULL, salt.u = NULL)]
    rm(tmp.ca0, tmp.cvd0, tmp.cvd)
  }
} else if (scenarios.list[[iterations]] == "salt slow intervention.Rc") {
  time.correction.cvd <- -2003 + 2011 - cvd.lag
  time.correction.ca  <- -2003 + 2011 - cancer.lag
  
  if ((i + 2011 - cancer.lag) < 2003 && (i + 2011 - cvd.lag) < 2003) {
    tmp.cvd  <- pred.salt(i, cvd.lag) 
    tmp.ca  <- pred.salt(i, cancer.lag)
    setkey(tmp.cvd,  age, sex, qimd, percentile)
    setkey(tmp.ca,  age, sex, qimd, percentile)
    
    POP <- tmp.cvd[POP, roll = "nearest"]
    POP[between(age, 19, ageH), salt24h.cvdlag := runif(.N, salt.l, salt.u)]
    POP[, `:=`(salt.l = NULL, salt.u = NULL)]
    
    POP[between(age, 19, ageH), salt24h.cvdlag.alt := salt24h.cvdlag] # so the difference is 0 and doesn't affect sbp
    
    POP <- tmp.ca[POP, roll = "nearest"]
    POP[between(age, 19, ageH), salt24h.calag := runif(.N, salt.l, salt.u)]
    POP[, `:=`(percentile = NULL, salt.l = NULL, salt.u = NULL)]
    rm(tmp.ca, tmp.cvd)
  } else if ((i + 2011 - cancer.lag) < 2003 && (i + 2011 - cvd.lag) >= 2003) {
    tmp.cvd0 <- pred.salt(cvd.lag - 7.5 + (time.correction.cvd + i)/5, cvd.lag) # salt exposure remains as of 2003
    tmp.cvd  <- pred.salt(i, cvd.lag) # alternative current policy
    tmp.ca0  <- pred.salt(i, cancer.lag) 
    setkey(tmp.cvd0, age, sex, qimd, percentile)
    setkey(tmp.cvd,  age, sex, qimd, percentile)
    setkey(tmp.ca0,  age, sex, qimd, percentile)
    
    POP <- tmp.cvd0[POP, roll = "nearest"]
    POP[between(age, 19, ageH), salt24h.cvdlag := runif(.N, salt.l, salt.u)]
    POP[, `:=`(salt.l = NULL, salt.u = NULL)]
    
    POP <- tmp.cvd[POP, roll = "nearest"]
    POP[between(age, 19, ageH), salt24h.cvdlag.alt := runif(.N, salt.l, salt.u)]
    POP[, `:=`(salt.l = NULL, salt.u = NULL)]
    
    POP <- tmp.ca0[POP, roll = "nearest"]
    POP[between(age, 19, ageH), salt24h.calag := runif(.N, salt.l, salt.u)]
    POP[, `:=`(percentile = NULL, salt.l = NULL, salt.u = NULL)]
    rm(tmp.ca0, tmp.cvd0, tmp.cvd)
  } else {
    tmp.cvd0 <- pred.salt(cvd.lag - 7.5 + (time.correction.cvd + i)/5, cvd.lag) # salt exposure remains as of 2003
    tmp.cvd  <- pred.salt(i, cvd.lag) # alternative current policy
    tmp.ca0  <- pred.salt(cancer.lag - 7.5 + (time.correction.ca + i)/5, cancer.lag) # as of 2003
    setkey(tmp.cvd0, age, sex, qimd, percentile)
    setkey(tmp.cvd, age, sex, qimd, percentile)
    setkey(tmp.ca0,  age, sex, qimd, percentile)
    
    POP <- tmp.cvd0[POP, roll = "nearest"]
    POP[between(age, 19, ageH), salt24h.cvdlag := runif(.N, salt.l, salt.u)]
    POP[, `:=`(salt.l = NULL, salt.u = NULL)]
    
    POP <- tmp.cvd[POP, roll = "nearest"]
    POP[between(age, 19, ageH), salt24h.cvdlag.alt := runif(.N, salt.l, salt.u)]
    POP[, `:=`(salt.l = NULL, salt.u = NULL)]
    
    POP <- tmp.ca0[POP, roll = "nearest"]
    POP[between(age, 19, ageH), salt24h.calag := runif(.N, salt.l, salt.u)]
    POP[, `:=`(percentile = NULL, salt.l = NULL, salt.u = NULL)]
    rm(tmp.ca0, tmp.cvd0, tmp.cvd)
  }
} else {
  tmp.cvd <- pred.salt(i, cvd.lag) # alternative current policy
  tmp.ca  <- pred.salt(i, cancer.lag)
  setkey(tmp.cvd, age, sex, qimd, percentile)
  setkey(tmp.ca,  age, sex, qimd, percentile)
  
  POP <- tmp.cvd[POP, roll = "nearest"]
  POP[between(age, 19, ageH), salt24h.cvdlag := runif(.N, salt.l, salt.u)]
  POP[, `:=`(salt.l = NULL, salt.u = NULL)]
  
  POP <- tmp.ca[POP, roll = "nearest"]
  POP[between(age, 19, ageH), salt24h.calag := runif(.N, salt.l, salt.u)]
  POP[, `:=`(percentile = NULL, salt.l = NULL, salt.u = NULL)]
  rm(tmp.ca, tmp.cvd)
}
#qplot(percentile, salt.u, col = age, data = tmp.ca, facets = sex~qimd)

# PA estimation -----------------------------------------------------------
cat("PA estimation\n")
setkey(POP, age, sex, qimd)

POP[between(age, 20, 84), a30to06m.cvdlag := pred.pa(i, age, sex, qimd, cvd.lag)]

POP[between(age, ageL, ageH), a30to06m.calag := pred.pa(i, age, sex, qimd, cancer.lag)]

# F&V estimation ----------------------------------------------------------
cat("F&V estimation\n")
POP[between(age, ageL, ageH), porftvg.cvdlag := pred.fv(i, age, sex, qimd,  cvd.lag)]
POP[between(age, ageL, ageH), frtpor.cvdlag := pred.fvrate(age, sex, qimd, porftvg.cvdlag, cvd.lag)]

POP[between(age, ageL, ageH), porftvg.calag := pred.fv(i, age, sex, qimd, cancer.lag)]
POP[between(age, ageL, ageH), frtpor.calag := pred.fvrate(age, sex, qimd, porftvg.calag, cancer.lag)]

# Smoking -----------------------------------------------------------------
# cigst1 is updated anually. The cigst1.cvdlag is derived from cigst1 annually
cat("Smoking initiation/cessation simulation\n")
if (i == init.year - 2011) {
  POP[, `:=`(endsmoke.curr   = endsmoke, # .curr means current
             cigdyalCat.curr = cigdyalCat,
             numsmok.curr    = numsmok,
             packyears.curr  = packyears)]
}

cat("Smoking lag\n")
if (i > init.year - 2011) {
  POP[, cigst1.cvdlag := NULL]
}
POP[cigst1 == "1", cigst1.cvdlag := "1"]

POP[cigst1 %in% c("2", "3"), 
    cigst1.cvdlag := cigst1] 
POP[cigst1 %in% c("2", "3") & endsmoke.curr < cvd.lag, 
    cigst1.cvdlag := "4"] # slightly wrong.could also be a never
# smoker. see below for correction 

POP[cigst1.cvdlag %in% c("2", "3"),
    endsmoke := endsmoke.curr - as.integer(cvd.lag)]

POP[cigst1.cvdlag == "4",
    `:=`(cigdyalCat = numsmok.curr, endsmoke = 0L)]

POP[cigst1.cvdlag == "4" & (packyears.curr * 20 /numsmok.curr) <= cvd.lag,
    cigst1.cvdlag := "1"] # correction of above error

POP[cigst1 == "4", 
    cigst1.cvdlag := "4"]

POP[cigst1 == "4" & (packyears.curr * 20 /cigdyalCat.curr) <= cvd.lag, 
    cigst1.cvdlag := "1"]

POP[cigst1.cvdlag == "4",
    packyears := packyears.curr - cvd.lag * cigdyalCat.curr/20] # packyears are for cvdlag

POP[, cigst1.cvdlag := factor(cigst1.cvdlag)]

POP[age < 15, cigst1.cvdlag := "1"]

POP[cigst1.cvdlag == "1", `:=`(cigdyalCat = 0, packyears = 0)]


if (i > init.year - 2011) {
  POP[cigst1 %in% c("2", "3"), endsmoke.curr := endsmoke.curr + 1L]
  POP[cigst1 == "4", packyears.curr := packyears.curr + cigdyalCat.curr/20]
  
  POP[age <= 15, cigst1 := "1"]
  POP[age == 15, cigst1.temp := pred.sm0prev(i, 15, sex, qimd)]
  POP[cigst1.temp == 1,
      `:=`(cigdyalCat.curr = resample(POP[qimd == .BY[[1L]] & cigst1 == "4",
                                          cigdyalCat.curr], .N, replace = T)),
      by = qimd] 
  POP[cigst1.temp == 1, cigst1 := "4"]
  POP[,cigst1.temp := NULL]
  
  POP[between(age, 16, 50) & cigst1 == "1", 
      cigst1.temp1 := pred.nev0sm1(i, age, sex, qimd)] 
  POP[cigst1.temp1 == 1, `:=` 
      (cigdyalCat.curr = resample(POP[qimd == .BY[[1L]] & cigst1 == "4",
                                      cigdyalCat.curr], .N, replace = T)), 
      by = qimd]  
  
  POP[between(age, 16, ageH) & cigst1 == "4", 
      cigst1.temp2 := pred.sm0ex1(i , age , sex, qimd)]
  
  POP[between(age, 16, 75) & between(endsmoke.curr, 1, 10), 
      cigst1.temp3 :=  dice(.N) < pred.ex0sm1(endsmoke.curr, sex, qimd)]
  POP[cigst1.temp3 == T, cigdyalCat.curr := numsmok.curr] 
  
  POP[cigst1.temp1 == 1, `:=`(cigst1 = "4", packyears.curr = cigdyalCat.curr/20)]
  POP[, cigst1.temp1 := NULL]
  POP[cigst1.temp2 == 1, `:=`(cigst1 = "3",
                              endsmoke.curr = 0, 
                              numsmok.curr = cigdyalCat.curr)]
  POP[, cigst1.temp2 := NULL]
  POP[cigst1.temp3 == T, `:=`(cigst1 = "4",
                              packyears.curr = packyears.curr + cigdyalCat.curr/20)]
  POP[, cigst1.temp3 := NULL]
}

# needs fixing for cancers lag
POP[cigst1.cvdlag == "1", cigst1.calag := "1"]
POP[cigst1.cvdlag %in% c("2", "3"),
    cigst1.calag := cigst1.cvdlag]
POP[cigst1.cvdlag %in% c("2", "3") & endsmoke < cancer.lag,
    cigst1.calag := "4"]
POP[cigst1.calag == "4" & (packyears * 20 /numsmok) < cancer.lag,
    cigst1.calag := "1"]
POP[cigst1.cvdlag == "4",
    cigst1.calag := "4"]
POP[cigst1.cvdlag == "4" & (packyears * 20 /cigdyalCat) < cancer.lag,
    cigst1.calag := "1"]
POP[, cigst1.calag := factor(cigst1.calag)]

# needed for QRisk and QDrisk
POP[, smoke_cat := 0L]
POP[cigst1 == "3", smoke_cat := 1L]
POP[cigst1 == "4", smoke_cat := 3L]
POP[cigst1 == "4" & cigdyalCat < 10, smoke_cat := 2L]
POP[cigst1 == "4" & cigdyalCat > 19, smoke_cat := 4L]

# BMI estimation ----------------------------------------------------------
cat("BMI estimation\n")
if (i > (init.year - 2011)) ageing.distr(bmi.rank, bmival) # to match distribution shape to that of SPOP2011
POP[between(age, 20, 84), 
    bmival.cvdlag := scale(bmival, scale = F), 
    by = group]
POP[between(age, 20, 84), 
    bmival.cvdlag := bmival.cvdlag + 
      pred.bmi(i, age, sex, qimd, a30to06m.cvdlag, cvd.lag)]

POP[between(age, ageL, ageH),
    bmival.calag := scale(bmival, scale = F),
    by = group]
POP[between(age, ageL, ageH),
    bmival.calag := bmival.calag + 
      pred.bmi(i, age, sex, qimd, a30to06m.calag, cancer.lag)]
POP[bmival.cvdlag < 16, bmival.cvdlag := 16]
POP[bmival.calag  < 16, bmival.calag  := 16]

# SBP estimation ----------------------------------------------------------
cat("SBP estimation\n")
if (i > (init.year - 2011)) ageing.distr(sbp.rank, omsysval)
POP[between(age, ageL, ageH), 
    omsysval.cvdlag := scale(omsysval, scale = F),
    by = group] 
POP[between(age, ageL, ageH), 
    omsysval.cvdlag := omsysval.cvdlag + 
      pred.sbp(i, age, sex, qimd, bmival.cvdlag, cigst1, a30to06m.cvdlag, cvd.lag)]
POP[omsysval.cvdlag < 70, omsysval.cvdlag := 70]


# TC estimation -----------------------------------------------------------
cat("CHOL estimation\n")
if (i > (init.year - 2011)) ageing.distr(chol.rank, cholval)
POP[between(age, ageL, ageH), 
    cholval.cvdlag := scale(cholval, scale = F),
    by = group] 
POP[between(age, ageL, ageH), 
    cholval.cvdlag := 
      cholval.cvdlag + 
      pred.chol(i, age, sex, qimd, bmival.cvdlag, porftvg.cvdlag, a30to06m.cvdlag, cvd.lag)] 
POP[cholval.cvdlag < 2.2, cholval.cvdlag := 2.2]


# TC to HDL estimation ----------------------------------------------------------
POP[between(age, ageL, ageH),
    tctohdl := pred.tctohdl(cholval.cvdlag, age, sex, qimd, bmival.cvdlag, a30to06m.cvdlag, cigst1.cvdlag, cvd.lag)]

# FamCVD ------------------------------------------------------------------
POP[between(age, ageL, ageH), famcvd := pred.famcvd(.N, age, qimd)]

# AF prevalence ------------------------------------------------------------------
POP[between(age, ageL, ageH), af := pred.af(.N, age, qimd, cigst1.cvdlag)]

# Kidney prevalence ------------------------------------------------------------------
POP[between(age, ageL, ageH), kiddiag := pred.kiddiag(.N, age, sex, qimd)]

# BP meds prevalence ------------------------------------------------------------------
POP[between(age, 25, ageH), bpmed := pred.bpmed(.N, age, sex, qimd, omsysval.cvdlag)] # 25 for QDrisk

# Rheum arthr prevalence --------------------------------------------------
POP[RAincid.rr.l, on = c("age", "sex"), ra := rbinom(.N, 1, rr)]

# ETS ---------------------------------------------------------------------
cat("Estimating ETS...\n") # assumes linear relation with 
# smoking prevalence by qimd, stratified by age and sex
if (i == init.year - 2011) {
  pred.ets <-
    POP[, sm.pr := sum(cigst1 =="4") / .N, by = .(qimd)
        ][, ets.pr := sum(expsmokCat == "1")/.N, by = .(agegroup, sex, qimd)
          ][, .(ets.coef = mean(ets.pr/sm.pr)), by = .(agegroup, sex, qimd)]
  POP[, ets.pr := NULL]
} else {
  setkey(POP, agegroup, sex, qimd)
  POP[, sm.pr := sum(cigst1 =="4") / .N, by = qimd]
  POP[pred.ets, expsmokCat := as.character(rbinom(.N, 1, sm.pr * ets.coef))]
}
  

# Scenario fn -------------------------------------------------------------
cat(paste0("before scenario.fn ",Sys.time(), "\n\n"))
post.ageing.scenario.fn(i)# placed here so bmi intervensions affect diabetes

# Diabetes estimation ---------------------------------------------------------


cat("Diabetes estimation\n")
# to predict diabetics that where healthy x years ago you need to apply
# current.prevalence-x*(diab.incid - mortality) Diabetes incidence from Holden
# SE, Barnett AH, Peters JR, et al. The incidence of type 2 diabetes in the
# United Kingdom from 1991 to 2010. Diabetes Obes Metab 2013;15:844–52.
# doi:10.1111/dom.12123 NOTE: this is for type 2 diabetes only. For this ages I
# am concerned with this is absolutely fine. very few new diabetes I patients
# older than 35

if (qdrisk == F) {
  # between(age, 20, 84) IS ALSO INCLUDED IN THE FUNCTION!!! DON'T CHANGE IT
  if (i > (init.year - 2011)) {
    POP[between(age, 20, 84) & diabtotr == "1", 
        diabtotr := pred.diab.incid(i, age, sex, qimd, 
                                    bmival.cvdlag, a30to06m.cvdlag)]
  } 
  
  POP[, diabtotr.cvdlag := diabtotr]
  POP[between(age, ageL, ageH) & diabtotr == "2", 
      diabtotr.cvdlag := pred.diab.incid.lag(age,
                                             sex,
                                             qimd,
                                             bmival.cvdlag,
                                             a30to06m.cvdlag,
                                             cvd.lag)]
} 

if (qdrisk == T) {
  if (i == (init.year - 2011)) {
    POP[, diabtotr.cvdlag := diabtotr]
    POP[between(age, 25, ageH) & diabtotr == "2", 
        diabtotr.cvdlag := pred.diab.incid.lag(age,
                                               sex,
                                               qimd,
                                               bmival.cvdlag,
                                               a30to06m.cvdlag,
                                               cvd.lag - i - 2011 + init.year)]
    
  } else {
    POP[between(age, 25, ageH) & diabtotr.cvdlag == "1",
        diabtotr.cvdlag := pred.diab.qdrisk(.SD)] 
    # This ignore type 1 diabetes. For ages <25 the prevalence is ~0.2% and for ages <35 ~0.37% (HSE2006)
    # However my synthpop already contains diabetics for ages <25. For short horisons this is absolutely fine
  } 
}


# Predict undiagnosed in the population -----------------------------------
POP[, undiag.diab := 0L]
POP[diabtotr.cvdlag == "2", undiag.diab := pred.undiag.diab(.N, qimd)] # 25 for QDrisk

cat("DIAB finished\n")

agegroup.fn(POP)

# Export ------------------------------------------------------------------
cat(paste0("before export ",Sys.time(), "\n\n"))

if (i == init.year - 2011) riskfactors <- vector("list", yearstoproject * 5)
#if (exists("riskfactors.rds")) output[[1]] <- riskfactors.rds

riskfactors[[(2011 - init.year + i) * 5 + 1]] <- 
  output.rf(POP, c("qimd", "sex", "agegroup"), 20, 84)

riskfactors[[(2011 - init.year + i) * 5 + 2]] <-
  output.rf(POP, c("sex", "agegroup"), 20, 84)

riskfactors[[(2011 - init.year + i) * 5 + 3]] <- 
  output.rf(POP, c("qimd", "sex"), ageL, ageH) 

riskfactors[[(2011 - init.year + i) * 5 + 4]] <- 
  output.rf(POP, c("sex"), ageL, ageH)

riskfactors[[(2011 - init.year + i) * 5 + 5]] <- 
  output.rf(POP, c(), ageL, ageH)

# when not stratified by agegroup only ageL to ageH is considered

if (i == yearstoproject + init.year - 2012) {
  lapply(riskfactors,
         function (x) setnames(x,
                               paste0("V", 1:50),
                               output.rf.names)
  )
  
  saveRDS(rbindlist(riskfactors, T, T) ,
          file = paste0(output.dir(), "riskfactors.rds"))
}
