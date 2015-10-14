#cmpfile("./ageing engine.R")
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

# ETS ---------------------------------------------------------------------
# ETS (start 3 years after init.year to avoid artefactects from cigst1 estimatin)
if (i == init.year - 2011 + 1) {
  smoking.preva.forets0 <-
    POP[between(age, ageL, ageH),
        list(old.preval = .SD[cigst1.cvdlag == "4", .N]/ .N),
        by = .(qimd)] # to be used for ETS
  setkey(smoking.preva.forets0, qimd)
}

if (i > (init.year - 2011 + 1)) {
  cat("Estimating ETS...\n")
  smoking.preva.forets1 <-
    POP[between(age, ageL, ageH),
        list(new.preval = .SD[cigst1.cvdlag == "4", .N]/ .N),
        by = .(qimd)]
  setkey(smoking.preva.forets1, qimd)
  # works for decreasing smoking prevelence
  smoking.preva.forets1[smoking.preva.forets0,
                        change := 1 - new.preval / old.preval][
                          change < 0, change := 0][
                            is.na(change), change := 0]
  
  POP[id %in% POP[expsmokCat == "1", 
                  sample_frac(.SD,
                              smoking.preva.forets1[qimd == .BY[1],
                                                    change])[, id],
                  by = qimd,
                  .SDcols = "id"][, V1],
      expsmokCat := "0"]
  
  # works for increasing smoking prevelence (the change apply to ets==1)
  smoking.preva.forets1[smoking.preva.forets0,
                        change := 1 - old.preval / new.preval][
                          change < 0, change := 0][
                            is.na(change), change := 0]
  
  POP[id %in% POP[expsmokCat == "0",
                  sample_n(.SD,
                           smoking.preva.forets1[qimd == .BY[1], change] * 
                             POP[expsmokCat == "1" & qimd == .BY[1], .N])[, id], 
                  by = qimd,
                  .SDcols = "id"][,V1], 
      expsmokCat := "1"]
  
  smoking.preva.forets0 = copy(smoking.preva.forets1[, change := NULL])
  setnames(smoking.preva.forets0, "new.preval", "old.preval")
}

# Scenario fn -------------------------------------------------------------
cat(paste0("before scenario.fn",Sys.time(), "\n\n"))
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
