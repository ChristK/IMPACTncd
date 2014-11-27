cat("Initiating lag/ageing engine...\n\n")
setkey(POP, age, sex, qimd)

#****************************************** Lagtimes/Ageing implementation **********************************#
cat("BMI estimation\n")
if (i > (init.year-2011)) POP <- ageing.distr("bmival") # to match distribution shape to that of SPOP2011
POP[between(age, ageL, ageH), bmival.cvdlag := scale(bmival, scale=F) + pred.bmi(i, age, sex, qimd, cvd.lag), by= group]
POP[between(age, ageL, ageH), bmival.calag := scale(bmival, scale=F) + pred.bmi(i, age, sex, qimd, cancer.lag), by= group]

cat("SBP estimation\n")
if (i > (init.year-2011)) POP <- ageing.distr("omsysval")
POP[between(age, ageL, ageH), omsysval.cvdlag := scale(omsysval, scale=F) + pred.sbp(i, age, sex, qimd, bmival.cvdlag, cvd.lag), by= group] 

cat("CHOL estimation\n")
if (i > (init.year-2011)) POP <- ageing.distr("cholval")
POP[between(age, ageL, ageH), cholval.cvdlag := scale(cholval, scale=F) + pred.chol(i, age, sex, qimd, bmival.cvdlag, cvd.lag), by= group] 

cat("F&V estimation\n")
POP[between(age, ageL, ageH), porftvg.cvdlag := pred.fv(i, age, sex, qimd, bmival.cvdlag, cvd.lag)]
POP[between(age, ageL, ageH), frtpor.cvdlag := pred.fvrate(i, age, sex, qimd, porftvg.cvdlag, cvd.lag)]

POP[between(age, ageL, ageH), porftvg.calag := pred.fv(i, age, sex, qimd, bmival.calag, cancer.lag)]
POP[between(age, ageL, ageH), frtpor.calag := pred.fvrate(i, age, sex, qimd, porftvg.calag, cancer.lag)]


cat("Smoking initiation/cessation simulation\n")

if (i == (init.year-2011)) {
  cat("Smoking lag\n")
  POP[cigst1=="1", cigst1.cvdlag := "1"]
  POP[cigst1 %in% c("2", "3"), cigst1.cvdlag := ifelse(endsmoke < cvd.lag, "4", cigst1)] # slightly wrong.could also be a never smoker 
  POP[cigst1.cvdlag %in% c("2", "3"), endsmoke := endsmoke - cvd.lag]
  POP[cigst1.cvdlag == "4", `:=` (cigdyalCat = numsmok, endsmoke=0)]
  POP[cigst1.cvdlag == "4", cigst1.cvdlag := ifelse((packyears * 20 /numsmok) < cvd.lag, "1", "4")] # correction of above error
  POP[cigst1 == "4", cigst1.cvdlag := ifelse((packyears * 20 /cigdyalCat) < cvd.lag, "1", "4")]
  POP[cigst1.cvdlag == "4", packyears := packyears - cvd.lag * cigdyalCat/20]
  POP[cigst1.cvdlag =="1", `:=` (cigdyalCat = 0, packyears = 0)]
  POP[, cigst1.cvdlag := factor(cigst1.cvdlag)]
  POP[age < 16, cigst1.cvdlag :="1"]
} else {
  
  POP[cigst1.cvdlag %in% c("2", "3"), endsmoke := endsmoke + 1]
  POP[cigst1.cvdlag == "4", packyears := packyears + cigdyalCat/20]
  
  cat("Estimate smoking prevalence for age = 16\n")
  POP[age == 16 + cvd.lag, cigst1:="1"]
  POP[age == 16 + cvd.lag, cigst1.temp := dice(.N) < pred.sm0prev(i-cvd.lag, 16, sex, qimd)]
  POP[cigst1.temp == T, `:=` (cigdyalCat = resample(POP[qimd == .BY[[1L]] & cigst1.cvdlag == "4", cigdyalCat], .N, replace=T)), by=qimd] 
  POP[cigst1.temp == T, cigst1.cvdlag := "4"]
  POP[,cigst1.temp := NULL]
  
  cat("Smoking never to active smoker\n")
  POP[between(age, 16 + cvd.lag, 50 + cvd.lag) & (cigst1.cvdlag== "1"), 
      cigst1.temp1 := dice(.N) < pred.nev0sm1(i - cvd.lag, age - cvd.lag, qimd)] 
  POP[cigst1.temp1 == T, `:=` (cigdyalCat = resample(POP[qimd == .BY[[1L]] & cigst1.cvdlag == "4", cigdyalCat], .N, replace=T)), by=qimd]  
  
  cat("Smoking active to ex smoker\n")
  POP[between(age, 16 + cvd.lag, ageH) & cigst1.cvdlag== "4", 
      cigst1.temp2 := dice(.N) < pred.sm0ex1(i - cvd.lag, age - cvd.lag, sex, qimd)]
  
  cat("Smoking ex to active smoker\n")
  POP[between(age, 16 + cvd.lag, 75 + cvd.lag) & between(endsmoke, 1, 10), 
      cigst1.temp3 :=  dice(.N) < pred.ex0sm1(endsmoke, sex, qimd)]
  POP[cigst1.temp3 == T, cigdyalCat := numsmok] 
  
  POP[cigst1.temp1 == T, `:=` (cigst1.cvdlag = "4", packyears = cigdyalCat/20)]
  POP[,cigst1.temp1 := NULL]
  POP[cigst1.temp2 == T, `:=`(cigst1.cvdlag = "3", endsmoke = 0, numsmok = cigdyalCat)]
  POP[,cigst1.temp2 := NULL]
  POP[cigst1.temp3 == T, `:=` (cigst1.cvdlag = "4", packyears = packyears + cigdyalCat/20)]
  POP[,cigst1.temp3 := NULL]
}

POP[cigst1.cvdlag =="1", cigst1.calag := "1"]
POP[cigst1.cvdlag %in% c("2", "3"), cigst1.calag := ifelse(endsmoke < cancer.lag, "4", cigst1.cvdlag)]
POP[cigst1.calag == "4", cigst1.calag := ifelse((packyears * 20 /numsmok) < cancer.lag,  "1", "4")]
POP[cigst1.cvdlag == "4", cigst1.calag := ifelse((packyears * 20 /cigdyalCat) < cancer.lag,  "1", "4")]
POP[, cigst1.calag := factor(cigst1.calag)]


# ETS 
if (i > (init.year-2011)) {
  cat("Estimating ETS...\n")
  smoking.preva.forets1 <- POP[between(age, 18, ageH), list(new.preval=prop.table(table(cigst1.cvdlag=="4"))[2]), by=.(qimd)]
  setkey(smoking.preva.forets1)
  # works for decreasing smoking prevelence
  smoking.preva.forets <- merge(
    smoking.preva.forets1, 
    smoking.preva.forets0,
    by="qimd"
    )[,
      change := (old.preval - new.preval) / old.preval][change<0, change:=0]
  
  POP[id %in% POP[expsmokCat=="1", sample_frac(.SD, smoking.preva.forets[qimd == .BY, change]), by=qimd, .SDcols="id"][,id], expsmokCat :="0"]
  # works for increasing smoking prevelence
  smoking.preva.forets <- merge(
    smoking.preva.forets1, smoking.preva.forets0, by="qimd"
    )[,change := (new.preval - old.preval) / new.preval][change<0,change:=0]
  
  POP[id %in% POP[expsmokCat=="0", sample_frac(.SD, smoking.preva.forets[qimd==.BY, change]), by=qimd, .SDcols="id"][,id], expsmokCat :="1"]
  
}

smoking.preva.forets0 <- POP[between(age, 18, ageH), list(old.preval=prop.table(table(cigst1.cvdlag=="4"))[2]), by=.(qimd)] # to be used for ETS
setkey(smoking.preva.forets0) # to be used as a baseline for new years calculation

cat("DIAB estimation\n")
# to predict diabetics that where healthy x years ago you need to apply current.prevalence-x*(diab.incid - mortality)
# Diabetes incidence from Holden SE, Barnett AH, Peters JR, et al. The incidence of type 2 diabetes in the United Kingdom from 1991 to 2010. Diabetes Obes Metab 2013;15:844–52. doi:10.1111/dom.12123
# NOTE: this is for type 2 diabetes only. For this ages I am concerned with this is absolutely fine. very few new diabetes I patients older than 35 
if (i == (init.year - 2011)) { # will need special case when cvd.lag = 0
  POP[, diabtotr.cvdlag := diabtotr]
  POP[between(age, 20, 70) & diabtotr == "2", 
      diabtotr.cvdlag := pred.diab.incid.lag(i, age, sex, qimd, bmival, cvd.lag, cvd.lag, .N)]
} 

if (i > (init.year - 2011) & i < (init.year - 2011 + cvd.lag)) {
  POP[between(age, 20, 70) & diabtotr == "2" & diabtotr.cvdlag == "1", 
      diabtotr.cvdlag := as.factor(ifelse(dice(.N) < 1/(cvd.lag + 1-i), 2, 1))]
} 

if (i == (init.year - 2011 + cvd.lag)) {
  POP[, diabtotr.cvdlag := diabtotr]
} 

if (i > (init.year - 2011 + cvd.lag)) {
  POP[between(age, ageL, ageH) & diabtotr.cvdlag == "1", 
      diabtotr.cvdlag := ifelse(pred.diab.incid(i, age, sex, qimd, bmival.cvdlag, cvd.lag), 2L, 1L)]
} 

cat("DIAB finished\n")
agegroup.fn(POP)
agegroup.fn(SPOP2011)
setkey(POP, qimd, sex, agegroup)
setkey(SPOP2011, qimd, sex, agegroup)

output <- vector("list", 5)
if (file.exists(paste0(output.dir(), "riskfactors.rds"))) output[[1]] <- readRDS(paste0(output.dir(), "riskfactors.rds"))

output[[2]] <- POP[, output.rf(.SD), 
                   by=.(qimd, sex, agegroup)]

output[[3]] <- POP[, output.rf(.SD), by=.(sex, agegroup)]

output[[4]] <- POP[between(age, ageL, ageH), output.rf(.SD), by=.(qimd, sex)]

output[[5]] <- POP[between(age, ageL, ageH), output.rf(.SD), by=.(sex)]

saveRDS(rbindlist(output, fill = T), file = paste0(output.dir(), "riskfactors.rds"))
rm(output)









