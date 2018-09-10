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

cat("Salt start\n")
POP[, salt24h.cvdlag := pred.salt(i, age, sex, salt_rank, cvd.lag, salt_data)]
POP[, salt24h.calag := pred.salt(i, age, sex, salt_rank, cancer.lag, salt_data)]
POP[, salt24h.report := pred.salt(i, age, sex, salt_rank, 0, salt_data)]
POP[, salt24h.report.rd := pred.salt.rd(i, age, sex, salt_rank, 0, salt_data)]
POP[, salt24h.report.diff := salt24h.report.rd - salt24h.report]
cat("Salt finish\n")

# PA estimation -----------------------------------------------------------
#cat("PA estimation\n")
setkey(POP, age, sex, qimd)

POP[between(age, 20, 84), a30to06m.cvdlag := pred.pa(i, age, sex, qimd, cvd.lag)]

POP[between(age, ageL, ageH), a30to06m.calag := pred.pa(i, age, sex, qimd, cancer.lag)]

# F&V estimation ----------------------------------------------------------
#cat("F&V estimation\n")
POP[between(age, ageL, ageH), porftvg.cvdlag := pred.fv(i, age, sex, qimd,  cvd.lag)]

POP[between(age, ageL, ageH), porftvg.calag := pred.fv(i, age, sex, qimd, cancer.lag)]

# Correct mean cigarettes -----------------------------------------------------------
# cigdyal in HSE was how meny cigarettes are you smoking now. Not through lifetime. Below is
# the adjustment for this bias
if (i == init.year - 2011) {
  tt <- POP[between(age, 40, ageH) & cigst1 == "4", .(id, year = i, age, sex, qimd, smokyrs, cigdyal)]
  tt <- rbindlist(sample(list(tt), tt[, max(smokyrs)], T), idcol = "kk")
  tt[, `:=` (smokyrs = smokyrs - kk + 1,
             year    = year - kk + 1,
             age     = age - kk +1)]
  tt <- tt[smokyrs >= 0]
  tt[, cigdyal.history := predict(smok.cigdyal.svylm, data.table(year = year, sex = sex, qimd = qimd, smokyrs = smokyrs),
                                  type = "response", se.fit=F)]
  tt[, cigdyal.history := mean(cigdyal.history), by = id]
  setkey(tt, id)
  tt <- unique(tt)
  class(tt$cigdyal.history) <- "numeric"
  tt[cigdyal > cigdyal.history, cigdyal.history := cigdyal]
  tt[, cigdyal.history := 0.9 * cigdyal.history + 0.1 * cigdyal]
  POP[tt, on = "id", cigdyal := cigdyal.history]
  #POP[age>54 & sex == "1", cigdyal := cigdyal * 1.3]
  #POP[age>54 & sex == "2", cigdyal := cigdyal * 1.5]
  rm(tt)
}

# Smoking dynamics --------------------------------------------------------
#cat("Smoking dynamics\n")
if (i > init.year - 2011) {
  POP[cigst1 %in% c("2", "3"), endsmoke := endsmoke + 1L]
  POP[cigst1 == "4", 
      `:=` (cigdyal = 
              (cigdyal * smokyrs + pred.cigdyal(i, sex, qimd, smokyrs + 1L)) /
              (smokyrs + 1L),
            smokyrs = smokyrs + 1L
      )
      ]
  
  POP[age <= 20L, cigst1 := "1"]
  POP[age == 20L & pred.sm0prev(i, sex, qimd) == 1L,
      `:=` (cigst1  = "4", 
            cigdyal  = pred.cigdyal(i, sex, qimd, rpois(.N, 3)),
            numsmok  = 0,  # not needed, for safety
            endsmoke = 0L, # not needed, for safety 
            smokyrs  = rpois(.N, 3)
      )
      ]
  
  POP[age == 20L & cigst1 == "1" & pred.ex0prev(i, sex) == 1L, 
      `:=` (cigst1   = "3", 
            cigdyal  = 0,  # not needed, for safety
            numsmok  = pred.cigdyal(i, sex, qimd, rpois(.N, 1)),
            endsmoke = rpois(.N, 1),
            smokyrs  = rpois(.N, 1) 
      )
      ]
  
  POP[between(age, 21L, 24L) & cigst1 == "1" & 
        pred.nev0sm1(i, age, sex, qimd) == 1L,
      `:=` (cigst1   = "4", 
            cigdyal  = pred.cigdyal(i, sex, qimd, smokyrs),
            numsmok  = 0,  # not needed, for safety
            endsmoke = 0L, # not needed, for safety 
            smokyrs  = 0L
      )
      ]
  
  POP[between(age, 21L, ageH) & cigst1 == "4" &
        pred.sm0ex1(i, age, sex, qimd) == 1L, 
      `:=`(cigst1   = "3",
           endsmoke = 0L,
           numsmok  = cigdyal,
           cigdyal  = 0)
      ]
  
  POP[between(age, 20L, 64L) & between(endsmoke, 1, 9) &
        pred.ex0sm1(endsmoke, sex, qimd, "log") == 0L, 
      `:=`(cigst1   = "4",
           endsmoke = 0L,
           cigdyal  = 
             (numsmok * smokyrs + pred.cigdyal(i, sex, qimd, smokyrs)) / (smokyrs + 1L),
           numsmok = 0
      )
      ]
  POP[between(age, 65L, 79L) & sex == "1" & between(endsmoke, 1, 9) &
        pred.ex0sm1(endsmoke, sex, qimd, "log") == 0L, 
      `:=`(cigst1   = "4",
           endsmoke = 0L,
           cigdyal  = 
             (numsmok * smokyrs + pred.cigdyal(i, sex, qimd, smokyrs)) / (smokyrs + 1L),
           numsmok = 0
      )
      ]
}

# Calculate packyears
POP[, packyears := 
      smokyrs * (cigdyal + numsmok) / 20]

# needed for QRisk and QDrisk
POP[, smoke_cat := 0L]
POP[cigst1 == "3", smoke_cat := 1L]
POP[cigst1 == "4", smoke_cat := 3L]
POP[cigst1 == "4" & cigdyal < 10L, smoke_cat := 2L]
POP[cigst1 == "4" & cigdyal > 19L, smoke_cat := 4L]

# Smoking CVD lag -----------------------------------------------------------------
# cigst1 is updated anually. The cigst1.cvdlag is derived from cigst1 annually
#cat("Smoking cvd lag\n")
POP[, `:=`(
  endsmoke.cvdlag = endsmoke,
  cigdyal.cvdlag  = cigdyal,
  numsmok.cvdlag  = numsmok,
  smokyrs.cvdlag  = smokyrs,
  cigst1.cvdlag   = "1"
)]

POP[cigst1 %in% c("2", "3"),
    `:=` (cigst1.cvdlag   = cigst1,
          endsmoke.cvdlag = endsmoke - as.integer(cvd.lag))]
POP[endsmoke.cvdlag < 0L & (smokyrs + endsmoke) >= as.integer(cvd.lag),
    `:=` (cigst1.cvdlag   = "4",
          endsmoke.cvdlag = 0L,
          smokyrs.cvdlag  = smokyrs + endsmoke - as.integer(cvd.lag),
          cigdyal.cvdlag  = numsmok.cvdlag,
          numsmok.cvdlag  = 0)] 
POP[endsmoke.cvdlag < 0L & (smokyrs + endsmoke) < as.integer(cvd.lag),
    `:=` (cigst1.cvdlag   = "1",
          endsmoke.cvdlag = 0L,
          smokyrs.cvdlag  = 0L,
          cigdyal.cvdlag  = 0,
          numsmok.cvdlag  = 0)] 

POP[cigst1 == "4",
    `:=` (cigst1.cvdlag  = "4",
          smokyrs.cvdlag = smokyrs - as.integer(cvd.lag))]
POP[smokyrs.cvdlag < 0L,
    `:=` (cigst1.cvdlag   = "1",
          smokyrs.cvdlag  = 0L,
          endsmoke.cvdlag = 0L,
          cigdyal.cvdlag  = 0,
          numsmok.cvdlag  = 0)]
POP[cigst1 == "4" & smokyrs.cvdlag > 0L & pred.sm0ex1(i - cvd.lag, age - cvd.lag, sex, qimd) == 1L, 
    `:=` (cigst1.cvdlag   = "3",
          endsmoke.cvdlag = 0L,
          numsmok.cvdlag  = cigdyal.cvdlag,
          cigdyal.cvdlag  = 0)]

POP[, cigst1.cvdlag := factor(cigst1.cvdlag)]

POP[age < (5L + cvd.lag), cigst1.cvdlag := "1"]

# Calculate packyears
POP[, packyears.cvdlag := 
      smokyrs.cvdlag * (cigdyal.cvdlag + numsmok.cvdlag) / 20]

# Smoking cancer lag -----------------------------------------------------------------
#cat("Smoking ca lag\n")
POP[, `:=`(
  endsmoke.calag  = endsmoke,
  cigdyal.calag   = cigdyal,
  numsmok.calag   = numsmok,
  smokyrs.calag   = smokyrs,
  cigst1.calag    = "1",
  packyears.calag = 0
)]
POP[cigst1 %in% c("2", "3"),
    `:=` (cigst1.calag   = cigst1,
          endsmoke.calag = endsmoke - as.integer(cancer.lag))]
POP[endsmoke.calag < 0L & (smokyrs + endsmoke) >= as.integer(cancer.lag),
    `:=` (cigst1.calag   = "4",
          endsmoke.calag = 0L,
          smokyrs.calag  = smokyrs + endsmoke - as.integer(cancer.lag),
          cigdyal.calag  = numsmok.calag,
          numsmok.calag  = 0)] 
POP[endsmoke.calag < 0L & (smokyrs + endsmoke) < as.integer(cancer.lag),
    `:=` (cigst1.calag   = "1",
          endsmoke.calag = 0L,
          smokyrs.calag  = 0L,
          cigdyal.calag  = 0,
          numsmok.calag  = 0)]

POP[cigst1 == "4",
    `:=` (cigst1.calag  = "4",
          smokyrs.calag = smokyrs - as.integer(cancer.lag))]
POP[smokyrs.calag < 0L,
    `:=` (cigst1.calag  = "1",
          smokyrs.calag = 0L,
          endmsok.calag = 0L,
          cigdyal.calag = 0,
          numsmok.calag = 0)]

POP[cigst1 == "4" & smokyrs.calag > 0L & pred.sm0ex1(i - cancer.lag, age - cancer.lag , sex, qimd) == 1L, 
    `:=` (cigst1.calag   = "3",
          endsmoke.calag = 0L,
          numsmok.calag  = cigdyal.calag,
          cigdyal.calag  = 0)]
POP[, cigst1.calag := factor(cigst1.calag)]

POP[age < (5L + cancer.lag), cigst1.calag := "1"]

# Calculate packyears
POP[, packyears.calag :=
      smokyrs.calag * (cigdyal.calag + numsmok.calag) / 20]

# BMI estimation ----------------------------------------------------------
#cat("BMI estimation\n")
if (i > (init.year - 2011)) ageing.distr(bmi.rank, "bmival") # to match distribution shape to that of SPOP
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
#cat("SBP estimation\n")
if (i > (init.year - 2011)) ageing.distr(sbp.rank, "omsysval")
POP[between(age, ageL, ageH),
    omsysval.cvdlag := scale(omsysval, scale = F),
    by = group]
POP[between(age, ageL, ageH),
    omsysval.cvdlag := omsysval.cvdlag +
      pred.sbp(i,
               age,
               sex,
               qimd,
               bmival.cvdlag,
               cigst1,
               a30to06m.cvdlag,
               cvd.lag)]
POP[omsysval.cvdlag < 70, omsysval.cvdlag := 70]

# TC estimation -----------------------------------------------------------
#cat("CHOL estimation\n")
if (i > (init.year - 2011)) ageing.distr(chol.rank, "cholval")
POP[between(age, ageL, ageH),
    cholval.cvdlag := scale(cholval, scale = F),
    by = group]
POP[between(age, ageL, ageH),
    cholval.cvdlag :=
      cholval.cvdlag +
      pred.chol(i,
                age,
                sex,
                qimd,
                bmival.cvdlag,
                porftvg.cvdlag,
                cvd.lag)]
POP[cholval.cvdlag < 2.2, cholval.cvdlag := 2.2]

# BP meds prevalence ------------------------------------------------------------------
POP[between(age, 25, ageH), bpmed := pred.bpmed(.N, age, sex, qimd, omsysval.cvdlag)] # 25 for QDrisk

# ETS ---------------------------------------------------------------------
#cat("Estimating ETS...\n") # assumes linear relation with
# smoking prevalence by qimd, stratified by age and sex
if (i == init.year - 2011) {
  pred.ets <-
    POP[, sm.pr := sum(cigst1 == "4") / .N, by = .(qimd)
        ][, ets.pr := sum(expsmokCat == "1") / .N, by = .(agegroup, sex, qimd)
          ][, .(ets.coef = mean(ets.pr / sm.pr)), by = .(agegroup, sex, qimd)
            ]
  POP[, ets.pr := NULL]
} 

setkey(POP, agegroup, sex, qimd)
POP[, sm.pr := sum(cigst1.cvdlag == "4") / .N, by = qimd]
POP[pred.ets, expsmok.cvdlag := factor(rbinom(.N, 1, bound(sm.pr * ets.coef)))]
POP[, sm.pr := sum(cigst1.calag == "4") / .N, by = qimd]
POP[pred.ets, expsmok.calag := factor(rbinom(.N, 1, bound(sm.pr * ets.coef)))]


# Scenario fn -------------------------------------------------------------
#cat(paste0("before scenario.fn ", Sys.time(), "\n\n"))
post.ageing.scenario.fn(i)# placed here so bmi intervensions affect diabetes

# Diabetes estimation ---------------------------------------------------------
#cat("Diabetes estimation\n")
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

# Predict undiagnosed DM in the population -----------------------------------
if (length(grep("health check", scenarios.list[[iterations]]))) {
  POP[, undiag.diab := 0L]
  POP[diabtotr.cvdlag == "2", undiag.diab := pred.undiag.diab(.N, qimd)] # 25 for QDrisk
}

#cat("DIAB finished\n")

agegroup.fn(POP)

# Export ------------------------------------------------------------------
#cat(paste0("before export ", Sys.time(), "\n\n"))

if (i == init.year - 2011)
  riskfactors <- vector("list", yearstoproject * 5)
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
         function (x)
           setnames(x,
                    paste0("V", 1:54),
                    output.rf.names))
  
  saveRDS(rbindlist(riskfactors, T, T) ,
          file = paste0(output.dir(), "riskfactors.rds"))
}
