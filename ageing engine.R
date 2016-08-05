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

if ("C16" %in% diseasestoexclude) {
  #cat("salt estimation\n")
  if (i == init.year - 2011) {
    POP[between(age, 19, ageH),
        percentile := (frank(salt.intersalt,
                             na.last = F,
                             ties.method = "random") - 1) / (.N - 1),
        by = .(age, sex, qimd)]
    
    POP[, `:=`(salt.intersalt = NULL)]
  } else {
    POP[between(age, 19, ageH),
        percentile := (frank(salt24h.calag,
                             na.last = F,
                             ties.method = "random") - 1) / (.N - 1),
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
      POP[, `:=`(percentile = NULL,
                 salt.l = NULL,
                 salt.u = NULL)]
      rm(tmp.ca, tmp.cvd)
    }
    else if ((i + 2011 - cancer.lag) < 2003 &&
             (i + 2011 - cvd.lag) >= 2003) {
      tmp.cvd0 <-
        pred.salt(cvd.lag - 7.5, cvd.lag) # salt exposure remains as of 2003
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
      POP[, `:=`(percentile = NULL,
                 salt.l = NULL,
                 salt.u = NULL)]
      rm(tmp.ca0, tmp.cvd0, tmp.cvd)
    } 
    else {
      tmp.cvd0 <-
        pred.salt(-7.5 + cvd.lag, cvd.lag) # salt exposure remains as of 2003
      tmp.cvd <- pred.salt(i, cvd.lag) # alternative current policy
      tmp.ca0  <-
        pred.salt(-7.5 + cancer.lag, cancer.lag) # as of 2003
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
      POP[, `:=`(percentile = NULL,
                 salt.l = NULL,
                 salt.u = NULL)]
      rm(tmp.ca0, tmp.cvd0, tmp.cvd)
    }
  } else if (scenarios.list[[iterations]] == "salt slow intervention.Rc") {
    time.correction.cvd <- -2003 + 2011 - cvd.lag
    time.correction.ca  <- -2003 + 2011 - cancer.lag
    
    if ((i + 2011 - cancer.lag) < 2003 &&
        (i + 2011 - cvd.lag) < 2003) {
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
      POP[, `:=`(percentile = NULL,
                 salt.l = NULL,
                 salt.u = NULL)]
      rm(tmp.ca, tmp.cvd)
    } else if ((i + 2011 - cancer.lag) < 2003 &&
               (i + 2011 - cvd.lag) >= 2003) {
      tmp.cvd0 <-
        pred.salt(cvd.lag - 7.5 + (time.correction.cvd + i) / 5, cvd.lag) # salt exposure remains as of 2003
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
      POP[, `:=`(percentile = NULL,
                 salt.l = NULL,
                 salt.u = NULL)]
      rm(tmp.ca0, tmp.cvd0, tmp.cvd)
    } else {
      tmp.cvd0 <-
        pred.salt(cvd.lag - 7.5 + (time.correction.cvd + i) / 5, cvd.lag) # salt exposure remains as of 2003
      tmp.cvd  <- pred.salt(i, cvd.lag) # alternative current policy
      tmp.ca0  <-
        pred.salt(cancer.lag - 7.5 + (time.correction.ca + i) / 5, cancer.lag) # as of 2003
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
      POP[, `:=`(percentile = NULL,
                 salt.l = NULL,
                 salt.u = NULL)]
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
    POP[, `:=`(percentile = NULL,
               salt.l = NULL,
               salt.u = NULL)]
    rm(tmp.ca, tmp.cvd)
  }
  POP[salt24h.cvdlag <1 , salt24h.cvdlag := 1]
  POP[salt24h.calag  <1 , salt24h.calag  := 1]
  POP[salt24h.cvdlag >30, salt24h.cvdlag := 30]
  POP[salt24h.calag  >30, salt24h.calag  := 30]
} else {
  POP[, salt24h.cvdlag := 6]
  POP[, salt24h.calag := 6]
}
#qplot(percentile, salt.u, col = age, data = tmp.ca, facets = sex~qimd)

# PA estimation -----------------------------------------------------------
#cat("PA estimation\n")
setkey(POP, age, sex, qimd)
if (i == init.year - 2011) {# first iteration
  POP[, 
      a30to06m.rank := (frank(a30to06m,
                              na.last = F, 
                              ties.method = "random") - 1) / (.N - 1),
      by = .(age, sex, qimd)
      ] # due to small group numbers some NA may be created. Thus the correction below
  POP[is.na(a30to06m.rank), a30to06m.rank := dice(.N)]
  POP[, a30to06m := NULL]
} else {
  POP[, 
      a30to06m.rank := (frank(a30to06m.cvdlag,
                              na.last = F, 
                              ties.method = "random") - 1) / (.N - 1),
      by = .(age, sex, qimd)
      ] # due to small group numbers some NA may be created. Thus the correction below
  POP[is.na(a30to06m.rank), a30to06m.rank := dice(.N)]
}
POP[between(age, 20, 84), a30to06m.cvdlag := pred.pa(i, age, sex, qimd, a30to06m.rank, cvd.lag)]

POP[between(age, ageL, ageH), a30to06m.calag := pred.pa(i, age, sex, qimd, a30to06m.rank, cancer.lag)]
POP[, a30to06m.rank := NULL]


# F&V estimation ----------------------------------------------------------
#cat("F&V estimation\n")
if (i == init.year - 2011) {# first iteration
  POP[, 
      porftvg.rank := (frank(porftvg,
                             na.last = F, 
                             ties.method = "random") - 1) / (.N - 1),
      by = .(age, sex, qimd)
      ] # due to small group numbers some NA may be created. Thus the correction below
  POP[is.na(porftvg.rank), porftvg.rank := dice(.N)]
  POP[, porftvg := NULL]
} else {
  POP[, 
      porftvg.rank := (frank(porftvg.cvdlag,
                             na.last = F, 
                             ties.method = "random") - 1) / (.N - 1),
      by = .(age, sex, qimd)
      ] # due to small group numbers some NA may be created. Thus the correction below
  POP[is.na(porftvg.rank), porftvg.rank := dice(.N)]
}

POP[between(age, ageL, ageH), porftvg.cvdlag := pred.fv(i, age, sex, qimd, porftvg.rank, cvd.lag)]

POP[between(age, ageL, ageH), porftvg.calag := pred.fv(i, age, sex, qimd, porftvg.rank, cancer.lag)]

POP[, porftvg.rank := NULL]

# Smoking smoothing  ---------------
if (i == init.year - 2011) {
  POP[between(age, 16, ageH),
      percentile := (frank(cigst1,
                           na.last = F,
                           ties.method = "random") - 1) / (.N - 1),
      by = .(age, sex, qimd)]
  setorder(POP, endsmoke)
  POP[cigst1 %in% c("2", "3"), percentile := sort(percentile, T), by = .(age, sex, qimd)]
  
  POP[between(age, 16, ageH),
      cigst2:=pred.nev0sm1(i, age, sex, qimd, percentile, 0, FALSE)] # 2 = start smoking
  
  POP[cigst1 == "1" & cigst2 == 2L, 
      `:=` (
        cigst1   = factor(4, 1:4),
        endsmoke = 0L,
        smokyrs  = POP[cigst1 == "4" & group == .BY[[1]],
                       as.integer(round(mean(smokyrs)))],
        cigdyal  = pred.cigdyal(i, age, sex, qimd, dice(.N),
                                0),
        numsmok  = 0), 
      by = group]
  
  
  POP[cigst1 != "1" & cigst2 == 1L, 
      `:=` (
        cigst1     = factor(1, 1:4),
        endsmoke   = 0L,
        smokyrs    = 0L,
        cigdyal    = 0,
        numsmok    = 0)]
  
  POP[between(age, 16, ageH) & cigst1!="1",
      percentile := (frank(cigst1,
                           na.last = F,
                           ties.method = "random") - 1) / (.N - 1),
      by = .(age, sex, qimd)]
  
  setorder(POP, endsmoke)
  POP[cigst1 %in% c("2", "3"), percentile :=
        sort(percentile,  T), 
      by = .(age, sex, qimd)]
  
  POP[between(age, 16, ageH) & cigst1!="1",
      cigst2 := pred.sm0ex1(i, age, sex, qimd, percentile, 0, FALSE)] # 4 == remain smoker
  
  POP[cigst1 == "4" & cigst2 == 3L, 
      `:=` (
        cigst1 = factor(3, 1:4), 
        endsmoke = POP[cigst1 == "3" & group == .BY[[1]],
                       as.integer(round(mean(endsmoke)))],
        cigdyal  = 0,
        numsmok  = cigdyal), 
      by = group]
  
  POP[cigst1 %in% c("2", "3") & cigst2 == 4L, 
      `:=` (
        cigst1 = factor(4, 1:4), 
        endsmoke = 0L,
        cigdyal  = numsmok,
        numsmok  = 0)]
  
  POP[, c("cigst2", "percentile") := NULL]
  
  # smooth smokyrs
  ss <- POP[between(age, 16, 84), .(mean(smokyrs), .N), by=.(age, sex, qimd, cigst1)][
    , V1.sm := predict(loess(V1~age, weights = N, span = 0.5)), by=.(sex, qimd, cigst1)][
      V1.sm < 0, V1.sm := 0][
        , corr.factor := V1.sm/V1][
          is.na(corr.factor) | is.infinite(corr.factor) | N < 5, corr.factor := 1]
  POP[ss, on=c("age", "sex", "qimd", "cigst1"), smokyrs := as.integer(round(smokyrs * corr.factor))]
  # smooth endsmok
  ss <- POP[between(age, 16, 84), .(mean(endsmoke), .N), by=.(age, sex, qimd, cigst1)][
    , V1.sm := predict(loess(V1~age, weights = N, span = 0.5)), by=.(sex, qimd, cigst1)][
      V1.sm < 0, V1.sm := 0][
        , corr.factor := V1.sm/V1][is.na(corr.factor) | is.infinite(corr.factor) | N < 5, corr.factor := 1]
  POP[ss, on=c("age", "sex", "qimd", "cigst1"), endsmoke := as.integer(round(endsmoke * corr.factor))]
  # smooth numsmok
  ss <- POP[between(age, 16, 84), .(mean(numsmok), .N), by=.(age, sex, qimd, cigst1)][
    , V1.sm := predict(loess(V1~age, weights = N, span = 0.5)), by=.(sex, qimd, cigst1)][
      V1.sm < 0, V1.sm := 0][
        , corr.factor := V1.sm/V1][is.na(corr.factor) | is.infinite(corr.factor) | N < 5, corr.factor := 1]
  POP[ss, on=c("age", "sex", "qimd", "cigst1"), numsmok := round(numsmok * corr.factor)]
  
  # cigdyal in HSE was how many cigarettes are you smoking now. 
  # Not through lifetime. Below is the adjustment for this bias
  tt <- POP[between(age, 60, ageH) & cigst1 == "4",
            .(id, year = i, age, sex, qimd, smokyrs, cigdyal, agegroup)]
  tt[cigdyal >30, cigdyal := 30]
  
  tt[, cigdyal.rank := (frank(cigdyal,
                              na.last = F,
                              ties.method = "random") - 1) / (.N - 1),
     by = .(age, sex, qimd)
     ]
  tt[is.na(cigdyal.rank), cigdyal.rank := dice(.N)]
  tt <- rbindlist(rep(as.list(list(tt)), 20), idcol = "kk")
  tt[, `:=` (smokyrs = smokyrs - kk + 1,
             year    = year - kk + 1,
             age     = age - kk +1)]
  tt <- tt[smokyrs >= 0]
  
  tt[, cigdyal.history := pred.cigdyal(year, age, sex, qimd,
                                       cigdyal.rank, 0)]
  tt[, cigdyal.history := mean(cigdyal.history), by = id]
  
  setkey(tt, id)
  tt <- unique(tt)
  tt[cigdyal>cigdyal.history, cigdyal.history := cigdyal]
  POP[tt, on = "id", cigdyal := cigdyal.history]
  
  # smooth cigdyal
  ss <- POP[between(age, 16, 84), .(mean(cigdyal), .N), by=.(age, sex, qimd, cigst1)][
    , V1.sm := predict(loess(V1~age, weights = N, span = 0.5)), by=.(sex, qimd, cigst1)][
      V1.sm < 0, V1.sm := 0][
        , corr.factor := V1.sm/V1][is.na(corr.factor) | is.infinite(corr.factor) | N < 5, corr.factor := 1]
  POP[ss, on=c("age", "sex", "qimd", "cigst1"), cigdyal := round(cigdyal * corr.factor)]
  
  rm(tt, ss)
}

# Smoking dynamics ----------------------------------------
#cat("Smoking dynamics\n")
if (i > init.year - 2011 & POP[cigst1 != "1", .N] > 0) {
  POP[cigst1 %in% c("2", "3"), endsmoke := endsmoke + 1L]
  POP[cigst1 == "4", smokyrs := smokyrs + 1L]
  POP[cigst1 == "4" & between(age, 16, 60),
      cigdyal.rank := (frank(cigdyal,
                             na.last = F,
                             ties.method = "random") - 1) / (.N - 1),
      by = .(age, sex, qimd)
      ]
  
  POP[is.na(cigdyal.rank) & between(age, 16, 60),
      cigdyal.rank := dice(.N)]
  
  POP[cigst1 == "4" & between(age, 16, 60),
      `:=` (cigdyal.curr =
              pred.cigdyal(i, age, sex, qimd,
                           cigdyal.rank, policy.effect.cigdyal))]
  POP[cigst1 == "4" & between(age, 16, 60),
      `:=` (cigdyal =
              (cigdyal * 9 + cigdyal.curr) / 10 # 4, 5
      )
      ]
  POP[cigst1 == "4" & between(age, 61, ageH),
      `:=` (cigdyal =
              (cigdyal * 9 + cigdyal*(1 - policy.effect.cigdyal)) / 10 # 4, 5
      )
      ]
  POP[, cigdyal.rank := NULL]
  
  
  POP[between(age, 16, ageH),
      percentile := (frank(cigst1,
                           na.last = F,
                           ties.method = "random") - 1) / (.N - 1),
      by = .(age, sex, qimd)]
  
  setorder(POP, endsmoke)
  POP[cigst1 %in% c("2", "3"), percentile := sort(percentile, T), by = .(age, sex, qimd)]
  
  POP[between(age, 16, ageH),
      cigst2:=pred.nev0sm1(i, age, sex, qimd, percentile, 0, FALSE)] # 2 = start smoking

  if (millenia == FALSE) {
  POP[cigst1 == "1" & cigst2 == 2L & rbinom(.N, 1, policy.effect.cigst1) == 0, 
      `:=` (
        cigst1   = factor(4, 1:4),
        endsmoke = 0L,
        smokyrs  = 0L,
        cigdyal  = pred.cigdyal(i, age, sex, qimd, dice(.N),
                                policy.effect.cigdyal),
        numsmok  = 0)]
  }
  
if (millenia == TRUE) {
  POP[(i + 2011 - age) >= 2000 & cigst1 == "1" & cigst2 == 2L & 
        rbinom(.N, 1, policy.effect.cigst1) == 0, 
      `:=` (
        cigst1   = factor(4, 1:4),
        endsmoke = 0L,
        smokyrs  = 0L,
        cigdyal  = pred.cigdyal(i, age, sex, qimd, dice(.N),
                                policy.effect.cigdyal),
        numsmok  = 0)]
  
  POP[(i + 2011 - age) < 2000 & cigst1 == "1" & cigst2 == 2L, 
      `:=` (
        cigst1   = factor(4, 1:4),
        endsmoke = 0L,
        smokyrs  = 0L,
        cigdyal  = pred.cigdyal(i, age, sex, qimd, dice(.N),
                                policy.effect.cigdyal),
        numsmok  = 0)]
}
  POP[cigst1 != "1" & cigst2 == 1L,
      `:=` (
        cigst1     = factor(1, 1:4),
        endsmoke   = 0L,
        smokyrs    = 0L,
        cigdyal    = 0,
        numsmok    = 0)]

  
  POP[between(age, 16, ageH) & cigst1!="1",
      percentile := (frank(cigst1,
                           na.last = F,
                           ties.method = "random") - 1) / (.N - 1),
      by = .(age, sex, qimd)]
  
  # give higher prob to smaller endsmoke to relapse
  setorder(POP, endsmoke)
  POP[cigst1 %in% c("2", "3"), percentile :=
                            sort(percentile,  T), 
                          by = .(age, sex, qimd)]
  
  POP[between(age, 16, ageH) & cigst1!="1",
      cigst2:=pred.sm0ex1(i, age, sex, qimd, percentile, policy.effect.cigst1, millenia)] # 4 == remain smoker
  
  POP[cigst1 == "4" & cigst2 == 3L, 
      `:=` (
        cigst1 = factor(3, 1:4), 
        endsmoke = 0L,
        cigdyal  = 0,
        numsmok  = cigdyal)]
  
  POP[cigst1 %in% c("2", "3") & cigst2 == 4L, 
      `:=` (
        cigst1 = factor(4, 1:4), 
        endsmoke = 0L,
        cigdyal  = numsmok,
        numsmok  = 0)]
  
  POP[, c("cigst2", "percentile"):= NULL]
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
if (POP[cigst1 != "1", .N] > 0) {
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
POP[endsmoke.cvdlag < 0L & (smokyrs + endsmoke) > as.integer(cvd.lag),
    `:=` (cigst1.cvdlag   = "4",
          endsmoke.cvdlag = 0L,
          smokyrs.cvdlag  = smokyrs + endsmoke - as.integer(cvd.lag),
          cigdyal.cvdlag  = numsmok.cvdlag,
          numsmok.cvdlag  = 0)] 
POP[endsmoke.cvdlag < 0L & (smokyrs + endsmoke) <= as.integer(cvd.lag),
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
POP[cigst1 == "4" & smokyrs.cvdlag > 0L &
      rbinom(.N, 1, 0.2) == 1L, 
    `:=` (cigst1.cvdlag   = "3",
          endsmoke.cvdlag = 0L,
          numsmok.cvdlag  = cigdyal.cvdlag,
          cigdyal.cvdlag  = 0)]

POP[, cigst1.cvdlag := factor(cigst1.cvdlag, 1:4)]

POP[age < (9L + cvd.lag), `:=`(cigst1.cvdlag   = "1",
                               smokyrs.cvdlag  = 0L,
                               endsmoke.cvdlag = 0L,
                               cigdyal.cvdlag  = 0,
                               numsmok.cvdlag  = 0)]
}
# Calculate packyears
POP[, packyears.cvdlag := 
      smokyrs.cvdlag * (cigdyal.cvdlag + numsmok.cvdlag) / 20]

# Smoking cancer lag -------------------------------
# needed for ets.calag
#cat("Smoking ca lag\n")
if (POP[cigst1 != "1", .N] > 0) {
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
POP[endsmoke.calag < 0L & (smokyrs + endsmoke) > as.integer(cancer.lag),
    `:=` (cigst1.calag   = "4",
          endsmoke.calag = 0L,
          smokyrs.calag  = smokyrs + endsmoke - as.integer(cancer.lag),
          cigdyal.calag  = numsmok.calag,
          numsmok.calag  = 0)]
POP[endsmoke.calag < 0L & (smokyrs + endsmoke) <= as.integer(cancer.lag),
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

POP[cigst1 == "4" & smokyrs.calag > 0L &
      rbinom(.N, 1, 0.2) == 1L,
    `:=` (cigst1.calag   = "3",
          endsmoke.calag = 0L,
          numsmok.calag  = cigdyal.calag,
          cigdyal.calag  = 0)]
POP[, cigst1.calag := factor(cigst1.calag, 1:4)]

POP[age < (5L + cancer.lag), `:=`(cigst1.calag   = "1",
                                  smokyrs.calag  = 0L,
                                  endsmoke.calag = 0L,
                                  cigdyal.calag  = 0,
                                  numsmok.calag  = 0)]
}

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
    POP[, smok.prev := sum(cigst1 == "4") / .N, by = .(qimd)
        ][, ets.pr := sum(expsmokCat == "1") / .N, by = .(agegroup, sex, qimd)
          ][, .(ets.coef = mean(ets.pr / smok.prev)), by = .(agegroup, sex, qimd)
            ]
  POP[, ets.pr := NULL]
} 

setkey(POP, agegroup, sex, qimd)
if (POP[cigst1 != "1", .N] > 0) {
POP[, smok.prev := sum(cigst1.cvdlag == "4") / .N, by = qimd]
POP[pred.ets, expsmok.cvdlag := factor(rbinom(.N, 1, bound(smok.prev * ets.coef)))]
POP[, smok.prev := sum(cigst1.calag == "4") / .N, by = qimd]
POP[pred.ets, expsmok.calag := factor(rbinom(.N, 1, bound(smok.prev * ets.coef)))]
} else {
  POP[, `:=` (
    expsmok.cvdlag = factor(0, 0:1),
    expsmok.calag  = factor(0, 0:1)
  )]
}

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
                    paste0("V", 1:52),
                    output.rf.names))
  
  saveRDS(rbindlist(riskfactors, T, T) ,
          file = paste0(output.dir(), "riskfactors.rds"))
}
