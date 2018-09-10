#cmpfile("./gastric cancer model.R")
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


# RF to RR  ---------------------------------------------------------------
cat("Loading gastric cancer model (C16) model...\n")
cat(paste0(Sys.time(), "\n\n"))
if (i == init.year - 2011) set(POP, NULL, "c16.incidence",  0) # Only needs to run the very first time of each simulation
POP[age==0, c16.incidence := 0]

# RR for tobacco from González CA, et al. Smoking and the risk of gastric cancer in the 
# European Prospective Investigation Into Cancer and Nutrition (EPIC). Int J Cancer 2003;107:629–34. 
# Table III for duration. Then I fit lm(rr-1 ~ 0 + duration) to force line pass from (0, 1). I do it 
# stochastically to extract the SE. Then I add 1 to the result because I estimated rr - 1
# out <- vector("numeric", 1e4)
# xu <- (c(rtrunc(rpois, 13, linf=1, lsup=30, lambda=14),
#          rtrunc(rpois, 27, linf=31, lsup=40, lambda=34),
#          rtrunc(rpois, 19, linf=41, lsup=70, lambda=50)))
# for (kkk in 1:1e4) {
#   yu <- c(exp(rnorm(13, log(1.31), (log(2.59)-log(0.66))/3.92)),
#           exp(rnorm(17, log(1.58), (log(2.83)-log(0.88))/3.92)),
#           exp(rnorm(29, log(2.36), (log(3.91)-log(1.42))/3.92)))
#   out[[kkk]] <- coef(glm(I(yu)~0+xu, family=gaussian(link="log")))
# }
# mean(exp(out))
# sd(exp(out))
# hist(out)
# hist(rnorm(1e4, mean(exp(out)), sd(exp(out))))
# I use CVD lag time for it because EPIC has a follow up of 5 years. 
#cat("smoking RR\n")
set(POP, NULL, "c16.tob.rr", 1)
POP[smokyrs.cvdlag > 0, c16.tob.rr := c16.tob.rr.mc^smokyrs.cvdlag]
# POP[packyears.cvdlag > 0,
#     `:=` (c16.tob.rr = c16.tob.rr.mc^log2(packyears.cvdlag * 20))] # packyears.cvdlag * 20 = cigarette years
POP[endsmoke.cvdlag > 0, c16.tob.rr := c16.tob.rr * (c16.extob.rr.mc^log2(endsmoke.cvdlag))] # packyears.cvdlag * 20 = cigarette years
POP[age > 69, c16.tob.rr := c16.tob.rr * (1-(age-69)/100)] # decrease risk for elderly
POP[is.na(c16.tob.rr) | c16.tob.rr < 1, c16.tob.rr := 1] #ex smokers of more than 20 years atract no risk. 

# Calculate RR for C16 from Comparative quantification of health risks [Internet].
# Geneva: World Health Organisation; 2004. 
# Chapter 9 table 9.28 (p100 in pdf)
# Now OUTDATED after the 2016 AIRC/WCRF gastric ca report
# cat("f&v RR\n")
setkey(POP, age)
set(POP, NULL, "c16.fv.rr", 1)
POP[c16.fv.rr.mc,  c16.fv.rr := rr^(porftvg.calag - 2L)] # no effect for more than 2 portions
POP[is.na(c16.fv.rr) | c16.fv.rr < 1, c16.fv.rr := 1]

# From Continuous Update Project Report: Diet, 
# Nutrition, Physical Activity and Stomach Cancer. 2016
# figure/table 8, p38
# Bmi risk factor only for cardia GCa. In the UK 34% in men and 20% 
# look here http://www.cancerresearchuk.org/health-professional/cancer-statistics/statistics-by-cancer-type/stomach-cancer/incidence#heading-Four
#cat("BMI RR\n")
set(POP, NULL, "c16.bmi.rr", 1)
POP[bmival.calag >= 26,  c16.bmi.rr := c16.bmi.rr.fn(bmival.calag, c16.bmi.rr.mc)] # no effect for bmi < 26
POP[age > 69, c16.bmi.rr := c16.bmi.rr * (1-(age-69)/100)] # decrease risk for elderly
POP[is.na(c16.bmi.rr) | c16.bmi.rr < 1, c16.bmi.rr := 1]

# Calculate RR for C16. From World Cancer Research Fund, 
# American Institute for Cancer Research. Food, nutrition, 
# physical activity, and the prevention of cancer:
# a global perspective. Washington, DC: WCRF/AICR; 2007.
# p145. RR 1.08 (1 - 1.17) per 1gr/day of salt. Risk starts from
# 3gr/day (around reference category of the 2 cohort studies) 
#cat("salt RR\n")
set(POP, NULL, "c16.salt.rr", 1)
POP[c16.salt.rr.mc, c16.salt.rr := rr^(salt24h.calag - c16.salt.optim)]
POP[age > 69, c16.salt.rr := c16.salt.rr * (1-(age-69)/100)] # decrease risk for elderly
POP[is.na(c16.salt.rr) | c16.salt.rr < 1, c16.salt.rr := 1]

# PAF estimation ----------------------------------------------------------
if (i == init.year - 2011) {
  #cat("Estimating gastric cancer PAF...\n")
  c16.cardia.paf <- 
    POP[between(age, ageL, ageH), 
        .(cardia.paf = 1 - 1 / (sum(c16.tob.rr * c16.bmi.rr * c16.salt.rr * c16.fv.rr) / .N)), 
        by = .(age, sex)
        ]
  #c16.cardia.paf[, cardia.paf := predict(loess(cardia.paf~age, span=0.20)), by = .(sex)]
  setkey(c16.cardia.paf, age, sex)
  
  c16.noncardia.paf <- 
    POP[between(age, ageL, ageH), 
        .(noncardia.paf = 1 - 1 / (sum(c16.tob.rr * c16.salt.rr * c16.fv.rr) / .N)), 
        by = .(age, sex)
        ]
  #c16.noncardia.paf[, noncardia.paf := predict(loess(noncardia.paf~age, span=0.20)), by = .(sex)]
  setkey(c16.noncardia.paf, age, sex)
  setkey(C16incid, age, sex)
  # From http://www.cancerresearchuk.org/health-professional/cancer-statistics/statistics-by-cancer-type/stomach-cancer/incidence#heading-Four
  C16incid[sex == "1", incid.cardia := incidence * 0.34]
  C16incid[sex == "2", incid.cardia := incidence * 0.20]
  C16incid[, incid.noncardia := incidence - incid.cardia]
  C16incid[c16.cardia.paf, p0.cardia := incid.cardia * (1 - cardia.paf)]
  C16incid[c16.noncardia.paf, p0.noncardia := incid.noncardia * (1 - noncardia.paf)]
  C16incid[is.na(p0.cardia), p0.cardia := incid.cardia]
  C16incid[is.na(p0.noncardia), p0.noncardia := incid.noncardia]
}

setkey(POP, age, sex)
POP[C16incid, `:=`(p0.cardia = p0.cardia, p0.noncardia = p0.noncardia)]

# Prevalence of C16 only in first run  ------------------------------------
if (i == init.year - 2011) {
  cat(paste0("Estimating gastric cancer prevalence in ",
             init.year - 1, " ...\n\n"))
  age.structure <- setkey(POP[age <= ageH, .N, by = .(age, sex)], age, sex)
  Temp <- merge(C16preval[age <= ageH], C16incid[age <= ageH], by = c("age", "sex"))
  Temp[, prevalence := prevalence - incidence]
  Temp[prevalence < 0, prevalence := 0]
  age.structure[Temp, Nprev := rbinom(.N, N, prevalence)]
  
  #age.structure[C16fatal[C16remis, on = c("age", "sex")],
  #             Nprev := round(Nprev * (1 + fatality + remission))]
  #age.structure[C16remis,  Nprev := round(Nprev * (1 + remission))]
  # dismod calculates fatality/remission from 
  # the prevalent cases and not the prevalent + incident cases 
  setnames(age.structure, "N", "population")
  POP <- merge(POP,
               deaths.causes.secgrad[cause == "Malignant neoplasm of stomach", .(agegroup, sex, qimd, sec.grad.adj)],
               by = c("agegroup", "sex", "qimd"), all.x = T)
  POP[is.na(sec.grad.adj), sec.grad.adj := 1]
  
  Temp <- POP[age <= ageH, 
              sample(id, 
                     age.structure[age == .BY[[1]] & sex == .BY[[2]] , Nprev], 
                     prob = c16.tob.rr * c16.bmi.rr * c16.salt.rr  * c16.fv.rr *
                       sec.grad.adj, 
                     replace = F), 
              by = .(age, sex)][, V1]
  POP[id %in% Temp, c16.incidence := init.year - 1] # and then we assign these ids to the population
  POP[, sec.grad.adj := NULL]
}

#cat("Estimating gastric cancer incidence...\n\n")
POP[between(age, ageL, ageH) &
      c16.incidence == 0 &
      dice(.N) < p0.cardia * c16.tob.rr * c16.bmi.rr * c16.salt.rr * c16.fv.rr, 
    c16.incidence := 2011 + i] 
POP[between(age, ageL, ageH) &
      c16.incidence == 0 &
      dice(.N) < p0.noncardia * c16.tob.rr * c16.salt.rr * c16.fv.rr, 
    c16.incidence := 2011 + i] 

# Estimate C16 mortality 
#cat("Estimating gastric cancer mortality...\n\n")

# Apply assumptions about improvement of fatality by year
if (i > init.year - 2011) {
  C16fatal[, fatality  := fatality  * (100 - fatality.annual.improvement.c16)/100]
  C16remis[, remission := remission * (100 + fatality.annual.improvement.c16/2)/100]
  # It make sense when fatality decreasing remission to increase and vice versa. 
  # The increase of remission was set to half the decrease in survival to adjust
  # for increase survival but not cure 
  fatality.annual.improvement.c16 <- fatality.annual.improvement.c16 * 0.99
}

setkey(POP, age, sex)
POP[C16fatal, fatality := fatality]

Temp <- POP[between(age, ageL, ageH), 
            .(before = sum(c16.incidence > 0) * mean(fatality)),
            by = .(agegroup, sex)] #expected number of deaths

POP[between(age, ageL, 69) & qimd == "1", fatality := (100 - fatality.sec.gradient.c16 / 2) * fatality/100]
POP[between(age, ageL, 69) & qimd == "2", fatality := (100 - fatality.sec.gradient.c16 / 4) * fatality/100]
POP[between(age, ageL, 69) & qimd == "4", fatality := (100 + fatality.sec.gradient.c16 / 4) * fatality/100]
POP[between(age, ageL, 69) & qimd == "5", fatality := (100 + fatality.sec.gradient.c16 / 2) * fatality/100]
POP[between(age, 70, ageH) & qimd == "1", fatality := (100 - fatality.sec.gradient.c16 / 1) * fatality/100]
POP[between(age, 70, ageH) & qimd == "2", fatality := (100 - fatality.sec.gradient.c16 / 2) * fatality/100]
POP[between(age, 70, ageH) & qimd == "4", fatality := (100 + fatality.sec.gradient.c16 / 2) * fatality/100]
POP[between(age, 70, ageH) & qimd == "5", fatality := (100 + fatality.sec.gradient.c16 / 1) * fatality/100]

#expected number of deaths after gradient and needs to be corrected by Temp/Temp1
Temp1 <- POP[between(age, ageL, ageH), 
             .(after = sum(c16.incidence > 0) * mean(fatality)),
             by = .(agegroup, sex)] #expected number of deaths

Temp[Temp1, corr := before/after, on = c("agegroup", "sex")]
Temp[is.na(corr), corr := 1]
POP[Temp, fatality := fatality * corr, on = c("agegroup", "sex")]
POP[fatality > 1, fatality := 0.99]

# Mortality and remission need to be calculated on the same step
# I NEED TO implement also decreasing probability of death by duration
POP[C16remis, `:=` (remission = remission)]

Temp <- POP[between(age, ageL, ageH), 
            .(before = sum(c16.incidence > 0) * mean(remission)),
            by = .(agegroup, sex)] #expected number of deaths

POP[between(age, ageL, 69) & qimd == "1", remission := (100 + fatality.sec.gradient.c16 / 2) * remission/100]
POP[between(age, ageL, 69) & qimd == "2", remission := (100 + fatality.sec.gradient.c16 / 4) * remission/100]
POP[between(age, ageL, 69) & qimd == "4", remission := (100 - fatality.sec.gradient.c16 / 4) * remission/100]
POP[between(age, ageL, 69) & qimd == "5", remission := (100 - fatality.sec.gradient.c16 / 2) * remission/100]
POP[between(age, 70, ageH) & qimd == "1", remission := (100 + fatality.sec.gradient.c16 / 2) * remission/100]
POP[between(age, 70, ageH) & qimd == "2", remission := (100 + fatality.sec.gradient.c16 / 4) * remission/100]
POP[between(age, 70, ageH) & qimd == "4", remission := (100 - fatality.sec.gradient.c16 / 4) * remission/100]
POP[between(age, 70, ageH) & qimd == "5", remission := (100 - fatality.sec.gradient.c16 / 2) * remission/100]
#expected number of deaths after gradient and needs to be corrected by Temp/Temp1

Temp1 <- POP[between(age, ageL, ageH), 
             .(after = sum(c16.incidence > 0) * mean(remission)),
             by = .(agegroup, sex)] #expected number of deaths

Temp[Temp1, corr := before/after, on = c("agegroup", "sex")]
Temp[is.na(corr), corr := 1]
POP[Temp, remission := remission * corr, on = c("agegroup", "sex")]
POP[remission > 1, remission := 0.99]

POP[c16.incidence > 0, 
    `:=` (v = dice(.N) <= fatality + remission, # T = dead or cured (F = diseased)
          dc = dice(.N) <= fatality / (remission + fatality))] # T = dead (F = cured)

#POP[c16.incidence > 0 & v == T & dc == T, .N] # dead
#POP[c16.incidence > 0 & v == T & dc == F, .N] # cured

POP[, dead := as.logical(v * dc)]
POP[v == T & dead == F, 
    `:=` (c16.remission = paste0(c16.incidence, " - ", i + 2011))]
if ("c16.remission" %!in% names(POP)) set(POP, NULL, "c16.remission", NA_character_)

#cat("Export gastric cancer burden summary...\n\n")
#cat(paste0(Sys.time(), "\n\n"))
if (i == init.year-2011) c16.burden <- vector("list", yearstoproject * 5)

c16.burden[[(2011 - init.year + i) * 5 + 1]] <-
  output.c16(POP, c("qimd", "sex", "agegroup"))

c16.burden[[(2011 - init.year + i) * 5 + 2]] <- 
  output.c16(POP, c("sex", "agegroup"))

c16.burden[[(2011 - init.year + i) * 5 + 3]] <-
  output.c16(POP, c("qimd", "sex"))

c16.burden[[(2011 - init.year + i) * 5 + 4]] <- 
  output.c16(POP, c("sex"))

c16.burden[[(2011 - init.year + i) * 5 + 5]] <- 
  output.c16(POP, c())

if (i == yearstoproject + init.year - 2012) {
  saveRDS(rbindlist(c16.burden, T, T)
          , file = paste0(output.dir(), "c16.burden.rds"))
}

#cat("Export gastric cancer burden individuals...\n\n")
indiv.incid[[which(diseasestoexclude=="C16")]] <- 
  POP[c16.incidence == 2011 + i,
      .(age, sex, qimd, agegroup, eqv5, id, hserial, hpnssec8, sha)
      ][, `:=` (scenario = gsub(".R", "", scenarios.list[[iterations]]),
                mc = haha, year = 2011 + i, cause = "c16")]

indiv.mort[[which(diseasestoexclude=="C16") + 1]] <-
  POP[dead == T, 
      .(age, sex, qimd, agegroup, eqv5, id, hserial, hpnssec8, sha)
      ][,
        `:=` (year = 2011 + i, cause = "c16",
              scenario = gsub(".R", "", scenarios.list[[iterations]]), 
              mc = haha)]
POP[v == T & dead == F, 
    `:=` (c16.incidence = 0)]
POP[,`:=` (v = NULL, dc = NULL)] 

POP = copy(POP[dead == F | is.na(dead)== T,])

rm(Temp, Temp1)

POP[, `:=` (c16.tob.rr = NULL, p0.cardia = NULL, p0.noncardia = NULL,
            c16.salt.rr = NULL, c16.bmi.rr = NULL,
            dead = NULL, fatality = NULL, remission = NULL)] 
setkey(POP, age, sex, qimd)
