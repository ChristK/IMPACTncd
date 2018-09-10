#cmpfile("./chd model.R")
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


cat("Loading CHD (I20-I25) model...\n")
cat(paste0(Sys.time(), "\n\n"))
if (i == init.year - 2011) {
  set(POP, NULL, "chd.incidence",  0L) # Only needs to run the very first time of each simulation
  if ("stroke.incidence" %!in% names(POP)) set(POP, NULL, "stroke.incidence",  0L)
}

POP[age == 0, chd.incidence := 0]
# RR for tobacco from Ezzati M, Henley SJ, Thun MJ, Lopez AD. Role of Smoking in Global and Regional 
# Cardiovascular Mortality. Circulation. 2005 Jul 26;112(4):489–97.
# Table 1 Model B
#cat("smoking RR\n")
set(POP, NULL, "chd.tob.rr",  1)
setkey(POP, age, sex, cigst1.cvdlag)
POP[tobacco.rr.chd, chd.tob.rr := rr]
POP[is.na(chd.tob.rr), chd.tob.rr := 1]
# RR for ex-smokers from Huxley RR, Woodward M. 
# Cigarette smoking as a risk factor for coronary heart disease
# in women compared with men: a systematic review and meta-analysis of prospective cohort studies. 
# The Lancet. 2011 Oct 14;378(9799):1297–305. 
# Appendix webfigure 8
# POP[sex=="1"&qimd=="2", mean(chd.tob.rr), by = age][, plot(age, V1)]
# POP[, sm := predict(loess(chd.tob.rr~age, span=0.40)), by = .(sex, qimd)]
# POP[, rr:= scale(chd.tob.rr, unique(sm)), by=.(age, sex, qimd)]
# POP[sex=="1"&qimd=="2", mean(rr), by = age][, lines(age, V1)]

# RR for ETS He J, Vupputuri S, Allen K, Prerost MR, Hughes J, Whelton PK. Passive Smoking and the Risk of 
# Coronary Heart Disease — A Meta-Analysis of Epidemiologic Studies. New England Journal of Medicine. 1999;340(12):920–6. 
# Table 3. Adjusted RR
set(POP, NULL, "chd.ets.rr",  1)
POP[cigst1 != "4" & expsmok.cvdlag == "1", chd.ets.rr := chd.ets.rr.mc]
POP[age > 69, chd.ets.rr := chd.ets.rr * (1-(age-69)/100)] # decrease risk for elderly
POP[chd.ets.rr < 1, chd.ets.rr := 1]
POP[is.na(chd.ets.rr), chd.ets.rr := 1]

# RR for SBP from Optimal SBP level at 115mmHg and RR(HR) of dying from CHD was taken from "Age-specific relevance of
# usual blood pressure to vascular mortality: 
# a meta-analysis of individual data for one million adults in 61 prospective studies. 
# The Lancet. 2002 Dec 14;360(9349):1903–1913" 
# Figure 5
#cat("sbp RR\n")
set(POP, NULL, "chd.sbp.rr",  1)
setkey(POP, age, sex)
POP[sbp.rr.chd, chd.sbp.rr := bound(rr^((115 - omsysval.cvdlag)/20), 1, Inf)]
POP[is.na(chd.sbp.rr), chd.sbp.rr := 1]

# RR for Chol from "Blood cholesterol and 
# vascular mortality by age, sex, and blood pressure: a meta-analysis of individual data from 61 prospective studies 
# with 55.000 vascular deaths. The Lancet. 2007 Dec 7;370(9602):1829–39. 
# Appendix Webtable 6  fully adjusted
#cat("chol RR\n")
set(POP, NULL, "chd.chol.rr",  1)
setkey(POP, age)
POP[chol.rr.chd, chd.chol.rr := bound(rr^(3.8 - cholval.cvdlag), 1, Inf)]
POP[is.na(chd.chol.rr), chd.chol.rr := 1]

# RR for BMI from "The Emerging Risk Factors Collaboration.
# Separate and combined associations of body-mass index and abdominal adiposity with cardiovascular disease:
# collaborative analysis of 58 prospective studies.
# The Lancet 2011;377:1085–95. doi:10.1016/S0140-6736(11)60105-0
# Table 1 (Adjusted for age, sex, smoking status, systolic blood pressure, history of diabetes, and total and HDL cholesterol)
# and figure 2 for age specific gradient
#cat("bmi RR\n")
set(POP, NULL, "chd.bmi.rr",  1)
POP[chd.bmi.rr.mc, chd.bmi.rr := bound(rr^((bmival.cvdlag - 20)/4.56), 1, Inf)]
POP[is.na(chd.bmi.rr), chd.bmi.rr := 1]

# RR for diab from The Emerging Risk Factors Collaboration. Diabetes mellitus, fasting blood glucose concentration, 
# and risk of vascular disease: a collaborative 
# meta-analysis of 102 prospective studies. The Lancet 2010;375:2215–22. doi:10.1016/S0140-6736(10)60484-9
# figure 2 (HRs were adjusted for age, smoking status, body-mass index, and  systolic blood pressure)
#cat("diab RR\n")
set(POP, NULL, "chd.diab.rr",  1)
setkey(POP, age, diabtotr.cvdlag)
POP[chd.diab.rr.mc, chd.diab.rr := rr]
POP[is.na(chd.diab.rr), chd.diab.rr := 1]

# RR for F&V from From Dauchet L, Amouyel P, Hercberg S, Dallongeville J. Fruit and Vegetable Consumption and Risk of Coronary Heart Disease: 
# A Meta-Analysis of Cohort Studies. J Nutr. 2006 Oct 1;136(10):2588–93. 
# To avoid negative PAF an optimal level of F&V has to be set arbitrarily. I set it to 10 
# when convert porftvg from categorical to numeric I create bias. eg 1=less than 1 portion
#cat("fv RR\n")
set(POP, NULL, "chd.fv.rr",  1)
POP[porftvg.cvdlag < 8L, chd.fv.rr := bound(chd.fv.rr.mc^(porftvg.cvdlag - 7L), 1, Inf)] # x^0 is 1 anyway
POP[age > 69, chd.fv.rr := chd.fv.rr * (1-(age-69)/100)] # decrease risk for elderly
POP[chd.fv.rr < 1, chd.fv.rr := 1]
POP[is.na(chd.fv.rr), chd.fv.rr := 1]
POP[is.na(chd.fv.rr), chd.fv.rr := 1]

# RR for PA 1. WHO | Comparative Quantification of Health Risks [Internet]. 
# WHO [cited 2014 Jan 30];Available from: http://www.who.int/publications/cra/en/
# Table 10.19 (with adjustment for measurement error)
#cat("pa RR\n")
set(POP, NULL, "chd.pa.rr",  1)
setkey(POP, age, a30to06m.cvdlag)
POP[pa.rr.chd, chd.pa.rr := rr]
POP[is.na(chd.pa.rr), chd.pa.rr := 1]

# Estimate PAF ------------------------------------------------------------
if (i == init.year - 2011) {
  #cat("Estimating CHD PAF...\n")
  chdpaf <- 
    POP[between(age, ageL, ageH), 
        .(paf = 1 - 1 / (sum(chd.tob.rr * chd.ets.rr *
                               chd.sbp.rr * chd.chol.rr * 
                               chd.bmi.rr * chd.diab.rr * chd.fv.rr *
                               chd.pa.rr) / .N)), 
        by = .(age, sex)
        ]
  setkey(chdpaf, age, sex)
  #chdpaf[, paf := predict(loess(paf~age, span=0.20)), by = .(sex)]
  setkey(CHDincid, age, sex)
  CHDincid[chdpaf, p0 := incidence * (1 - paf)]
  CHDincid[is.na(p0), p0 := incidence]
}

setkey(POP, age, sex)
POP[CHDincid, p0 := p0]

# Estimate prevalence -----------------------------------------------------
if (i == init.year - 2011) {
  #cat(paste0("Estimating CHD prevalence in ", init.year, " ...\n\n"))
  age.structure <- setkey(POP[age <= ageH, .N, by = .(age, sex)], age, sex)
  age.structure[CHDpreval[age <= ageH], Nprev := rbinom(.N, N, prevalence)]
  age.structure[CHDincid[age <= ageH],  Nprev := Nprev - rbinom(.N, N - Nprev, incidence)]
  age.structure[Nprev < 0, Nprev := 0]
  #age.structure[CHDsurv  , Nprev := round(Nprev *(1-fatality * 1.03))]
  setnames(age.structure, "N", "population")
  
  POP <- merge(POP,
               deaths.causes.secgrad[cause == "Ischaemic heart diseases", 
                                     .(agegroup, sex, qimd, sec.grad.adj)],
               by = c("agegroup", "sex", "qimd"), all.x = T)
  POP[is.na(sec.grad.adj), sec.grad.adj := 1]
  
  id.chd <- POP[age <=  ageH, 
                sample(id, age.structure[sex == .BY[[2]] & age == .BY[[1]],
                                            Nprev], 
                         prob = chd.tob.rr * chd.ets.rr * 
                           chd.sbp.rr * chd.chol.rr * chd.bmi.rr * 
                           chd.diab.rr * chd.fv.rr * chd.pa.rr * 
                           sec.grad.adj, 
                         replace = F), 
                by = .(age, sex)][, V1]
  
  POP[id %in% id.chd, chd.incidence := init.year - 1] # and then we assign
  # these ids to the population
  POP[, sec.grad.adj := NULL]
  rm(id.chd)
}

#cat("Estimating CHD incidence...\n\n")
POP[between(age, ageL, ageH) & 
      chd.incidence == 0L &
      dice(.N) < p0 * chd.tob.rr * chd.ets.rr *
      chd.sbp.rr * chd.chol.rr * 
      chd.bmi.rr * chd.diab.rr * chd.fv.rr *
      chd.pa.rr,
    chd.incidence := 2011 + i]
#POP[,summary(as.factor(v))]

#setkey(Out.Inc.CHD, agegroup, sex)

# Estimate CHD mortality 
#cat("Estimating CHD mortality...\n\n")

# Apply assumptions about improvement of fatality by year
# ie 3% improvemnt by year (as supportet by BHF)

if (i > init.year - 2011) {
  CHDsurv[, fatality := fatality * (100 - fatality.annual.improvement.chd)/100]
  fatality.annual.improvement.chd <- fatality.annual.improvement.chd * 0.98
}
setkey(POP, age, sex)
POP[CHDsurv, fatality := fatality]

Temp <- POP[between(age, ageL, ageH), 
            .(before = sum(chd.incidence > 0) * mean(fatality)),
            by = .(agegroup, sex)] #expected number of deaths

# Fatality SEC gradient and healthcare improvement (supported by table 1.13 BHF2012)
POP[between(age, ageL, 69) & qimd == "1", fatality := (100 - fatality.sec.gradient.chd/2) * fatality/100]
POP[between(age, ageL, 69) & qimd == "2", fatality := (100 - fatality.sec.gradient.chd/4) * fatality/100]
POP[between(age, ageL, 69) & qimd == "4", fatality := (100 + fatality.sec.gradient.chd/4) * fatality/100]
POP[between(age, ageL, 69) & qimd == "5", fatality := (100 + fatality.sec.gradient.chd/2) * fatality/100]
POP[between(age, 70, ageH) & qimd == "1", fatality := (100 - fatality.sec.gradient.chd/4) * fatality/100]
POP[between(age, 70, ageH) & qimd == "2", fatality := (100 - fatality.sec.gradient.chd/8) * fatality/100]
POP[between(age, 70, ageH) & qimd == "4", fatality := (100 + fatality.sec.gradient.chd/8) * fatality/100]
POP[between(age, 70, ageH) & qimd == "5", fatality := (100 + fatality.sec.gradient.chd/4) * fatality/100]

#expected number of deaths after gradient and needs to be corrected by Temp/Temp1
Temp1 <- POP[between(age, ageL, ageH), 
             .(after = sum(chd.incidence > 0) * mean(fatality)),
             by = .(agegroup, sex)] #expected number of deaths

# POP[, fatality := fatality * Temp / Temp1]
Temp[Temp1, corr := before/after, on = c("agegroup", "sex")]
Temp[is.na(corr), corr := 1]
POP[Temp, fatality := fatality * corr, on = c("agegroup", "sex")]

# 30 day fatality from bhf2012 (see MI 30 days fatality.xlsx for the adjustments)
setkey(POP, agegroup, sex)
POP[fatality30chd, fatality30 := fatality30]
POP[between(age, ageL, ageH),
    fatality2 := (.SD[chd.incidence > 0, .N * mean(fatality)] - .SD[chd.incidence == 2011 + i, .N * mean(fatality30)]) /
      (.SD[chd.incidence > 0, .N] - .SD[chd.incidence == 2011 + i, .N * mean(fatality30)]),
    by = group] # mean is used instead of unique to maintain groups with no events in the result

POP[ fatality2 < 0, fatality2 := 0]

POP[between(age, ageL, ageH) & is.na(fatality2), fatality2 := fatality]

POP[chd.incidence > 0, dead := F]
POP[chd.incidence == 2011 + i, dead:= dice(.N) <= fatality30] # 30 days mortality T = dead, F = alive 

POP[chd.incidence > 0 & dead == F, dead:= dice(.N) <= fatality2] # T = dead, F = alive 

#cat("Export CHD burden summary...\n\n")
#cat(paste0(Sys.time(), "\n\n"))
if (i == init.year-2011) chd.burden <- vector("list", yearstoproject * 5)

chd.burden[[(2011 - init.year + i) * 5 + 1]] <-
  output.chd(POP, c("qimd", "sex", "agegroup"))

chd.burden[[(2011 - init.year + i) * 5 + 2]] <- 
  output.chd(POP, c("sex", "agegroup"))

chd.burden[[(2011 - init.year + i) * 5 + 3]] <- 
  output.chd(POP, c("qimd", "sex"))

chd.burden[[(2011 - init.year + i) * 5 + 4]] <- 
  output.chd(POP, c("sex"))

chd.burden[[(2011 - init.year + i) * 5 + 5]] <- 
  output.chd(POP, c())

if (i == yearstoproject + init.year - 2012) {
  saveRDS(rbindlist(chd.burden, T, T), file = paste0(output.dir(), "chd.burden.rds"))
}

#cat("Export CHD burden individuals...\n\n")
indiv.incid[[which(diseasestoexclude=="CHD")]] <- 
  POP[chd.incidence == 2011 + i,
      .(age, sex, qimd, agegroup, eqv5, id, hserial, hpnssec8, sha
      )][ , `:=` (
        scenario = gsub(".R", "", scenarios.list[[iterations]]),
        mc = haha, year = 2011 + i, cause = "chd")]

# output <- vector("list", 2)
# 
# if (exists("chd.ind.preval.rds")) output[[1]] <- chd.ind.preval.rds
# 
# output[[2]] <- POP[chd.incidence > 0, .(age, sex, qimd, agegroup, eqv5, id, hserial, hpnssec8, sha, chd.incidence)][,`:=` (scenario = gsub(".R", "", scenarios.list[[iterations]]), mc = haha)]
# 
# chd.ind.preval.rds <- rbindlist(output, fill = T)
# 
# if (i == yearstoproject + init.year - 2012) {
#   saveRDS(chd.ind.preval.rds, file = paste0(output.dir(), "chd.ind.preval.rds"))
# }

indiv.mort[[which(diseasestoexclude=="CHD") + 1]] <-
  POP[dead == T, .(age, sex, qimd, agegroup, eqv5, id, hserial, hpnssec8, sha)
      ][,`:=` (year = 2011 + i, cause = "chd",
               scenario = gsub(".R", "", scenarios.list[[iterations]]), 
               mc = haha)]

POP = copy(POP[dead == F | is.na(dead)== T,])

rm(Temp, Temp1)

POP[, `:=` (chd.tob.rr = NULL, p0 = NULL,
            chd.ets.rr = NULL, chd.sbp.rr = NULL, fatality2 = NULL,
            chd.chol.rr = NULL, chd.bmi.rr = NULL, chd.diab.rr = NULL, chd.pa.rr = NULL,
            chd.fv.rr = NULL, dead = NULL, fatality = NULL, fatality30 = NULL)] 
