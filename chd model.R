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

#cmpfile("./chd model.R")
cat("Loading CHD (I20-I25) model...\n")
cat(paste0(Sys.time(), "\n\n"))
if (i == init.year - 2011) set(POP, NULL, "chd.incidence",  0L) # Only needs to run the very first time of each simulation
POP[age == 0, chd.incidence := 0]

# RR for tobacco from Ezzati M, Henley SJ, Thun MJ, Lopez AD. Role of Smoking in Global and Regional 
# Cardiovascular Mortality. Circulation. 2005 Jul 26;112(4):489–97.
# Table 1 Model B
cat("smoking RR\n")
set(POP, NULL, "chd.tob.rr",  1)
setkey(POP, agegroup, sex, cigst1.cvdlag)
POP[tobacco.rr.chd, chd.tob.rr := rr]

# RR for ex-smokers from Huxley RR, Woodward M. 
# Cigarette smoking as a risk factor for coronary heart disease
# in women compared with men: a systematic review and meta-analysis of prospective cohort studies. 
# The Lancet. 2011 Oct 14;378(9799):1297–305. 
# Appendix webfigure 8


# RR for ETS He J, Vupputuri S, Allen K, Prerost MR, Hughes J, Whelton PK. Passive Smoking and the Risk of 
# Coronary Heart Disease — A Meta-Analysis of Epidemiologic Studies. New England Journal of Medicine. 1999;340(12):920–6. 
# Table 3. Adjusted RR
set(POP, NULL, "chd.ets.rr",  1)
POP[cigst1 %in% c("1", "2", "3") & expsmokCat == "1", chd.ets.rr := chd.ets.rr.mc]

# RR for SBP from Optimal SBP level at 115mmHg and RR(HR) of dying from CHD was taken from "Age-specific relevance of
# usual blood pressure to vascular mortality: 
# a meta-analysis of individual data for one million adults in 61 prospective studies. 
# The Lancet. 2002 Dec 14;360(9349):1903–1913" 
# Figure 5
cat("sbp RR\n")
set(POP, NULL, "chd.sbp.rr",  1)
setkey(POP, agegroup, sex)
POP[sbp.rr.chd, chd.sbp.rr := rr^((115 - omsysval.cvdlag)/20)]
POP[chd.sbp.rr<1, chd.sbp.rr := 1]

# RR for Chol from "Blood cholesterol and 
# vascular mortality by age, sex, and blood pressure: a meta-analysis of individual data from 61 prospective studies 
# with 55.000 vascular deaths. The Lancet. 2007 Dec 7;370(9602):1829–39. 
# Appendix Webtable 6  fully adjusted
cat("chol RR\n")
set(POP, NULL, "chd.chol.rr",  1)
setkey(POP, agegroup)
POP[chol.rr.chd, chd.chol.rr := rr^(3.8 - cholval.cvdlag)]
POP[chd.chol.rr < 1, chd.chol.rr := 1]

# RR for BMI from "The Emerging Risk Factors Collaboration.
# Separate and combined associations of body-mass index and abdominal adiposity with cardiovascular disease:
# collaborative analysis of 58 prospective studies.
# The Lancet 2011;377:1085–95. doi:10.1016/S0140-6736(11)60105-0
# Table 1 (Adjusted for age, sex, smoking status, systolic blood pressure, history of diabetes, and total and HDL cholesterol)
# and figure 2 for age specific gradient
cat("bmi RR\n")
set(POP, NULL, "chd.bmi.rr",  1)
POP[chd.bmi.rr.mc, chd.bmi.rr := rr^((bmival.cvdlag - 20)/4.56)]
POP[chd.bmi.rr < 1, chd.bmi.rr := 1]

# RR for diab from The Emerging Risk Factors Collaboration. Diabetes mellitus, fasting blood glucose concentration, 
# and risk of vascular disease: a collaborative 
# meta-analysis of 102 prospective studies. The Lancet 2010;375:2215–22. doi:10.1016/S0140-6736(10)60484-9
# figure 2 (HRs were adjusted for age, smoking status, body-mass index, and  systolic blood pressure)
cat("diab RR\n")
set(POP, NULL, "chd.diab.rr",  1)
setkey(POP, agegroup, diabtotr.cvdlag)
POP[chd.diab.rr.mc, chd.diab.rr := rr]

# RR for F&V from From Dauchet L, Amouyel P, Hercberg S, Dallongeville J. Fruit and Vegetable Consumption and Risk of Coronary Heart Disease: 
# A Meta-Analysis of Cohort Studies. J Nutr. 2006 Oct 1;136(10):2588–93. 
# To avoid negative PAF an optimal level of F&V has to be set arbitrarily. I set it to 10 
# when convert porftvg from categorical to numeric I create bias. eg 1=less than 1 portion
cat("fv RR\n")
set(POP, NULL, "chd.fv.rr",  1)
POP[porftvg.cvdlag > 0, chd.fv.rr := chd.fv.rr.mc^(porftvg.cvdlag)] # x^0 is 1 anyway

# RR for PA 1. WHO | Comparative Quantification of Health Risks [Internet]. 
# WHO [cited 2014 Jan 30];Available from: http://www.who.int/publications/cra/en/
# Table 10.19 (with adjustment for measurement error)
cat("pa RR\n")
set(POP, NULL, "chd.pa.rr",  1)
setkey(POP, agegroup, a30to06m.cvdlag)
POP[pa.rr.chd, chd.pa.rr := rr]

# Estimate PAF ------------------------------------------------------------
cat("Estimating CHD PAF...\n")
if (i == init.year - 2011) {
  chdtobpaf <- 
    POP[between(age, ageL, ageH), 
        .(tobpaf = 
            (sum(chd.tob.rr - 1, na.rm = T) / .N) / 
            ((sum(chd.tob.rr - 1, na.rm = T) / .N) + 1)
        ), 
        by = .(agegroup, sex)
        ]
  setkey(chdtobpaf, agegroup, sex)
  
  chdetspaf <- 
    POP[between(age, ageL, ageH), 
        .(etspaf = 
            (sum(chd.ets.rr - 1, na.rm = T) / .N) / 
            ((sum(chd.ets.rr - 1, na.rm = T) / .N) + 1)
        ), 
        by = .(agegroup, sex)
        ]
  setkey(chdetspaf, agegroup, sex)
  
  chdsbppaf <- 
    POP[between(age, ageL, ageH), 
        .(sbppaf = 
            (sum(chd.sbp.rr - 1, na.rm = T) / .N) / 
            ((sum(chd.sbp.rr - 1, na.rm = T) / .N) + 1)
        ), 
        by = .(agegroup, sex)
        ]
  setkey(chdsbppaf, agegroup, sex)
  
  chdcholpaf <- 
    POP[between(age, ageL, ageH), 
        .(cholpaf = 
            (sum(chd.chol.rr - 1, na.rm = T) / .N) / 
            ((sum(chd.chol.rr - 1, na.rm = T) / .N) + 1)
        ), 
        by = .(agegroup, sex)
        ]
  setkey(chdcholpaf, agegroup, sex)
  
  chdbmipaf <- 
    POP[between(age, ageL, ageH), 
        .(bmipaf = 
            (sum(chd.bmi.rr - 1, na.rm = T) / .N) / 
            ((sum(chd.bmi.rr - 1, na.rm = T) / .N) + 1)
        ), 
        by = .(agegroup, sex)
        ]
  setkey(chdbmipaf, agegroup, sex)
  
  chddiabpaf <- 
    POP[between(age, ageL, ageH), 
        .(diabpaf = 
            (sum(chd.diab.rr - 1, na.rm = T) / .N) / 
            ((sum(chd.diab.rr - 1, na.rm = T) /.N) + 1)
        ), 
        by = .(agegroup, sex)
        ]
  setkey(chddiabpaf, agegroup, sex)
  
  chdfvpaf <- 
    POP[between(age, ageL, ageH), 
        .(fvpaf = 
            (sum((chd.fv.rr^-1) - 1, na.rm = T) / .N) / 
            ((sum((chd.fv.rr^-1) - 1, na.rm = T) /.N ) + 1)
        ), 
        by = .(agegroup, sex)
        ]
  setkey(chdfvpaf, agegroup, sex)
  
  chdpapaf <- 
    POP[between(age, ageL, ageH), 
        .(papaf = 
            (sum(chd.pa.rr - 1, na.rm = T) / .N) / 
            ((sum(chd.pa.rr - 1, na.rm = T) /.N ) + 1)
        ), 
        by = .(agegroup, sex)
        ]
  setkey(chdpapaf, agegroup, sex)
  
  CHDincid[, agegroup := agegroup.fn(age)]
  setkey(CHDincid, agegroup, sex)
  CHDincid[chdbmipaf[chdcholpaf[chddiabpaf[chdetspaf[chdfvpaf[chdsbppaf[chdtobpaf[chdpapaf]]]]]]], 
           p0 := incidence * (1 - bmipaf) *
             (1 - cholpaf) * (1 - diabpaf) * 
             (1 - etspaf)  * (1 - fvpaf) * 
             (1 - sbppaf)  * (1 - tobpaf) *
             (1 - papaf)]
  CHDincid[is.na(p0), p0 := incidence]
  CHDincid[, agegroup := NULL]
  rm(chdbmipaf,chdcholpaf,chddiabpaf,chdetspaf,chdfvpaf,chdsbppaf,chdtobpaf,chdpapaf)
  setkey(CHDincid, NULL)
}

setkey(POP, age, sex)
POP[CHDincid, p0 := p0]


# Estimate prevalence -----------------------------------------------------
if (i == init.year - 2011) {
  cat(paste0("Estimating CHD prevalence in ", init.year, " ...\n\n"))
  age.structure <- setkey(POP[age <= ageH, .N, by = .(age, sex)], age, sex)
  age.structure[CHDpreval[age <= ageH], Nprev := rbinom(.N, N, prevalence)]
  age.structure[CHDincid[age <= ageH],  Nprev := Nprev - rbinom(.N, N - Nprev, incidence)]
  age.structure[Nprev < 0, Nprev := 0]
  #age.structure[CHDsurv  , Nprev := round(Nprev *(1-fatality * 1.03))]
  setnames(age.structure, "N", "population")
  
  POP <- merge(POP,
        deaths.causes.secgrad[cause == "Ischaemic heart diseases", .(agegroup, sex, qimd, sec.grad.adj)],
        by = c("agegroup", "sex", "qimd"), all.x = T)
  POP[is.na(sec.grad.adj), sec.grad.adj := 1]
  
  id.chd <- POP[age <=  ageH, 
                sample_n(.SD, age.structure[sex == .BY[[2]] & age == .BY[[1]],
                                            Nprev], 
                         weight = chd.tob.rr * chd.ets.rr * 
                           chd.sbp.rr * chd.chol.rr * chd.bmi.rr * 
                           chd.diab.rr * chd.fv.rr * chd.pa.rr * 
                           sec.grad.adj, 
                         replace = F), 
                by = .(age, sex)][, id]
  
  POP[id %in% id.chd, chd.incidence := init.year - 1] # and then we assign
  # these ids to the population
  POP[, sec.grad.adj := NULL]
  rm(id.chd)
}

# correction factor NEED TO make it work only for i==0
if (alignment == T) {
  if (i == init.year-2011) {
    corr.factor.chd <- merge(
      POP[between(age, ageL, ageH) & chd.incidence == 0,
          mean(p0 * chd.tob.rr * chd.ets.rr * 
                 chd.sbp.rr * chd.chol.rr * 
                 chd.bmi.rr * chd.diab.rr * 
                 chd.fv.rr * chd.pa.rr),
          by = .(age, sex)],
      CHDincid,
      by = c("age", "sex"), all.x = T)
    corr.factor.chd[, b := incidence/V1]
    corr.factor.chd[, `:=` (p0 = NULL, incidence = NULL, V1 = NULL)]
    POP <- merge(POP, corr.factor.chd, by = c("age", "sex"), all.x = T)
  } else {
    POP <- merge(POP, corr.factor.chd, by = c("age", "sex"), all.x = T)
  }
} else set(POP, NULL, "b", 1)

# P= p0 * chd.tob.rr * chd.ets.rr * chd.sbp.rr * chd.chol.rr * chd.bmi.rr * chd.diab.rr * chd.fv.rr
cat("Estimating CHD incidence...\n\n")
if (alignment == T) cat("Alignment will be performed\n\n")
POP[between(age, ageL, ageH) & 
      chd.incidence == 0L &
      dice(.N) < p0 * chd.tob.rr * chd.ets.rr *
      chd.sbp.rr * chd.chol.rr * 
      chd.bmi.rr * chd.diab.rr * chd.fv.rr *
      chd.pa.rr * b,
    chd.incidence := 2011 + i] # b is the correction factor
#POP[,summary(as.factor(v))]

#setkey(Out.Inc.CHD, agegroup, sex)

# Estimate CHD mortality 
cat("Estimating CHD mortality...\n\n")

# Apply assumptions about improvement of fatality by year
# ie 3% improvemnt by year (as supportet by BHF)

if (i > init.year - 2011) CHDsurv[, fatality := fatality * (100 - fatality.annual.improvement.chd)/100]
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

cat("Export CHD burden summary...\n\n")
cat(paste0(Sys.time(), "\n\n"))
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

cat("Export CHD burden individuals...\n\n")
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

indiv.mort[[which(diseasestoexclude=="CHD") + 1]] <- POP[dead == T, .(age, sex, qimd, agegroup, eqv5, id, hserial, hpnssec8, sha)
                                                         ][,`:=` (year = 2011 + i, cause = "chd", scenario = gsub(".R", "", scenarios.list[[iterations]]), 
                                                                  mc = haha)]

POP = copy(POP[dead == F | is.na(dead)== T,])

rm(Temp, Temp1)

POP[, `:=` (chd.tob.rr = NULL, p0 = NULL,
            chd.ets.rr = NULL, chd.sbp.rr = NULL, fatality2 = NULL, b = NULL ,
            chd.chol.rr = NULL, chd.bmi.rr = NULL, chd.diab.rr = NULL, chd.pa.rr = NULL,
            chd.fv.rr = NULL, dead = NULL, fatality = NULL, fatality30 = NULL)] 
