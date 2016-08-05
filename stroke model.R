#cmpfile("./stroke model.R")
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


cat("Loading stroke (I60-I69) model...\n")
cat(paste0(Sys.time(), "\n\n"))
if (i == init.year-2011) {
  set(POP, NULL, "stroke.incidence",  0L) # Only needs to run the very first time of each simulation
  if ("chd.incidence" %!in% names(POP)) {
    set(POP, NULL, "chd.incidence",  0L)
  }
}
POP[age==0, stroke.incidence := 0]

# RR for tobacco from Ezzati M, Henley SJ, Thun MJ, Lopez AD. Role of Smoking in Global and Regional 
# Cardiovascular Mortality. Circulation. 2005 Jul 26;112(4):489–97.
# Table 1 Model B
#cat("smoking RR\n")
set(POP, NULL, "stroke.tob.rr",  1)
setkey(POP, age, sex, cigst1.cvdlag)
POP[tobacco.rr.stroke, stroke.tob.rr := rr]
POP[is.na(stroke.tob.rr) | stroke.tob.rr < 1, stroke.tob.rr := 1]
#ex-smokers
# Cigarette smoking as a risk factor for stroke: The Framingham study
# "Stroke risk decreased significantly by two years and was at the
# level of nonsmokers by five years after cessation of cigarette smoking"

# Calculate PAF of ETS for stroke
# RR from Oono IP, Mackay DF, Pell JP. Meta-analysis of the association between secondhand smoke exposure and stroke. 
# J Public Health 2011;33:496–502. doi:10.1093/pubmed/fdr025
set(POP, NULL, "stroke.ets.rr",  1)
POP[cigst1 %!in% c("4") & expsmok.cvdlag == "1", stroke.ets.rr := stroke.ets.rr.mc]
POP[age > 69, stroke.ets.rr := stroke.ets.rr * (1-(age-69)/100)] # decrease risk for elderly
POP[is.na(stroke.ets.rr) | stroke.ets.rr < 1, stroke.ets.rr := 1]

# Calculate RR for stroke. Optimal SBP level at 115mmHg and RR(HR) of dying from
# stroke was taken from "Age-specific relevance of usual blood pressure to 
# vascular mortality: a meta-analysis of individual data for one million adults in 61 prospective studies. 
# The Lancet. 2002 Dec 14;360(9349):1903–1913" 
# Figure 3
#cat("sbp RR\n")
set(POP, NULL, "stroke.sbp.rr",  1)
setkey(POP, age, sex)
POP[sbp.rr.stroke, stroke.sbp.rr := bound(rr^((115 - omsysval.cvdlag)/20), 1, 20 )]
POP[is.na(stroke.sbp.rr), stroke.sbp.rr := 1]

# Calculate RR for stroke. Optimal chol level at 3.8 mmol/L and RR(HR) of 
# dying from stroke was taken from "Blood cholesterol and 
# vascular mortality by age, sex, and blood pressure: a meta-analysis of 
# individual data from 61 prospective studies 
# with 55.000 vascular deaths. The Lancet. 2007;370:1829–39. 
# Figure 4 (for total stroke. I used only significant HR's).
#cat("chol RR\n")
set(POP, NULL, "stroke.chol.rr",  1)
setkey(POP, age)
POP[chol.rr.stroke, stroke.chol.rr := bound(rr^(3.8 - cholval.cvdlag), 1, 20)]
POP[is.na(stroke.chol.rr), stroke.chol.rr := 1]

# RR for BMI from "The Emerging Risk Factors Collaboration.
# Separate and combined associations of body-mass index and abdominal adiposity
# with cardiovascular disease: collaborative analysis of 58 prospective studies.
# The Lancet 2011;377:1085–95. doi:10.1016/S0140-6736(11)60105-0
# Table 1 (Adjusted for age, sex, smoking status, systolic blood pressure,
# history of diabetes, and total and HDL cholesterol)
# BMI not significant for ischaemic stroke but other obesity metrics are. 
#!! NEED TO decide if I want to use it
#cat("bmi RR\n")
set(POP, NULL, "stroke.bmi.rr",  1)
POP[stroke.bmi.rr.mc, stroke.bmi.rr := bound(rr^((bmival.cvdlag - 20) / 4.56), 1, 20)]
POP[is.na(stroke.bmi.rr), stroke.bmi.rr := 1]
# RR for diabetes from The Emerging Risk Factors Collaboration. 
# Diabetes mellitus, fasting blood glucose concentration, 
# and risk of vascular disease: a collaborative 
# meta-analysis of 102 prospective studies. The Lancet 2010;375:2215–22
# figure 2 (HRs were adjusted for age, smoking status, body-mass index, 
# and  systolic blood pressure)
#cat("diab RR\n")
set(POP, NULL, "stroke.diab.rr",  1)
setkey(POP, age, diabtotr.cvdlag)
POP[stroke.diab.rr.mc, stroke.diab.rr := rr]
POP[is.na(stroke.diab.rr) | stroke.diab.rr < 1, stroke.diab.rr := 1]
# Calculate RR for stroke. From Dauchet L, Amouyel P, Dallongeville J. 
# Fruit and vegetable consumption and risk of stroke A meta-analysis of cohort 
# studies. Neurology. 2005;65:1193–7. 
# To avoid negative PAF an optimal level of F&V has to be set arbitrarily. I set it to 8 
#cat("fv RR\n")
set(POP, NULL, "stroke.fv.rr",  1)
POP[porftvg.cvdlag < 8L, stroke.fv.rr := stroke.fv.rr.mc^(porftvg.cvdlag - 7L)] 
POP[age > 69, stroke.fv.rr := stroke.fv.rr * (1-(age-69)/100)] # decrease risk for elderly
POP[is.na(stroke.fv.rr) | stroke.fv.rr < 1, stroke.fv.rr := 1]

# RR for PA 1. WHO | Comparative Quantification of Health Risks [Internet]. 
# WHO [cited 2014 Jan 30];Available from: http://www.who.int/publications/cra/en/
# Table 10.20 (with adjustment for measurement error)
#cat("pa RR\n")
set(POP, NULL, "stroke.pa.rr",  1)
setkey(POP, age, a30to06m.cvdlag)
POP[pa.rr.stroke, stroke.pa.rr := rr]
POP[is.na(stroke.pa.rr), stroke.pa.rr := 1]

# Estimate prevalence -----------------------------------------------------
if (i == init.year - 2011) {
  #cat(paste0("Estimating stroke prevalence in ", init.year, " ...\n\n"))
  age.structure <- setkey(POP[age <= ageH, .N, by = .(age, sex)], age, sex)
  age.structure[strokepreval[age <= ageH], Nprev := rbinom(.N, N, prevalence)]
  age.structure[strokeincid[age <= ageH],  Nprev := Nprev - rbinom(.N, N, incidence)]
  age.structure[Nprev < 0, Nprev := 0]
  
  #age.structure[strokesurv, Nprev := round(Nprev * (1 - fatality * 1.03))]
  setnames(age.structure, "N", "population")
  
  POP <- merge(POP,
               deaths.causes.secgrad[cause == "Cerebrovascular diseases", .(agegroup, sex, qimd, sec.grad.adj)],
               by = c("agegroup", "sex", "qimd"), all.x = T)
  POP[is.na(sec.grad.adj), sec.grad.adj := 1]
  
  id.stroke <- POP[age <= ageH, 
                   sample(id, age.structure[sex == .BY[[2]] & age == .BY[[1]], Nprev], 
                          prob = stroke.tob.rr * stroke.ets.rr * stroke.sbp.rr * 
                            stroke.chol.rr * stroke.bmi.rr * stroke.diab.rr *
                            stroke.fv.rr * stroke.pa.rr * 
                            sec.grad.adj, 
                          replace = F), 
                   by = .(age, sex)][, V1]
  POP[id %in% id.stroke, stroke.incidence := init.year - 1] # and then we assign
  # these ids to the population
  POP[, sec.grad.adj := NULL]
  rm(id.stroke)
}

# Estimate PAF ------------------------------------------------------------
if (i == init.year - 2011) {
  #cat("Estimating stroke PAF...\n")
  strokepaf <- 
    POP[between(age, ageL, ageH) & stroke.incidence == 0, 
        .(paf = 1 - 1 / (sum(stroke.tob.rr * stroke.ets.rr *
                               stroke.sbp.rr * stroke.chol.rr * 
                               stroke.bmi.rr * stroke.diab.rr * 
                               stroke.fv.rr * stroke.pa.rr) / .N)), 
        by = .(age, sex)
        ]
  setkey(strokepaf, age, sex)
  #strokepaf[, paf := predict(loess(paf~age, span=0.20)), by = .(sex)]
  setkey(strokeincid, age, sex)
  strokeincid[strokepaf, p0 := incidence * (1 - paf)]
  strokeincid[is.na(p0), p0 := incidence]
}

setkey(POP, age, sex)
POP[strokeincid, p0 := p0]



#cat("Estimating stroke incidence...\n\n")
POP[between(age, ageL, ageH) & 
      stroke.incidence == 0 & 
      dice(.N) < p0 * stroke.tob.rr * stroke.ets.rr *
      stroke.sbp.rr * stroke.chol.rr * 
      stroke.bmi.rr * stroke.diab.rr * 
      stroke.fv.rr * stroke.pa.rr, 
    stroke.incidence := 2011 + i]
#setkey(Out.Inc.stroke, agegroup, sex)

# Estimate stroke mortality 
#cat("Estimating stroke mortality...\n\n")

# Apply assumptions about improvement of fatality by year
# ie 3% improvemnt by year (as supportet by BHF)

if (i > init.year - 2011) {
  strokesurv[, fatality := fatality * (100 - fatality.annual.improvement.stroke)/100]
  fatality.annual.improvement.stroke <- fatality.annual.improvement.stroke * 0.98
}
setkey(POP, age, sex)
POP[strokesurv, fatality := fatality]

Temp <- POP[between(age, ageL, ageH), 
            .(before = sum(stroke.incidence > 0) * mean(fatality)),
            by = .(agegroup, sex)] #expected number of deaths


# Fatality SEC gradient and healthcare improvement (supported by table 1.13 BHF2012)
POP[between(age, ageL, 69) & qimd == "1", fatality := (100 - fatality.sec.gradient.stroke / 2) * fatality/100]
POP[between(age, ageL, 69) & qimd == "2", fatality := (100 - fatality.sec.gradient.stroke / 4) * fatality/100]
POP[between(age, ageL, 69) & qimd == "4", fatality := (100 + fatality.sec.gradient.stroke / 4) * fatality/100]
POP[between(age, ageL, 69) & qimd == "5", fatality := (100 + fatality.sec.gradient.stroke / 2) * fatality/100]
POP[between(age, 70, ageH) & qimd == "1", fatality := (100 - fatality.sec.gradient.stroke / 5) * fatality/100]
POP[between(age, 70, ageH) & qimd == "2", fatality := (100 - fatality.sec.gradient.stroke / 10) * fatality/100]
POP[between(age, 70, ageH) & qimd == "4", fatality := (100 + fatality.sec.gradient.stroke / 10) * fatality/100]
POP[between(age, 70, ageH) & qimd == "5", fatality := (100 + fatality.sec.gradient.stroke / 5) * fatality/100]
#expected number of deaths after gradient and needs to be corrected by Temp/Temp1
# Temp1 <- POP[stroke.incidence>0, mean(fatality)*.N, by = .(age, sex, qimd) 
#              ][, sum(V1, na.rm = T)]
Temp1 <- POP[between(age, ageL, ageH), 
             .(after = sum(stroke.incidence > 0) * mean(fatality)),
             by = .(agegroup, sex)] #expected number of deaths

# POP[, fatality := fatality * Temp / Temp1]
Temp[Temp1, corr := before/after, on = c("agegroup", "sex")]
Temp[is.na(corr), corr := 1]
POP[Temp, fatality := fatality * corr, on = c("agegroup", "sex")]

# 30 day fatality from OXVASC (see 30 days fatality.xlsx for the adjustments)
setkey(POP, agegroup, sex)
POP[fatality30stroke, fatality30 := fatality30]

POP[between(age, ageL, ageH),
    fatality2 := (.SD[stroke.incidence > 0, .N * mean(fatality)] - .SD[stroke.incidence == 2011 + i, .N * mean(fatality30)]) /
      (.SD[stroke.incidence > 0, .N] - .SD[stroke.incidence == 2011 + i, .N * mean(fatality30)]),
    by = group] # mean is used instaed of unique to maintain groups with no events in the result

POP[ fatality2 < 0, fatality2 := 0]

POP[between(age, ageL, ageH) & is.na(fatality2), fatality2 := fatality]

POP[stroke.incidence > 0, dead := F]
POP[stroke.incidence == 2011 + i, dead:= dice(.N) < fatality30] # 30 days mortality T = dead, F = alive 

POP[stroke.incidence > 0 & dead == F, dead:= dice(.N) < fatality2] # T = dead, F = alive 

#cat("Export stroke burden summary...\n\n")
if (i == init.year - 2011) stroke.burden <- vector("list", yearstoproject * 5)

#if (exists("stroke.burden.rds")) output[[1]] <- stroke.burden.rds
stroke.burden[[(2011 - init.year + i) * 5 + 1]] <-
  output.stroke(POP, c("qimd", "sex", "agegroup"))

stroke.burden[[(2011 - init.year + i) * 5 + 2]] <- 
  output.stroke(POP, c("sex", "agegroup"))

stroke.burden[[(2011 - init.year + i) * 5 + 3]] <- 
  output.stroke(POP, c("qimd", "sex"))

stroke.burden[[(2011 - init.year + i) * 5 + 4]] <- 
  output.stroke(POP, c("sex"))

stroke.burden[[(2011 - init.year + i) * 5 + 5]] <- 
  output.stroke(POP, c())

if (i == yearstoproject + init.year - 2012) {
  saveRDS(rbindlist(stroke.burden, T, T), 
          file = paste0(output.dir(), "stroke.burden.rds"))
}

#cat("Export stroke burden individuals...\n\n")
indiv.incid[[which(diseasestoexclude=="stroke")]] <- 
  POP[stroke.incidence == 2011 + i, 
      .(age, sex, qimd, agegroup, eqv5, id, hserial, hpnssec8, sha
      )][,`:=` (scenario = gsub(".Rc", "", scenarios.list[[iterations]]), 
                mc = haha, year = 2011 + i, cause = "stroke")]


# output <- vector("list", 2)
# 
# if (exists("stroke.ind.preval.rds")) output[[1]] <- stroke.ind.preval.rds
# 
# output[[2]] <- POP[stroke.incidence > 0, .(age, sex, qimd, agegroup, eqv5, id, hserial, hpnssec8, sha, stroke.incidence)][,`:=` (scenario = gsub(".R", "", scenarios.list[[iterations]]), mc = haha)]
# 
# 
# stroke.ind.preval.rds <- rbindlist(output, fill = T)
# 
# if (i == yearstoproject + init.year - 2012) {
#   saveRDS(stroke.ind.preval.rds, file = paste0(output.dir(), "stroke.ind.preval.rds"))
# }

indiv.mort[[which(diseasestoexclude=="stroke")+1]] <-
  POP[dead == T, .(age, sex, qimd, agegroup, eqv5, id, hserial, hpnssec8, sha)
      ][,`:=` (year = 2011 + i, cause = "stroke",
               scenario = gsub(".Rc", "", scenarios.list[[iterations]]),
               mc = haha)]

POP = copy(POP[dead == F | is.na(dead)== T,])

rm(Temp, Temp1)

POP[, `:=` (stroke.tob.rr = NULL, p0 = NULL,
            stroke.ets.rr = NULL, stroke.sbp.rr = NULL, 
            fatality30 = NULL, stroke.pa.rr = NULL,
            stroke.chol.rr = NULL, stroke.bmi.rr = NULL, stroke.diab.rr = NULL,
            stroke.fv.rr = NULL, dead = NULL, fatality = NULL, fatality2 = NULL)] 
