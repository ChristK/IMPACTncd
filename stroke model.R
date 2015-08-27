#cmpfile("./stroke model.R")
cat("Loading stroke (I60-I69) model...\n")
cat(paste0(Sys.time(), "\n\n"))
if (i == init.year-2011) set(POP, NULL, "stroke.incidence",  0L) # Only needs to run the very first time of each simulation
POP[age==0, stroke.incidence := 0]


# RR for tobacco from Ezzati M, Henley SJ, Thun MJ, Lopez AD. Role of Smoking in Global and Regional 
# Cardiovascular Mortality. Circulation. 2005 Jul 26;112(4):489–97.
# Table 1 Model B
cat("smoking RR\n")
set(POP, NULL, "stroke.tob.rr",  1)
setkey(POP, agegroup, sex, cigst1.cvdlag)
POP[tobacco.rr.stroke, stroke.tob.rr := rr]

#ex-smokers
# Stroke risk decreased significantly by two years and was at the level of nonsmokers
# by five years after cessation of cigarette smoking.


# Calculate PAF of ETS for stroke
# RR from Oono IP, Mackay DF, Pell JP. Meta-analysis of the association between secondhand smoke exposure and stroke. 
# J Public Health 2011;33:496–502. doi:10.1093/pubmed/fdr025
set(POP, NULL, "stroke.ets.rr",  1)
POP[cigst1 %!in% c("4") & expsmokCat == "1", stroke.ets.rr := stroke.ets.rr.mc]

# Calculate RR for stroke. Optimal SBP level at 115mmHg and RR(HR) of dying from
# stroke was taken from "Age-specific relevance of usual blood pressure to 
# vascular mortality: a meta-analysis of individual data for one million adults in 61 prospective studies. 
# The Lancet. 2002 Dec 14;360(9349):1903–1913" 
# Figure 3
cat("sbp RR\n")
set(POP, NULL, "stroke.sbp.rr",  1)
setkey(POP, agegroup, sex)
POP[sbp.rr.stroke, stroke.sbp.rr := rr^((115 - omsysval.cvdlag)/20)]
POP[stroke.sbp.rr < 1, stroke.sbp.rr := 1]

# Calculate RR for stroke. Optimal chol level at 3.8 mmol/L and RR(HR) of dying from stroke was taken from "Blood cholesterol and 
# vascular mortality by age, sex, and blood pressure: a meta-analysis of individual data from 61 prospective studies 
# with 55.000 vascular deaths. The Lancet. 2007 Dec 7;370(9602):1829–39. 
# Figure 4 (for total stroke. I used only significant HR's). Didn't use non significant 
cat("chol RR\n")
set(POP, NULL, "stroke.chol.rr",  1)
setkey(POP, agegroup)
POP[chol.rr.stroke, stroke.chol.rr := rr^(3.8 - cholval.cvdlag)]
POP[stroke.chol.rr<1, stroke.chol.rr := 1]

# RR for BMI from "The Emerging Risk Factors Collaboration.
# Separate and combined associations of body-mass index and abdominal adiposity with cardiovascular disease:
# collaborative analysis of 58 prospective studies.
# The Lancet 2011;377:1085–95. doi:10.1016/S0140-6736(11)60105-0
# Table 1 (Adjusted for age, sex, smoking status, systolic blood pressure, history of diabetes, and total and HDL cholesterol)
# BMI not significant for ischaemic stroke but other obesity metrics are. 
#!! NEED TO decide if I want to use it
cat("bmi RR\n")
set(POP, NULL, "stroke.bmi.rr",  1)
POP[stroke.bmi.rr.mc, stroke.bmi.rr := rr^((bmival.cvdlag - 20) / 4.56)]
POP[stroke.bmi.rr < 1, stroke.bmi.rr := 1]

# RR for BMI from The Emerging Risk Factors Collaboration. Diabetes mellitus, fasting blood glucose concentration, 
# and risk of vascular disease: a collaborative 
# meta-analysis of 102 prospective studies. The Lancet 2010;375:2215–22. doi:10.1016/S0140-6736(10)60484-9
# figure 2 (HRs were adjusted for age, smoking status, body-mass index, and  systolic blood pressure)
cat("diab RR\n")
set(POP, NULL, "stroke.diab.rr",  1)
setkey(POP, agegroup, diabtotr.cvdlag)
POP[stroke.diab.rr.mc, stroke.diab.rr := rr]

# Calculate RR for stroke. From Dauchet L, Amouyel P, Dallongeville J. Fruit and vegetable consumption and risk of stroke A meta-analysis of cohort studies. Neurology. 2005 Oct 25;65(8):1193–7. 
# To avoid negative PAF an optimal level of F&V has to be set arbitrarily. I set it to 8 
# when convert porftvg from categorical to numeric I create bias. eg 1=less than 1 portion
cat("fv RR\n")
set(POP, NULL, "stroke.fv.rr",  1)
POP[porftvg.cvdlag > 0, stroke.fv.rr := stroke.fv.rr.mc^(porftvg.cvdlag)] 

# RR for PA 1. WHO | Comparative Quantification of Health Risks [Internet]. 
# WHO [cited 2014 Jan 30];Available from: http://www.who.int/publications/cra/en/
# Table 10.20 (with adjustment for measurement error)
cat("pa RR\n")
set(POP, NULL, "stroke.pa.rr",  1)
setkey(POP, agegroup, a30to06m.cvdlag)
POP[pa.rr.stroke, stroke.pa.rr := rr]

# Estimate PAF ------------------------------------------------------------
cat("Estimating stroke PAF...\n")
if (i == init.year - 2011) {
  stroketobpaf <- 
    POP[between(age, ageL, ageH), 
        .(tobpaf = 
            (sum(stroke.tob.rr - 1, na.rm = T) / .N) / 
            ((sum(stroke.tob.rr - 1, na.rm = T) / .N) + 1)
        ), 
        by = .(agegroup, sex)
        ]
  setkey(stroketobpaf, agegroup, sex)
  
  strokeetspaf <- 
    POP[between(age, ageL, ageH), 
        .(etspaf = 
            (sum(stroke.ets.rr - 1, na.rm = T) / .N) / 
            ((sum(stroke.ets.rr - 1, na.rm = T) / .N) + 1)
        ), 
        by = .(agegroup, sex)
        ]
  setkey(strokeetspaf, agegroup, sex)
  
  strokesbppaf <- 
    POP[between(age, ageL, ageH), 
        .(sbppaf = 
            (sum(stroke.sbp.rr - 1, na.rm = T) / .N) / 
            ((sum(stroke.sbp.rr - 1, na.rm = T) / .N) + 1)
        ), 
        by = .(agegroup, sex)
        ]
  setkey(strokesbppaf, agegroup, sex)
  
  strokecholpaf <- 
    POP[between(age, ageL, ageH), 
        .(cholpaf = 
            (sum(stroke.chol.rr - 1, na.rm = T) / .N) / 
            ((sum(stroke.chol.rr - 1, na.rm = T) / .N) + 1)
        ), 
        by = .(agegroup, sex)
        ]
  setkey(strokecholpaf, agegroup, sex)
  
  strokebmipaf <- 
    POP[between(age, ageL, ageH), 
        .(bmipaf = 
            (sum(stroke.bmi.rr - 1, na.rm = T) / .N) / 
            ((sum(stroke.bmi.rr - 1, na.rm = T) / .N) + 1)
        ), 
        by = .(agegroup, sex)
        ]
  setkey(strokebmipaf, agegroup, sex)
  
  strokediabpaf <- 
    POP[between(age, ageL, ageH), 
        .(diabpaf = 
            (sum(stroke.diab.rr - 1, na.rm = T) / .N) / 
            ((sum(stroke.diab.rr - 1, na.rm = T) /.N) + 1)
        ), 
        by = .(agegroup, sex)
        ]
  setkey(strokediabpaf, agegroup, sex)
  
  strokefvpaf <- 
    POP[between(age, ageL, ageH), 
        .(fvpaf = 
            (sum((stroke.fv.rr^-1) - 1, na.rm = T) / .N) / 
            ((sum((stroke.fv.rr^-1) - 1, na.rm = T) /.N ) + 1)
        ), 
        by = .(agegroup, sex)
        ]
  setkey(strokefvpaf, agegroup, sex)
  
  strokepapaf <- 
    POP[between(age, ageL, ageH), 
        .(papaf = 
            (sum(stroke.pa.rr - 1, na.rm = T) / .N) / 
            ((sum(stroke.pa.rr - 1, na.rm = T) /.N ) + 1)
        ), 
        by = .(agegroup, sex)
        ]
  setkey(strokepapaf, agegroup, sex)
  
  strokeincid[, agegroup := agegroup.fn(age)]
  setkey(strokeincid, agegroup, sex)
  strokeincid[strokebmipaf[strokecholpaf[strokediabpaf[strokeetspaf[strokefvpaf[strokesbppaf[stroketobpaf[strokepapaf]]]]]]], 
              p0 := incidence * (1 - bmipaf) * 
                (1 - cholpaf) * (1 - diabpaf) * 
                (1 - etspaf)  * (1 - fvpaf) * 
                (1 - sbppaf)  * (1 - tobpaf) *
                (1 - papaf)]
  strokeincid[is.na(p0), p0 := incidence]
  strokeincid[, agegroup := NULL]
  setkey(strokeincid, NULL)
  rm(strokebmipaf,strokecholpaf,strokediabpaf,strokeetspaf,
     strokefvpaf,strokesbppaf,stroketobpaf,strokepapaf)
}

setkey(POP, age, sex)
POP[strokeincid, p0 := p0]

# Estimate prevalence of stroke only in first run when i does not exist yet
if (i == init.year-2011) {
  cat(paste0("Estimating stroke prevalence in ", init.year, " ...\n\n"))
  age.structure <- setkey(POP[age <= ageH, .N, by = .(age, sex)], age, sex)
  age.structure[strokepreval[age <= ageH], Nprev := rbinom(.N, N, prevalence)]
  age.structure[strokeincid[age <= ageH],  Nprev := Nprev - rbinom(.N, N, incidence)]
  age.structure[Nprev < 0, Nprev := 0]
  
  #age.structure[strokesurv, Nprev := round(Nprev * (1 - fatality * 1.03))]
  setnames(age.structure, "N", "population")
  
  id.stroke <- POP[age <= ageH, 
                   sample_n(.SD, age.structure[sex == .BY[[2]] & age == .BY[[1]], Nprev], 
                            weight = stroke.tob.rr * stroke.ets.rr * stroke.sbp.rr * 
                              stroke.chol.rr * stroke.bmi.rr * stroke.diab.rr *
                              stroke.fv.rr * stroke.pa.rr, 
                            replace = F), 
                   by = .(age, sex)][, id]
  POP[id %in% id.stroke, stroke.incidence := init.year - 1] # and then we assign
  # these ids to the population
  rm(id.stroke)
}

# correction factor NEED TO make it work only for i==0
if (alignment == T) {
  if (i == init.year-2011) {
    corr.factor.stroke <- merge(
      POP[between(age, ageL, ageH) & stroke.incidence == 0, 
          mean(p0 * stroke.tob.rr * stroke.ets.rr * 
                 stroke.sbp.rr * stroke.chol.rr * 
                 stroke.bmi.rr * stroke.diab.rr * 
                 stroke.fv.rr * stroke.pa.rr), 
          by = .(age, sex)],
      strokeincid, 
      by = c("age", "sex"), all.x = T)
    corr.factor.stroke[, b := incidence/V1]
    corr.factor.stroke[, `:=` (p0 = NULL, incidence = NULL, V1 = NULL)]
    POP <- merge(POP, corr.factor.stroke, by = c("age", "sex"), all.x = T)
  } else {
    POP <- merge(POP, corr.factor.stroke, by = c("age", "sex"), all.x = T)
  }
} else set(POP, NULL, "b", 1)

cat("Estimating stroke incidence...\n\n")
if (alignment == T) cat("Alignment will be performed\n\n")
POP[between(age, ageL, ageH) & 
      stroke.incidence == 0 & 
      dice(.N) < p0 * stroke.tob.rr * stroke.ets.rr *
      stroke.sbp.rr * stroke.chol.rr * 
      stroke.bmi.rr * stroke.diab.rr * 
      stroke.fv.rr * stroke.pa.rr * b, 
    stroke.incidence := init.year + i]
#setkey(Out.Inc.stroke, agegroup, sex)

# Estimate stroke mortality 
cat("Estimating stroke mortality...\n\n")

# Apply assumptions about improvement of fatality by year
# ie 3% improvemnt by year (as supportet by BHF)

if (i > init.year - 2011) strokesurv[, fatality := fatality * (100 - fatality.annual.improvement.stroke)/100]
setkey(POP, age, sex)
POP[strokesurv, fatality := fatality]

Temp <- POP[stroke.incidence > 0, .N, by = .(age, sex) #expected number of deaths
            ][strokesurv, sum(fatality * N, na.rm = T)]

# Fatality SEC gradient and healthcare improvement (supported by table 1.13 BHF2012)
POP[qimd == "1", fatality := (100 - fatality.sec.gradient.stroke / 2) * fatality/100]
POP[qimd == "2", fatality := (100 - fatality.sec.gradient.stroke / 4) * fatality/100]
POP[qimd == "4", fatality := (100 + fatality.sec.gradient.stroke / 4) * fatality/100]
POP[qimd == "5", fatality := (100 + fatality.sec.gradient.stroke / 2) * fatality/100]

#expected number of deaths after gradient and needs to be corrected by Temp/Temp1
Temp1 <- POP[stroke.incidence>0, mean(fatality)*.N, by = .(age, sex, qimd) 
             ][, sum(V1, na.rm = T)]

POP[, fatality := fatality * Temp / Temp1]

# 30 day fatality from OXVASC (see 30 days fatality.xlsx for the adjustments)
setkey(POP, agegroup, sex)
POP[fatality30stroke, fatality30 := fatality30]

POP[between(age, ageL, ageH),
    fatality2 := (.SD[stroke.incidence > 0, .N * mean(fatality)] - .SD[stroke.incidence == 2011 + i, .N * mean(fatality30)])/.SD[stroke.incidence > 0, .N],
    by = group] # mean is used instaed of unique to maintain groups with no events in the result

POP[ fatality2 < 0, fatality2 := 0]

POP[between(age, ageL, ageH) & is.na(fatality2), fatality2 := fatality]

POP[stroke.incidence > 0, dead := F]
POP[stroke.incidence == 2011 + i, dead:= dice(.N) < fatality30] # 30 days mortality T = dead, F = alive 

POP[stroke.incidence > 0 & dead == F, dead:= dice(.N) < fatality2] # T = dead, F = alive 

cat("Export stroke burden summary...\n\n")
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

cat("Export stroke burden individuals...\n\n")
indiv.incid[[which(diseasestoexclude=="stroke")]] <- 
  POP[stroke.incidence == 2011 + i, 
      .(age, sex, qimd, agegroup, eqv5, id, hserial, hpnssec8, sha
      )][,`:=` (scenario = gsub(".R", "", scenarios.list[[iterations]]), 
                mc = haha, year = 2011+i, cause = "stroke")]


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

indiv.mort[[which(diseasestoexclude=="stroke")+1]] <-  POP[dead == T, .(age, sex, qimd, agegroup, eqv5, id, hserial, hpnssec8, sha)
                                                           ][,`:=` (year = 2011 + i, cause = "stroke", scenario = gsub(".R", "", scenarios.list[[iterations]]),
                                                                    mc = haha)]

POP = copy(POP[dead == F | is.na(dead)== T,])

rm(Temp, Temp1)

POP[, `:=` (stroke.tob.rr = NULL, p0 = NULL,
            stroke.ets.rr = NULL, stroke.sbp.rr = NULL, 
            fatality30 = NULL, b = NULL, stroke.pa.rr = NULL,
            stroke.chol.rr = NULL, stroke.bmi.rr = NULL, stroke.diab.rr = NULL,
            stroke.fv.rr = NULL, dead = NULL, fatality = NULL, fatality2 = NULL)] 
