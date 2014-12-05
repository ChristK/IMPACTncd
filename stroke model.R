cat("Loading stroke (I60-I69) model...\n\n")
if (i == init.year-2011) POP[, stroke.incidence := 0] # Only needs to run the very first time of each simulation
length.of.POP <- length(POP)
POP <- merge(POP, strokeincid, by = c("agegroup", "sex"), all.x = T)
setkey(POP, age, sex, agegroup)


# RR for tobacco from Ezzati M, Henley SJ, Thun MJ, Lopez AD. Role of Smoking in Global and Regional 
# Cardiovascular Mortality. Circulation. 2005 Jul 26;112(4):489–97.
# Table 1 Model B
cat("smoking RR\n")
POP[, stroke.tob.rr := 1]
POP[cigst1 == "4" & age < 60 & sex == "1", stroke.tob.rr := stochRR(.N, 3.12, 4.64)]
POP[cigst1 == "4" & between(age, 60, 69) & sex == "1", stroke.tob.rr := stochRR(.N, 1.87, 2.44)]
POP[cigst1 == "4" & between(age, 70, 79) & sex == "1", stroke.tob.rr := stochRR(.N, 1.39, 1.77)]
#POP[cigst1 == "4" & age >79 & sex == "1", stroke.tob.rr := stochRR(.N, 1.05, 1.43)] not significant
POP[cigst1 == "4" & age < 60 & sex == "2", stroke.tob.rr := stochRR(.N, 4.61, 6.37)]
POP[cigst1 == "4" & between(age, 60, 69) & sex == "2", stroke.tob.rr := stochRR(.N, 2.81, 3.58)]
POP[cigst1 == "4" & between(age, 70, 79) & sex == "2", stroke.tob.rr := stochRR(.N, 1.95, 2.45)]

#ex-smokers
# Stroke risk decreased significantly by two years and was at the level of nonsmokers
# by five years after cessation of cigarette smoking.


# Calculate PAF of ETS for stroke
# RR from Oono IP, Mackay DF, Pell JP. Meta-analysis of the association between secondhand smoke exposure and stroke. 
# J Public Health 2011;33:496–502. doi:10.1093/pubmed/fdr025
POP[, stroke.ets.rr := 1]
POP[cigst1 %!in% c("4") & expsmokCat == "1", stroke.ets.rr := stochRR(.N, 1.25, 1.38)]

# Calculate RR for stroke. Optimal SBP level at 115mmHg and RR(HR) of dying from
# stroke was taken from "Age-specific relevance of usual blood pressure to 
# vascular mortality: a meta-analysis of individual data for one million adults in 61 prospective studies. 
# The Lancet. 2002 Dec 14;360(9349):1903–1913" 
# Figure 3
cat("sbp RR\n")
POP[, omsysval.usual := round(omsysval.cvdlag, 1)]
POP[omsysval.usual > 200, omsysval.usual := 200]# calculate usual sbp. NEED TO improve for t>1
POP[, stroke.sbp.rr := 1]
POP[between(age, ageL, 49) & sex == "1", stroke.sbp.rr := stochRR(.N, 0.33^((115 - omsysval.usual)/20), 0.38^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[between(age, ageL, 49) & sex == "2", stroke.sbp.rr := stochRR(.N, 0.41^((115 - omsysval.usual)/20), 0.49^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[between(age, 50, 59) & sex == "1", stroke.sbp.rr := stochRR(.N, 0.34^((115 - omsysval.usual)/20), 0.37^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[between(age, 50, 59) & sex == "2", stroke.sbp.rr := stochRR(.N, 0.45^((115 - omsysval.usual)/20), 0.50^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[between(age, 60, 69) & sex == "1", stroke.sbp.rr := stochRR(.N, 0.41^((115 - omsysval.usual)/20), 0.44^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[between(age, 60, 69) & sex == "2", stroke.sbp.rr := stochRR(.N, 0.47^((115 - omsysval.usual)/20), 0.51^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[between(age, 70, 79) & sex == "1", stroke.sbp.rr := stochRR(.N, 0.48^((115 - omsysval.usual)/20), 0.51^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[between(age, 70, 79) & sex == "2", stroke.sbp.rr := stochRR(.N, 0.53^((115 - omsysval.usual)/20), 0.56^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[between(age, 80, ageH) & sex == "1", stroke.sbp.rr := stochRR(.N, 0.68^((115 - omsysval.usual)/20), 0.75^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[between(age, 80, ageH) & sex == "2", stroke.sbp.rr := stochRR(.N, 0.65^((115 - omsysval.usual)/20), 0.71^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[stroke.sbp.rr<1, stroke.sbp.rr := 1]


# Calculate RR for stroke. Optimal chol level at 3.8 mmol/L and RR(HR) of dying from stroke was taken from "Blood cholesterol and 
# vascular mortality by age, sex, and blood pressure: a meta-analysis of individual data from 61 prospective studies 
# with 55.000 vascular deaths. The Lancet. 2007 Dec 7;370(9602):1829–39. 
# Figure 4 (for total stroke. I used only significant HR's). Didn't use non significant 
cat("chol RR\n")
POP[, cholval1.usual := round(cholval.cvdlag, 2)] # calculate usual chol. 
POP[cholval1.usual>12, cholval1.usual := 12]
POP[, stroke.chol.rr := 1]
POP[between(age, ageL, 59), stroke.chol.rr := stochRR(.N, 0.90^(3.8 - cholval1.usual), 0.97^(3.8 - cholval1.usual)), by = cholval1.usual]
POP[stroke.chol.rr < 1, stroke.chol.rr := 1]

# RR for BMI from "The Emerging Risk Factors Collaboration.
# Separate and combined associations of body-mass index and abdominal adiposity with cardiovascular disease:
# collaborative analysis of 58 prospective studies.
# The Lancet 2011;377:1085–95. doi:10.1016/S0140-6736(11)60105-0
# Table 1 (Adjusted for age, sex, smoking status, systolic blood pressure, history of diabetes, and total and HDL cholesterol)
# BMI not significant for ischaemic stroke but other obesity metrics are. 
#!! NEED TO decide if I want to use it
cat("bmi RR\n")
POP[, stroke.bmi.rr := 1]
POP[, bmival.cvdlag.round := round(bmival.cvdlag,1)]
POP[between(age, ageL, ageH), stroke.bmi.rr := stochRR(.N, 1.06^((bmival.cvdlag.round - 20) / 4.56), 1.13^((bmival.cvdlag.round - 20) / 4.56)), by = bmival.cvdlag.round]
POP[stroke.bmi.rr < 1, stroke.bmi.rr := 1]

# RR for BMI from The Emerging Risk Factors Collaboration. Diabetes mellitus, fasting blood glucose concentration, 
# and risk of vascular disease: a collaborative 
# meta-analysis of 102 prospective studies. The Lancet 2010;375:2215–22. doi:10.1016/S0140-6736(10)60484-9
# figure 2 (HRs were adjusted for age, smoking status, body-mass index, and  systolic blood pressure)
cat("diab RR\n")
POP[, stroke.diab.rr := 1]
POP[diabtotr.cvdlag == "2" & age < 60, stroke.diab.rr := stochRR(.N, 3.74, 4.58)]
POP[diabtotr.cvdlag == "2" & between(age, 60, 69), stroke.diab.rr := stochRR(.N, 2.06, 2.58)]
POP[diabtotr.cvdlag == "2" & age > 69, stroke.diab.rr := stochRR(.N, 1.8, 2.27)]

# Calculate RR for stroke. From Dauchet L, Amouyel P, Dallongeville J. Fruit and vegetable consumption and risk of stroke A meta-analysis of cohort studies. Neurology. 2005 Oct 25;65(8):1193–7. 
# To avoid negative PAF an optimal level of F&V has to be set arbitrarily. I set it to 10 
# when convert porftvg from categorical to numeric I create bias. eg 1=less than 1 portion
cat("fv RR\n")
POP[, stroke.fv.rr := 1]
POP[porftvg.cvdlag < 97, stroke.fv.rr := stochRR(.N, 0.95^(porftvg.cvdlag), 0.97^(porftvg.cvdlag)), by = porftvg.cvdlag] 

# Estimate prevalence of stroke only in first run when i does not exist yet
if (i == init.year-2011) {
  cat(paste0("Estimating stroke prevalence in ", init.year, " ...\n\n"))
  
  age.structure <- data.table(table(POP[, sex, by = agegroup]), key = c("sex", "agegroup"))
  age.structure[strokepreval, "Nprev" := as.integer(N * prevalence)]
  setnames(age.structure, "N", "population")
  setkey(POP, id)
  
  Temp <- POP[between(age, ageL, ageH), 
              sample_n(.SD, age.structure[sex == .BY[[2]] & agegroup == .BY[[1]], Nprev], 
                       weight = p0 * stroke.tob.rr * stroke.ets.rr * 
                         stroke.sbp.rr * stroke.chol.rr * stroke.bmi.rr * stroke.diab.rr * stroke.fv.rr, 
                       replace = F), 
              by = list(agegroup, sex)]
  setkey(Temp, id) # Temp is only used to select id for people with prevalent stroke
  POP[Temp, stroke.incidence := init.year-1] # and then we assign these ids to the population
}

# correction factor NEED TO make it work only for i==0
if (alignment == T) {
  if (i == init.year-2011) {
    corr.factor.stroke <- merge(POP[, mean(p0* stroke.tob.rr * stroke.ets.rr * 
                                             stroke.sbp.rr * stroke.chol.rr * 
                                             stroke.bmi.rr * stroke.diab.rr * stroke.fv.rr), by = c("agegroup", "sex")], strokeincid, by = c("agegroup", "sex"), all.x = T)
    corr.factor.stroke[,b := incidence/V1]
    corr.factor.stroke[, `:=` (p0=NULL, incidence=NULL, V1=NULL)]
  } else {
    POP <- merge(POP, corr.factor, by = c("agegroup", "sex"), all.x = T)
  }
} else {POP[, b:=1]}

# P= p0 * stroke.tob.rr * stroke.ets.rr * stroke.sbp.rr * stroke.chol.rr * stroke.bmi.rr * stroke.diab.rr * stroke.fv.rr
cat("Estimating stroke incidence...\n\n")
if (alignment == T) cat("Alignment will be performed\n\n")
POP[between(age, ageL, ageH) & stroke.incidence == 0, v := dice(.N) <= (p0 * stroke.tob.rr * stroke.ets.rr * 
                                                                          stroke.sbp.rr * stroke.chol.rr * 
                                                                          stroke.bmi.rr * stroke.diab.rr * stroke.fv.rr * b)] # b is the correction factor
#POP[,summary(as.factor(v))]

POP[v == T, stroke.incidence := 2011 + i]  

POP[, v := NULL]

#setkey(Out.Inc.stroke, agegroup, sex)

# Estimate stroke mortality 
cat("Estimating stroke mortality...\n\n")

# Apply assumptions about improvement of fatality by year
# ie 3% improvemnt by year (as supportet by BHF)
strokesurv[, fatality := fatality * (100 - fatality.annual.improvement.stroke)/100]

POP <- merge(POP, strokesurv, by = c("agegroup", "sex"), all.x = T)
setkey(POP, agegroup, sex)

# Fatality SEC gradient and healthcare improvement (supported by table 1.13 BHF2012)
fatality.sec.gradient.stroke/4
POP[qimd=="1", fatality := (100 - fatality.sec.gradient.stroke/2) * fatality/100]
POP[qimd=="2", fatality := (100 - fatality.sec.gradient.stroke/4) * fatality/100]
POP[qimd=="4", fatality := (100 + fatality.sec.gradient.stroke/4) * fatality/100]
POP[qimd=="5", fatality := (100 + fatality.sec.gradient.stroke/2) * fatality/100]

POP[stroke.incidence > 0, dead:= dice(.N) <= fatality] # T = dead, F = alive 

cat("Export stroke burden summary...\n\n")
output <- vector("list", 5)

if (exists("stroke.burden.rds")) output[[1]] <- stroke.burden.rds

output[[2]] <- POP[between(age, ageL, ageH), output.stroke(.SD), by=.(qimd, sex, agegroup)]

output[[3]] <- POP[between(age, ageL, ageH), output.stroke(.SD), by=.(sex, agegroup)]

output[[4]] <- POP[between(age, ageL, ageH), output.stroke(.SD), by=.(qimd, sex)]

output[[5]] <- POP[between(age, ageL, ageH), output.stroke(.SD), by=.(sex)]

stroke.burden.rds <- rbindlist(output, fill = T)

if (i == yearstoproject + init.year - 2012) {
  saveRDS(stroke.burden.rds, file = paste0(output.dir(), "stroke.burden.rds"))
}

cat("Export stroke burden individuals...\n\n")
output <- vector("list", 2)

if (exists("stroke.ind.incid.rds")) output[[1]] <- stroke.ind.incid.rds

output[[2]] <- POP[stroke.incidence == 2011+i, .(age, sex, qimd, agegroup, eqv5, id, hserial, hpnssec8, sha, stroke.incidence)][,`:=` (scenario = gsub(".R", "", scenarios.list[[iterations]]), mc = haha)]

stroke.ind.incid.rds <- rbindlist(output, fill = T)

if (i == yearstoproject + init.year - 2012) {
  saveRDS(stroke.ind.incid.rds, file = paste0(output.dir(), "stroke.ind.incid.rds"))
}

output <- vector("list", 2)

if (exists("stroke.ind.preval.rds")) output[[1]] <- stroke.ind.preval.rds

output[[2]] <- POP[stroke.incidence > 0, .(age, sex, qimd, agegroup, eqv5, id, hserial, hpnssec8, sha, stroke.incidence)][,`:=` (scenario = gsub(".R", "", scenarios.list[[iterations]]), mc = haha)]


stroke.ind.preval.rds <- rbindlist(output, fill = T)

if (i == yearstoproject + init.year - 2012) {
  saveRDS(stroke.ind.preval.rds, file = paste0(output.dir(), "stroke.ind.preval.rds"))
}

output <- vector("list", 2)

if (exists("stroke.ind.mortal.rds")) output[[1]] <- stroke.ind.mortal.rds

output[[2]] <- POP[dead == T, .(age, sex, qimd, agegroup, eqv5, id, hserial, hpnssec8, sha, stroke.incidence)][,`:=` (year.death = 2011+i, cause.death = "stroke", scenario = gsub(".R", "", scenarios.list[[iterations]]), mc = haha)]

stroke.ind.mortal.rds <- rbindlist(output, fill = T)

rm(output)

if (i == yearstoproject + init.year - 2012) {
  saveRDS(stroke.ind.mortal.rds, file = paste0(output.dir(), "stroke.ind.mortal.rds"))
}

POP = copy(POP[dead == F | is.na(dead)== T,])

rm(length.of.POP)
POP[, `:=` (incidence = NULL, stroke.tob.rr = NULL, p0 = NULL,
            stroke.ets.rr = NULL, omsysval.usual = NULL, stroke.sbp.rr = NULL, cholval1.usual = NULL,
            stroke.chol.rr = NULL, stroke.bmi.rr = NULL, stroke.diab.rr = NULL,
            stroke.fv.rr = NULL, dead = NULL, fatality = NULL, b = NULL)] 
