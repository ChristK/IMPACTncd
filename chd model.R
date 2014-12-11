#cmpfile("./chd model.R")
cat("Loading CHD (I20-I25) model...\n")
cat(paste0(Sys.time(), "\n\n"))
if (i == init.year - 2011) POP[, chd.incidence := 0] # Only needs to run the very first time of each simulation
length.of.POP <- length(POP)
POP <- merge(POP, CHDincid, by = c("agegroup", "sex"), all.x = T)
setkey(POP, age, sex, agegroup)


# RR for tobacco from Ezzati M, Henley SJ, Thun MJ, Lopez AD. Role of Smoking in Global and Regional 
# Cardiovascular Mortality. Circulation. 2005 Jul 26;112(4):489–97.
# Table 1 Model B
cat("smoking RR\n")
POP[, chd.tob.rr := 1]
POP[cigst1.cvdlag == "4" & age < 45 & sex == "1", chd.tob.rr := stochRR(.N, 5.51, 12.25)]
POP[cigst1.cvdlag == "4" & between(age, 45, 59) & sex == "1", chd.tob.rr := stochRR(.N, 3.04, 3.48)]
POP[cigst1.cvdlag == "4" & between(age, 60, 69) & sex == "1", chd.tob.rr := stochRR(.N, 1.88, 2.08)]
POP[cigst1.cvdlag == "4" & between(age, 70, 79) & sex == "1", chd.tob.rr := stochRR(.N, 1.44, 1.63)]
# POP[cigst1 == "4" & age >79 & sex == "1", chd.tob.rr := stochRR(.N, 1.05, 1.43)] # not signifficant
POP[cigst1.cvdlag == "4" & age < 45 & sex == "2", chd.tob.rr := stochRR(.N, 2.26, 6.14)]
POP[cigst1.cvdlag == "4" & between(age, 45, 59) & sex == "2", chd.tob.rr := stochRR(.N, 3.78, 4.62)]
POP[cigst1.cvdlag == "4" & between(age, 60, 69) & sex == "2", chd.tob.rr := stochRR(.N, 2.53, 2.87)]
POP[cigst1.cvdlag == "4" & between(age, 70, 79) & sex == "2", chd.tob.rr := stochRR(.N, 1.68, 1.93)]
POP[cigst1.cvdlag == "4" & age >79 & sex == "2", chd.tob.rr := stochRR(.N, 1.38, 1.77)]

# RR for ex-smokers from Huxley RR, Woodward M. 
# Cigarette smoking as a risk factor for coronary heart disease
# in women compared with men: a systematic review and meta-analysis of prospective cohort studies. 
# The Lancet. 2011 Oct 14;378(9799):1297–305. 
# Appendix webfigure 8
POP[cigst1.cvdlag == "3" & sex == "1" , chd.tob.rr := stochRR(.N, 1.25, 1.32)]
POP[cigst1.cvdlag == "3" & sex == "2" , chd.tob.rr := stochRR(.N, 1.20, 1.34)]

# RR for ETS He J, Vupputuri S, Allen K, Prerost MR, Hughes J, Whelton PK. Passive Smoking and the Risk of 
# Coronary Heart Disease — A Meta-Analysis of Epidemiologic Studies. New England Journal of Medicine. 1999;340(12):920–6. 
# Table 3. Adjusted RR
POP[, chd.ets.rr := 1]
POP[cigst1 %!in% c("4") & expsmokCat == "1", chd.ets.rr := stochRR(.N, 1.26, 1.38)]

# RR for SBP from Optimal SBP level at 115mmHg and RR(HR) of dying from CHD was taken from "Age-specific relevance of
# usual blood pressure to vascular mortality: 
# a meta-analysis of individual data for one million adults in 61 prospective studies. 
# The Lancet. 2002 Dec 14;360(9349):1903–1913" 
# Figure 5
cat("sbp RR\n")
POP[, omsysval.usual := round(omsysval.cvdlag, 1)]
POP[omsysval.usual > 200, omsysval.usual := 200]# calculate usual sbp. NEED TO improve for t>1
POP[, chd.sbp.rr := 1]
POP[between(age, ageL, 49) & sex == "1", chd.sbp.rr := stochRR(.N, 0.50^((115 - omsysval.usual)/20), 0.54^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[between(age, ageL, 49) & sex == "2", chd.sbp.rr := stochRR(.N, 0.40^((115 - omsysval.usual)/20), 0.49^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[between(age, 50, 59) & sex == "1", chd.sbp.rr := stochRR(.N, 0.50^((115 - omsysval.usual)/20), 0.52^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[between(age, 50, 59) & sex == "2", chd.sbp.rr := stochRR(.N, 0.49^((115 - omsysval.usual)/20), 0.54^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[between(age, 60, 69) & sex == "1", chd.sbp.rr := stochRR(.N, 0.55^((115 - omsysval.usual)/20), 0.57^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[between(age, 60, 69) & sex == "2", chd.sbp.rr := stochRR(.N, 0.50^((115 - omsysval.usual)/20), 0.61^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[between(age, 70, 79) & sex == "1", chd.sbp.rr := stochRR(.N, 0.62^((115 - omsysval.usual)/20), 0.64^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[between(age, 70, 79) & sex == "2", chd.sbp.rr := stochRR(.N, 0.55^((115 - omsysval.usual)/20), 0.58^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[between(age, 80, ageH) & sex == "1", chd.sbp.rr := stochRR(.N, 0.69^((115 - omsysval.usual)/20), 0.73^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[between(age, 80, ageH) & sex == "2", chd.sbp.rr := stochRR(.N, 0.64^((115 - omsysval.usual)/20), 0.68^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[chd.sbp.rr<1, chd.sbp.rr := 1]

# RR for Chol from "Blood cholesterol and 
# vascular mortality by age, sex, and blood pressure: a meta-analysis of individual data from 61 prospective studies 
# with 55.000 vascular deaths. The Lancet. 2007 Dec 7;370(9602):1829–39. 
# Appendix Webtable 6  fully adjusted
cat("chol RR\n")
POP[, cholval1.usual := round(cholval.cvdlag, 2)] # calculate usual chol. 
POP[cholval1.usual>12, cholval1.usual := 12]
POP[, chd.chol.rr := 1]
POP[between(age, ageL, 49), chd.chol.rr := stochRR(.N, 0.49^(3.8 - cholval1.usual), 0.52^(3.8 - cholval1.usual)), by = cholval1.usual]
POP[between(age, 50, 59), chd.chol.rr := stochRR(.N, 0.62^(3.8 - cholval1.usual), 0.65^(3.8 - cholval1.usual)), by = cholval1.usual]
POP[between(age, 60, 69), chd.chol.rr := stochRR(.N, 0.74^(3.8 - cholval1.usual), 0.76^(3.8 - cholval1.usual)), by = cholval1.usual]
POP[between(age, 70, 79), chd.chol.rr := stochRR(.N, 0.84^(3.8 - cholval1.usual), 0.86^(3.8 - cholval1.usual)), by = cholval1.usual]
POP[between(age, 80, ageH), chd.chol.rr := stochRR(.N, 0.87^(3.8 - cholval1.usual), 0.90^(3.8 - cholval1.usual)), by = cholval1.usual]
POP[chd.chol.rr < 1, chd.chol.rr := 1]

# RR for BMI from "The Emerging Risk Factors Collaboration.
# Separate and combined associations of body-mass index and abdominal adiposity with cardiovascular disease:
# collaborative analysis of 58 prospective studies.
# The Lancet 2011;377:1085–95. doi:10.1016/S0140-6736(11)60105-0
# Table 1 (Adjusted for age, sex, smoking status, systolic blood pressure, history of diabetes, and total and HDL cholesterol)
cat("bmi RR\n")
POP[, chd.bmi.rr := 1]
POP[, bmival.cvdlag.round := round(bmival.cvdlag,1)]
POP[between(age, ageL, ageH), chd.bmi.rr := stochRR(.N, 1.11^((bmival.cvdlag.round - 20) / 4.56), 1.17^((bmival.cvdlag.round - 20) / 4.56)), by = bmival.cvdlag.round]
POP[chd.bmi.rr < 1, chd.bmi.rr := 1]

# RR for BMI from The Emerging Risk Factors Collaboration. Diabetes mellitus, fasting blood glucose concentration, 
# and risk of vascular disease: a collaborative 
# meta-analysis of 102 prospective studies. The Lancet 2010;375:2215–22. doi:10.1016/S0140-6736(10)60484-9
# figure 2 (HRs were adjusted for age, smoking status, body-mass index, and  systolic blood pressure)
cat("diab RR\n")
POP[, chd.diab.rr := 1]
POP[diabtotr.cvdlag == "2" & age < 60, chd.diab.rr := stochRR(.N, 2.51, 2.8)]
POP[diabtotr.cvdlag == "2" & between(age, 60, 69), chd.diab.rr := stochRR(.N, 2.01, 2.26)]
POP[diabtotr.cvdlag == "2" & age > 69, chd.diab.rr := stochRR(.N, 1.78, 2.05)]

# RR for F&V from From Dauchet L, Amouyel P, Hercberg S, Dallongeville J. Fruit and Vegetable Consumption and Risk of Coronary Heart Disease: 
# A Meta-Analysis of Cohort Studies. J Nutr. 2006 Oct 1;136(10):2588–93. 
# To avoid negative PAF an optimal level of F&V has to be set arbitrarily. I set it to 10 
# when convert porftvg from categorical to numeric I create bias. eg 1=less than 1 portion
cat("fv RR\n")
POP[, chd.fv.rr := 1]
POP[porftvg.cvdlag < 97, chd.fv.rr := stochRR(.N, 0.96^(porftvg.cvdlag), 0.99^(porftvg.cvdlag)), by = porftvg.cvdlag] 

# Estimate prevalence of CHD only in first run when i does not exist yet
if (i == init.year - 2011) {
  cat(paste0("Estimating CHD prevalence in ", init.year, " ...\n\n"))
  
  age.structure <- data.table(table(POP[, sex, by = agegroup]), key = c("sex", "agegroup"))
  age.structure[CHDpreval, "Nprev" := as.integer(N * prevalence)]
  setnames(age.structure, "N", "population")
  setkey(POP, id)
  
  Temp <- POP[between(age, ageL, ageH), 
              sample_n(.SD, age.structure[sex == .BY[[2]] & agegroup == .BY[[1]], Nprev], 
                       weight = p0 * chd.tob.rr * chd.ets.rr * 
                         chd.sbp.rr * chd.chol.rr * chd.bmi.rr * chd.diab.rr * chd.fv.rr, 
                       replace = F), 
              by = list(agegroup, sex)]
  setkey(Temp, id) # Temp is only used to select id for people with prevalent chd
  POP[Temp, chd.incidence := init.year-1] # and then we assign these ids to the population
}

# correction factor NEED TO make it work only for i==0
if (alignment == T) {
  if (i == init.year-2011) {
    corr.factor.chd <- merge(POP[, mean(p0* chd.tob.rr * chd.ets.rr * 
                                          chd.sbp.rr * chd.chol.rr * 
                                          chd.bmi.rr * chd.diab.rr * chd.fv.rr),
                                 by = c("agegroup", "sex")], CHDincid, by = c("agegroup", "sex"), all.x = T)
    corr.factor.chd[,b := incidence/V1]
    corr.factor.chd[, `:=` (p0=NULL, incidence=NULL, V1=NULL)]
  } else {
    POP <- merge(POP, corr.factor, by = c("agegroup", "sex"), all.x = T)
  }
} else {POP[, b := 1]}

# P= p0 * chd.tob.rr * chd.ets.rr * chd.sbp.rr * chd.chol.rr * chd.bmi.rr * chd.diab.rr * chd.fv.rr
cat("Estimating CHD incidence...\n\n")
if (alignment == T) cat("Alignment will be performed\n\n")
POP[between(age, ageL, ageH) & chd.incidence == 0, v := dice(.N) <= (p0 * chd.tob.rr * chd.ets.rr * 
                                                                       chd.sbp.rr * chd.chol.rr * 
                                                                       chd.bmi.rr * chd.diab.rr * chd.fv.rr * b)] # b is the correction factor
#POP[,summary(as.factor(v))]

POP[v == T, chd.incidence := 2011 + i]  

POP[, v := NULL]

#setkey(Out.Inc.CHD, agegroup, sex)

# Estimate CHD mortality 
cat("Estimating CHD mortality...\n\n")

# Apply assumptions about improvement of fatality by year
# ie 3% improvemnt by year (as supportet by BHF)
CHDsurv[, fatality := fatality * (100 - fatality.annual.improvement.chd)/100]
POP <- merge(POP, CHDsurv, by = c("agegroup", "sex"), all.x = T)
setkey(POP, agegroup, sex)

# Fatality SEC gradient and healthcare improvement (supported by table 1.13 BHF2012)
POP[qimd=="1", fatality := (100 - fatality.sec.gradient.chd/2) * fatality/100]
POP[qimd=="2", fatality := (100 - fatality.sec.gradient.chd/4) * fatality/100]
POP[qimd=="4", fatality := (100 + fatality.sec.gradient.chd/4) * fatality/100]
POP[qimd=="5", fatality := (100 + fatality.sec.gradient.chd/2) * fatality/100]



POP[chd.incidence > 0, dead:= dice(.N) <= fatality] # T = dead, F = alive 

cat("Export CHD burden summary...\n\n")
output <- vector("list", 5)

if (exists("chd.burden.rds")) output[[1]] <- chd.burden.rds

output[[2]] <- POP[between(age, ageL, ageH), output.chd(.SD), by=.(qimd, sex, agegroup)]

output[[3]] <- POP[between(age, ageL, ageH), output.chd(.SD), by=.(sex, agegroup)]

output[[4]] <- POP[between(age, ageL, ageH), output.chd(.SD), by=.(qimd, sex)]

output[[5]] <- POP[between(age, ageL, ageH), output.chd(.SD), by=.(sex)]

chd.burden.rds <- rbindlist(output, fill = T)

if (i == yearstoproject + init.year - 2012) {
  saveRDS(chd.burden.rds, file = paste0(output.dir(), "chd.burden.rds"))
}

cat("Export CHD burden individuals...\n\n")
output <- vector("list", 2)

if (exists("chd.ind.incid.rds")) output[[1]] <- chd.ind.incid.rds

output[[2]] <- POP[chd.incidence == 2011+i, .(age, sex, qimd, agegroup, eqv5, id, hserial, hpnssec8, sha, chd.incidence)][,`:=` (scenario = gsub(".R", "", scenarios.list[[iterations]]), mc = haha)]

chd.ind.incid.rds <- rbindlist(output, fill = T)

if (i == yearstoproject + init.year - 2012) {
  saveRDS(chd.ind.incid.rds, file = paste0(output.dir(), "chd.ind.incid.rds"))
}

output <- vector("list", 2)

if (exists("chd.ind.preval.rds")) output[[1]] <- chd.ind.preval.rds

output[[2]] <- POP[chd.incidence > 0, .(age, sex, qimd, agegroup, eqv5, id, hserial, hpnssec8, sha, chd.incidence)][,`:=` (scenario = gsub(".R", "", scenarios.list[[iterations]]), mc = haha)]

chd.ind.preval.rds <- rbindlist(output, fill = T)

if (i == yearstoproject + init.year - 2012) {
  saveRDS(chd.ind.preval.rds, file = paste0(output.dir(), "chd.ind.preval.rds"))
}

output <- vector("list", 2)

if (exists("chd.ind.mortal.rds")) output[[1]] <- chd.ind.mortal.rds

output[[2]] <- POP[dead == T, .(age, sex, qimd, agegroup, eqv5, id, hserial, hpnssec8, sha, chd.incidence)][,`:=` (year.death = 2011+i, cause.death = "CHD", scenario = gsub(".R", "", scenarios.list[[iterations]]), mc = haha)]

chd.ind.mortal.rds <- rbindlist(output, fill = T)

rm(output)

if (i == yearstoproject + init.year - 2012) {
  saveRDS(chd.ind.mortal.rds, file = paste0(output.dir(), "chd.ind.mortal.rds"))
}

POP = copy(POP[dead == F | is.na(dead)== T,])

rm(length.of.POP)
POP[, `:=` (incidence = NULL, chd.tob.rr = NULL, p0 = NULL,
            chd.ets.rr = NULL, omsysval.usual = NULL, chd.sbp.rr = NULL, cholval1.usual = NULL,
            chd.chol.rr = NULL, chd.bmi.rr = NULL, chd.diab.rr = NULL,
            chd.fv.rr = NULL, dead = NULL, fatality = NULL, b = NULL)] 
