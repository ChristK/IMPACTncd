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

cat("Loading CHD (I20-I25) model...\n\n")
length.of.POP <- length(POP)
POP <- merge(POP, CHDincid, by = c("agegroup", "sex"), all.x = T)
setkey(POP, age, sex, agegroup)

# RR for tobacco from Ezzati M, Henley SJ, Thun MJ, Lopez AD. Role of Smoking in Global and Regional 
# Cardiovascular Mortality. Circulation. 2005 Jul 26;112(4):489–97.
# Table 1 Model B
POP[, chd.tob.rr := 1]
POP[cigst1 == "4" & age < 45 & sex == "1", chd.tob.rr := stochRR(.N, 5.49, 12.25)]
POP[cigst1 == "4" & between(age, 45, 59) & sex == "1", chd.tob.rr := stochRR(.N, 3.04, 3.48)]
POP[cigst1 == "4" & between(age, 60, 69) & sex == "1", chd.tob.rr := stochRR(.N, 1.88, 2.08)]
POP[cigst1 == "4" & between(age, 70, 79) & sex == "1", chd.tob.rr := stochRR(.N, 1.44, 1.63)]
POP[cigst1 == "4" & age >79 & sex == "1", chd.tob.rr := stochRR(.N, 1.05, 1.43)]
POP[cigst1 == "4" & age < 45 & sex == "2", chd.tob.rr := stochRR(.N, 2.26, 6.14)]
POP[cigst1 == "4" & between(age, 45, 59) & sex == "2", chd.tob.rr := stochRR(.N, 3.78, 4.62)]
POP[cigst1 == "4" & between(age, 60, 69) & sex == "2", chd.tob.rr := stochRR(.N, 2.53, 2.87)]
POP[cigst1 == "4" & between(age, 70, 79) & sex == "2", chd.tob.rr := stochRR(.N, 1.68, 1.93)]
POP[cigst1 == "4" & age >79 & sex == "2", chd.tob.rr := stochRR(.N, 1.38, 1.77)]

# RR for ex-smokers from Huxley RR, Woodward M. Cigarette smoking as a risk factor for coronary heart disease
# in women compared with men: a systematic review and meta-analysis of prospective cohort studies. The Lancet. 2011 Oct 14;378(9799):1297–305. 
# Appendix webfigure 8
POP[cigst1 == "3" & sex == "1" , chd.tob.rr := stochRR(.N, 1.25, 1.32)]
POP[cigst1 == "3" & sex == "2" , chd.tob.rr := stochRR(.N, 1.20, 1.34)]

# RR for ETS He J, Vupputuri S, Allen K, Prerost MR, Hughes J, Whelton PK. Passive Smoking and the Risk of 
# Coronary Heart Disease — A Meta-Analysis of Epidemiologic Studies. New England Journal of Medicine. 1999;340(12):920–6. 
# Table 3. Adjusted RR
POP[, chd.ets.rr := 1]
POP[cigst1 %!in% c("4") & expsmokCat != "0", chd.ets.rr := stochRR(.N, 1.26, 1.38)]

# RR for SBP from Optimal SBP level at 115mmHg and RR(HR) of dying from CHD was taken from "Age-specific relevance of
# usual blood pressure to vascular mortality: 
# a meta-analysis of individual data for one million adults in 61 prospective studies. 
# The Lancet. 2002 Dec 14;360(9349):1903–1913" 
# Figure 5
POP[, omsysval.usual := usual.sys.bp(omsysval)] # calculate usual sbp. NEED TO improve for t>1
POP[, chd.sbp.rr := 1]
POP[age < 50 & sex == "1", chd.sbp.rr := stochRR(.N, 0.50^((115 - omsysval.usual)/20), 0.54^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[age < 50 & sex == "2", chd.sbp.rr := stochRR(.N, 0.40^((115 - omsysval.usual)/20), 0.49^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[between(age, 50, 59) & sex == "1", chd.sbp.rr := stochRR(.N, 0.50^((115 - omsysval.usual)/20), 0.52^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[between(age, 50, 59) & sex == "2", chd.sbp.rr := stochRR(.N, 0.49^((115 - omsysval.usual)/20), 0.54^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[between(age, 60, 69) & sex == "1", chd.sbp.rr := stochRR(.N, 0.55^((115 - omsysval.usual)/20), 0.57^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[between(age, 60, 69) & sex == "2", chd.sbp.rr := stochRR(.N, 0.60^((115 - omsysval.usual)/20), 0.61^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[between(age, 70, 79) & sex == "1", chd.sbp.rr := stochRR(.N, 0.62^((115 - omsysval.usual)/20), 0.64^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[between(age, 70, 79) & sex == "2", chd.sbp.rr := stochRR(.N, 0.55^((115 - omsysval.usual)/20), 0.58^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[age > 79 & sex == "1", chd.sbp.rr := stochRR(.N, 0.69^((115 - omsysval.usual)/20), 0.73^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[age > 79 & sex == "2", chd.sbp.rr := stochRR(.N, 0.64^((115 - omsysval.usual)/20), 0.68^((115 - omsysval.usual)/20)), by = omsysval.usual]
POP[chd.sbp.rr<1, chd.sbp.rr := 1]

# RR for Chol from "Blood cholesterol and 
# vascular mortality by age, sex, and blood pressure: a meta-analysis of individual data from 61 prospective studies 
# with 55.000 vascular deaths. The Lancet. 2007 Dec 7;370(9602):1829–39. 
# Figure 1 (I may use Appendix Webtable 6 for fully adjusted) 
POP[, cholval1.usual := usual.chol(cholval1)] # calculate usual chol. NEED TO improve for t>1
POP[, chd.chol.rr := 1]
POP[age < 50 & sex == "1", chd.chol.rr := stochRR(.N, 0.45^(3.8 - cholval1.usual), 0.48^(3.8 - cholval1.usual)), by = cholval1.usual]
POP[age < 50 & sex == "2", chd.chol.rr := stochRR(.N, 0.43^(3.8 - cholval1.usual), 0.55^(3.8 - cholval1.usual)), by = cholval1.usual]
POP[between(age, 50, 59) & sex == "1", chd.chol.rr := stochRR(.N, 0.59^(3.8 - cholval1.usual), 0.61^(3.8 - cholval1.usual)), by = cholval1.usual]
POP[between(age, 50, 59) & sex == "2", chd.chol.rr := stochRR(.N, 0.55^(3.8 - cholval1.usual), 0.61^(3.8 - cholval1.usual)), by = cholval1.usual]
POP[between(age, 60, 69) & sex == "1", chd.chol.rr := stochRR(.N, 0.71^(3.8 - cholval1.usual), 0.74^(3.8 - cholval1.usual)), by = cholval1.usual]
POP[between(age, 60, 69) & sex == "2", chd.chol.rr := stochRR(.N, 0.73^(3.8 - cholval1.usual), 0.78^(3.8 - cholval1.usual)), by = cholval1.usual]
POP[between(age, 70, 79) & sex == "1", chd.chol.rr := stochRR(.N, 0.80^(3.8 - cholval1.usual), 0.83^(3.8 - cholval1.usual)), by = cholval1.usual]
POP[between(age, 70, 79) & sex == "2", chd.chol.rr := stochRR(.N, 0.86^(3.8 - cholval1.usual), 0.90^(3.8 - cholval1.usual)), by = cholval1.usual]
POP[age > 79 & sex == "1", chd.chol.rr := stochRR(.N, 0.79^(3.8 - cholval1.usual), 0.84^(3.8 - cholval1.usual)), by = cholval1.usual]
POP[age > 79 & sex == "2", chd.chol.rr := stochRR(.N, 0.92^(3.8 - cholval1.usual), 0.97^(3.8 - cholval1.usual)), by = cholval1.usual]
POP[chd.chol.rr < 1, chd.chol.rr := 1]

# RR for BMI from "The Emerging Risk Factors Collaboration.
# Separate and combined associations of body-mass index and abdominal adiposity with cardiovascular disease:
# collaborative analysis of 58 prospective studies.
# The Lancet 2011;377:1085–95. doi:10.1016/S0140-6736(11)60105-0
# Table 1 (Adjusted for age, sex, smoking status, systolic blood pressure, history of diabetes, and total and HDL cholesterol)
POP[, chd.bmi.rr := 1]
POP[, chd.bmi.rr := stochRR(.N, 1.11^((bmival - 20) / 4.56), 1.17^((bmival - 20) / 4.56)), by = bmival]
POP[chd.bmi.rr < 1, chd.bmi.rr := 1]

# RR for BMI from The Emerging Risk Factors Collaboration. Diabetes mellitus, fasting blood glucose concentration, 
# and risk of vascular disease: a collaborative 
# meta-analysis of 102 prospective studies. The Lancet 2010;375:2215–22. doi:10.1016/S0140-6736(10)60484-9
# figure 2 (HRs were adjusted for age, smoking status, body-mass index, and  systolic blood pressure)
POP[, chd.diab.rr := 1]
POP[diabtotr == "2" & age < 60, chd.diab.rr := stochRR(.N, 2.51, 2.8)]
POP[diabtotr == "2" & between(age, 60, 69), chd.diab.rr := stochRR(.N, 2.01, 2.26)]
POP[diabtotr == "2" & age > 69, chd.diab.rr := stochRR(.N, 1.78, 2.05)]

# RR for F&V from From Dauchet L, Amouyel P, Hercberg S, Dallongeville J. Fruit and Vegetable Consumption and Risk of Coronary Heart Disease: 
# A Meta-Analysis of Cohort Studies. J Nutr. 2006 Oct 1;136(10):2588–93. 
# To avoid negative PAF an optimal level of F&V has to be set arbitrarily. I set it to 10 
# when convert porftvg from categorical to numeric I create bias. eg 1=less than 1 portion
POP[, chd.fv.rr := 1]
POP[porftvg < 97, chd.fv.rr := stochRR(.N, 0.96^(porftvg), 0.99^(porftvg)), by = porftvg] 

# Estimate prevalence of lung cancer only in first run when i does not exist yet
if (!exists("i")){
    cat("Estimating CHD prevalence in 2010...\n\n")
    
    age.structure <- table(POP[, sex, by = agegroup])
    age.structure <- data.table(age.structure, key = c("sex", "agegroup"))
    age.structure[CHDpreval, N := as.integer(N * prevalence)]
    setkey(POP, id)
    
    for (l in POP[, levels(sex)]) {
        for (k in unique(droplevels(POP[between(age, ageL, ageH), agegroup]))) {
            Temp <- sample_n(POP[cvdcon == 1 & agegroup == k & sex == "1",], age.structure[sex == "1" & agegroup == k, N], replace = F)
            setkey(Temp, id)
            POP[Temp, chd.incidence := 2010]
        }
    }
}


# P= p0 * chd.tob.rr * chd.ets.rr * chd.sbp.rr * chd.chol.rr * chd.bmi.rr * chd.diab.rr * chd.fv.rr
cat("Estimating CHD incidence...\n\n")
POP[between(age, ageL, ageH) & chd.incidence == 0, v := dice(.N) <= (p0 * chd.tob.rr * chd.ets.rr * 
                                                                         chd.sbp.rr * chd.chol.rr * 
                                                                         chd.bmi.rr * chd.diab.rr * chd.fv.rr)]
POP[,summary(as.factor(v))]

if (!exists("i")) {
    POP[v == T, chd.incidence := 2011]
} else {
    POP[v == T, chd.incidence := 2011+i]  
}

POP[, v := NULL]

if (!exists("Out.Inc.CHD")) {
    Out.Inc.CHD = copy(POP[chd.incidence == 2011])
} else {
    Temp = copy(POP[chd.incidence == 2011+i])
    Out.Inc.CHD <- rbind_all(list(Out.Inc.CHD, Temp))
}

Out.Inc.CHD <- data.table(Out.Inc.CHD, key="id")

# Estimate CHD mortality
cat("Estimating CHD mortality...\n\n")
POP <- merge(POP, CHDsurv, by = c("agegroup", "sex"), all.x = T)
setkey(POP, agegroup, sex)

if (!exists("i")) { # for 2011 that i doesn't exists yet    
    POP[chd.incidence > 0, v := dice(.N) <= fatality] # T= dead, F=alive 
    Out.Mort.CHD = copy(POP[v == T])
    Out.Mort.CHD[, `:=` (yearofdeath = 2011, v = NULL)]
} else {
    POP[chd.incidence > 0, v:= dice(.N) <= fatality] # T= dead, F=alive 
    Temp = copy(POP[v == T])
    Temp[, `:=` (yearofdeath = 2011+i, v = NULL)]
    Out.Mort.CHD = rbind_all(list(Out.Mort.CHD, Temp))
}
 
POP = copy(POP[v == F | is.na(v)==T,])
Out.Mort.CHD <- data.table(Out.Mort.CHD, key="id")

rm(length.of.POP)
POP[, `:=` (incidence = NULL, chd.tob.rr = NULL, p0 = NULL, chd.ets.rr = NULL, omsysval.usual = NULL, chd.sbp.rr = NULL, cholval1.usual = NULL,
            chd.chol.rr = NULL, chd.bmi.rr = NULL, chd.diab.rr = NULL, chd.fv.rr = NULL, v = NULL, fatality = NULL)] 
