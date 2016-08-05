#cmpfile("./lung cancer model.R")
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

cat("Loading lung cancer (C34) model...\n")
cat(paste0(Sys.time(), "\n\n"))
if (i == init.year - 2011) set(POP, NULL, "c34.incidence",  0) # Only needs to run the very first time of each simulation
POP[age == 0, c34.incidence := 0]

# Alignment ----------------------------
if ((alignment == T) & between(i, init.year - 2011 + 1, init.year - 2011 + 6)) {
  cat("alignmment ON\n")
  POP[sex == "2" & between(age, 55, 69), cigdyal := cigdyal * 1.1] # calibration
}

# RF to RR  -------------------------------
# PLCO model to estimate risk from: Tammemägi M, et al. Evaluation of the Lung
# Cancer Risks at which to Screen Ever- and Never-Smokers: Screening Rules
# Applied to the PLCO and NLST Cohorts. PLoS medicine. 2014;11(12): e1001764.
# I intentionally used cvdlag because it is closer to 6 years that was the duration of the study
set(POP, NULL, "c34.tob.rr", 1)
if (POP[cigst1.cvdlag != "1" & between(age, ageL, ageH), .N] > 0) {
  POP[cigst1.cvdlag != "1" & between(age, ageL, ageH),
      c34.tob.rr := bound(tob.cum.risk(age - cvd.lag, sex, qimd,
                                       bmival.cvdlag, cigst1.cvdlag,
                                       cigdyal.cvdlag, numsmok.cvdlag, smokyrs.cvdlag,
                                       endsmoke.cvdlag, origin, i - cvd.lag), 1, Inf)]
  POP[age > 69, c34.tob.rr := (c34.tob.rr-1) * (1-(age-69)/100)+1] # decrease risk for elderly
}


# RR for ETS from Kim CH, et al. Exposure to secondhand tobacco smoke and lung
# cancer by histological type: A pooled analysis of the International Lung
# Cancer Consortium (ILCCO). Int J Cancer 2014;135:1918–30.
# doi:10.1002/ijc.28835
# Very similar to Taylor R, et al. Meta-analysis of studies of passive smoking and lung cancer:
# effects of study type and continent. Int J Epidemiol. 2007 Jan
# 10;36(5):1048–59. Table 4 for Europe
set(POP, NULL, "c34.ets.rr", 1)
POP[expsmok.calag == "1" & cigst1 == "1", c34.ets.rr := c34.ets.rr.mc]
POP[age > 69, c34.ets.rr := (c34.ets.rr-1) * (1-(age-69)/100)+1] # decrease risk for elderly

# RR for fruit from Vieira AR, et al. Fruits, vegetables and lung cancer risk: a
# systematic review and meta-analysis. Ann Oncol 2016;27:81–96. 
# doi:10.1093/annonc/mdv381
# Very similar to Wang Y, et al. Fruit and vegetable
# consumption and risk of lung cancer: A dose–response meta-analysis of
# prospective cohort studies. Lung Cancer 2015;88:124–30.
# doi:10.1016/j.lungcan.2015.02.015
set(POP, NULL, "c34.fv.rr", 1)
POP[porftvg.calag < 6L, c34.fv.rr := c34.fv.rr.mc^(porftvg.calag - 5L)] # No benefit from >5 portions
POP[age > 69, c34.fv.rr := (c34.fv.rr-1) * (1-(age-69)/100)+1] # decrease risk for elderly

# Prevalence of C34 only in first run  ------------------------------------
if (i == init.year - 2011) {
  #cat(paste0("Estimating lung cancer prevalence in ",
            # init.year - 1L, " ...\n\n"))
  age.structure <- setkey(POP[age <= ageH, .N, by = .(age, sex)], age, sex)
  Temp <- merge(C34preval[age <= ageH], C34incid[age <= ageH], by = c("age", "sex"))
  Temp[, prevalence := prevalence - incidence]
  Temp[prevalence < 0, prevalence := 0]
  age.structure[Temp, Nprev := rbinom(.N, N, prevalence)]
  
  # dismod calculates fatality/remission from 
  # the prevalent cases and not the prevalent + incident cases 
  setnames(age.structure, "N", "population")
  POP <- merge(POP,
               deaths.causes.secgrad[
                 cause == "Malignant neoplasm of trachea, bronchus and lung",
                 .(agegroup, sex, qimd, sec.grad.adj)],
               by = c("agegroup", "sex", "qimd"), all.x = T)
  POP[is.na(sec.grad.adj), sec.grad.adj := 1]
  
  Temp <- POP[age <= ageH, 
              sample(id, 
                     age.structure[age == .BY[[1]] & sex == .BY[[2]] , Nprev], 
                     prob = c34.tob.rr * c34.fv.rr * c34.ets.rr * sec.grad.adj, 
                     replace = F), 
              by = .(age, sex)][, V1]
  POP[id %in% Temp, c34.incidence := init.year - 1L] # and then we assign these ids to the population
  POP[, sec.grad.adj := NULL]
}


# PAF estimation ----------------------------------------------------------
if (i == init.year - 2011) {
  #cat("Estimating lung cancer PAF...\n")
  c34paf <- 
    POP[between(age, ageL, ageH) & c34.incidence == 0, 
        .(paf = 1 - 1 / (sum(c34.tob.rr * c34.fv.rr * c34.ets.rr) / .N)), 
        by = .(age, sex)
        ]
  setkey(c34paf, age, sex)
  #c34paf[, paf := predict(loess(paf~age, span=0.2)), by = .(sex)]
  #c34paf[sex == "2" & between(age, 60, 79), paf := paf.sm]
  #c34paf[, paf := rollmean(paf, 5, fill = "extend", align = "center"), by = .(sex)]
  
  setkey(C34incid, age, sex)
  C34incid[c34paf, p0 := incidence * (1 - paf)]
  C34incid[is.na(p0), p0 := incidence]
}

setkey(POP, age, sex)
POP[C34incid, p0 := p0]

# Incidence of C34  ------------------------------------
#cat("Estimating lung cancer incidence...\n\n")
POP[between(age, ageL, ageH) &
      c34.incidence == 0 &
      dice(.N) < p0 * c34.tob.rr * c34.fv.rr * c34.ets.rr, 
    c34.incidence := 2011 + i] 

# Estimate C34 mortality --------------------------------------------------
#cat("Estimating lung cancer mortality...\n\n")
# Apply assumptions about improvement of fatality by year
if (i > init.year - 2011) {
  C34fatal[, fatality  := fatality  * (100 - fatality.annual.improvement.c34)/100]
  C34remis[, remission := remission * (100 + fatality.annual.improvement.c34)/100]
  # It make sense when fatality decreasing remission to increase and vice versa. 
  fatality.annual.improvement.c34 <- fatality.annual.improvement.c34 * 0.99
}

setkey(POP, age, sex)
POP[C34fatal, fatality := fatality]

Temp <- POP[between(age, ageL, ageH), 
            .(before = sum(c34.incidence > 0) * mean(fatality)),
            by = .(agegroup, sex)] #expected number of deaths

POP[between(age, ageL, 69) & qimd == "1", fatality := (100 - fatality.sec.gradient.c34 / 2) * fatality/100]
POP[between(age, ageL, 69) & qimd == "2", fatality := (100 - fatality.sec.gradient.c34 / 4) * fatality/100]
POP[between(age, ageL, 69) & qimd == "4", fatality := (100 + fatality.sec.gradient.c34 / 4) * fatality/100]
POP[between(age, ageL, 69) & qimd == "5", fatality := (100 + fatality.sec.gradient.c34 / 2) * fatality/100]
POP[between(age, 70, ageH) & qimd == "1", fatality := (100 - fatality.sec.gradient.c34 / 4) * fatality/100]
POP[between(age, 70, ageH) & qimd == "2", fatality := (100 - fatality.sec.gradient.c34 / 8) * fatality/100]
POP[between(age, 70, ageH) & qimd == "4", fatality := (100 + fatality.sec.gradient.c34 / 8) * fatality/100]
POP[between(age, 70, ageH) & qimd == "5", fatality := (100 + fatality.sec.gradient.c34 / 4) * fatality/100]

#expected number of deaths after gradient and needs to be corrected by Temp/Temp1
Temp1 <- POP[between(age, ageL, ageH), 
             .(after = sum(c34.incidence > 0) * mean(fatality)),
             by = .(agegroup, sex)] #expected number of deaths

Temp[Temp1, corr := before/after, on = c("agegroup", "sex")]
Temp[is.na(corr), corr := 1]
POP[Temp, fatality := fatality * corr, on = c("agegroup", "sex")]
POP[fatality > 1, fatality := 0.99]

# Mortality and remission need to be calculated on the same step
# I NEED TO implement also decreasing probability of death by duration
POP[C34remis, `:=` (remission = remission)]

Temp <- POP[between(age, ageL, ageH), 
            .(before = sum(c34.incidence > 0) * mean(remission)),
            by = .(agegroup, sex)] #expected number of deaths

POP[between(age, ageL, 69) & qimd == "1", remission := (100 + fatality.sec.gradient.c34 / 2) * remission/100]
POP[between(age, ageL, 69) & qimd == "2", remission := (100 + fatality.sec.gradient.c34 / 4) * remission/100]
POP[between(age, ageL, 69) & qimd == "4", remission := (100 - fatality.sec.gradient.c34 / 4) * remission/100]
POP[between(age, ageL, 69) & qimd == "5", remission := (100 - fatality.sec.gradient.c34 / 2) * remission/100]
POP[between(age, 70, ageH) & qimd == "1", remission := (100 + fatality.sec.gradient.c34 / 4) * remission/100]
POP[between(age, 70, ageH) & qimd == "2", remission := (100 + fatality.sec.gradient.c34 / 8) * remission/100]
POP[between(age, 70, ageH) & qimd == "4", remission := (100 - fatality.sec.gradient.c34 / 8) * remission/100]
POP[between(age, 70, ageH) & qimd == "5", remission := (100 - fatality.sec.gradient.c34 / 4) * remission/100]
#expected number of deaths after gradient and needs to be corrected by Temp/Temp1

Temp1 <- POP[between(age, ageL, ageH), 
             .(after = sum(c34.incidence > 0) * mean(remission)),
             by = .(agegroup, sex)] #expected number of deaths

Temp[Temp1, corr := before/after, on = c("agegroup", "sex")]
Temp[is.na(corr), corr := 1]
POP[Temp, remission := remission * corr, on = c("agegroup", "sex")]
POP[remission > 1, remission := 0.99]

POP[c34.incidence > 0, 
    `:=` (v = dice(.N) <= fatality + remission, # T = dead or cured (F = diseased)
          dc = dice(.N) <= fatality / (remission + fatality))] # T = dead (F = cured)

#POP[c34.incidence > 0 & v == T & dc == T, .N] # dead
#POP[c34.incidence > 0 & v == T & dc == F, .N] # cured

POP[, dead := as.logical(v * dc)]
POP[v == T & dead == F, 
    `:=` (c34.remission = paste0(c34.incidence, " - ", i + 2011))]
if ("c34.remission" %!in% names(POP)) set(POP, NULL, "c34.remission", NA_character_)

#cat("Export lung cancer burden summary...\n\n")
#cat(paste0(Sys.time(), "\n\n"))
if (i == init.year - 2011) c34.burden <- vector("list", yearstoproject * 5)

c34.burden[[(2011 - init.year + i) * 5 + 1]] <-
  output.c34(POP, c("qimd", "sex", "agegroup"))

c34.burden[[(2011 - init.year + i) * 5 + 2]] <- 
  output.c34(POP, c("sex", "agegroup"))

c34.burden[[(2011 - init.year + i) * 5 + 3]] <-
  output.c34(POP, c("qimd", "sex"))

c34.burden[[(2011 - init.year + i) * 5 + 4]] <- 
  output.c34(POP, c("sex"))

c34.burden[[(2011 - init.year + i) * 5 + 5]] <- 
  output.c34(POP, c())

if (i == yearstoproject + init.year - 2012) {
  saveRDS(rbindlist(c34.burden, T, T)
          , file = paste0(output.dir(), "c34.burden.rds"))
}

#cat("Export lung cancer burden individuals...\n\n")
indiv.incid[[which(diseasestoexclude=="C34")]] <- 
  POP[c34.incidence == 2011 + i,
      .(age, sex, qimd, agegroup, eqv5, id, hserial, hpnssec8, sha)
      ][, `:=` (scenario = gsub(".Rc", "", scenarios.list[[iterations]]),
                mc = haha, year = 2011 + i, cause = "c34")]

indiv.mort[[which(diseasestoexclude == "C34") + 1]] <-
  POP[dead == T, 
      .(age, sex, qimd, agegroup, eqv5, id, hserial, hpnssec8, sha)
      ][,
        `:=` (year = 2011 + i, cause = "c34",
              scenario = gsub(".Rc", "", scenarios.list[[iterations]]), 
              mc = haha)]
POP[v == T & dead == F, 
    `:=` (c34.incidence = 0)]
POP[,`:=` (v = NULL, dc = NULL)] 

POP = copy(POP[dead == F | is.na(dead)== T,])

rm(Temp, Temp1)

POP[, `:=` (c34.tob.rr = NULL, p0 = NULL,
            c34.ets.rr = NULL, c34.fv.rr = NULL,
            dead = NULL, fatality = NULL, remission = NULL)] 
setkey(POP, age, sex, qimd)
