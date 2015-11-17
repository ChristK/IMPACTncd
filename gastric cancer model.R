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
# "the HR were very similar (HR of the log-2 of cigarette-years 1.040 in males and 1.040 in females),
# both statistically significant". I assume CI from 1 to 1.08 (see Simulation.R)
# I use CVD lag time for it because EPIC has a follow up of 5 years. 
cat("smoking RR\n")
set(POP, NULL, "c16.tob.rr", 1)
POP[cigst1.cvdlag %in% c("2", "3", "4") & packyears > 0,
    `:=` (c16.tob.rr = c16.tob.rr.mc^log2(packyears * 20))] # packyears * 20 = cigarette years
POP[cigst1.cvdlag %in% c("2", "3") & endsmoke>0, `:=` (c16.tob.rr = c16.tob.rr * (c16.extob.rr.mc^log2(endsmoke)))] # packyears * 20 = cigarette years
POP[c16.tob.rr < 1 | endsmoke > 20, c16.tob.rr := 1] #ex smokersof more than 20 years 
# atract no risk. This is not true when PAF is calculated. The results seem more accurate
# this way

# Calculate RR for C16 from Comparative quantification of health risks [Internet].
# Geneva: World Health Organisation; 2004. 
# Chapter 9 table 9.28 (p100 in pdf)
cat("f&v RR\n")
setkey(POP, agegroup)
set(POP, NULL, "c16.fv.rr", 1)
POP[c16.fv.rr.mc,  c16.fv.rr := rr^(porftvg.calag - 7)] # no effect for more than 7 portions
POP[c16.fv.rr < 1, c16.fv.rr := 1]
POP[is.na(c16.fv.rr), c16.fv.rr := 1]
# Calculate RR for C16. From World Cancer Research Fund, 
# American Institute for Cancer Research. Food, nutrition, 
# physical activity, and the prevention of cancer:
# a global perspective. Washington, DC: WCRF/AICR; 2007.
# p145. RR 1.08 (1 - 1.17) per 1gr/day of salt. Risk starts from
# 3gr/day (around reference category of the 2 cohort studies) 
cat("salt RR\n")
set(POP, NULL, "c16.salt.rr", 1)
POP[c16.salt.rr.mc, c16.salt.rr := rr^(salt24h.calag - c16.salt.optim)]
POP[c16.salt.rr < 1, c16.salt.rr := 1]

cat("Estimating gastric cancer PAF...\n")
if (i == init.year - 2011) {
  c16tobpaf <- 
    POP[between(age, ageL, ageH), 
        .(tobpaf = 
            (sum(c16.tob.rr - 1, na.rm = T) / .N) / 
            ((sum(c16.tob.rr - 1, na.rm = T) / .N) + 1)
        ), 
        by = .(agegroup, sex)
        ]
  setkey(c16tobpaf, agegroup, sex)
  
  c16fvpaf <- 
    POP[between(age, ageL, ageH), 
        .(fvpaf = 
            (sum(c16.fv.rr - 1, na.rm = T) / .N) / 
            ((sum(c16.fv.rr - 1, na.rm = T) / .N ) + 1)
        ), 
        by = .(agegroup, sex)
        ]
  setkey(c16fvpaf, agegroup, sex)
  
  c16saltpaf <- 
    POP[between(age, ageL, ageH), 
        .(saltpaf = 
            (sum(c16.salt.rr - 1, na.rm = T) / .N) / 
            ((sum(c16.salt.rr - 1, na.rm = T) / .N ) + 1)
        ), 
        by = .(agegroup, sex)
        ]
  setkey(c16saltpaf, agegroup, sex)
  
  C16incid[, agegroup := agegroup.fn(age)]
  setkey(C16incid, agegroup, sex)
  C16incid[c16tobpaf[c16fvpaf[c16saltpaf]],
           p0 := incidence * (1 - tobpaf) * (1 - fvpaf) * (1 - saltpaf)]
  C16incid[is.na(p0), p0 := incidence]
  C16incid[, agegroup := NULL]
  setkey(C16incid, NULL)
  rm(c16tobpaf, c16saltpaf, c16fvpaf)
}

setkey(POP, age, sex)
POP[C16incid, p0 := p0]


# Prevalence of C16 only in first run  ------------------------------------
if (i == init.year - 2011) {
  cat(paste0("Estimating gastric cancer prevalence in ",
             init.year - 1, " ...\n\n"))
  age.structure <- setkey(POP[age <= ageH, .N, by = .(age, sex)], age, sex)
  Temp <- merge(C16preval[age <= ageH], C16incid[age <= ageH], by = c("age", "sex"))
  Temp[, prevalence := prevalence - incidence][prevalence < 0, prevalence := 0]
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
              sample_n(.SD, 
                       age.structure[age == .BY[[1]] & sex == .BY[[2]] , Nprev], 
                       weight = c16.tob.rr * c16.fv.rr * c16.salt.rr *
                         sec.grad.adj, 
                       replace = F), 
              by = .(age, sex)][, id]
  POP[id %in% Temp, c16.incidence := init.year - 1] # and then we assign these ids to the population
  POP[, sec.grad.adj := NULL]
}

# correction factor NEED TO make it work only for i==0
if (alignment == T) {
  if (i == init.year - 2011) {
    corr.factor.c16 <- merge(
      POP[between(age, ageL, ageH) & c16.incidence == 0, 
          mean(p0 * c16.tob.rr * c16.fv.rr * c16.salt.rr),
          by = .(age, sex)], 
      C16incid, 
      by = c("age", "sex"), 
      all.x = T)
    corr.factor.c16[, b := incidence/V1]
    corr.factor.c16[,  `:=` (p0 = NULL, incidence = NULL, V1 = NULL)]
    POP <- merge(POP, corr.factor.c16, by = c("age", "sex"), all.x = T)
  } else {
    POP <- merge(POP, corr.factor.c16, by = c("age", "sex"), all.x = T)
  }
} else set(POP, NULL, "b", 1)

cat("Estimating gastric cancer incidence...\n\n")
if (alignment == T) cat("Alignment will be performed\n\n")
POP[between(age, ageL, ageH) &
      c16.incidence == 0 &
      dice(.N) < p0 * c16.tob.rr * c16.fv.rr * c16.salt.rr * b, 
    c16.incidence := 2011 + i] # b is the correction factor

# Estimate C16 mortality 
cat("Estimating gastric cancer mortality...\n\n")

# Apply assumptions about improvement of fatality by year
if (i > init.year - 2011) {
  C16fatal[, fatality  := fatality  * (100 - fatality.annual.improvement.c16)/100]
  C16remis[, remission := remission * (100 + fatality.annual.improvement.c16)/100]
  # It make sense when fatality decreasing remission to increase and vice versa. 
  # The increase of remission was set to half the decrease in survival to adjust
  # for increase survival but not cure 
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

cat("Export gastric cancer burden summary...\n\n")
cat(paste0(Sys.time(), "\n\n"))
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

cat("Export gastric cancer burden individuals...\n\n")
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

POP[, `:=` (c16.tob.rr = NULL, p0 = NULL, b = NULL,
            c16.salt.rr = NULL, c16.fv.rr = NULL,
            dead = NULL, fatality = NULL, remission = NULL)] 
setkey(POP, age, sex, qimd)
