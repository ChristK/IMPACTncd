#cmpfile("./load synthetic population.R")
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


# Define function for output dir

cat("Loading synthetic population...\n\n")

SPOP <- readRDS(file = random.spop.file[[counter[[iterations]]]])

setDT(SPOP)
if (init.year == 2006) {
  SPOP[, `:=`(hsize = NULL, 
                  diabtype = NULL, 
                  bpmedc = NULL,
                  lipid = NULL,
                  omsysvalCat = NULL,
                  saltCat.intersalt = NULL)] 
} else {
  SPOP[, `:=`(hsize = NULL, 
                  totalwug = NULL, 
                  diabtyper = NULL, 
                  bpmedc = NULL,
                  lipid = NULL,
                  segment = NULL,
                  omsysvalCat = NULL,
                  saltCat.intersalt = NULL)]  
}
# SPOP[, omsysvalCat := cut(omsysval, 
#                               breaks = c(0,130,160,Inf), 
#                               labels = c("normotensive", "hypertensive", "severely hypertensive"), 
#                               include.lowest = T, 
#                               right = F, 
#                               ordered_result = T)]
# SPOP[, age := as.integer(as.character(age))]
# SPOP[, numsmok := as.numeric(as.character(numsmok))]
# SPOP[, cigdyalCat := as.numeric(cigdyalCat)]
# SPOP[cigst1 == "2", numsmok:= 0.5]
# SPOP[expsmokCat != "0", expsmokCat:= "1"][,expsmokCat := factor(expsmokCat)]
# SPOP[, a30to06m := as.integer(as.character(a30to06m))]
# SPOP[age<15, cigst1 := "1"]
# SPOP[age > 99, age := 99] # for combatibility with lifetables
agegroup.fn(SPOP)
# # create individuals with age ==98 (originally missing) by assigning half of those aged 99, to age 98
# SPOP[id %in% (sample_frac(SPOP[age==99,.(id)], 0.5)[, id]), age := 98] 
# 
# # Same technic for other sex/ages
# SPOP[id %in% (sample_frac(SPOP[age==96 & sex=="2",.(id)], 0.5)[, id]), age := 95] 
# SPOP[id %in% (sample_frac(SPOP[age==95 & sex=="1",.(id)], 0.3)[, id]), age := 96] 
# SPOP[id %in% (sample_frac(SPOP[age==97 & sex=="1",.(id)], 0.3)[, id]), age := 98] 
# SPOP[id %in% (sample_frac(SPOP[age==97 & sex=="1",.(id)], 0.3)[, id]), age := 99] 

# Stratified sampling
SPOP[, age2 := age]
SPOP[age2>90, age2 := 90]
SPOP[, id:= seq_len(.N)]
if (paired) set.seed(seed[[counter[[iterations]]]])
tt <- SPOP[, sample(id, population.actual[age==.BY[1] & sex==.BY[2] & qimd==.BY[3], pct2], T), by = .(age2, sex, qimd)][, V1]
POP = copy(SPOP[tt, ])
# SPOP = copy(SPOP[, sample_n(.SD, population.actual[age==.BY[1] & sex==.BY[2] & qimd==.BY[3], pct2], T), by = .(age2, sex, qimd)])
# SPOP[, age2 := NULL]
POP[, age2 := NULL]

POP <- POP[age > 0,] # Delete all newborns from the population (an overestimation, probably because data collection lasted more than a year)
POP[, id:= seq_len(.N)]

# datasets for ageing.distr function
bmi.rank <- setkey(
  SPOP[bmival>0, list(bmival, 
                          "bmival.rank" = (frank(bmival, 
                                                 na.last = F, 
                                                 ties.method = "random")-1)/(.N - 1)), by = group],
  group, bmival.rank)

sbp.rank <- setkey(
  SPOP[omsysval>0, list(omsysval, 
                            "omsysval.rank" = (frank(omsysval, 
                                                     na.last = F, 
                                                     ties.method = "random")-1)/(.N - 1)), by = group],
  group, omsysval.rank)

chol.rank <- setkey(
  SPOP[cholval>0, list(cholval, 
                           "cholval.rank" = (frank(cholval, 
                                                   na.last = F, 
                                                   ties.method = "random")-1)/(.N - 1)), by = group],
  group, cholval.rank)


# Define origin (ethnicity) -----------------------------------------------
load(file="./Lagtimes/origin.multinom.rda")
# 1 = white # 2 = indian # 3 = pakistani # 4 = bangladeshi # 5 = other asian
# 6 = black caribbean # 7 = black african # 8 = chinese # 9 = other
if (paired) set.seed(seed[[counter[[iterations]]]])
POP[, origin := as.integer(pred.origin(age, sex, qimd))]

# Estimate Townsend score
# townsend score 2001, -6 to 11, higher score means more deprived 
# (like qimd). 
# quintiles from census 2001, 1: -4.95 to -2.63
#                             2: -2.63 to -1.67
#                             3: -1.67 to -0.27
#                             4: -0.27 to 2.2
#                             5:   2.2 to 20.67
if (paired) set.seed(seed[[counter[[iterations]]]])
POP[qimd == "1", townsend := runif(.N,   -7, -3.4)]
if (paired) set.seed(seed[[counter[[iterations]]]])
POP[qimd == "2", townsend := runif(.N, -3.4, 0.2)]
if (paired) set.seed(seed[[counter[[iterations]]]])
POP[qimd == "3", townsend := runif(.N,  0.2, 3.8)]
if (paired) set.seed(seed[[counter[[iterations]]]])
POP[qimd == "4", townsend := runif(.N,  3.8, 7.4)]
if (paired) set.seed(seed[[counter[[iterations]]]])
POP[qimd == "5", townsend := runif(.N,  7.4, 11)]

rm(tt, SPOP, origin.multinom)
# sink() 
