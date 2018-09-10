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
  SPOP[, `:=`(hsize             = NULL, 
              diabtype          = NULL, 
              bpmedc            = NULL,
              lipid             = NULL,
              cigst1.raw        = NULL,
              diabtotr.raw      = NULL)] 
} else {
  SPOP[, `:=`(hsize             = NULL, 
              diabtyper         = NULL, 
              bpmedc            = NULL,
              lipid             = NULL,
              segment           = NULL,
              saltCat.intersalt = NULL)]  
}
SPOP[cigst1 == "1", smokyrs := 0L]
agegroup.fn(SPOP)

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

POP[cigst1 == "4" & cigdyal < 1L, cigdyal := 1L]
POP[cigst1 == "3" & numsmok < 1L, numsmok := 1L]

POP[, `:=` (cigdyal = as.numeric(cigdyal), numsmok = as.numeric(numsmok))]

POP[, salt_rank := (frank(salt.intersalt, na.last = F, ties.method = "random") - 1)/(.N - 1), by = .(age, sex)]
POP[salt_rank == 0, salt_rank := 0.0001]
POP[salt_rank == 1, salt_rank := 0.9999]
rm(tt, SPOP, origin.multinom)
# sink() 
