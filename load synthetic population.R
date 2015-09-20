#cmpfile("./load synthetic population.R")
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

rm(tt, SPOP)
# sink() 
