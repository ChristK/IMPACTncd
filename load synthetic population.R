#cmpfile("./load synthetic population.R")
# Define function for output dir

# sink(file = paste0(output.dir(), "log.txt"),
#      append = T, 
#      type = "output",
#      split = F)
cat("Loading synthetic population...\n\n")
random.pop.file <- sample(list.files("./SynthPop", 
                                     pattern = glob2rx("spop2011*.rds"), 
                                     full.names = T), 
                          1) # pick a random file from the available population files
SPOP2011 <- readRDS(file = random.pop.file)


# sink(file = paste0(output.dir(), "synthetic population.txt"),
#      append = T, 
#      type = "output",
#      split = F)
# cat(paste0("Population file = ", random.pop.file), "\n")
# cat(paste0(scenarios.list[[iterations]], "\n"))
# sink()


setDT(SPOP2011)
SPOP2011[, `:=`(hsize = NULL, 
                totalwug = NULL, 
                diabtyper = NULL, 
                bpmedc = NULL,
                lipid = NULL,
                segment = NULL,
                omsysvalCat = NULL,
                saltCat.intersalt = NULL)]  

# SPOP2011[, omsysvalCat := cut(omsysval, 
#                               breaks = c(0,130,160,Inf), 
#                               labels = c("normotensive", "hypertensive", "severely hypertensive"), 
#                               include.lowest = T, 
#                               right = F, 
#                               ordered_result = T)]
# SPOP2011[, age := as.integer(as.character(age))]
# SPOP2011[, numsmok := as.numeric(as.character(numsmok))]
# SPOP2011[, cigdyalCat := as.numeric(cigdyalCat)]
# SPOP2011[cigst1 == "2", numsmok:= 0.5]
# SPOP2011[expsmokCat != "0", expsmokCat:= "1"][,expsmokCat := factor(expsmokCat)]
# SPOP2011[, a30to06m := as.integer(as.character(a30to06m))]
# SPOP2011[age<15, cigst1 := "1"]
# SPOP2011[age > 99, age := 99] # for combatibility with lifetables
agegroup.fn(SPOP2011)
# # create individuals with age ==98 (originally missing) by assigning half of those aged 99, to age 98
# SPOP2011[id %in% (sample_frac(SPOP2011[age==99,.(id)], 0.5)[, id]), age := 98] 
# 
# # Same technic for other sex/ages
# SPOP2011[id %in% (sample_frac(SPOP2011[age==96 & sex=="2",.(id)], 0.5)[, id]), age := 95] 
# SPOP2011[id %in% (sample_frac(SPOP2011[age==95 & sex=="1",.(id)], 0.3)[, id]), age := 96] 
# SPOP2011[id %in% (sample_frac(SPOP2011[age==97 & sex=="1",.(id)], 0.3)[, id]), age := 98] 
# SPOP2011[id %in% (sample_frac(SPOP2011[age==97 & sex=="1",.(id)], 0.3)[, id]), age := 99] 

# Stratified sampling
SPOP2011[, age2 := age]
SPOP2011[age2>90, age2 := 90]
SPOP2011[, id:= seq_len(.N)]
tt <- SPOP2011[, sample(id, population.actual[age==.BY[1] & sex==.BY[2] & qimd==.BY[3], pct2], T), by = .(age2, sex, qimd)][, V1]
POP = copy(SPOP2011[tt, ])
# SPOP2011 = copy(SPOP2011[, sample_n(.SD, population.actual[age==.BY[1] & sex==.BY[2] & qimd==.BY[3], pct2], T), by = .(age2, sex, qimd)])
# SPOP2011[, age2 := NULL]
POP[, age2 := NULL]

POP <- POP[age > 0,] # Delete all newborns from the population (an overestimation, probably because data collection lasted more than a year)
POP[, id:= seq_len(.N)]

# datasets for ageing.distr function
bmi.rank <- setkey(
  SPOP2011[bmival>0, list(bmival, 
                          "bmival.rank" = (frank(bmival, 
                                                 na.last = F, 
                                                 ties.method = "random")-1)/(.N - 1)), by = group],
  group, bmival.rank)

sbp.rank <- setkey(
  SPOP2011[omsysval>0, list(omsysval, 
                            "omsysval.rank" = (frank(omsysval, 
                                                     na.last = F, 
                                                     ties.method = "random")-1)/(.N - 1)), by = group],
  group, omsysval.rank)

chol.rank <- setkey(
  SPOP2011[cholval>0, list(cholval, 
                           "cholval.rank" = (frank(cholval, 
                                                   na.last = F, 
                                                   ties.method = "random")-1)/(.N - 1)), by = group],
  group, cholval.rank)

rm(random.pop.file, tt, SPOP2011)
# sink() 
