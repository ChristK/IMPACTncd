#cmpfile("./load synthetic population.R")
# Define function for output dir
sink(file = paste0(output.dir(), "log.txt"),
     append = T, 
     type = "output",
     split = F)

random.pop.file <- sample(list.files("./SynthPop", pattern = glob2rx("spop2011*.RData"), full.names = T), 1) # pick a random file from the available population files


cat("Loading synthetic population...\n\n")
# sink(file = paste0(output.dir(), "synthetic population.txt"),
#      append = T, 
#      type = "output",
#      split = F)
# cat(paste0("Population file = ", random.pop.file), "\n")
# cat(paste0(scenarios.list[[iterations]], "\n"))
# sink()

load(file = random.pop.file)
SPOP2011[, `:=`(hsize = NULL, 
                totalwug = NULL, 
                diabtyper = NULL, 
                bpmedc = NULL,
                lipid = NULL,
                omdiaval = NULL,
                segment = NULL,
                t59su06 = NULL,
                omsysvalCat = NULL)]  

# SPOP2011[, omsysvalCat := cut(omsysval, 
#                               breaks = c(0,130,160,Inf), 
#                               labels = c("normotensive", "hypertensive", "severely hypertensive"), 
#                               include.lowest = T, 
#                               right = F, 
#                               ordered_result = T)]
SPOP2011[cigst1 == "2", numsmok:= 0.5]
SPOP2011[expsmokCat != "0", expsmokCat:= "1"][,expsmokCat := factor(expsmokCat)]
SPOP2011[age<16, cigst1 := "1"]

SPOP2011[age > 99, age := 99] # for combatibility with lifetables

# # create individuals with age ==98 (originally missing) by assigning half of those aged 99, to age 98
# SPOP2011[id %in% (sample_frac(SPOP2011[age==99,.(id)], 0.5)[, id]), age := 98] 
# 
# # Same technic for other sex/ages
# SPOP2011[id %in% (sample_frac(SPOP2011[age==96 & sex=="2",.(id)], 0.5)[, id]), age := 95] 
# SPOP2011[id %in% (sample_frac(SPOP2011[age==95 & sex=="1",.(id)], 0.3)[, id]), age := 96] 
# SPOP2011[id %in% (sample_frac(SPOP2011[age==97 & sex=="1",.(id)], 0.3)[, id]), age := 98] 
SPOP2011[id %in% (sample_frac(SPOP2011[age==97 & sex=="1",.(id)], 0.3)[, id]), age := 99] 

# Stratified sampling
SPOP2011[, age2 := age]
SPOP2011[age2>90, age2 := 90]
POP = copy(SPOP2011[, sample_n(.SD, population.actual[age==.BY[1] & sex==.BY[2] & qimd==.BY[3], pct], T), by = .(age2, sex, qimd)])
SPOP2011[, age2 := NULL]
POP[, age2 := NULL]

POP = copy(POP[age > 0,]) # Delete all newborns from the population (an overestimation, probably because data collection lasted more than a year)
POP[, id:= 1:.N]

rm(random.pop.file)
sink() 
