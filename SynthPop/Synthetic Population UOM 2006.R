#!/opt/gridware/apps/gcc/R/3.2.3/lib64/R/bin/Rscript
# ************************************************************************************************
#                  England's synthetic population
# ************************************************************************************************

# User input
iterations <- 50 # Number of synthetic population to produce
prop <- 1/iterations  # Set proportion of the populations to be synthesized (set 1 for total population)
clusternumber <- 25 # Number of cores to be used ONLY VALID FOR LINUX


# Preample ----------------------------------------------------------------
gc()
dependencies <- function(x) {
  for (j in x) {
    # require returns TRUE invisibly if it was able to load package
    if (!require(j, character.only = TRUE)) {
      # If package was not able to be loaded then re-install
      install.packages(j, dependencies = TRUE)
      # Load package after installing
      require(j, character.only = TRUE)
    }
  }
}

# Then try/install packages...
dependencies(c("simPopulation",
               "data.table",
               "pryr",
               "dplyr", 
               "Hmisc",
               "doParallel",
               "compiler",
               "survey",
               "StatMatch"))

# Define function to clear labels form SPSS labelled imports
clear.labels <- function(x) {
  if(is.list(x)) {
    for(i in 1 : length(x)) class(x[[i]]) <- setdiff(class(x[[i]]), "labelled") 
    for(i in 1 : length(x)) attr(x[[i]],"label") <- NULL
  }
  else {
    class(x) <- setdiff(class(x), "labelled")
    attr(x, "label") <- NULL
  }
  return(invisible(x))
}

# Define function to calculate SD from svy objects
svysd <- function(...) sqrt(coef(svyvar(...)))

# Define function to split agegroups and create groups
agegroup.fn <- cmpfun(function(x, lagtime = 0) {
  breaks                   <- c(0, 1, seq(5, 85, 5), Inf)
  labels                   <- c("<1   ", "01-04", "05-09",
                                "10-14", "15-19", "20-24", 
                                "25-29", "30-34", "35-39", 
                                "40-44", "45-49", "50-54",
                                "55-59", "60-64", "65-69",
                                "70-74", "75-79", "80-84", 
                                "85+")
  if (is.numeric(x)) { 
    agegroup = cut(x + lagtime, 
                   breaks = breaks, 
                   labels = labels, 
                   include.lowest = T, 
                   right = F, 
                   ordered_result = T)
    return(invisible(agegroup))    
  } else {
    if (is.data.table(x)) {
      x[, agegroup := cut(age + lagtime, 
                          breaks = breaks, 
                          labels = labels, 
                          include.lowest = T, 
                          right = F, 
                          ordered_result = T)]
      setkey(x, qimd, sex, agegroup)
      x[, group := rleid(qimd, sex, agegroup)]
      return(invisible(x))
    } else return(print("only datatables and vectors are eligible inputs"))
  }
}
)

options(survey.lonely.psu = "adjust") #Lonely PSU (center any single-PSU strata around the sample grand mean rather than the stratum mean)

# Import datasets (household and individual files)
load("hse05ai.RData")
HSE2005original <- clear.labels(HSE2005original)
HSE2005original <- setDT(HSE2005original, key="age")
HSE2005original <- HSE2005original[samptype==1] 
setnames(HSE2005original, c("imd2004", "area", "wt.bldel"), c("qimd", "psu", "wt.blood"))
agegroup.fn(HSE2005original)
HSE2005original[, `:=` (cholval1 = cholval1 + 0.1, hdlval1 = hdlval1 - 0.1)]
HSE2005original[diabete2 == 2, diabtotr := 1]
HSE2005original[diabete2 == 1 | glyhbval > 6.5, diabtotr := 2]
HSE2005original[porftvg == 0, porftvg := 1L]
HSE2005original[porftvg <= 9L, porftvg := porftvg - 1L]
HSE2005original[, `:=`(year=-6, a30to06 = NA, wt.urine = wt.nurse)]
HSE2005original[, psu := paste0(psu, "2005")]
HSE2005original[, cluster := paste0(cluster, "2005")]

load(file="hse07ai.RData")
HSE2007original <- setDT(clear.labels(HSE2007original), key = "age")
HSE2007original <- HSE2007original[samptype == 1]
setnames(HSE2007original, c("imd2007", "area"), c("qimd", "psu"))
agegroup.fn(HSE2007original)
HSE2007original[, `:=`(year=-4, diabtotr = NA, diabete2 = NA, wt.blood = 1, cholval1 = NA, a30to06 = NA, wt.urine = wt.nurse)]
HSE2007original[, psu := paste0(psu, "2007")]
HSE2007original[, cluster := paste0(cluster, "2007")]

load("hse06ai.RData")
HSE2006original <- clear.labels(HSE)
rm(HSE)
setDT(HSE2006original, key="age")
HSE2006original <- HSE2006original[samptype != 3]
setnames(HSE2006original, c("imd2004", "newsha"), c("qimd", "sha"))
HSE2006original[, cholval1 := cholval1 + 0.1]
agegroup.fn(HSE2006original)
HSE2006original[diabete2 == 2, diabtotr := 1]
HSE2006original[diabete2 == 1 | glyhbval > 6.5, diabtotr := 2]
HSE2006original[, a30to06m := round(a30to06/4)]
HSE2006original[porftvg == 0, porftvg := 1L]
HSE2006original[porftvg <= 9L, porftvg := porftvg - 1L]
HSE2006original[, `:=`(year=-5, wt.urine = wt.nurse)]
HSE2006original[, psu := paste0(psu, "2006")]
HSE2006original[, cluster := paste0(cluster, "2006")]

load("hse06ah.RData")
HSE2006hh <- clear.labels(HSE2006hh)
setDT(HSE2006hh, key="age")
HSE2006hh <- HSE2006hh[samptype != 3, ]
tt <- unique(HSE2006original[, wt.hhld, by = hserial])
HSE2006hh[tt, on = "hserial", wt.hhld := wt.hhld]

# Intersalt formula -------------------------------------------------------
Na24.men <- function(n, Na, cre, K, bmi, age) {
  m.interc %<a-% rnorm(n, 25.46, 16.63)
  m.Na %<a-% rnorm(n, 0.46, 0.02) # mmol/L
  m.cre %<a-% rnorm(n, -2.75, 0.22)
  m.K %<a-% rnorm(n, -0.13, 0.04)
  m.bmi %<a-% rnorm(n, 4.10, 0.31)
  m.age %<a-% rnorm(n, 0.26, 0.78)
  m.age2 %<a-% rnorm(n, 0, 0.01)
  m.region %<a-% rnorm(n, 23.17, 4.51) # Northern Europe
  return(m.interc + m.region + 
           m.Na*Na + m.cre*cre + m.K*K + m.bmi*bmi + m.age * age + m.age2 * age^2)
}

Na24.women <- function(n, Na, cre, K, bmi, age) {
  w.interc %<a-% rnorm(n, 5.07, 13.42)
  w.Na %<a-% rnorm(n, 0.34, 0.02) # mmol/L
  w.cre %<a-% rnorm(n, -2.16, 0.20)
  w.K %<a-% rnorm(n, -0.09, 0.03)
  w.bmi %<a-% rnorm(n, 2.39, 0.20)
  w.age %<a-% rnorm(n, 2.35, 0.65)
  w.age2 %<a-% rnorm(n, -0.03, 0.01)
  w.region %<a-% rnorm(n, 15.73, 3.62) # Northern Europe
  return(w.interc + w.region + 
           w.Na*Na + w.cre*cre + w.K*K + w.bmi*bmi + w.age * age + w.age2 * age^2)
}

Na24.men.det <- function(n, Na, cre, K, bmi, age) {
  m.interc <-  25.46
  m.Na     <-  0.46 # mmol/L
  m.cre    <- -2.75
  m.K      <- -0.13
  m.bmi    <-  4.10
  m.age    <-  0.26
  m.age2   <-  0
  m.region <-  23.17 # Northern Europe
  return(m.interc + m.region + 
           m.Na*Na + m.cre*cre + m.K*K + m.bmi*bmi + m.age * age + m.age2 * age^2)
}

Na24.women.det <- function(n, Na, cre, K, bmi, age) {
  w.interc <-  5.07 
  w.Na     <-  0.34  # mmol/L
  w.cre    <- -2.16 
  w.K      <- -0.09 
  w.bmi    <-  2.39 
  w.age    <-  2.35 
  w.age2   <- -0.03 
  w.region <-  15.73  # Northern Europe
  return(w.interc + w.region + 
           w.Na*Na + w.cre*cre + w.K*K + w.bmi*bmi + w.age * age + w.age2 * age^2)
}

HSE2006original[eqv5 < 0, eqv5 := NA]
HSE2006original[hpnssec8 > 8, hpnssec8 := NA]
HSE2006original[is.na(porftvg) == T & age < 5, porftvg := 99]  # code ages of 0-5 as 99 = not  applicable

HSE2006original[is.na(cigdyal) == T & age < 16, cigdyal := 0]  # code ages of 0-15 as non smokers
HSE2006original[is.na(cigst1) == T & age < 16, cigst1 := 1]  # code ages of 0-15 as never smokers
HSE2006original[is.na(endsmoke) & (cigst1 == 1 | cigst1 == 4), endsmoke := 0]  # code non ex smokers as 0
HSE2006original[is.na(numsmok) & cigst1 !=3, numsmok := 0]  # code non ex smokers as 0
setnames(HSE2006original, "hhsize", "hsize")

# Calculate actual lld prescription and overwrite "lipid" variable
HSE2006original[, lipid := 0L]
for (i in 1:9) {
  nam <- paste("medbi0", i, sep="")
  man <- paste("medbia", i, sep="")
  HSE2006original[get(nam) == 21200 & get(man) == 1, lipid := 1L ]
}
for (i in 10:22) {
  nam <- paste("medbi", i, sep="")
  man <- paste("medbia", i, sep="")
  HSE2006original[get(nam) == 21200 & get(nam) == 1, lipid := 1L ]
}
HSE2006original[statins == 1, lipid := 1L] # incluse OTC statins
HSE2006original[, lipid := as.factor(lipid)]

# Rescale weights of the household files (page 25 of HSE2011-Methods-and-docs.pdf)
# pop2010 <- 51818267 # Mid 2010 adjusted England's population (page 25 of
# HSE2011-Methods-and-docs.pdf)
pop20 <- 55e6  # Mid 2011 England's population (not adjusted) from ONS. I will use this for compatibility under the assumption that institutionalised population above 65 (excluded from original HSE) has the same characteristics as the rest of the population.
d <- pop20 * prop/HSE2006hh[, sum(wt.hhld, na.rm = T)]
HSE2006hh[, wt.hhld := wt.hhld * d]

# Recreate SHA information for household from the individual's file
#Temp = copy(HSE2011original[, list(hserial, sha)])
Temp <- unique(HSE2006original[, list(hserial, sha, qimd, eqv5, hpnssec8)], by="hserial")  # Keep only unique values
HSE2006hh <- merge(HSE2006hh, Temp, by = "hserial")

rm(Temp, tt)

# Create household size (by summing adults, children and neonates)
HSE2006hh[,hsize := adults + children + infants]

# Define SynthPOP function ------------------------------------------------
SynthPOP <- function(i = 1) {
  # Create household structure of the synthetic population
  SPOP2006 <- simStructure(HSE2006hh, 
                           hid = "hserial", 
                           w = "wt.hhld", 
                           strata = "sha", 
                           hsize = "hsize", 
                           pid = "pserial", 
                           additional = c("age", "sex", "reltohrp"), 
                           method = "direct", # multinom for 2011
                           keep = F)
  
  setDT(SPOP2006, key="age")
  
  # Create unique ID
  SPOP2006[, id := c(1:.N)]
  
  # Remove NA (missing age)
  SPOP2006 = copy(SPOP2006[is.na(age) == F])
  
  # Create categorical age var, etc
  breaks <- c(0, seq(15, 75, 20), 130)
  SPOP2006[, ageCat20 := cut(age, breaks = breaks, 
                             include.lowest = TRUE, 
                             ordered_result = TRUE)]
  
  breaks                   <- c(0, 1, seq(5, 85, 5), 130)
  labels                   <- c("<1   ", "01-04", "05-09",
                                "10-14", "15-19", "20-24", 
                                "25-29", "30-34", "35-39", 
                                "40-44", "45-49", "50-54",
                                "55-59", "60-64", "65-69",
                                "70-74", "75-79", "80-84", "85+")
  SPOP2006[, agegroup := cut(age, breaks = breaks, 
                             labels = labels, 
                             include.lowest = T, 
                             right = F, 
                             ordered_result = TRUE)]
  
  # Create wt.hhld in individual file to be compatible with graphs
  # HSE2011original$wt.hhld <- HSE2011original$wt.int*d
  
  # Add qimd (quantile of index of multiple deprivation) variable (I started from it because no NAs)
  basic <- c("age", "sex", "hsize")
  additional <- c("qimd")
  HSE2006original[HSE2006hh, on = "pserial", reltohrp := reltohrp]
  direct <- HSE2006original[, levels(factor(reltohrp))]
  direct <- direct[direct!="96"] #  remove head
  
  SPOP2006 <- simRelation(HSE2006original, SPOP2006,
                          hid = "hserial",
                          w = "wt.int", 
                          strata = "sha", 
                          basic = basic, 
                          additional = additional, 
                          relation = "reltohrp",
                          head = "96",
                          direct = direct)
  SPOP2006[is.na(qimd) == T, .N]
  # Remove NA (missing age)
  SPOP2006 = copy(SPOP2006[is.na(qimd) == F])
  
  # Start using qimd for strata from now on (5= more deprived) and add more SEC indicators
  HSE2006 = copy(HSE2006original[is.na(eqv5) == F])
  basic <- c("agegroup", "sex")
  additional <- c("eqv5")
  
  SPOP2006 <- simRelation(HSE2006, SPOP2006,
                          hid = "hserial",
                          w = "wt.int", 
                          strata = "qimd", 
                          basic = basic, 
                          additional = additional,
                          relation = "reltohrp",
                          head = "96",
                          direct = direct)
  SPOP2006[is.na(eqv5) == T, .N]
  # Remove NA (missing age)
  SPOP2006 = copy(SPOP2006[is.na(eqv5) == F])
  
  HSE2006 = copy(HSE2006original[is.na(hpnssec8) == F,])
  basic <- c("agegroup", "sex", "eqv5")
  additional <- c("hpnssec8")
  SPOP2006 <- simRelation(HSE2006, SPOP2006,
                          hid = "hserial",
                          w = "wt.int", 
                          strata = "qimd", 
                          basic = basic, 
                          additional = additional, 
                          relation = "reltohrp",
                          head = "96",
                          direct = direct)
  SPOP2006[is.na("hpnssec8") == T, .N]
  SPOP2006 = copy(SPOP2006[is.na(hpnssec8) == F])
  
  
  
  # For Fruit and Veg porftvg will be used (maximum 1 portion of juice, pulses or dried fruit
  # contributed to the total portions in porfv(continous var)) porftvg was measured for ages >=5 For
  # ages <5 I will code them as 99 = non applicable
  HSE2006 = copy(HSE2006original)  # initialise HSE2011
  HSE2006[porftvg == 99, porftvg := NA]
  basic <- c("agegroup", "sex", "hpnssec8", "eqv5")
  additional <- c("porftvg")
  SPOP2006 <- simCategorical(HSE2006, SPOP2006, 
                             w = "wt.int", 
                             strata = "qimd", 
                             basic = basic, 
                             additional = additional, 
                             method = "multinom", 
                             MaxNWts = 4000)
  
  #  For fruit portions
  # HSE2006 = copy(HSE2006original)  # initialise HSE2006
  # HSE2006[, frtpor := as.integer(frtpor)]
  # HSE2006[, frtpor := cut(frtpor, breaks = c(0, 1:8, Inf), 
  #                         labels = c(0:8), include.lowest = T, 
  #                         right = F, 
  #                         ordered_result = TRUE)]
  #   levels(HSE2011$frtpor) <- c(levels(HSE2011$frtpor), "99")
  #   HSE2011[is.na(frtpor)==T & age < 5, frtpor := as.factor("99")]  # code ages of 0-4 as 99 = non applicable
  # basic <- c("agegroup", "sex", "porftvg")
  # additional <- "frtpor"
  # SPOP2006 <- simCategorical(HSE2006, SPOP2006, 
  #                            w = "wt.int", 
  #                            strata = "qimd", 
  #                            basic = basic, 
  #                            additional = additional, 
  #                            method = "multinom", 
  #                            MaxNWts = 6000)
  
  # For smoking history cigst1 will be used (D) Cigarette Smoking Status - Never/Ex-reg/Ex-occ/Current
  # cigst1 has been measured in ages >=16. I will code all ages below 16 as never smokers
  HSE2006 = copy(HSE2006original)  # initialise HSE2006
  basic <- c("agegroup", "qimd", "hpnssec8" )
  additional <- "cigst1"
  SPOP2006 <- simCategorical(HSE2006, SPOP2006, 
                             w = "wt.int", 
                             strata = "sex", 
                             basic = basic, 
                             additional = additional, 
                             method = "multinom", 
                             MaxNWts = 4000)
  
  # For Smoking cigdyal will be used ((D) Number of cigarettes smoke a day - inc. non-smokers)
  # (measured for ages <=16) I will code ages < 16 as 0 (ie non smokers) I will recode into levels by 1
  # cigar and aggregate more than 40 cigars into 40. To be named cigdyalCat
  HSE2006 = copy(HSE2006original)
  limit <- list(cigst1 = list("1" = 0, "2" = 0, "3" = 0))
  censor <- list("0" = list(cigst1 = "4"))
  basic <- c("agegroup", "sex", "hpnssec8", "cigst1" )
  additional <- "cigdyal"
  SPOP2006 <- simContinuous(HSE2006, SPOP2006, 
                            w          = "wt.int", 
                            strata     = "qimd", 
                            basic      = basic, 
                            additional = additional, 
                            method     = "multinom",
                            zeros      = T, 
                            gpd        = F,
                            breaks     = c(0, 1, 3, 6, 12, 17, 22, 27, 32, 37, 42, 80),
                            upper      = 80,
                            keep       = T,
                            limit      = limit,
                            censor     = censor,
                            MaxNWts    = 6000)
  
  
  # For How long ago did you stop smoking cigarettes? (Applicable only to ex-smokers) I will use endsmoke
  HSE2006 = copy(HSE2006original)
  breaks <- c(0, 1, 3, 6, 12, 17, 22, 27, 32, 37, 42, 80)
  HSE2006[, cigdyalCat := cut(cigdyal, breaks = breaks, 
                              include.lowest = F, 
                              ordered_result = F)]
  HSE2006[cigdyal == 0, cigdyalCat := "0"][, cigdyalCat := relevel(cigdyalCat, "0")]
  basic <- c("agegroup", "sex", "cigst1")
  additional <- "endsmoke"
  limit <- list(cigst1 = list("1" = "0", "4" = "0"))
  SPOP2006 <- simContinuous(HSE2006, SPOP2006, 
                            w          = "wt.int", 
                            strata     = "qimd", 
                            basic      = basic, 
                            additional = additional, 
                            method     = "multinom",
                            zeros      = T, 
                            gpd        = F,
                            breaks     = c(0, 1, 2, 3, 4, 5, 7, 9, 13, 23, 33, 44, 55, 70),
                            upper      = 70,
                            keep       = T,
                            limit      = limit,
                            MaxNWts    = 6000)
  
  
  # For passive smoking expsmok will be used (Number of hours/week exposed to others' smoke (c+sc)) To
  # be recoded into a categorical variable (expsmokCat)
  HSE2006 = copy(HSE2006original)
  breaks <- c(0, seq(0.1, 99.1, 10), max(HSE2006$expsm, na.rm = T))
  HSE2006[, expsmokCat := cut(expsm, breaks = breaks,
                              labels = c(0:10),
                              include.lowest = TRUE, 
                              ordered_result = TRUE)]
  basic <- c("agegroup", "sex", "hpnssec8", "cigst1" )
  additional <- "expsmokCat"
  SPOP2006 <- simCategorical(HSE2006, SPOP2006, 
                             w = "wt.int", 
                             strata = "qimd", 
                             basic = basic, 
                             additional = additional, 
                             method = "multinom", 
                             MaxNWts = 4000)
  
  # Used medication
  # lipid: use statin over last 7 days
  HSE2006 = copy(HSE2006original)
  basic <- c("agegroup", "sex", "hpnssec8")
  additional <- c("lipid")
  SPOP2006 <- simCategorical(HSE2006, SPOP2006, 
                             w = "wt.nurse", 
                             strata = "qimd", 
                             basic = basic, 
                             additional = additional, 
                             method = "multinom", 
                             MaxNWts = 4000)
  
  # bpmedc: (D) Whether taking drugs affecting blood pressure
  HSE2006[is.na(bpmedc)==T, bpmedc := 0]
  basic <- c("agegroup", "sex", "hpnssec8", "lipid")
  additional <- c("bpmedc")
  SPOP2006 <- simCategorical(HSE2006, SPOP2006, 
                             w = "wt.nurse", 
                             strata = "qimd", 
                             basic = basic, 
                             additional = additional, 
                             method = "multinom", 
                             MaxNWts = 6000)
  
  # PA 
  basic <- c("agegroup", "sex", "hpnssec8", "eqv5")
  additional <- c("a30to06m")
  SPOP2006 <- simCategorical(HSE2006original, SPOP2006, 
                             w = "wt.int", 
                             strata = "qimd", 
                             basic = basic, 
                             additional = additional, 
                             method = "multinom", 
                             MaxNWts = 6000)
  #spMosaic(c("sex", "agegroup", "a30to06m"), 'wt.int', HSE2006original[age > 15, ], SPOP2006[as.integer(as.character(age)) > 15, ], margins = c(0.1))
  
  
  # For BMI I will use bmival (measured for for ages >=2) as continous variable
  HSE2006 = copy(HSE2006original)
  HSE2006[age < 16 & is.na(a30to06m), a30to06m := 99]
  basic <- c("agegroup", "sex", "eqv5", "porftvg")#, "a30to06m")
  additional <- c("bmival")
  limit <- list(agegroup = list("<1   " = 0))
  SPOP2006 <- simContinuous(HSE2006, SPOP2006, 
                            w = "wt.nurse", 
                            strata = "qimd", 
                            basic = basic, 
                            additional = additional, 
                            method = "multinom", 
                            zeros = F, 
                            gpd = T, 
                            limit = limit,
                            MaxNWts = 6000,
                            equidist = F,
                            keep = T)
  
  
  # For diabetes I will use diabtype : (D) Type of diabetes {revised} weighted with wt.int 
  # 1 = Diagnosed aged 35+ and/or not treated with insulin, 2 = Not diabetic, 3 = Diagnosed before the age of 35 and treated with insulin
  breaks <- c(0, 20, 25, 30, 35, 40, 50, Inf)
  HSE2006[, bmivalCat := cut(bmival, breaks = breaks,
                             include.lowest = TRUE, 
                             ordered_result = TRUE)]
  SPOP2006[, bmivalCat := cut(bmival, breaks = breaks,
                              include.lowest = TRUE, 
                              ordered_result = TRUE)]
  HSE2006[age < 16, diabtype := 2] # not asked for ages < 16
  basic <- c("agegroup", "sex", "hpnssec8", "qimd")#, "a30to06m")
  additional <- c("diabtype")
  SPOP2006 <- simCategorical(HSE2006, SPOP2006, 
                             w = "wt.nurse", 
                             strata = "bmivalCat", 
                             basic = basic, 
                             additional = additional, 
                             method = "multinom", 
                             MaxNWts = 6000)
  SPOP2006[, sum(diabtype==1)/.N, by=agegroup]
  # Try to capture undiagnosed diabetes using 
  # diabtotr: (D) Total diabetes from blood sample or doctor diagnosis (excluding pregnancy-only diabetes)
  # 1 =  No diabetes, 2 = Doctor diagnosed diabetes and/or HbAlc>=6.5
  basic <- c("agegroup", "sex", "qimd", "hpnssec8") # if add a30to06m gives wrong results
  additional <- c("diabtotr")
  HSE2006[age < 16, diabtotr := 1L]
  #limit <- list(diabtype = list("1" = "2", "3" = "2"))
  #censor <- list("1" = list(diabtype = "1", diabtype = "3"))
  SPOP2006 <- simCategorical(HSE2006, SPOP2006, 
                             w = "wt.blood", 
                             strata = "bmivalCat", 
                             basic = basic, 
                             additional = additional, 
                             method = "multinom",
                             #limit = limit,
                             #censor = censor,
                             MaxNWts = 6000)
  SPOP2006[, sum(diabtotr==2)/.N, by=agegroup]
  #SPOP2006[diabtype == "1" & diabtotr == "1", diabtotr := "2"]
  
  # Calculate numsmok (number of cigarette smoked for ex smokers)
  HSE2006 = copy(HSE2006original)
  limit <- list(cigst1 = list("1" = 0, "2" = 0, "4" = 0))
  censor <- list("0" = list(cigst1 = "3"))
  basic <- c("agegroup", "sex", "hpnssec8", "cigst1" )
  additional <- "numsmok"
  SPOP2006 <- simContinuous(HSE2006, SPOP2006, 
                            w          = "wt.int", 
                            strata     = "qimd", 
                            basic      = basic, 
                            additional = additional, 
                            method     = "multinom",
                            zeros      = T, 
                            gpd        = F,
                            breaks     = c(0, 1, 3, 6, 12, 17, 22, 27, 32, 37, 42, 80),
                            upper      = 80,
                            keep       = T,
                            limit      = limit,
                            censor     = censor,
                            MaxNWts    = 6000)
  
  # Smoking duration
  HSE2006 = copy(HSE2006original)
  HSE2006[startsmk==97, startsmk := NA] # 97 = "never smoke regularly"
  HSE2006[cigst1 == 4, duration := age - startsmk]  # Years smoking for smokers
  HSE2006[cigst1 < 4, duration := 0]  # Years smoked for non smokers
  breaks <- c(0, 1, 3, 6, 12, 17, 22, 27, 32, 37, 42, 80)
  HSE2006[, cigdyalCat := cut(cigdyal, breaks = breaks, 
                              include.lowest = F, 
                              ordered_result = F)]
  HSE2006[cigdyal == 0, cigdyalCat := "0"][, cigdyalCat := relevel(cigdyalCat, "0")]
  basic <- c("agegroup", "sex", "cigdyalCat", "cigst1")
  additional <- c("duration")
  limit <- list(cigst1 = list("1" = 0, "2" = 0, "3" = 0))
  
  SPOP2006 <- simContinuous(HSE2006, SPOP2006, 
                            w = "wt.int",
                            strata = "qimd",
                            basic = basic,
                            additional = additional, 
                            method = "multinom",
                            breaks = c(0, 1, 3, 17, 32, 37,  42, 47,  52, 62, Inf),
                            zeros = T,
                            limit = limit, 
                            gpd = T,
                            MaxNWts = 6000,
                            keep = F)
  
  HSE2006[cigst1 == 4, smokyrs := 0]  # Years smoking for smokers
  HSE2006[cigst1 == 1, smokyrs := 0]  # Years smoked for non smokers
  breaks <- c(0, 1, 3, 6, 12, 17, 22, 27, 32, 37, 42, 80)
  HSE2006[, numsmokCat := cut(numsmok, breaks = breaks, 
                              include.lowest = F, 
                              ordered_result = F)]
  HSE2006[cigdyal == 0, numsmokCat := "0"][, numsmokCat := relevel(numsmokCat, "0")]
  basic <- c("agegroup", "sex", "numsmokCat", "cigst1")
  additional <- c("smokyrs")
  limit <- list(cigst1 = list("1" = 0, "4" = 0))
  
  SPOP2006 <- simContinuous(HSE2006, SPOP2006, 
                            w = "wt.int",
                            strata = "qimd",
                            basic = basic,
                            additional = additional, 
                            method = "multinom",
                            #breaks = c(0, 1, 2, 4, 17, 27, 47, 62, Inf),
                            equidist = F,
                            zeros = T,
                            limit = limit, 
                            gpd = T,
                            MaxNWts = 6000,
                            keep = F)
  
  #Sanity check
  SPOP2006[cigst1 == "4" & as.numeric(as.character(age)) < (duration + 10), 
           duration := as.numeric(as.character(age)) - 10]
  SPOP2006[cigst1 %in% c("2", "3") & as.numeric(as.character(age)) < (smokyrs + 10), 
           smokyrs := as.numeric(as.character(age)) - 10]
  
  SPOP2006[cigst1 == "4", smokyrs := duration]
  
  
  # Salt --------------------------------------------------------------------
  HSE2006 = copy(HSE2006original)
  
  # Recalculate 24h salt stochastically for each iteration
  HSE2006[age>15 & sex== "1",
          Na24 := Na24.men.det(.N, sodium, creatin, potass, bmival, age)]
  HSE2006[age>15 & sex== "2",
          Na24 := Na24.women.det(.N, sodium, creatin, potass, bmival, age)]
  HSE2006[, salt.intersalt := Na24 * 58.5/1000]
  HSE2006[salt.intersalt < 1, salt.intersalt := 1]
  
  breaks <- c(0, 20, 25, 30, 35, 40, 50, Inf)
  HSE2006[, bmivalCat := cut(bmival, breaks = breaks,
                             include.lowest = TRUE, 
                             ordered_result = TRUE)]
  SPOP2006[, bmivalCat := cut(bmival, breaks = breaks,
                              include.lowest = TRUE, 
                              ordered_result = TRUE)]
  basic <- c("agegroup", "sex", "eqv5", "hpnssec8", "bmivalCat")
  additional <- c("salt.intersalt")
  SPOP2006 <- simContinuous(HSE2006, SPOP2006, 
                            w = "wt.nurse", 
                            strata = "qimd", 
                            basic = basic, 
                            additional = additional, 
                            method = "multinom", 
                            equidist = F,  
                            zeros = F, 
                            gpd = F, 
                            MaxNWts = 6000, 
                            keep = F)
  SPOP2006[, saltCat.intersalt := getCat(salt.intersalt,  HSE2006[, getBreaks(salt.intersalt, wt.nurse, F, equidist = F)], F, T)]
  
  HSE2006[, saltCat.intersalt := getCat(salt.intersalt,  HSE2006[, getBreaks(salt.intersalt, wt.nurse, F, equidist = F)], F, T)]
  
  if (SPOP2006[is.na(salt.intersalt) == T, .N] > 0) {
    SPOP2006 <- SPOP2006[is.na(salt.intersalt) == F,] # Remove NA
  }
  
  # For systolic BP (wt.nurse) I will use omsysval ((D) Omron Valid Mean Systolic BP) omsysval was
  # measured in ages >=5
  HSE2006[age<16 & is.na(a30to06m), a30to06m := 99]
  breaks <- c(0, 14.4, 16.2, 17.7, 20.8, 24.2, 26.9, 30.2, 33.4, 36.9, 42.3, Inf)
  HSE2006[, bmivalCat := cut(bmival, breaks = breaks,
                             include.lowest = TRUE, 
                             ordered_result = TRUE)]
  SPOP2006[, bmivalCat := cut(bmival, breaks = breaks,
                              include.lowest = TRUE, 
                              ordered_result = TRUE)]
  #basic <- c("agegroup", "sex", "hpnssec8", "bmivalCat", "qimd") used in the current synthpops
  #additional <- c("omsysval")
  
  # "bpmedc" should be used but makes the fit worse and I don't use medication yet anyway
  basic <- c("agegroup", "sex", "bmivalCat", "cigst1", "saltCat.intersalt")#, "a30to06m")
  additional <- c("omsysval")
  SPOP2006 <- simContinuous(HSE2006, SPOP2006, 
                            w = "wt.nurse", 
                            strata = "qimd", 
                            basic = basic, 
                            additional = additional, 
                            method = "multinom", 
                            equidist = F, 
                            zeros = F, 
                            gpd = T, 
                            MaxNWts = 20000, 
                            keep = T)
  #spCdfplot(x = 'omsysval', weights = 'wt.nurse', cond = 'qimd', dataS = HSE2011[age>19,], dataP = SPOP2006[as.numeric(age)>19,])
  
  
  # For total Chol (wt.blood) I will use cholval1: (D) Valid Cholesterol Result (incl those on lld)
  # cholval1 was measured in ages >=16
  basic <- c("agegroup", "sex", "bmivalCat", "lipid", "porftvg")#, "a30to06m")
  additional <- c("cholval1")
  SPOP2006 <- simContinuous(HSE2006, SPOP2006, 
                            w = "wt.blood", 
                            strata = "qimd", 
                            basic = basic, 
                            additional = additional, 
                            method = "multinom", 
                            equidist = F, 
                            zeros = F, 
                            gpd = T, 
                            MaxNWts = 6000, 
                            keep = T) # Produces NA for ages <16
  #spCdfplot(x = 'cholval1', weights = 'wt.blood', cond = 'qimd', dataS = HSE2011[age>19,], dataP = SPOP2006[as.numeric(age)>19,])
  setnames(SPOP2006, c("cholval1", "cholval1Cat"), c("cholval", "cholvalCat"))
  
  
  # CVD prevalence
  # I will use cvdcon: (D) IHD/stroke but no IHD/None (1 = IHD, 2 = Stroke but no IHD, 3 = None of these) 
  # I will not use redmeat and physical exercise as independent variables because they were not included in HSE2006.
  # In the future I might use statmatch to inform HSE2006 with these variables from previous surveys 
#   breaks <- c(0, 2.9, 3.5, 3.8, 4.2, 4.9, 5.4, 6.1, 6.7, 7.1, 8, Inf)
#   HSE2006[, cholvalCat := cut(cholval, breaks = breaks,
#                               include.lowest = TRUE, 
#                               ordered_result = TRUE)]
#   SPOP2006[, cholvalCat := cut(cholval, breaks = breaks,
#                                include.lowest = TRUE, 
#                                ordered_result = TRUE)]
#   breaks <- c(0, 91.5, 100, 106, 111, 120, 129, 140, 150, 160, 176, Inf)
#   HSE2006[, omsysvalCat := cut(omsysval, breaks = breaks,
#                                include.lowest = TRUE, 
#                                ordered_result = TRUE)]
#   SPOP2006[, omsysvalCat := cut(omsysval, breaks = breaks,
#                                 include.lowest = TRUE, 
#                                 ordered_result = TRUE)]
#   HSE2006[is.na(cvdcon) == T & age < 16, cvdcon := 3]  # code ages of 0-15 as non having CVD
#   HSE2006[age <= 16, diabtype := 2]
#   HSE2006[is.na(bpmedc)==T, bpmedc := 0]
#   
#   basic <- c("agegroup", "sex", "bmivalCat", "lipid", "bpmedc", "cigst1",
#              "diabtype", "omsysvalCat", "cholvalCat", "a30to06m")
#   additional <- c("cvdcon")
#   limit <- list(agegroup = list("<1   " = "3", "01-04" = "3", "05-09" = "3",
#                                 "10-14" = "3", "15-19" = "3", "20-24" = "3")) # I set this because otherwise it produces very high prevalence of CVD
#   SPOP2006 <- simCategorical(HSE2006, SPOP2006, 
#                              w = "wt.blood", 
#                              strata = "qimd", 
#                              basic = basic, 
#                              additional = additional, 
#                              method = "multinom",
#                              limit = limit,
#                              MaxNWts = 6000)
#   
#   SPOP2006[as.numeric(as.character(age)) < 16, cvdcon := 3L] # To fix NA produced due to NA cholesterol
#   if (SPOP2006[is.na(cvdcon) == T, .N] > 0) {
#     SPOP2006 <- SPOP2006[is.na(cvdcon) == F,] # Remove NA
#   }
  
  
  
  # Finalise SPOP2006 -------------------------------------------------------
  SPOP2006[, age := as.integer(as.character(age))]
  SPOP2006[, sex := as.factor(sex)]
  SPOP2006[, numsmok := as.integer(numsmok)]
  SPOP2006[cigst1 == "2", numsmok:= 1L]
  SPOP2006[expsmokCat != "0", expsmokCat:= "1"][,expsmokCat := factor(expsmokCat)]
  SPOP2006[, a30to06m := as.integer(as.character(a30to06m))]
  SPOP2006[age < 15, cigst1 := "1"]
  SPOP2006[age > 99, age := 99] # for combatibility with lifetables
  SPOP2006[, cigdyal := as.integer(cigdyal)]
  SPOP2006[, endsmoke := as.integer(endsmoke)]
  SPOP2006[, smokyrs := as.integer(smokyrs)]
  #SPOP2006[, frtpor := as.integer(as.character(frtpor))]
  SPOP2006[, hsize := as.integer(as.character(hsize))]
  SPOP2006[, porftvg := as.integer(as.character(porftvg))]
  SPOP2006[,  `:=` (endsmokeCat = NULL, 
                    numsmokCat  = NULL,
                    duration    = NULL,
                    cigdyalCat  = NULL,
                    bmivalCat   = NULL, 
                    cholvalCat  = NULL,
                    omsysvalCat = NULL,
                    ageCat20    = NULL, 
                    qimd = ordered(qimd, levels=as.character(1:5)))]
  #SPOP2006[, bmival := round(bmival,1)]
  agegroup.fn(SPOP2006)
  setkey(SPOP2006, age, sex, agegroup, qimd)
  setcolorder(SPOP2006, SPOP2006[,order(names(SPOP2006))]) # reorder columns alphabeticaly
  # Return datatable
  return(saveRDS(SPOP2006, file = paste("/mnt/iusers01/mhs01/mdxasck2/IMPACTncd/SynthPop/spop2006-", i, ".rds", sep= "")))
}
SynthPOP <- cmpfun(SynthPOP)

# ******************************************************* LOOP ********************************************
cl <- makeCluster(clusternumber) 
registerDoParallel(cl)

foreach(i = 1:iterations,
        .inorder = F,
        .verbose = T,
        .packages = c("data.table",
                      "simPopulation",
                      "survey",
                      "pryr",
                      "compiler"),
        .export = ls()) %dopar% SynthPOP(i)

stopCluster(cl)



# Garbage cleaning
rm(list = ls(all.names = TRUE))

