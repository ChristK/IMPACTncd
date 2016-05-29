#!/opt/gridware/apps/gcc/R/3.2.3/lib64/R/bin/Rscript
# ************************************************************************************************
#                  England's synthetic population
# ************************************************************************************************

## Note to myshefl. I build synth pop in layers, attempting to recreate causality. This is innovative

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
load("hse2012ai.RData")
HSE2012original <- clear.labels(HSE2012original)
HSE2012original <- data.table(HSE2012original, key="age")
agegroup.fn(HSE2012original)

load("hse2011ai.RData")
HSE2011original <- clear.labels(HSE.2011)
rm(HSE.2011)
HSE2011original <- data.table(HSE2011original, key="age")
agegroup.fn(HSE2011original)

load("hse2011ah.RData")
HSE2011hh <- clear.labels(HSE2011hh)
HSE2011hh <- data.table(HSE2011hh, key="age")

load("hse2012ah.RData")
HSE2012hh <- clear.labels(HSE2012hh)
HSE2012hh <- data.table(HSE2012hh, key="age")

# load("hse08ai.RData")
# HSE2008original <- clear.labels(HSE2008original)
# HSE2008original <- setDT(HSE2008original)
# HSE2008original =copy(HSE2008original[samptype==1,])
# HSE2008original[, cholval1 := cholval1 + 0.1]
# agegroup.fn(HSE2008original)

# Data fusion PA ----------------------------------------------------------
HSE2012original[,a30to06m := as.integer(a30to06/4)]

A = copy(HSE2011original[age >15 & !is.na(agegroup) & !is.na(sex) & 
                           !is.na(qimd) & !is.na(bmival) &
                           !is.na(omsysval) & !is.na(cholval1) & !is.na(diabtotr),
                         .(pserial, agegroup, sex, qimd, wt.blood, psu, cluster,
                           bmival, diabtotr, cholval1, omsysval)])

B = copy(HSE2012original[age >15 & !is.na(a30to06m) & !is.na(agegroup) & 
                           !is.na(sex) & !is.na(qimd) &
                           !is.na(bmival) & !is.na(omsysval) & 
                           !is.na(cholval12) & !is.na(diabtotr),
                         .(pserial, a30to06m, agegroup, sex, qimd, wt.blood, 
                           psu, cluster, bmival, omsysval, cholval12, diabtotr)])

B[, a30to06m   := factor(a30to06m)]
A[, agegroup   := factor(agegroup)]
B[, agegroup   := factor(agegroup)]
A[, bmival     := cut2(bmival,    c(0, seq(20,40,5), Inf))]
B[, bmival     := cut2(bmival,    c(0, seq(20,40,5), Inf))]
A[, omsysval   := cut2(omsysval,  c(0, seq(130, 180, 10), Inf))]
B[, omsysval   := cut2(omsysval,  c(0, seq(130, 180, 10), Inf))]
A[, cholval    := cut2(cholval1,  c(0, seq(4, 7, 1), Inf))]
B[, cholval    := cut2(cholval12, c(0, seq(4, 7, 1), Inf))]
A[, diabtotr   := factor(diabtotr)]
B[, diabtotr   := factor(diabtotr)]

A.srv <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.blood, nest = F,
                   data = A, check.strata = T)
B.srv <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.blood, nest = F,
                   data = B, check.strata = T)

out.hz <- harmonize.x(svy.A = A.srv,
                      svy.B = B.srv,
                      x.tot = NULL,
                      form.x = ~agegroup + sex + qimd + 
                                bmival + omsysval + cholval + diabtotr - 1,
                      cal.method="linear")
A.srv =copy(out.hz$cal.A)
B.srv =copy(out.hz$cal.B)

tt <- comb.samples(A.srv,
                   B.srv, NULL, 
                   "pserial", "a30to06m", 
                   form.x = ~agegroup + sex + qimd +
                     bmival + omsysval + cholval + diabtotr - 1,
                   micro = T)

ttt <- cbind(A, tt$Z.A)
setnames(ttt, paste0("a30to06m", 1:8), paste0(0:7))
ttt <- melt(ttt, id=c("pserial", "agegroup", "sex", "qimd",
                      "wt.blood", "psu", "cluster", "bmival", "omsysval", 
                      "cholval", "cholval1",  "diabtotr"),
            variable.name = "a30to06m")
setkey(ttt, pserial, value)
ttt[value < 0, value := 0 ]
ttt <- ttt[, sample_n(.SD, 1, weight = value), by = pserial]
HSE2011original = copy(
  merge(
    HSE2011original,
    ttt[,.(pserial, a30to06m)],
    by = "pserial",
    all.x = T)
)


# Data fusion FV ----------------------------------------------------------
HSE2011original[porftvg == 0, porftvg := 1]
HSE2011original[porftvg <= 9L, porftvg := porftvg - 1L]

A = copy(
  HSE2012original[
    age >15 & !is.na(agegroup) & !is.na(sex) & !is.na(qimd) &
      !is.na(hpnssec8) & !is.na(eqv5) & !is.na(bmival) & eqv5>0, 
    .(pserial, agegroup, sex, qimd, bmival, hpnssec8,
      bmival, eqv5, wt.nurse, psu, cluster)
    ]
)

B = copy(
  HSE2011original[
    age >15 & !is.na(porftvg) & !is.na(agegroup) & !is.na(sex) & !is.na(qimd) & 
      !is.na(hpnssec8) & !is.na(eqv5) & eqv5>0 & !is.na(bmival),
    .(pserial, porftvg, agegroup, sex, qimd, hpnssec8, eqv5,
      wt.nurse, psu, cluster, bmival)
    ]
)

B[, porftvg  := factor(porftvg)]
A[, agegroup := factor(agegroup)]
B[, agegroup := factor(agegroup)]
A[, bmival   := cut2(bmival, c(0, seq(20,40,5), Inf))]
B[, bmival   := cut2(bmival, c(0, seq(20,40,5), Inf))]
A.srv <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.nurse, nest = F,
                   data = A, check.strata = T)
B.srv <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.nurse, nest = F,
                   data = B, check.strata = T)

out.hz <- harmonize.x(svy.A = A.srv,
                      svy.B = B.srv,
                      x.tot = NULL,
                      form.x = ~agegroup + sex + qimd + hpnssec8 +
                        eqv5 + bmival - 1,
                      cal.method="linear")
A.srv = copy(out.hz$cal.A)
B.srv = copy(out.hz$cal.B)

tt <- comb.samples(A.srv,
                   B.srv, NULL, 
                   "pserial", "porftvg", 
                   form.x = ~agegroup + sex + qimd + hpnssec8 + 
                     eqv5 + bmival - 1,
                   micro = T)

ttt <- cbind(A, tt$Z.A)
setnames(ttt, paste0("porftvg", 1:9), paste0(0:8))
ttt <- melt(ttt, id=c("pserial", "agegroup", "sex", "qimd", "hpnssec8", "eqv5",
                      "wt.nurse", "psu", "cluster", "bmival"),
            variable.name = "porftvg")
setkey(ttt, pserial, value)
ttt[value < 0, value := 0 ]
ttt<- ttt[, sample_n(.SD, 1, weight = value), by = pserial]
HSE2012original = copy(
  merge(
    HSE2012original,
    ttt[,.(pserial, porftvg)],
    by = "pserial",
    all.x = T
  )
)

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

HSE2012original[age>15 & sex== "1",
                Na24 := Na24.men.det(.N, sodiumval, creatin, potass, bmival, age)]
HSE2012original[age>15 & sex== "2",
                Na24 := Na24.women.det(.N, sodiumval, creatin, potass, bmival, age)]
HSE2012original[, salt.intersalt := Na24 * 58.5/1000]
HSE2012original[salt.intersalt < 1, salt.intersalt := 1]
HSE2012original[, saltCat.intersalt := getCat(
  salt.intersalt,
  getBreaks(salt.intersalt, wt.urine, F, equidist = F),
  F, T
)
]

# Data fusion intersalt ---------------------------------------------------
A = copy(
  HSE2011original[
    age >15 & !is.na(agegroup) & !is.na(sex) & !is.na(qimd) & !is.na(hpnssec8) &
      eqv5>0 & !is.na(bmival) & !is.na(omsysval),
    .(pserial, agegroup, sex, qimd, hpnssec8, eqv5, wt.nurse,
      psu, cluster, bmival, omsysval)
    ]
)

B = copy(
  HSE2012original[
    age >15 & !is.na(saltCat.intersalt) & !is.na(agegroup) & !is.na(sex) & 
      !is.na(qimd) & !is.na(hpnssec8) & eqv5>0 &
      !is.na(bmival) & !is.na(omsysval),
    .(pserial, saltCat.intersalt, agegroup, sex, qimd, hpnssec8, 
      eqv5, wt.urine, psu, cluster, bmival, omsysval)
    ]
)


A[, agegroup := factor(agegroup)]
B[, agegroup := factor(agegroup)]
A[, bmival   := cut2(bmival, c(0, seq(20,40,5), Inf))]
B[, bmival   := cut2(bmival, c(0, seq(20,40,5), Inf))]
A[, omsysval := cut2(omsysval, c(0, seq(130, 180, 10), Inf))]
B[, omsysval := cut2(omsysval, c(0, seq(130, 180, 10), Inf))]

A.srv <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.nurse, nest = F,
                   data = A, check.strata = T)
B.srv <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.urine, nest = F,
                   data = B, check.strata = T)

out.hz <- harmonize.x(svy.A = A.srv,
                      svy.B = B.srv,
                      x.tot = NULL,
                      form.x = ~agegroup + sex + qimd + hpnssec8 + 
                        eqv5 + omsysval + bmival - 1,
                      cal.method = "linear")
A.srv =copy(out.hz$cal.A)
B.srv =copy(out.hz$cal.B)

tt <- comb.samples(A.srv,
                   B.srv, NULL, 
                   "pserial", "saltCat.intersalt", 
                   form.x = ~agegroup + sex + qimd + hpnssec8 + eqv5 +
                     omsysval + bmival - 1,
                   micro = T)

ttt<- cbind(A, tt$Z.A)
setnames(ttt, paste0("saltCat.intersalt", 1:11), paste0(B[, levels(saltCat.intersalt)]))
ttt <- melt(ttt, id=c("pserial", "agegroup", "sex", "qimd", "hpnssec8", "eqv5", 
                      "wt.nurse", "psu", "cluster", "omsysval", "bmival"), 
            variable.name = "saltCat.intersalt")
setkey(ttt, pserial, value)
ttt[value<0, value :=0 ]
ttt <- ttt[, sample_n(.SD, 1, weight = value), by = pserial]
HSE2011original = 
  merge(
    HSE2011original,
    ttt[,.(pserial, saltCat.intersalt)],
    by = "pserial",
    all.x = T
  )

HSE2011original[eqv5 < 0, eqv5 := NA]
HSE2011original[hpnssec8 > 8, hpnssec8 := NA]
HSE2011original[is.na(porftvg) == T & age < 5, porftvg := 99]  # code ages of 0-5 as 99 = not  applicable

HSE2011original[is.na(totalwug) == T & age < 16, totalwug := 0]  # code ages of 0-15 as non drinkers in the totalwug
HSE2011original[is.na(cigdyal) == T & age < 16, cigdyal := 0]  # code ages of 0-15 as non smokers
HSE2011original[is.na(cigst1) == T & age < 16, cigst1 := 1]  # code ages of 0-15 as never smokers
HSE2011original[is.na(endsmoke) == T & (cigst1 == 1 | cigst1 == 4), endsmoke := 0]  # code non ex smokers as 0
HSE2011original[is.na(numsmok) & cigst1 !=3, numsmok := 0]  # code non ex smokers as 0
HSE2011original[is.na(segment) == T & age < 16, segment := 99]  # code ages of 0-15 as not applicable= 99
setnames(HSE2011original, "hhsize", "hsize")

# Calculate actual statins prescription and overwrite "lipid" variable
HSE2011original[, lipid := 0]
for (i in 1:9) {
  nam <- paste("medbi0", i, sep="")
  man <- paste("medbia", i, sep="")
  HSE2011original[get(nam) == 21201 & get(man) == 1, lipid := 1 ]
}
for (i in 10:22) {
  nam <- paste("medbi", i, sep="")
  man <- paste("medbia", i, sep="")
  HSE2011original[get(nam) == 21201 & get(nam) == 1, lipid := 1 ]
}
HSE2011original[statina == 1, lipid := 1] # incluse OTC statins
HSE2011original[, lipid := as.factor(lipid)]

# Rescale weights of the household files (page 25 of HSE2011-Methods-and-docs.pdf)
# pop2010 <- 51818267 # Mid 2010 adjusted England's population (page 25 of
# HSE2011-Methods-and-docs.pdf)
pop20 <- 55e6  # Mid 2011 England's population (not adjusted) from ONS. I will use this for compatibility under the assumption that institutionalised population above 65 (excluded from original HSE) has the same characteristics as the rest of the population.
d <- pop20 * prop/HSE2011hh[, sum(wt.hhld)]
HSE2011hh[, wt.hhld := wt.hhld * d]

# Recreate SHA information for household from the individual's file
#Temp = copy(HSE2011original[, list(hserial, sha)])
Temp <- unique(HSE2011original[, list(hserial, sha, qimd, eqv5, hpnssec8)], by="hserial")  # Keep only unique values
HSE2011hh <- merge(HSE2011hh, Temp, by = "hserial")

rm(Temp)

# Create household size (by summing adults, children and neonates)
HSE2011hh[,hsize := adults + children + infants]

# Define SynthPOP function ------------------------------------------------
SynthPOP <- function(i = 1) {
  # Create household structure of the synthetic population
  SPOP2011 <- simStructure(HSE2011hh, 
                           hid = "hserial", 
                           w = "wt.hhld", 
                           strata = "sha", 
                           hsize = "hsize", 
                           pid = "pserial", 
                           additional = c("age", "sex", "reltohrp"), 
                           method = "multinom", 
                           keep = F)
  
  SPOP2011 <- data.table(SPOP2011, key="age")
  
  # Create unique ID
  SPOP2011[, id := c(1:.N)]
  
  # Remove NA (missing age)
  SPOP2011 = copy(SPOP2011[is.na(age) == F])
  
  # Create categorical age var, etc
  breaks <- c(0, seq(15, 75, 20), 130)
  SPOP2011[, ageCat20 := cut(age, breaks = breaks, 
                             include.lowest = TRUE, 
                             ordered_result = TRUE)]
  
  breaks                   <- c(0, 1, seq(5, 85, 5), 130)
  labels                   <- c("<1   ", "01-04", "05-09",
                                "10-14", "15-19", "20-24", 
                                "25-29", "30-34", "35-39", 
                                "40-44", "45-49", "50-54",
                                "55-59", "60-64", "65-69",
                                "70-74", "75-79", "80-84", "85+")
  SPOP2011[, agegroup := cut(age, breaks = breaks, 
                             labels = labels, 
                             include.lowest = T, 
                             right = F, 
                             ordered_result = TRUE)]
  
  # Create wt.hhld in individual file to be compatible with graphs
  # HSE2011original$wt.hhld <- HSE2011original$wt.int*d
  
  # Add qimd (quantile of index of multiple deprivation) variable (I started from it because no NAs)
  basic <- c("age", "sex", "hsize")
  additional <- c("qimd")
  direct <- HSE2011original[, levels(factor(reltohrp))]
  direct <- direct[direct!="96"] #  remove head
  
  SPOP2011 <- simRelation(HSE2011original, SPOP2011,
                          hid = "hserial",
                          w = "wt.int", 
                          strata = "sha", 
                          basic = basic, 
                          additional = additional, 
                          relation = "reltohrp",
                          head = "96",
                          direct = direct)
  SPOP2011[is.na(qimd) == T, .N]
  # Remove NA (missing age)
  SPOP2011 = copy(SPOP2011[is.na(qimd) == F])
  
  # Start using qimd for strata from now on (5= more deprived) and add more SEC indicators
  HSE2011 = copy(HSE2011original[is.na(eqv5) == F])
  basic <- c("agegroup", "sex")
  additional <- c("eqv5")
  
  SPOP2011 <- simRelation(HSE2011, SPOP2011,
                          hid = "hserial",
                          w = "wt.int", 
                          strata = "qimd", 
                          basic = basic, 
                          additional = additional,
                          relation = "reltohrp",
                          head = "96",
                          direct = direct)
  SPOP2011[is.na(eqv5) == T, .N]
  # Remove NA (missing age)
  SPOP2011 = copy(SPOP2011[is.na(eqv5) == F])
  
  HSE2011 = copy(HSE2011original[is.na(hpnssec8) == F,])
  basic <- c("agegroup", "sex", "eqv5")
  additional <- c("hpnssec8")
  SPOP2011 <- simRelation(HSE2011, SPOP2011,
                          hid = "hserial",
                          w = "wt.int", 
                          strata = "qimd", 
                          basic = basic, 
                          additional = additional, 
                          relation = "reltohrp",
                          head = "96",
                          direct = direct)
  SPOP2011[is.na("hpnssec8") == T, .N]
  SPOP2011 = copy(SPOP2011[is.na(hpnssec8) == F])
  
  # For alcohol totalwug will be used ((D) Total unit so of alcohol/week (grouped)) This was measured
  # for age>=16 for ages 13-15 this information has many missing values so I will code ages of 0-15 as
  # non drinkers in the totalwug var
  # HSE2011 = copy(HSE2011original)  # initialise HSE2011
  # basic <- c("agegroup", "sex", "hpnssec8", "eqv5")
  # additional <- c("totalwug")
  # SPOP2011 <- simCategorical(HSE2011, SPOP2011, 
  #                            w = "wt.int", 
  #                            strata = "qimd", 
  #                            basic = basic, 
  #                            additional = additional, 
  #                            method = "multinom", 
  #                            MaxNWts = 4000)
  
  # For Fruit and Veg porftvg will be used (maximum 1 portion of juice, pulses or dried fruit
  # contributed to the total portions in porfv(continous var)) porftvg was measured for ages >=5 For
  # ages <5 I will code them as 99 = non applicable
  HSE2011 = copy(HSE2011original)  # initialise HSE2011
  HSE2011[porftvg == 99, porftvg := NA]
  basic <- c("agegroup", "sex", "hpnssec8", "eqv5")
  additional <- c("porftvg")
  SPOP2011 <- simCategorical(HSE2011, SPOP2011, 
                             w = "wt.int", 
                             strata = "qimd", 
                             basic = basic, 
                             additional = additional, 
                             method = "multinom", 
                             MaxNWts = 4000)
  
  #  For fruit portions
  # HSE2011 = copy(HSE2011original)  # initialise HSE2011
  # HSE2011[, frtpor := as.integer(frtpor)]
  # HSE2011[, frtpor := cut(frtpor, breaks = c(0, 1:8, Inf), 
  #                         labels = c(0:8), include.lowest = T, 
  #                         right = F, 
  #                         ordered_result = TRUE)]
  #   levels(HSE2011$frtpor) <- c(levels(HSE2011$frtpor), "99")
  #   HSE2011[is.na(frtpor)==T & age < 5, frtpor := as.factor("99")]  # code ages of 0-4 as 99 = non applicable
  # basic <- c("agegroup", "sex", "porftvg")
  # additional <- "frtpor"
  # SPOP2011 <- simCategorical(HSE2011, SPOP2011, 
  #                            w = "wt.int", 
  #                            strata = "qimd", 
  #                            basic = basic, 
  #                            additional = additional, 
  #                            method = "multinom", 
  #                            MaxNWts = 6000)
  
  # For smoking history cigst1 will be used (D) Cigarette Smoking Status - Never/Ex-reg/Ex-occ/Current
  # cigst1 has been measured in ages >=16. I will code all ages below 16 as never smokers
  HSE2011 = copy(HSE2011original)  # initialise HSE2011
  basic <- c("agegroup", "qimd", "hpnssec8" )
  additional <- "cigst1"
  SPOP2011 <- simCategorical(HSE2011, SPOP2011, 
                             w = "wt.int", 
                             strata = "sex", 
                             basic = basic, 
                             additional = additional, 
                             method = "multinom", 
                             MaxNWts = 4000)
  
  # For Smoking cigdyal will be used ((D) Number of cigarettes smoke a day - inc. non-smokers)
  HSE2011 = copy(HSE2011original)
  limit <- list(cigst1 = list("1" = 0, "2" = 0, "3" = 0))
  censor <- list("0" = list(cigst1 = "4"))
  basic <- c("agegroup", "sex", "hpnssec8", "cigst1" )
  additional <- "cigdyal"
  SPOP2011 <- simContinuous(HSE2011, SPOP2011, 
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
  
  # For (would you) Like to give up smoking (Applicable only to smokers) I will use givupsk (1=yes
  # 2=no) I will recode all non-smokers to 99= non smoker
  HSE2011 = copy(HSE2011original)
  HSE2011[is.na(givupsk) & cigst1 != 4, givupsk := 99] # code non smokers as no smoker = 99
  breaks <- c(0, 1, 3, 6, 12, 17, 22, 27, 32, 37, 42, 80)
  HSE2011[, cigdyalCat := cut(cigdyal, breaks = breaks, 
                              include.lowest = F, 
                              ordered_result = F)]
  HSE2011[cigdyal == 0, cigdyalCat := "0"][, cigdyalCat := relevel(cigdyalCat, "0")]
  limit <- list(cigdyalCat = list("0" = "99"))
  censor <- list("99" = list(cigst1 = "4"))
  basic <- c("agegroup", "sex", "hpnssec8", "cigst1", "cigdyalCat" )
  additional <- "givupsk"
  SPOP2011 <- simCategorical(HSE2011, SPOP2011, 
                             w = "wt.int", 
                             strata = "qimd", 
                             basic = basic, 
                             additional = additional, 
                             method = "multinom", 
                             limit = limit,
                             censor = censor,
                             MaxNWts = 4000)
  
  # For How long ago did you stop smoking cigarettes? (Applicable only to ex-smokers) I will use endsmoke
  HSE2011 = copy(HSE2011original)
  breaks <- c(0, 1, 3, 6, 12, 17, 22, 27, 32, 37, 42, 80)
  HSE2011[, cigdyalCat := cut(cigdyal, breaks = breaks, 
                              include.lowest = F, 
                              ordered_result = F)]
  HSE2011[cigdyal == 0, cigdyalCat := "0"][, cigdyalCat := relevel(cigdyalCat, "0")]
  basic <- c("agegroup", "sex", "hpnssec8", "cigst1")
  additional <- "endsmoke"
  limit <- list(cigst1 = list("1" = "0", "4" = "0"))
  SPOP2011 <- simContinuous(HSE2011, SPOP2011, 
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
                            #censor     = censor,
                            MaxNWts    = 6000)
  
  # For passive smoking expsmok will be used (Number of hours/week exposed to others' smoke (c+sc)) To
  # be recoded into a categorical variable (expsmokCat)
  HSE2011 = copy(HSE2011original)
  breaks <- c(0, seq(0.1, 99.1, 10), max(HSE2011$expsmok, na.rm = T))
  HSE2011[, expsmokCat := cut(expsmok, breaks = breaks,
                              labels = c(0:10),
                              include.lowest = TRUE, 
                              ordered_result = TRUE)]
  basic <- c("agegroup", "sex", "hpnssec8", "cigst1" )
  additional <- "expsmokCat"
  SPOP2011 <- simCategorical(HSE2011, SPOP2011, 
                             w = "wt.int", 
                             strata = "qimd", 
                             basic = basic, 
                             additional = additional, 
                             method = "multinom", 
                             MaxNWts = 4000)
  
  # Prepare HSE2011 to inform the synth pop about behaviours/personalities.
  # For Health foundations lifestyle segment I will use segment (1=Hedonistic Immortal, 2=Live for
  # Today, 3=Unconfident Fatalist, 4=Health Conscious Realist, 5=Balanced Compensator) segment been
  # estimated for ages >=16.  I will recode ages <16 as 99= non-applicable
  HSE2011 = copy(HSE2011original)
  basic <- c("agegroup", "sex", "hpnssec8", "eqv5")
  additional <- c("segment")
  limit <- list(agegroup = list("<1   " = "99", "01-04" = "99", "05-09" = "99", "10-14" = "99"))
  SPOP2011 <- simCategorical(HSE2011, SPOP2011, 
                             w = "wt.int", 
                             strata = "qimd", 
                             basic = basic, 
                             additional = additional,
                             limit = limit,
                             method = "multinom", 
                             MaxNWts = 4000)
  
  # Used medication
  # lipid: use statin over last 7 days
  HSE2011 = copy(HSE2011original)
  basic <- c("agegroup", "sex", "hpnssec8")
  additional <- c("lipid")
  SPOP2011 <- simCategorical(HSE2011, SPOP2011, 
                             w = "wt.nurse", 
                             strata = "qimd", 
                             basic = basic, 
                             additional = additional, 
                             method = "multinom", 
                             MaxNWts = 4000)
  
  # bpmedc: (D) Whether taking drugs affecting blood pressure
  HSE2011[is.na(bpmedc)==T, bpmedc := 0]
  basic <- c("agegroup", "sex", "hpnssec8", "lipid")
  additional <- c("bpmedc")
  SPOP2011 <- simCategorical(HSE2011, SPOP2011, 
                             w = "wt.nurse", 
                             strata = "qimd", 
                             basic = basic, 
                             additional = additional, 
                             method = "multinom", 
                             MaxNWts = 6000)
  
  # For Redmeat (from HSE2008) Two vars are coding frequency of red meat consumption credmeat and
  # redmeatb. I will aggregate to redmeat
  # 1 = 6 or more times a week, 2 = 3-5 times a week, 3 = 1-2 times a week, 4 = Less than once a week, 5 = Rarely or never 
  # HSE2008 = copy(HSE2008original)
  # HSE2008[is.na(redmeatb) == F, redmeat := redmeatb]
  # HSE2008[is.na(credmeat) == F, redmeat := credmeat]
  # Redmeat was not available for ages <2 I will code age < 2 as 3 = 1 to 2 times a week (not fully apply because I use agegroups for synthesis)
  # HSE2008[is.na(redmeat) == T & age < 2, redmeat := 3]  #code ages of 0-1 as 3 = 1 to 2 times a week
  # HSE2008[eqv5 < 0, eqv5 := NA]
  # basic <- c("agegroup", "sex", "hpnssec8", "eqv5")
  # additional <- c("redmeat")
  # SPOP2011 <- simCategorical(HSE2008, SPOP2011, 
  #                            w = "wt.int", 
  #                            strata = "qimd", 
  #                            basic = basic, 
  #                            additional = additional, 
  #                            method = "multinom", 
  #                            MaxNWts = 6000)
  
  # PA 
  basic <- c("agegroup", "sex", "hpnssec8", "eqv5")
  additional <- c("a30to06m")
  SPOP2011 <- simCategorical(HSE2012original, SPOP2011, 
                             w = "wt.int", 
                             strata = "qimd", 
                             basic = basic, 
                             additional = additional, 
                             method = "multinom", 
                             MaxNWts = 6000)
  
  
  # For BMI I will use bmival (measured for for ages >=2) as continous variable
  HSE2011 = copy(HSE2011original)
  HSE2011[age<16 & is.na(a30to06m), a30to06m := "99"]
  basic <- c("agegroup", "sex", "eqv5", "porftvg", "a30to06m")
  additional <- c("bmival")
  limit <- list(agegroup = list("<1   " = 0))
  SPOP2011 <- simContinuous(HSE2011, SPOP2011, 
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
  
  
  # For diabetes I will use diabtyper : (D) Type of diabetes {revised} weighted with wt.int 
  # 1 = Diagnosed aged 35+ and/or not treated with insulin, 2 = Not diabetic, 3 = Diagnosed before the age of 35 and treated with insulin
  breaks <- c(0, 20, 25, 30, 35, 40, 50, Inf)
  HSE2011[, bmivalCat := cut(bmival, breaks = breaks,
                             include.lowest = TRUE, 
                             ordered_result = TRUE)]
  SPOP2011[, bmivalCat := cut(bmival, breaks = breaks,
                              include.lowest = TRUE, 
                              ordered_result = TRUE)]
  HSE2011[age < 16, diabtyper := 2] # not asked for ages < 16
  basic <- c("agegroup", "sex", "hpnssec8", "qimd", "a30to06m")
  additional <- c("diabtyper")
  SPOP2011 <- simCategorical(HSE2011, SPOP2011, 
                             w = "wt.int", 
                             strata = "bmivalCat", 
                             basic = basic, 
                             additional = additional, 
                             method = "multinom", 
                             MaxNWts = 6000)
  
  # Try to capture undiagnosed diabetes using 
  # diabtotr: (D) Total diabetes from blood sample or doctor diagnosis (excluding pregnancy-only diabetes)
  # 1 =  No diabetes, 2 = Doctor diagnosed diabetes and/or HbAlc>=6.5
  basic <- c("agegroup", "sex", "qimd", "diabtyper", "a30to06m")
  additional <- c("diabtotr")
  HSE2011[age < 16, diabtotr := 1]
  limit <- list(diabtyper = list("1" = "2", "3" = "2"))
  #censor <- list("1" = list(diabtyper = "1", diabtyper = "3"))
  SPOP2011 <- simCategorical(HSE2011, SPOP2011, 
                             w = "wt.blood", 
                             strata = "bmivalCat", 
                             basic = basic, 
                             additional = additional, 
                             method = "multinom",
                             limit = limit,
                             #censor = censor,
                             MaxNWts = 6000)
  SPOP2011[diabtyper == "1" & diabtotr == "1", diabtotr := "2"]
  
  # Calculate numsmok (number of cigarette smoked for ex smokers)
  HSE2011 = copy(HSE2011original)
  limit <- list(cigst1 = list("1" = 0, "2" = 0, "4" = 0))
  censor <- list("0" = list(cigst1 = "3"))
  basic <- c("agegroup", "sex", "hpnssec8", "cigst1" )
  additional <- "numsmok"
  SPOP2011 <- simContinuous(HSE2011, SPOP2011, 
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
  HSE2011 = copy(HSE2011original)
  HSE2011[startsmk==97, startsmk := NA] # 97 = "never smoke regularly"
  HSE2011[cigst1 == 4, duration := age - startsmk]  # Years smoking for smokers
  HSE2011[cigst1 < 4, duration := 0]  # Years smoked for non smokers
  breaks <- c(0, 1, 3, 6, 12, 17, 22, 27, 32, 37, 42, 80)
  HSE2011[, cigdyalCat := cut(cigdyal, breaks = breaks, 
                              include.lowest = F, 
                              ordered_result = F)]
  HSE2011[cigdyal == 0, cigdyalCat := "0"][, cigdyalCat := relevel(cigdyalCat, "0")]
  basic <- c("agegroup", "sex", "cigdyalCat", "cigst1")
  additional <- c("duration")
  limit <- list(cigst1 = list("1" = 0, "2" = 0, "3" = 0))
  
  SPOP2011 <- simContinuous(HSE2011, SPOP2011, 
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
  
  HSE2011[cigst1 == 4, smokyrs := 0]  # Years smoking for smokers
  HSE2011[cigst1 == 1, smokyrs := 0]  # Years smoked for non smokers
  breaks <- c(0, 1, 3, 6, 12, 17, 22, 27, 32, 37, 42, 80)
  HSE2011[, numsmokCat := cut(numsmok, breaks = breaks, 
                              include.lowest = F, 
                              ordered_result = F)]
  HSE2011[cigdyal == 0, numsmokCat := "0"][, numsmokCat := relevel(numsmokCat, "0")]
  basic <- c("agegroup", "sex", "numsmokCat", "cigst1")
  additional <- c("smokyrs")
  limit <- list(cigst1 = list("1" = 0, "4" = 0))
  
  SPOP2011 <- simContinuous(HSE2011, SPOP2011, 
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
  SPOP2011[cigst1 == "4" & as.numeric(as.character(age)) < (duration + 10), 
           duration := as.numeric(as.character(age)) - 10]
  SPOP2011[cigst1 %in% c("2", "3") & as.numeric(as.character(age)) < (smokyrs + 10), 
           smokyrs := as.numeric(as.character(age)) - 10]
  
  SPOP2011[cigst1 == "4", smokyrs := duration]

# Salt --------------------------------------------------------------------
  HSE2012 = copy(HSE2012original)
  
  # Recalculate 24h salt stochastically for each iteration
  HSE2012[age>15 & sex== "1",
                  Na24 := Na24.men.det(.N, sodiumval, creatin, potass, bmival, age)]
  HSE2012[age>15 & sex== "2",
                  Na24 := Na24.women.det(.N, sodiumval, creatin, potass, bmival, age)]
  HSE2012[, salt.intersalt := Na24 * 58.5/1000]
  HSE2012[salt.intersalt < 1, salt.intersalt := 1]
  
  breaks <- c(0, 20, 25, 30, 35, 40, 50, Inf)
  HSE2012[, bmivalCat := cut(bmival, breaks = breaks,
                             include.lowest = TRUE, 
                             ordered_result = TRUE)]
  SPOP2011[, bmivalCat := cut(bmival, breaks = breaks,
                              include.lowest = TRUE, 
                              ordered_result = TRUE)]
  basic <- c("agegroup", "sex", "eqv5", "hpnssec8", "bmivalCat")
  additional <- c("salt.intersalt")
  SPOP2011 <- simContinuous(HSE2012, SPOP2011, 
                            w = "wt.urine", 
                            strata = "qimd", 
                            basic = basic, 
                            additional = additional, 
                            method = "multinom", 
                            equidist = F,  
                            zeros = F, 
                            gpd = F, 
                            MaxNWts = 6000, 
                            keep = F)
  SPOP2011[, saltCat.intersalt := getCat(salt.intersalt,  HSE2012[, getBreaks(salt.intersalt, wt.urine, F, equidist = F)], F, T)]
  if (SPOP2011[is.na(salt.intersalt) == T, .N] > 0) {
    SPOP2011 <- SPOP2011[is.na(salt.intersalt) == F,] # Remove NA
  }
  
  # For systolic BP (wt.nurse) I will use omsysval ((D) Omron Valid Mean Systolic BP) omsysval was
  # measured in ages >=5
  HSE2011 = copy(HSE2011original)
  HSE2011[age<16 & is.na(a30to06m), a30to06m := "99"]
  breaks <- c(0, 14.4, 16.2, 17.7, 20.8, 24.2, 26.9, 30.2, 33.4, 36.9, 42.3, Inf)
  HSE2011[, bmivalCat := cut(bmival, breaks = breaks,
                             include.lowest = TRUE, 
                             ordered_result = TRUE)]
  SPOP2011[, bmivalCat := cut(bmival, breaks = breaks,
                              include.lowest = TRUE, 
                              ordered_result = TRUE)]
  #basic <- c("agegroup", "sex", "hpnssec8", "bmivalCat", "qimd") used in the current synthpops
  #additional <- c("omsysval")
  
  # "bpmedc" should be used but makes the fit worse and I don't use medication yet anyway
  basic <- c("agegroup", "sex", "bmivalCat", "cigst1", "a30to06m", "saltCat.intersalt")
  additional <- c("omsysval")
  SPOP2011 <- simContinuous(HSE2011, SPOP2011, 
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
  #spCdfplot(x = 'omsysval', weights = 'wt.nurse', cond = 'qimd', dataS = HSE2011[age>19,], dataP = SPOP2011[as.numeric(age)>19,])

  
  # For total Chol (wt.blood) I will use cholval1: (D) Valid Cholesterol Result (incl those on lld)
  # cholval1 was measured in ages >=16
  basic <- c("agegroup", "sex", "bmivalCat", "lipid", "a30to06m", "porftvg")
  additional <- c("cholval1")
  SPOP2011 <- simContinuous(HSE2011, SPOP2011, 
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
  #spCdfplot(x = 'cholval1', weights = 'wt.blood', cond = 'qimd', dataS = HSE2011[age>19,], dataP = SPOP2011[as.numeric(age)>19,])
  setnames(SPOP2011, c("cholval1", "cholval1Cat"), c("cholval", "cholvalCat"))

  
  # CVD prevalence
  # I will use cvdcon: (D) IHD/stroke but no IHD/None (1 = IHD, 2 = Stroke but no IHD, 3 = None of these) 
  # I will not use redmeat and physical exercise as independent variables because they were not included in HSE2011.
  # In the future I might use statmatch to inform HSE2011 with these variables from previous surveys 
  breaks <- c(0, 2.9, 3.5, 3.8, 4.2, 4.9, 5.4, 6.1, 6.7, 7.1, 8, Inf)
  HSE2011[, cholvalCat := cut(cholval, breaks = breaks,
                              include.lowest = TRUE, 
                              ordered_result = TRUE)]
  SPOP2011[, cholvalCat := cut(cholval, breaks = breaks,
                               include.lowest = TRUE, 
                               ordered_result = TRUE)]
  breaks <- c(0, 91.5, 100, 106, 111, 120, 129, 140, 150, 160, 176, Inf)
  HSE2011[, omsysvalCat := cut(omsysval, breaks = breaks,
                               include.lowest = TRUE, 
                               ordered_result = TRUE)]
  SPOP2011[, omsysvalCat := cut(omsysval, breaks = breaks,
                                include.lowest = TRUE, 
                                ordered_result = TRUE)]
  HSE2011[is.na(cvdcon) == T & age < 16, cvdcon := 3]  # code ages of 0-15 as non having CVD
  HSE2011[age <= 16, diabtyper := 2]
  HSE2011[is.na(bpmedc)==T, bpmedc := 0]
  
  basic <- c("agegroup", "sex", "bmivalCat", "lipid", "bpmedc", "cigst1",
             "diabtyper", "omsysvalCat", "cholvalCat", "a30to06m")
  additional <- c("cvdcon")
  limit <- list(agegroup = list("<1   " = "3", "01-04" = "3", "05-09" = "3",
                                "10-14" = "3", "15-19" = "3", "20-24" = "3")) # I set this because otherwise it produces very high prevalence of CVD
  SPOP2011 <- simCategorical(HSE2011, SPOP2011, 
                             w = "wt.blood", 
                             strata = "qimd", 
                             basic = basic, 
                             additional = additional, 
                             method = "multinom",
                             limit = limit,
                             MaxNWts = 6000)
  
  SPOP2011[as.numeric(as.character(age)) < 16, cvdcon := 3L] # To fix NA produced due to NA cholesterol
  if (SPOP2011[is.na(cvdcon) == T, .N] > 0) {
    SPOP2011 <- SPOP2011[is.na(cvdcon) == F,] # Remove NA
  }
  
 

# Finalise SPOP2011 -------------------------------------------------------
  SPOP2011[, age := as.integer(as.character(age))]
  SPOP2011[, sex := as.factor(sex)]
  SPOP2011[, numsmok := as.integer(numsmok)]
  SPOP2011[cigst1 == "2", numsmok:= 1L]
  SPOP2011[expsmokCat != "0", expsmokCat:= "1"][,expsmokCat := factor(expsmokCat)]
  SPOP2011[, a30to06m := as.integer(as.character(a30to06m))]
  SPOP2011[age<15, cigst1 := "1"]
  SPOP2011[age > 99, age := 99] # for combatibility with lifetables
  SPOP2011[, a30to06m := as.integer(as.character(a30to06m))]
  SPOP2011[, cigdyal := as.integer(cigdyal)]
  SPOP2011[, endsmoke := as.integer(endsmoke)]
  #SPOP2011[, frtpor := as.integer(as.character(frtpor))]
  SPOP2011[, hsize := as.integer(as.character(hsize))]
  SPOP2011[, porftvg := as.integer(as.character(porftvg))]
  SPOP2011[,  `:=` (endsmokeCat = NULL,
                    numsmokCat  = NULL,
                    duration    = NULL,
                    cigdyalCat  = NULL,
                    bmivalCat   = NULL, 
                    cholvalCat  = NULL,
                    omsysvalCat = NULL,
                    ageCat20    = NULL, 
                    qimd = ordered(qimd, levels=as.character(1:5)))]
  #SPOP2011[, bmival := round(bmival,1)]
  agegroup.fn(SPOP2011)
  setkey(SPOP2011, age, sex, agegroup, qimd)
  setcolorder(SPOP2011, SPOP2011[,order(names(SPOP2011))]) # reorder columns alphabeticaly
  # Return datatable
  return(saveRDS(SPOP2011, file = paste("/mnt/iusers01/mhs01/mdxasck2/IMPACTncd/SynthPop/spop2011-", i, ".rds", sep= "")))
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
rm(list = ls(all = TRUE))

