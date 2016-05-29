#!/opt/gridware/apps/gcc/R/3.2.3/lib64/R/bin/Rscript
# *******************************************************************************************
#                  England's synthetic population
# *******************************************************************************************

# User input
iterations <- 50 # Number of synthetic population to produce
prop <- 1/iterations  # Set proportion of the populations to be synthesized (set 1 for total population)
clusternumber <- 25 # Number of cores to be used ONLY VALID FOR LINUX
ncpu <- 1L
#setwd("G:/Dropbox/PhD/Models/SynthPop")
# Preample ----------------------------------------------------------------
gc()
dependencies <- function(x) {
  for (j in x) {
    if (!require(j, character.only = TRUE)) {
      install.packages(j, dependencies = TRUE)
      require(j, character.only = TRUE)
    }
  }
}

# Then try/install packages...
dependencies(c("simPop",
               "data.table",
               "pryr",
               "Hmisc",
               "doParallel",
               "doRNG",
               "compiler",
               "survey",
               "StatMatch"))
if (file.exists("./log.txt")) file.remove("./log.txt")
#setthreads(1)
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

# Import datasets (household and individual files) ----
load("hse05ai.RData")
HSE2005original <- clear.labels(HSE2005original)
HSE2005original <- setDT(HSE2005original, key="age")
HSE2005original <- HSE2005original[samptype==1] 
HSE2005original[, sha := newsha]
setnames(HSE2005original, c("imd2004", "area", "wt.bldel"), c("qimd", "psu", "wt.blood"))
agegroup.fn(HSE2005original)
HSE2005original[, `:=` (cholval1 = cholval1 + 0.1, hdlval1 = hdlval1 - 0.1)]
HSE2005original[diabete2 == 2, diabtotr := 1]
HSE2005original[diabete2 == 1 | glyhbval > 6.5, diabtotr := 2]
HSE2005original[porftvg == 0, porftvg := 1L]
HSE2005original[porftvg <= 9L, porftvg := porftvg - 1L]
HSE2005original[, `:=`(year=-6, a30to06m = NA, wt.urine = wt.nurse)]
HSE2005original[,  `:=` (psu     = paste0(psu, "2005"),
                         cluster = paste0(cluster, "2005"),
                         hserial = paste0(hserial, "2005"),
                         pserial = paste0(pserial, "2005")
)]
HSE2005original[, cluster := paste0(cluster, "2005")]

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
HSE2006original[,  `:=` (psu     = paste0(psu, "2006"),
                         cluster = paste0(cluster, "2006"),
                         hserial = paste0(hserial, "2006"),
                         pserial = paste0(pserial, "2006")
)]

load(file="hse07ai.RData")
HSE2007original <- setDT(clear.labels(HSE2007original), key = "age")
HSE2007original <- HSE2007original[samptype == 1]
setnames(HSE2007original, c("imd2007", "area", "hhsized", "newsha"), 
         c("qimd", "psu", "hhsize", "sha"))
agegroup.fn(HSE2007original)
HSE2007original[, `:=`(year=-4, diabtotr = NA, diabete2 = NA, wt.blood = 1, 
                       cholval1 = NA, a30to06m = NA, wt.urine = wt.nurse,
                       diabtype = NA)]
HSE2007original[,  `:=` (psu     = paste0(psu, "2007"),
                         cluster = paste0(cluster, "2007"),
                         hserial = paste0(hserial, "2007"),
                         pserial = paste0(pserial, "2007")
)]

intersectMany <- function(...) { Reduce(intersect, list(...)) } 
commoncols <- intersectMany(names(HSE2005original), names(HSE2006original), names(HSE2007original)) 

HSE2005original <- HSE2005original[, .SD, .SDcols = commoncols]
HSE2006original <- HSE2006original[, .SD, .SDcols = commoncols]
HSE2007original <- HSE2007original[, .SD, .SDcols = commoncols]

HSE <- rbindlist(list(HSE2005original, HSE2006original, HSE2007original), use.names=T, fill = F)

load("hse05ah.RData")
HSE2005hh <- clear.labels(HSE2005hh)
setDT(HSE2005hh, key="age")
HSE2005hh <- HSE2005hh[samptype == 1]
HSE2005hh[,  `:=` (hserial = paste0(hserial, "2005"),
                   pserial = paste0(pserial, "2005")
)]
tt <- unique(HSE2005original[, wt.hhld, by = hserial])
HSE2005hh[tt, on = "hserial", wt.hhld := wt.hhld]

load("hse06ah.RData")
HSE2006hh <- clear.labels(HSE2006hh)
setDT(HSE2006hh, key="age")
HSE2006hh <- HSE2006hh[samptype != 3]
HSE2006hh[,  `:=` (hserial = paste0(hserial, "2006"),
                   pserial = paste0(pserial, "2006")
)]
tt <- unique(HSE2006original[, wt.hhld, by = hserial])
HSE2006hh[tt, on = "hserial", wt.hhld := wt.hhld]

load("hse07ah.RData")
HSE2007hh <- clear.labels(HSE2007hh)
setDT(HSE2007hh, key="age")
HSE2007hh <- HSE2007hh[samptype == 1]
HSE2007hh[,  `:=` (hserial = paste0(hserial, "2007"),
                   pserial = paste0(pserial, "2007")
)]
tt <- unique(HSE2007original[, wt.hhld, by = hserial])
HSE2007hh[tt, on = "hserial", wt.hhld := wt.hhld]

commoncols <- intersectMany(names(HSE2005hh), names(HSE2006hh), names(HSE2007hh)) 
HSE2005hh <- HSE2005hh[, .SD, .SDcols = commoncols]
HSE2006hh <- HSE2006hh[, .SD, .SDcols = commoncols]
HSE2007hh <- HSE2007hh[, .SD, .SDcols = commoncols]

HSEhh <- rbindlist(list(HSE2005hh, HSE2006hh, HSE2007hh), use.names = T, fill = F)


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


# Further manipulation ----------------------------------------------------
HSE[eqv5 < 0, eqv5 := NA]
HSE[hpnssec8 > 8, hpnssec8 := NA]
HSE[is.na(porftvg) == T & age < 5, porftvg := 4]
HSE[porftvg == 0, porftvg := 1]
HSE[porftvg <= 9L, porftvg := porftvg - 1L]
HSE[is.na(cigdyal) & age < 16, cigdyal := 0]  # code ages of 0-15 as non smokers
HSE[is.na(cigst1) & age < 16, cigst1 := 1]  # code ages of 0-15 as never smokers
HSE[is.na(endsmoke) & (cigst1 == 1 | cigst1 == 4), endsmoke := 0]  # code non ex smokers as 0
HSE[is.na(numsmok) & cigst1 !=3, numsmok := 0]  # code non ex smokers as 0
setnames(HSE, "hhsize", "hsize")

# Calculate actual lld prescription and overwrite "lipid" variable
HSE[, lipid := 0L]
for (i in 1:9) {
  nam <- paste("medbi0", i, sep="")
  man <- paste("medbia", i, sep="")
  HSE[get(nam) == 21200 & get(man) == 1, lipid := 1L ]
}
for (i in 10:22) {
  nam <- paste("medbi", i, sep="")
  man <- paste("medbia", i, sep="")
  HSE[get(nam) == 21200 & get(nam) == 1, lipid := 1L ]
}
HSE[statins == 1, lipid := 1L] # incluse OTC statins
HSE[, lipid := as.factor(lipid)]

# Rescale weights of the household files (page 25 of HSE2011-Methods-and-docs.pdf)
# pop2010 <- 51818267 # Mid 2010 adjusted England's population (page 25 of
# HSE2011-Methods-and-docs.pdf)
pop20 <- 55e6  # Mid 2011 England's population (not adjusted) from ONS. I will use this for compatibility under the assumption that institutionalised population above 65 (excluded from original HSE) has the same characteristics as the rest of the population.
d <- pop20 * prop/HSE[, sum(wt.hhld, na.rm = T)]
HSEhh[, wt.hhld := wt.hhld * d]

# Recreate SHA information for household from the individual's file
#Temp = copy(HSE2011original[, list(hserial, sha)])
Temp  <- unique(HSE[, list(hserial, sha, qimd, eqv5, hpnssec8)], by = "hserial")  # Keep only unique values
HSEhh <- merge(HSEhh, Temp, by = "hserial")

rm(Temp, tt, HSE2005hh, HSE2005original, HSE2006hh, HSE2006original, HSE2007hh, HSE2007original)

# Create household size (by summing adults, children and neonates)
HSEhh[,hsize := adults + children + infants]
HSEhh = copy(HSEhh[age >= 0])
HSE[, wt.hhld := wt.hhld * d]
HSEhh[, reltohrp:=factor(reltohrp)]
HSEhh[reltohrp!="96", reltohrp:="relative"] # exact relations not needed atm
HSEhh[reltohrp=="96", reltohrp:="head"]
HSEhh[, reltohrp:=factor(reltohrp)]
HSE[HSEhh, on = "pserial", reltohrp := reltohrp]
HSEhh[, `:=` (sha = as.character(sha), reltohrp = as.character(reltohrp))]

# HSE[, relate := "nothead"]
# tt <- HSE[, .SD[1, pserial], by=hserial]
# HSE[tt, on = c("pserial" = "V1"), relate := "head"]
# HSE[, sum(relate == "head"), by = hserial][, table(V1)]


# Smoothing ---------------------------------------------------------------
# span.param <- 0.50
# cigst1.calib.data <- na.omit(HSE[, .(age, sex, qimd, cigst1, wt.int)])
# totals <- tableWt(cigst1.calib.data[, .(age,sex,cigst1)], weights=cigst1.calib.data[, wt.int])
# totals  <- setDT(as.data.frame(totals))
# totals[, age := as.integer(as.character(age))]
# totals[cigst1 == "4", plot(age, Freq, col = sex)]
# totals[, freq := round(predict(loess(Freq~age, span=span.param))), by = .(sex, cigst1)]
# totals[!between(age, 16, 84), freq := Freq]
# totals[is.na(freq), freq := 0L]
# totals[, age := factor(age)]
# setDF(totals[, Freq := NULL])
# wt.smok.calib <- calibSample(SPOP2006, totals)
# cigst1.calib.data[, wt.smok.calib := wt.smok.calib$final_weights]
# save(cigst1.calib.data, file = "./cigst1.calib.data.RData")
# 
# span.param <- 0.60
# SPOP2006@sample@data <-
#   na.omit(HSE[wt.blood>0,
#               .(age, sex, qimd, diabtotr, bmivalCat=getCat(bmival, bmi.breaks, zeros = F), wt.blood)])
# SPOP2006@sample@strata <- SPOP2006@pop@strata <-"bmivalCat"
# SPOP2006@sample@weight <- "wt.blood"
# diabtotr.calib.data <- na.omit(HSE[wt.blood>0,
#                                    .(age, sex, qimd, diabtotr, bmivalCat=getCat(bmival, bmi.breaks, zeros = F), wt.blood)])
# totals <- tableWt(diabtotr.calib.data[, .(age,sex,diabtotr)], weights=diabtotr.calib.data[, wt.blood])
# totals  <- setDT(as.data.frame(totals))
# totals[, age := as.integer(as.character(age))]
# totals[diabtotr == "2", plot(age, Freq, col = sex)]
# totals[, freq := predict(loess(Freq~age, span=span.param)), by = .(sex, diabtotr)]
# totals[diabtotr == "2", plot(age, freq, col = sex)]
# totals[!between(age, 16, 88), freq := Freq]
# totals[is.na(freq), freq := 0L]
# totals[freq<0, freq := 0L]
# totals[, age := factor(age)]
# setDF(totals[, Freq := NULL])
# wt.diab.calib <- calibSample(SPOP2006, totals)
# diabtotr.calib.data[, wt.diab.calib := wt.diab.calib$final_weights]
# save(diabtotr.calib.data, file = "./diabtotr.calib.data.RData")

load("./cigst1.calib.data.RData")
load("./diabtotr.calib.data.RData")
setDT(cigst1.calib.data)
setDT(diabtotr.calib.data)
# Define SynthPOP function ------------------------------------------------
SynthPOP <- function(i = 1) {
  sink(file = "./log.txt",
       append = T, 
       type = "output",
       split = F)
  inp <- specifyInput(data=setDF(HSEhh), hhid="hserial", 
                      strata="sha", weight="wt.hhld")
  
  # Create household structure of the synthetic population
  cat("structure\n")
  SPOP2006 <- simStructure(inp, 
                           basicHHvars = c("age", "sex", "reltohrp"), 
                           method     = "multinom",
                           seed       = as.integer(runif(1, 1e3, 1e6)))
  
  # Add qimd (quantile of index of multiple deprivation) variable (I started from it because no NAs)
  #SPOP2006@basicHHvars <- c("age", "sex")
  #dire <- HSEhh[, levels(factor(reltohrp))]
  #dire <- dire[dire!="head"] #  remove head
  pop(SPOP2006, "reltohrp") <- factor(pop(SPOP2006, "reltohrp"))# BUG in checkFactor if reltohrp not factor because is is duplicate in convert
  samp(SPOP2006, "reltohrp") <- factor(samp(SPOP2006, "reltohrp"))# BUG in checkFactor if reltohrp not factor because is is duplicate in convert
  cat("qimd\n")
  SPOP2006 <- simRelation(SPOP2006,
                          relation   = "reltohrp",
                          head       = "head",
                          direct     = "relative",
                          additional = "qimd", 
                          nr_cpus    =  ncpu,
                          seed       =  as.integer(runif(1, 1e3, 1e6)))
  # SPOP2006@pop@data[is.na(qimd), table(age)] # few NA 
  
  # Start using qimd for strata from now on (5= more deprived) and add more SEC indicators
  #SPOP2006@basicHHvars <- c("age", "sex")
  SPOP2006@sample@strata <- SPOP2006@pop@strata <-"qimd"
  SPOP2006@pop@data <- na.omit(SPOP2006@pop@data)
  cat("eqv5\n")
  SPOP2006 <- simRelation(SPOP2006,
                          additional = "eqv5", 
                          relation   = "reltohrp",
                          head       = "head",
                          direct     = "relative",
                          nr_cpus    = ncpu,
                          seed       =  as.integer(runif(1, 1e3, 1e6)))
  # SPOP2006@pop@data[is.na(eqv5), table(age)] # few NA 
  
  
  SPOP2006@basicHHvars <- c((SPOP2006@basicHHvars), "eqv5")
  SPOP2006@pop@data <- na.omit(SPOP2006@pop@data)
  cat("hpnssec8\n")
  SPOP2006 <- simRelation(SPOP2006,
                          additional = "hpnssec8", 
                          relation   = "reltohrp",
                          head       = "head",
                          direct     = "relative",
                          nr_cpus    = ncpu,
                          seed       =  as.integer(runif(1, 1e3, 1e6)))
  # SPOP2006@pop@data[is.na(hpnssec8), table(age)] # few NA in ages 98-99
  
  
  # For smoking history cigst1 will be used (D) Cigarette Smoking Status - Never/Ex-reg/Ex-occ/Current
  # cigst1 has been measured in ages >=16. I will code all ages below 16 as never smokers
  SPOP2006@sample@data   <- na.omit(HSE[, .(age, sex, qimd, cigst1, wt.int)])
  SPOP2006@sample@strata <- SPOP2006@pop@strata <-"qimd"
  SPOP2006@sample@weight <- "wt.int"
  SPOP2006@pop@data <- na.omit(SPOP2006@pop@data)
  cat("cigst1\n")
  SPOP2006 <- simCategorical(SPOP2006, 
                             additional = "cigst1", 
                             method     = "multinom", 
                             regModel   = ~ age + sex, 
                             MaxNWts    = 4000,
                             nr_cpus    = ncpu,
                             seed       =  as.integer(runif(1, 1e3, 1e6)))
  
  # SPOP2006@pop@data[is.na(cigst1), table(age)] # few NA in ages 98-99
  SPOP2006@pop@data[is.na(cigst1), cigst1 := "1"]
  pop(SPOP2006, "agegroup") <- agegroup.fn(as.integer(as.character(pop(SPOP2006, "age"))))
  
  # validation
  # SPOP2006@sample@data[, agegroup := agegroup.fn(as.integer(as.character(age)))]
  # spCdfplot(SPOP2006, "cigst1", cond=NULL)
  # spCdfplot(SPOP2006, "cigst1", cond="sex")
  # spCdfplot(SPOP2006, "cigst1", cond="agegroup")
  # spCdfplot(SPOP2006, "cigst1", cond="qimd")
  
  
  # cigst1 smoothing --------------------------------------------------------
  # ss <- setDT(copy(SPOP2006@pop@data))
  # ss[, age := as.integer(as.character(age))]
  # setkey(ss, age, sex, qimd)
  # # ss[, sum(cigst1=="4")/.N, by = .(age, sex)][, plot(age, V1, col = sex)]
  # tt <- ss[rbinom(.N, 1, 1/10) == 1L, #sample 1/3 for faster converge
  #          .(cigst1.1 = sum(cigst1=="1")/.N,
  #            cigst1.2 = sum(cigst1=="2")/.N,
  #            cigst1.3 = sum(cigst1=="3")/.N,
  #            cigst1.4 = sum(cigst1=="4")/.N, 
  #            pop = .N, `1` = 1, `2` = 0, `3` = 0, `4` = 0),
  #          by = .(age, sex)]
  # span.param <- runif(1, 0.25, 0.50)
  # tt[age>15,  `:=` (`1` = predict(loess(cigst1.1~age, span=span.param)),
  #                   `3` = predict(loess(cigst1.3~age, span=span.param)),
  #                   `4` = predict(loess(cigst1.4~age, span=span.param))),
  #    by = .(sex)]
  # tt[, `2` := 1 - `1` - `3`- `4`]
  # tt[!between(`1`, 0, 1), `1` := 1]
  # tt[!between(`2`, 0, 1), `2` := 1]
  # tt[!between(`3`, 0, 1), `3` := 1]
  # tt[!between(`4`, 0, 1), `4` := 1]
  # 
  # tt[, plot(age, cigst1.1, col = sex)]
  # tt[sex == "1", lines(age, `1`)]
  # tt[sex == "2", lines(age, `1`, col = "red")]
  # tt[, plot(age, cigst1.2, col = sex)]
  # tt[sex == "1", lines(age, `2`)]
  # tt[sex == "2", lines(age, `2`, col = "red")]
  # tt[, plot(age, cigst1.3, col = sex)]
  # tt[sex == "1", lines(age, `3`)]
  # tt[sex == "2", lines(age, `3`, col = "red")]
  # tt[, plot(age, cigst1.4, col = sex)]
  # tt[sex == "1", lines(age, `4`)]
  # tt[sex == "2", lines(age, `4`, col = "red")]
  # tt[, `:=` (`1` = round(`1` * pop), `3` = round(`3` * pop), `4` = round(`4` * pop))]
  # tt[, `2` := pop - `1` - `3` - `4`]
  # tt[`1` < 0, `1` := 0 ]
  # tt[`2` < 0, `2` := 0 ]
  # tt[`3` < 0, `3` := 0 ]
  # tt[`4` < 0, `4` := 0 ]
  # tt <- melt(tt, c("age", "sex"), c("1","2", "3", "4"), variable.name = "cigst1", value.name = "freq")
  # tt[, age := factor(age)]
  # 
  # totals.samp <- tableWt(SPOP2006@sample@data[, .(age,sex,cigst1)], weights=SPOP2006@sample@data[, wt.int])
  # totals.samp <- setDT(as.data.frame(totals.samp))
  # totals <- tt[totals.samp, on = c("age", "sex", "cigst1")]
  # totals[is.na(freq), freq := 0L]
  # setDF(totals[, Freq := NULL])
  # cat("cigst1.smoothing\n")
  # wt.smok.calib <- calibSample(SPOP2006, totals)
  # cat("post cigst1.smoothing\n")
  SPOP2006@sample@data   <- copy(cigst1.calib.data)
  SPOP2006@sample@weight <- "wt.smok.calib"
  setnames(SPOP2006@pop@data, "cigst1", "cigst1.raw")
  cat("cigst1.smoothed\n")
  SPOP2006 <- simCategorical(SPOP2006, 
                             additional = "cigst1", 
                             method     = "multinom", 
                             regModel   = ~ age + sex, 
                             MaxNWts    = 4000,
                             nr_cpus    = ncpu,
                             seed       =  as.integer(runif(1, 1e3, 1e6)))
  SPOP2006@pop@data[is.na(cigst1), cigst1 := "1"]
  
  
  # For Fruit and Veg porftvg will be used (maximum 1 portion of juice, pulses or dried fruit
  # contributed to the total portions in porfv(continous var)) porftvg was measured for ages >=5 For
  # ages <5 I will code them as 99 = non applicable
  SPOP2006@sample@data   <- na.omit(HSE[, .(age, sex, qimd, porftvg, wt.int)])
  SPOP2006@sample@strata <- SPOP2006@pop@strata <-"qimd"
  SPOP2006@sample@weight <- "wt.int"
  SPOP2006@pop@data <- na.omit(SPOP2006@pop@data)
  cat("porftvg\n")
  SPOP2006 <- simCategorical(SPOP2006, 
                             additional = "porftvg", 
                             method     = "multinom", 
                             regModel   = ~ age + sex, 
                             MaxNWts    = 4000,
                             nr_cpus    = ncpu,
                             seed       =  as.integer(runif(1, 1e3, 1e6)))
  # SPOP2006@pop@data[is.na(porftvg), table(age)] # few NA in ages 98-99
  SPOP2006@pop@data[is.na(porftvg), porftvg := sample(8, .N, T)]
  
  # validation
  # SPOP2006@sample@data[, agegroup := agegroup.fn(as.integer(as.character(age)))]
  # spCdfplot(SPOP2006, "porftvg", cond=NULL)
  # spCdfplot(SPOP2006, "porftvg", cond="sex")
  # spCdfplot(SPOP2006, "porftvg", cond="agegroup")
  # spCdfplot(SPOP2006, "porftvg", cond="qimd")
  
  # For Smoking cigdyal will be used ((D) Number of cigarettes smoke a day - inc. non-smokers)
  # (measured for ages <=16) I will code ages < 16 as 0 (ie non smokers) I will recode into levels by 1
  # cigar and aggregate more than 40 cigars into 40. To be named cigdyalCat
  limit <- list(cigst1 = list("1" = 0, "2" = 0, "3" = 0))
  censor <- list("0" = list(cigst1 = "4"))
  SPOP2006@sample@data   <- na.omit(HSE[, .(age, sex, qimd, cigst1, cigdyal, wt.int)])
  SPOP2006@sample@data[age>90, age := 90]
  SPOP2006@sample@weight <- "wt.int"
  SPOP2006@sample@strata <- SPOP2006@pop@strata <-"qimd"
  SPOP2006@pop@data <- na.omit(SPOP2006@pop@data)
  cat("cigdyal\n")
  SPOP2006 <- simContinuous(SPOP2006, 
                            additional = "cigdyal", 
                            method     = "multinom", 
                            regModel   = ~ age + sex + cigst1, 
                            MaxNWts    = 6000,
                            zeros      = T, 
                            limit      = limit,
                            censor     = censor,
                            gpd        = T,
                            keep       = F,
                            imputeMissings = T,
                            nr_cpus    = ncpu,
                            seed       =  as.integer(runif(1, 1e3, 1e6)))
  set2key(SPOP2006@pop@data, NULL)
  # SPOP2006@pop@data[is.na(cigdyal), table(age)] # missing >90 age
  SPOP2006@pop@data[is.na(cigdyal) & cigst1 != "4", cigdyal := 0]
  imp <- SPOP2006@pop@data[age == "90" & cigst1 == "4", mean(cigdyal, na.rm = T)]
  SPOP2006@pop@data[is.na(cigdyal) & cigst1 == "4", cigdyal := imp]
  
  # validation
  # SPOP2006@sample@data[, agegroup := agegroup.fn(as.integer(as.character(age)))]
  # spCdfplot(SPOP2006, "cigdyal", cond=NULL)
  # spCdfplot(SPOP2006, "cigdyal", cond="sex")
  # spCdfplot(SPOP2006, "cigdyal", cond="agegroup")
  # spCdfplot(SPOP2006, "cigdyal", cond="qimd")
  
  # For how long ago did you stop smoking cigarettes? (Applicable only to ex-smokers) I will use endsmoke
  limit <- list(cigst1 = list("1" = "0", "4" = "0"))
  SPOP2006@sample@data   <- na.omit(HSE[, .(age, sex, qimd, cigst1, endsmoke, wt.int)])
  SPOP2006@sample@data[age>90, age := 90]
  SPOP2006@sample@weight <- "wt.int"
  SPOP2006@sample@strata <- SPOP2006@pop@strata <-"qimd"
  SPOP2006@pop@data <- na.omit(SPOP2006@pop@data)
  cat("endsmoke\n")
  SPOP2006 <- simContinuous(SPOP2006, 
                            additional = "endsmoke", 
                            method     = "multinom", 
                            regModel   = ~ age + sex + cigst1, 
                            MaxNWts    = 6000,
                            zeros      = T, 
                            limit      = limit,
                            gpd        = T,
                            keep       = F,
                            imputeMissings = T,
                            nr_cpus    = ncpu,
                            seed       =  as.integer(runif(1, 1e3, 1e6)))
  # SPOP2006@pop@data[is.na(endsmoke), table(age)] 
  set2key(SPOP2006@pop@data, NULL)
  SPOP2006@pop@data[cigst1 %in% c("1", "4"), endsmoke := 0] # for safety
  imp <- SPOP2006@pop@data[age == "90" & cigst1 %in% c("2", "3"), mean(endsmoke, na.rm = T)]
  SPOP2006@pop@data[is.na(endsmoke) & cigst1 %in% c("2", "3"), endsmoke := imp]
  
  # validation
  # SPOP2006@sample@data[, agegroup := agegroup.fn(as.integer(as.character(age)))]
  # spCdfplot(SPOP2006, "endsmoke", cond=NULL)
  # spCdfplot(SPOP2006, "endsmoke", cond="sex")
  # spCdfplot(SPOP2006, "endsmoke", cond="agegroup")
  # spCdfplot(SPOP2006, "endsmoke", cond="qimd")
  
  # For passive smoking expsmok will be used (Number of hours/week exposed to others' smoke (c+sc)) To
  # be recoded into a categorical variable (expsmokCat)
  HSE[expsm > 0, expsmokCat := "1"]
  HSE[expsm == 0, expsmokCat := "0"]
  HSE[, expsmokCat := factor(expsmokCat)]
  SPOP2006@sample@data   <- na.omit(HSE[, .(age, sex, qimd, expsmokCat, cigst1, wt.int)])
  SPOP2006@sample@strata <- SPOP2006@pop@strata <-"qimd"
  SPOP2006@sample@weight <- "wt.int"
  SPOP2006@pop@data <- na.omit(SPOP2006@pop@data)
  cat("expsmokCat\n")
  SPOP2006 <- simCategorical(SPOP2006, 
                             additional = "expsmokCat", 
                             method     = "multinom", 
                             regModel   = ~ age + sex + cigst1, 
                             MaxNWts    = 4000,
                             limit      = limit,
                             nr_cpus    = ncpu,
                             seed       =  as.integer(runif(1, 1e3, 1e6)))
  set2key(SPOP2006@pop@data, NULL)
  set2key(SPOP2006@sample@data, NULL)
  # SPOP2006@pop@data[is.na(expsmokCat), table(age)] # all for ages < 13
  imp <- SPOP2006@pop@data[age %in% "13", sum(expsmokCat == "1", na.rm = T)/ .N]
  SPOP2006@pop@data[is.na(expsmokCat), expsmokCat := factor(rbinom(.N, 1, imp/2))] # /2 because parents protect small children
  
  # validation
  # SPOP2006@sample@data[, agegroup := agegroup.fn(as.integer(as.character(age)))]
  # spCdfplot(SPOP2006, "expsmokCat", cond=NULL)
  # spCdfplot(SPOP2006, "expsmokCat", cond="sex")
  # spCdfplot(SPOP2006, "expsmokCat", cond="agegroup")
  # spCdfplot(SPOP2006, "expsmokCat", cond="qimd")
  
  
  # Used medication
  # lipid: use statin over last 7 days
  SPOP2006@sample@data   <- na.omit(HSE[, .(age, sex, qimd, lipid, wt.nurse)])
  SPOP2006@sample@strata <- SPOP2006@pop@strata <-"qimd"
  SPOP2006@sample@weight <- "wt.nurse"
  SPOP2006@pop@data <- na.omit(SPOP2006@pop@data)
  cat("lipid\n")
  SPOP2006 <- simCategorical(SPOP2006, 
                             additional = "lipid", 
                             method     = "multinom", 
                             regModel   = ~ age + sex, 
                             MaxNWts    = 4000,
                             nr_cpus    = ncpu,
                             seed       =  as.integer(runif(1, 1e3, 1e6)))
  set2key(SPOP2006@pop@data, NULL)
  set2key(SPOP2006@sample@data, NULL)
  # SPOP2006@pop@data[is.na(lipid), table(age)] 
  
  # validation
  # SPOP2006@sample@data[, agegroup := agegroup.fn(as.integer(as.character(age)))]
  # spCdfplot(SPOP2006, "lipid", cond=NULL)
  # spCdfplot(SPOP2006, "lipid", cond="sex")
  # spCdfplot(SPOP2006, "lipid", cond="agegroup")
  # spCdfplot(SPOP2006, "lipid", cond="qimd")
  
  # bpmedc: (D) Whether taking drugs affecting blood pressure
  SPOP2006@sample@data   <- na.omit(HSE[, .(age, sex, bpmedc, wt.nurse)])
  SPOP2006@sample@strata <- SPOP2006@pop@strata <-"sex"
  SPOP2006@sample@weight <- "wt.nurse"
  SPOP2006@pop@data <- na.omit(SPOP2006@pop@data)
  cat("bpmed\n")
  SPOP2006 <- simCategorical(SPOP2006, 
                             additional = "bpmedc", 
                             method     = "multinom", 
                             regModel   = ~ age, 
                             MaxNWts    = 4000,
                             nr_cpus    = ncpu,
                             seed       =  as.integer(runif(1, 1e3, 1e6)))
  set2key(SPOP2006@pop@data, NULL)
  set2key(SPOP2006@sample@data, NULL)
  # SPOP2006@pop@data[is.na(bpmedc), table(age)] # gew in ages >97
  SPOP2006@pop@data[is.na(bpmedc), bpmedc := "0"] 
  
  # validation
  # SPOP2006@sample@data[, agegroup := agegroup.fn(as.integer(as.character(age)))]
  # spCdfplot(SPOP2006, "bpmedc", cond = NULL)
  # spCdfplot(SPOP2006, "bpmedc", cond = "sex")
  # spCdfplot(SPOP2006, "bpmedc", cond = "agegroup")
  # spCdfplot(SPOP2006, "bpmedc", cond = "qimd")
  
  # PA 
  HSE[age<16, a30to06m := as.numeric(sample(7, .N, T))]
  SPOP2006@sample@data   <- na.omit(HSE[, .(age, sex, qimd, a30to06m, wt.int)])
  SPOP2006@sample@data[, agegroup := agegroup.fn(as.integer(as.character(age)))]
  SPOP2006@pop@data[, agegroup := agegroup.fn(as.integer(as.character(age)))]
  SPOP2006@sample@strata <- SPOP2006@pop@strata <-"qimd"
  SPOP2006@sample@weight <- "wt.int"
  SPOP2006@pop@data <- na.omit(SPOP2006@pop@data)
  cat("a30to06m\n")
  SPOP2006 <- simCategorical(SPOP2006, 
                             additional = "a30to06m", 
                             method     = "multinom", 
                             regModel   = ~ age + sex, 
                             MaxNWts    = 4000,
                             nr_cpus    = ncpu,
                             seed       =  as.integer(runif(1, 1e3, 1e6)))
  set2key(SPOP2006@pop@data, NULL)
  set2key(SPOP2006@sample@data, NULL)
  # SPOP2006@pop@data[is.na(a30to06m), table(age)] # missing ages >97
  imp <- SPOP2006@pop@data[age == "96", mean(as.numeric(as.character(a30to06m)), na.rm = T)]
  SPOP2006@pop@data[is.na(a30to06m), a30to06m := as.character(round(imp))] 
  
  # validation
  # SPOP2006@sample@data[, agegroup := agegroup.fn(as.integer(as.character(age)))]
  # spCdfplot(SPOP2006, "a30to06m", cond = NULL)
  # spCdfplot(SPOP2006, "a30to06m", cond = "sex")
  # spCdfplot(SPOP2006, "a30to06m", cond = "agegroup")
  # spCdfplot(SPOP2006, "a30to06m", cond = "qimd")
  
  # For BMI I will use bmival (measured for for ages >=2) as continous variable
  SPOP2006@sample@data   <- na.omit(HSE[, .(age, sex, qimd, bmival, porftvg, wt.nurse)])
  SPOP2006@sample@data[age>90, age := 90]
  SPOP2006@sample@weight <- "wt.nurse"
  SPOP2006@sample@strata <- SPOP2006@pop@strata <-"qimd"
  SPOP2006@pop@data <- na.omit(SPOP2006@pop@data)
  cat("bmival\n")
  SPOP2006 <- simContinuous(SPOP2006, 
                            additional = "bmival", 
                            method     = "multinom", 
                            regModel   = ~ age + sex + porftvg, 
                            MaxNWts    = 6000,
                            zeros      = F, 
                            gpd        = T,
                            keep       = F,
                            imputeMissings = T,
                            nr_cpus    = ncpu,
                            seed       =  as.integer(runif(1, 1e3, 1e6)))
  # SPOP2006@pop@data[is.na(bmival), table(age)] # missing ages >90
  set2key(SPOP2006@pop@data, NULL)
  imp <- SPOP2006@pop@data[age == "90", mean(bmival, na.rm = T)]
  SPOP2006@pop@data[is.na(bmival), bmival := imp]
  
  # validation
  # SPOP2006@sample@data[, agegroup := agegroup.fn(as.integer(as.character(age)))]
  # spCdfplot(SPOP2006, "bmival", cond=NULL)
  # spCdfplot(SPOP2006, "bmival", cond="sex")
  # spCdfplot(SPOP2006, "bmival", cond="agegroup")
  # spCdfplot(SPOP2006, "bmival", cond="qimd")
  
  # For diabetes I will use diabtype : (D) Type of diabetes {revised} weighted with wt.int 
  # 1 = Diagnosed aged 35+ and/or not treated with insulin, 2 = Not diabetic, 3 = Diagnosed before the age of 35 and treated with insulin
  bmi.breaks <- SPOP2006@pop@data[, getBreaks(bmival, probs = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.97, 0.99), zeros = F)]
  pop(SPOP2006, "bmivalCat") <- getCat(pop(SPOP2006, "bmival"), bmi.breaks, zeros = F)
  SPOP2006@sample@data <-na.omit(HSE[, .(age, sex, qimd, diabtype, bmival, bmivalCat=getCat(bmival, bmi.breaks, zeros = F), wt.nurse)])
  SPOP2006@sample@strata <- SPOP2006@pop@strata <-"bmivalCat"
  SPOP2006@sample@weight <- "wt.nurse"
  SPOP2006@pop@data <- na.omit(SPOP2006@pop@data)
  cat("diabtype\n")
  SPOP2006 <- simCategorical(SPOP2006, 
                             additional = "diabtype", 
                             method     = "multinom", 
                             regModel   = ~ age + sex + qimd, 
                             MaxNWts    = 4000,
                             nr_cpus    = ncpu,
                             seed       =  as.integer(runif(1, 1e3, 1e6)))
  set2key(SPOP2006@pop@data, NULL)
  set2key(SPOP2006@sample@data, NULL)
  # SPOP2006@pop@data[is.na(diabtype), table(age)] # missing ages >97
  imp <- SPOP2006@pop@data[age %in% "90", sum(diabtype == "2", na.rm = T)/ .N]
  SPOP2006@pop@data[is.na(diabtype), diabtype := factor(1 + rbinom(.N, 1, imp))]
  
  # validation
  # SPOP2006@sample@data[, agegroup := agegroup.fn(as.integer(as.character(age)))]
  # spCdfplot(SPOP2006, "diabtype", cond = NULL)
  # spCdfplot(SPOP2006, "diabtype", cond = "sex")
  # spCdfplot(SPOP2006, "diabtype", cond = "agegroup")
  # spCdfplot(SPOP2006, "diabtype", cond = "qimd")
  
  # Try to capture undiagnosed diabetes using 
  # diabtotr: (D) Total diabetes from blood sample or doctor diagnosis (excluding pregnancy-only diabetes)
  # 1 =  No diabetes, 2 = Doctor diagnosed diabetes and/or HbAlc>=6.5
  
  SPOP2006@sample@data <-na.omit(HSE[
    wt.blood>0,
    .(age, sex, qimd, diabtotr, bmivalCat=getCat(bmival, bmi.breaks, zeros = F), wt.blood)])
  #limit <- list(diabtype = list("1" = "2", "3" = "2"))
  #censor <- list("1" = list(diabtype = "1", diabtype = "3"))
  SPOP2006@sample@strata <- SPOP2006@pop@strata <-"bmivalCat"
  SPOP2006@sample@weight <- "wt.blood"
  SPOP2006@pop@data <- na.omit(SPOP2006@pop@data)
  SPOP2006@sample@data[age>90, age := 90]
  cat("diabtotr\n")
  SPOP2006 <- simCategorical(SPOP2006, 
                             additional = "diabtotr", 
                             method     = "multinom", 
                             regModel   = ~ age + sex + qimd, 
                             MaxNWts    = 4000,
                             nr_cpus    = ncpu,
                             seed       =  as.integer(runif(1, 1e3, 1e6)))
  set2key(SPOP2006@pop@data, NULL)
  set2key(SPOP2006@sample@data, NULL)
  # SPOP2006@pop@data[is.na(diabtotr), table(age)] # missing ages >97 and < 16
  SPOP2006@pop@data[is.na(diabtotr), diabtotr := "1"]
  imp <- SPOP2006@pop@data[age %in% "85", sum(diabtotr == "2", na.rm = T)/ .N]
  SPOP2006@pop@data[age %in% as.character(91:99), diabtotr := factor(1 + rbinom(.N, 1, imp/4))]
  
  # validation
  # SPOP2006@sample@data[, agegroup := agegroup.fn(as.integer(as.character(age)))]
  # SPOP2006@sample@data[, agegroup := factor(agegroup)]
  # spCdfplot(SPOP2006, "diabtotr", cond = NULL)
  # spCdfplot(SPOP2006, "diabtotr", cond = "sex")
  # spCdfplot(SPOP2006, "diabtotr", cond = "agegroup")
  # spCdfplot(SPOP2006, "diabtotr", cond = "qimd")
  
  
  # diabtotr smoothing ------------------------------------------------------
  # ss <- setDT(copy(SPOP2006@pop@data))
  # ss[, age := as.integer(as.character(age))]
  # setkey(ss, age, sex, qimd)
  # ss[, sum(diabtotr=="2")/.N, by = .(age, sex)][, plot(age, V1, col = sex)]
  # tt <- ss[rbinom(.N, 1, 1/10) == 1L, #sample 1/3 for faster converge,
  #          .(diabtotr.1 = sum(diabtotr == "1")/.N,
  #            diabtotr.2 = sum(diabtotr == "2")/.N,
  #            pop = .N, `1` = 1, `2` = 0),
  #          by = .(age, sex)]
  # span.param <- runif(1, 0.60, 0.80)
  # tt[between(age, 16, 90),  `:=` (`1` = predict(loess(diabtotr.1~age, span=span.param))),
  #    by = .(sex)]
  # tt[, `2` := 1 - `1`]
  # tt[!between(`1`, 0, 1), `1` := 1]
  # tt[!between(`2`, 0, 1), `2` := 0]
  # 
  # tt[, plot(age, diabtotr.1, col = sex)]
  # tt[sex == "1", lines(age, `1`)]
  # tt[sex == "2", lines(age, `1`, col = "red")]
  # tt[, plot(age, diabtotr.2, col = sex)]
  # tt[sex == "1", lines(age, `2`)]
  # tt[sex == "2", lines(age, `2`, col = "red")]
  
  # tt[, `:=` (`1` = round(`1` * pop))]
  # tt[`1` < 0, `1` := 0 ]
  # tt[!between(age, 16, 90), `1`:= round(diabtotr.1 * pop)]
  # tt[, `2` := pop - `1`]
  # tt <- melt(tt, c("age", "sex"), c("1","2"), variable.name = "diabtotr", value.name = "freq")
  # tt[, age := factor(age)]
  # 
  # totals.samp <- tableWt(SPOP2006@sample@data[, .(age,sex,diabtotr)], weights=SPOP2006@sample@data[, wt.blood])
  # totals.samp <- setDT(as.data.frame(totals.samp))
  # totals <- tt[totals.samp, on = c("age", "sex", "diabtotr")]
  # totals[is.na(freq), freq := 0L]
  # setDF(totals[, Freq := NULL])
  # cat("diabtotr.smoothing\n")
  # wt.diab.calib <- calibSample(SPOP2006, totals)
  # cat("post diabtotr.smoothing\n")
  #SPOP2006@sample@data[, wt.diab.calib := wt.diab.calib$final_weights]
  SPOP2006@sample@data <- copy(diabtotr.calib.data)
  SPOP2006@sample@weight <- "wt.diab.calib"
  setnames(SPOP2006@pop@data, "diabtotr", "diabtotr.raw")
  SPOP2006@sample@strata <- SPOP2006@pop@strata <-"bmivalCat"
  cat("diabtotr.smoothed\n")
  SPOP2006 <- simCategorical(SPOP2006, 
                             additional = "diabtotr", 
                             method     = "multinom", 
                             regModel   = ~ age + sex + qimd, 
                             MaxNWts    = 4000,
                             nr_cpus    = ncpu,
                             seed       =  as.integer(runif(1, 1e3, 1e6)))
  set2key(SPOP2006@pop@data, NULL)
  set2key(SPOP2006@sample@data, NULL)
  # SPOP2006@pop@data[is.na(diabtotr), table(age)] # missing ages >97
  SPOP2006@pop@data[is.na(diabtotr), diabtotr := "1"]
  imp <- SPOP2006@pop@data[age %in% "85", sum(diabtotr == "2", na.rm = T)/ .N]
  SPOP2006@pop@data[age %in% as.character(91:99), diabtotr := factor(1 + rbinom(.N, 1, imp/4))]
  
  
  # Calculate numsmok (number of cigarette smoked for ex smokers)
  SPOP2006@sample@data   <- na.omit(HSE[, .(age, sex, qimd, numsmok, cigst1, wt.int)])
  SPOP2006@sample@data[age>90, age := 90]
  SPOP2006@sample@weight <- "wt.int"
  SPOP2006@sample@strata <- SPOP2006@pop@strata <-"qimd"
  SPOP2006@pop@data <- na.omit(SPOP2006@pop@data)
  limit <- list(cigst1 = list("1" = 0, "2" = 0, "4" = 0))
  censor <- list("0" = list(cigst1 = "3"))
  cat("numsmok\n")
  SPOP2006 <- simContinuous(SPOP2006, 
                            additional = "numsmok", 
                            method     = "multinom", 
                            regModel   = ~ age + sex + cigst1, 
                            MaxNWts    = 6000,
                            zeros      = T, 
                            gpd        = T,
                            keep       = F,
                            imputeMissings = T,
                            limit      = limit,
                            censor     = censor,
                            nr_cpus    = ncpu,
                            seed       =  as.integer(runif(1, 1e3, 1e6)))
  # SPOP2006@pop@data[is.na(numsmok), table(age)] # missing ages >90
  set2key(SPOP2006@pop@data, NULL)
  SPOP2006@pop@data[cigst1 != "3" & is.na(numsmok), numsmok := 0]
  imp <- SPOP2006@pop@data[age == "90" & cigst1 == "3", mean(numsmok, na.rm = T)]
  SPOP2006@pop@data[is.na(numsmok), numsmok := imp]
  
  # validation
  # SPOP2006@sample@data[, agegroup := agegroup.fn(as.integer(as.character(age)))]
  # spCdfplot(SPOP2006, "numsmok", cond=NULL)
  # spCdfplot(SPOP2006, "numsmok", cond="sex")
  # spCdfplot(SPOP2006, "numsmok", cond="agegroup")
  # spCdfplot(SPOP2006, "numsmok", cond="qimd")
  
  # Smoking duration
  HSE[startsmk==97, startsmk := NA] # 97 = "never smoke regularly"
  HSE[cigst1 == 4, duration := age - startsmk]  # Years smoking for smokers
  HSE[cigst1 < 4, duration := 0]  # Years smoked for non smokers
  cigdyal.breaks <- SPOP2006@pop@data[, getBreaks(cigdyal, probs = seq(0.2,1,0.2))]
  pop(SPOP2006, "cigdyalCat") <- getCat(pop(SPOP2006, "cigdyal"), cigdyal.breaks)
  limit <- list(cigst1 = list("1" = 0, "2" = 0, "3" = 0))
  SPOP2006@sample@data   <- na.omit(HSE[
    , .(age, sex, qimd, cigdyalCat = getCat(cigdyal, cigdyal.breaks),
        duration, cigst1, wt.int)])
  SPOP2006@sample@data[age>90, age := 90]
  SPOP2006@sample@weight <- "wt.int"
  SPOP2006@sample@strata <- SPOP2006@pop@strata <-"qimd"
  SPOP2006@pop@data <- na.omit(SPOP2006@pop@data)
  cat("duration\n")
  SPOP2006 <- simContinuous(SPOP2006, 
                            additional = "duration", 
                            method     = "multinom", 
                            regModel   = ~ age + sex + cigdyalCat + cigst1, 
                            MaxNWts    = 6000,
                            zeros      = T, 
                            gpd        = T,
                            upper      = 80,
                            keep       = F,
                            imputeMissings = T,
                            limit      = limit,
                            nr_cpus    = ncpu,
                            seed       =  as.integer(runif(1, 1e3, 1e6)))
  # SPOP2006@pop@data[is.na(duration), table(age)] # missing ages >90
  # SPOP2006@pop@data[cigst1 %in% as.character(1:3), table(duration)] 
  set2key(SPOP2006@pop@data, NULL)
  SPOP2006@pop@data[cigst1 != "4" & is.na(duration), duration := 0]
  imp <- SPOP2006@pop@data[age == "90" & cigst1 == "4", mean(duration, na.rm = T)]
  SPOP2006@pop@data[is.na(duration), duration := imp]
  
  # validation
  # SPOP2006@sample@data[, agegroup := agegroup.fn(as.integer(as.character(age)))]
  # spCdfplot(SPOP2006, "duration", cond=NULL)
  # spCdfplot(SPOP2006, "duration", cond="sex")
  # spCdfplot(SPOP2006, "duration", cond="agegroup")
  # spCdfplot(SPOP2006, "duration", cond="qimd")
  
  HSE[cigst1 == 4, smokyrs := 0]  # Years smoking for smokers
  HSE[cigst1 == 1, smokyrs := 0]  # Years smoked for non smokers
  numsmok.breaks <- SPOP2006@pop@data[, getBreaks(numsmok, probs = seq(0.2,1,0.2))]
  pop(SPOP2006, "numsmokCat") <- getCat(pop(SPOP2006, "numsmok"), numsmok.breaks)
  SPOP2006@sample@data   <- na.omit(HSE[
    , .(age, sex, qimd, numsmokCat = getCat(numsmok, numsmok.breaks),
        smokyrs, cigst1, wt.int)])
  SPOP2006@sample@data[age>90, age := 90]
  SPOP2006@sample@weight <- "wt.int"
  SPOP2006@sample@strata <- SPOP2006@pop@strata <-"qimd"
  SPOP2006@pop@data <- na.omit(SPOP2006@pop@data)
  limit <- list(cigst1 = list("1" = 0, "4" = 0))
  cat("smokyrs\n")
  SPOP2006 <- simContinuous(SPOP2006, 
                            additional = "smokyrs", 
                            method     = "multinom", 
                            regModel   = ~ age + sex + numsmokCat + cigst1, 
                            MaxNWts    = 6000,
                            zeros      = T, 
                            gpd        = T,
                            upper      = 80,
                            keep       = F,
                            imputeMissings = T,
                            limit      = limit,
                            nr_cpus    = ncpu,
                            seed       =  as.integer(runif(1, 1e3, 1e6)))
  # SPOP2006@pop@data[is.na(smokyrs), table(age)] # missing ages >90
  # SPOP2006@pop@data[cigst1 %in% as.character(c(1,4)), table(smokyrs)] 
  set2key(SPOP2006@pop@data, NULL)
  SPOP2006@pop@data[cigst1 %in% as.character(c(1,4)) & is.na(smokyrs), smokyrs := 0]
  imp <- SPOP2006@pop@data[age == "90" & cigst1 == "3", mean(smokyrs, na.rm = T)]
  SPOP2006@pop@data[is.na(smokyrs), smokyrs := imp]
  
  # validation
  # SPOP2006@sample@data[, agegroup := agegroup.fn(as.integer(as.character(age)))]
  # spCdfplot(SPOP2006, "smokyrs", cond=NULL)
  # spCdfplot(SPOP2006, "smokyrs", cond="sex")
  # spCdfplot(SPOP2006, "smokyrs", cond="agegroup")
  # spCdfplot(SPOP2006, "smokyrs", cond="qimd")
  
  #Sanity check
  SPOP2006@pop@data[cigst1 == "4" & as.numeric(as.character(age)) < (duration + 10), 
                    duration := as.numeric(as.character(age)) - 10]
  SPOP2006@pop@data[cigst1 %in% c("2", "3") & as.numeric(as.character(age)) < (smokyrs + 10), 
                    smokyrs := as.numeric(as.character(age)) - 10]
  
  SPOP2006@pop@data[cigst1 == "4", smokyrs := duration]
  
  
  # Salt --------------------------------------------------------------------
  # Recalculate 24h salt stochastically for each iteration
  HSE[age>15 & sex== "1",
      Na24 := Na24.men.det(.N, sodium, creatin, potass, bmival, age)]
  HSE[age>15 & sex== "2",
      Na24 := Na24.women.det(.N, sodium, creatin, potass, bmival, age)]
  HSE[, salt.intersalt := Na24 * 58.5/1000]
  HSE[salt.intersalt < 1, salt.intersalt := 1]
  SPOP2006@sample@data   <- HSE[
    wt.nurse > 0, .(age, sex, qimd, bmivalCat = getCat(bmival, bmi.breaks, zeros = F),
                    salt.intersalt, wt.nurse)]
  SPOP2006@sample@data[age>90, age := 90]
  SPOP2006@sample@weight <- "wt.nurse"
  SPOP2006@sample@strata <- SPOP2006@pop@strata <-"qimd"
  SPOP2006@pop@data <- na.omit(SPOP2006@pop@data)
  cat("salt\n")
  SPOP2006 <- simContinuous(SPOP2006, 
                            additional = "salt.intersalt", 
                            method     = "multinom", 
                            regModel   = ~ age + sex + bmivalCat, 
                            MaxNWts    = 6000,
                            equidist   = F, 
                            zeros      = F, 
                            gpd        = T,
                            keep       = F,
                            imputeMissings = T,
                            nr_cpus    = ncpu,
                            seed       =  as.integer(runif(1, 1e3, 1e6)))
  # SPOP2006@pop@data[is.na(salt.intersalt), table(age)] # missing ages >90 & <16
  set2key(SPOP2006@pop@data, NULL)
  imp <- SPOP2006@pop@data[age %in% as.character(c(80:90)), mean(salt.intersalt, na.rm = T)]
  SPOP2006@pop@data[age %in% as.character(c(91:99)) & is.na(salt.intersalt), salt.intersalt := imp]
  
  
  # validation
  # SPOP2006@sample@data[, agegroup := agegroup.fn(as.integer(as.character(age)))]
  # spCdfplot(SPOP2006, "salt.intersalt", cond=NULL)
  # spCdfplot(SPOP2006, "salt.intersalt", cond="sex")
  # spCdfplot(SPOP2006, "salt.intersalt", cond="agegroup")
  # spCdfplot(SPOP2006, "salt.intersalt", cond="qimd")
  
  # For systolic BP (wt.nurse) I will use omsysval ((D) Omron Valid Mean Systolic BP) omsysval was
  # measured in ages >=5
  # "bpmedc" should be used but makes the fit worse and I don't use medication yet anyway
  salt.breaks <- SPOP2006@pop@data[, getBreaks(salt.intersalt, equidist = F, zeros = F)]
  pop(SPOP2006, "saltCat") <- getCat(pop(SPOP2006, "salt.intersalt"), salt.breaks, zeros = F)
  SPOP2006@sample@data <- HSE[
    wt.nurse > 0, .(age, sex, qimd, 
                    bmivalCat = getCat(bmival, bmi.breaks, zeros = F),
                    saltCat = getCat(salt.intersalt, salt.breaks, zeros = F),
                    cigst1, omsysval, wt.nurse)]
  SPOP2006@sample@data[age > 90, age := 90]
  SPOP2006@sample@weight <- "wt.nurse"
  SPOP2006@sample@strata <- SPOP2006@pop@strata <-"qimd"
  SPOP2006@pop@data <- na.omit(SPOP2006@pop@data)
  cat("omsysval\n")
  SPOP2006 <- simContinuous(SPOP2006, 
                            additional = "omsysval", 
                            method     = "multinom", 
                            regModel   = ~ age + sex + bmivalCat + saltCat + cigst1, 
                            MaxNWts    = 6000,
                            equidist   = F, 
                            zeros      = F, 
                            gpd        = T,
                            keep       = F,
                            imputeMissings = T,
                            nr_cpus    = ncpu,
                            seed       =  as.integer(runif(1, 1e3, 1e6)))
  # SPOP2006@pop@data[is.na(omsysval), table(age)] # missing ages >90 & <16
  set2key(SPOP2006@pop@data, NULL)
  imp <- SPOP2006@pop@data[age %in% as.character(c(89:90)), mean(omsysval, na.rm = T)]
  SPOP2006@pop@data[age %in% as.character(c(91:99)) & is.na(omsysval), omsysval := imp]
  
  
  # validation
  # SPOP2006@sample@data[, agegroup := agegroup.fn(as.integer(as.character(age)))]
  # spCdfplot(SPOP2006, "omsysval", cond=NULL)
  # spCdfplot(SPOP2006, "omsysval", cond="sex")
  # spCdfplot(SPOP2006, "omsysval", cond="agegroup")
  # spCdfplot(SPOP2006, "omsysval", cond="qimd")
  
  
  # For total Chol (wt.blood) I will use cholval1: (D) Valid Cholesterol Result (incl those on lld)
  # cholval1 was measured in ages >=16
  SPOP2006@sample@data <- HSE[
    wt.blood > 0, .(age, sex, qimd, 
                    bmivalCat = getCat(bmival, bmi.breaks, zeros = F),
                    lipid, porftvg, cholval1, wt.blood)]
  SPOP2006@sample@data[age > 90, age := 90]
  SPOP2006@sample@weight <- "wt.blood"
  SPOP2006@sample@strata <- SPOP2006@pop@strata <-"qimd"
  SPOP2006@pop@data <- na.omit(SPOP2006@pop@data)
  setnames(SPOP2006@sample@data, "cholval1", "cholval")
  cat("cholval\n")
  SPOP2006 <- simContinuous(SPOP2006, 
                            additional = "cholval", 
                            method     = "multinom", 
                            regModel   = ~ age + sex + bmivalCat + lipid + porftvg, 
                            MaxNWts    = 6000,
                            equidist   = F, 
                            zeros      = F, 
                            gpd        = T,
                            keep       = F,
                            imputeMissings = T,
                            nr_cpus    = ncpu,
                            seed       =  as.integer(runif(1, 1e3, 1e6)))
  
  # SPOP2006@pop@data[is.na(cholval), table(age)] # missing ages >90 
  set2key(SPOP2006@pop@data, NULL)
  imp <- SPOP2006@pop@data[age %in% as.character(c(89:90)), mean(cholval, na.rm = T)]
  SPOP2006@pop@data[age %in% as.character(c(91:99)) & is.na(cholval), cholval := imp]
  
  
  # validation
  # SPOP2006@sample@data[, agegroup := agegroup.fn(as.integer(as.character(age)))]
  # spCdfplot(SPOP2006, "cholval", cond=NULL)
  # spCdfplot(SPOP2006, "cholval", cond="sex")
  # spCdfplot(SPOP2006, "cholval", cond="agegroup")
  # spCdfplot(SPOP2006, "cholval", cond="qimd")
  # 
  # HSE[wt.blood>0 & !is.na(cholval1), plot(density(cholval1, weights = wt.blood/sum(wt.blood)))]
  # SPOP2006@pop@data[, lines(density(cholval, na.rm = T), col = "red")]
  # SPOP2006@pop@data[, lines(density(sample(cholval, 2e4), na.rm = T), col = "blue")]
  # 
  # HSE[wt.nurse>0 & !is.na(bmival), plot(density(bmival, weights = wt.nurse/sum(wt.nurse)))]
  # SPOP2006@pop@data[, lines(density(bmival, na.rm = T), col = "red")]
  # SPOP2006@pop@data[, lines(density(sample(bmival, 2e4), na.rm = T), col = "blue")]
  # 
  # HSE[wt.nurse>0 & !is.na(omsysval), plot(density(omsysval, na.rm = T, weights = wt.nurse/sum(wt.nurse)))]
  # SPOP2006@pop@data[, lines(density(omsysval, na.rm = T), col = "red")]
  # SPOP2006@pop@data[, lines(density(sample(omsysval, 2e4), na.rm = T), col = "blue")]
  
  # Finalise SPOP2006 -------------------------------------------------------
  cat("finalise\n")
  SPOP2006 = copy(SPOP2006@pop@data)
  setDT(SPOP2006)
  set2key(SPOP2006, NULL)
  setkey(SPOP2006, age, sex, agegroup, qimd)
  SPOP2006[, id := 1:.N] # Create unique ID
  SPOP2006[, age := as.integer(as.character(age))]
  SPOP2006[, sex := as.factor(sex)]
  SPOP2006[, numsmok := as.integer(numsmok)]
  SPOP2006[cigst1 == "2", numsmok:= 1L]
  SPOP2006[, expsmokCat := factor(expsmokCat)]
  SPOP2006[, a30to06m := as.integer(as.character(a30to06m))]
  SPOP2006[age < 15, cigst1 := "1"]
  SPOP2006[age > 99, age := 99] # for combatibility with lifetables
  SPOP2006[, cigdyal := as.integer(cigdyal)]
  SPOP2006[, endsmoke := as.integer(endsmoke)]
  SPOP2006[, smokyrs := as.integer(smokyrs)]
  #SPOP2006[, frtpor := as.integer(as.character(frtpor))]
  SPOP2006[, hsize := as.integer(as.character(hhsize))]
  SPOP2006[, porftvg := as.integer(as.character(porftvg))]
  SPOP2006[,  `:=` (saltCat     = NULL, 
                    numsmokCat  = NULL,
                    duration    = NULL,
                    cigdyalCat  = NULL,
                    bmivalCat   = NULL, 
                    weight      = NULL,
                    pid         = NULL,
                    qimd = ordered(qimd, levels=as.character(1:5)))]
  #SPOP2006[, bmival := round(bmival,1)]
  agegroup.fn(SPOP2006)
  setcolorder(SPOP2006, SPOP2006[,order(names(SPOP2006))]) # reorder columns alphabeticaly
  
  cat("save\n")
  return(saveRDS(SPOP2006, file = paste0("/mnt/iusers01/mhs01/mdxasck2/IMPACTncd/SynthPop/spop050607-", i, ".rds")))
  #return(saveRDS(SPOP2006, file = paste0("./spop050607-", i, ".rds")))
  sink()
}
SynthPOP <- cmpfun(SynthPOP)

# LOOP ---------
cl <- makeCluster(clusternumber) 
registerDoParallel(cl)

foreach(i = (1):(iterations),
        .inorder = F,
        .verbose = T,
        .packages = c("data.table",
                      "simPop",
                      "survey",
                      "pryr",
                      "compiler"),
        .export = ls()) %dorng% SynthPOP(i)

stopCluster(cl)



# Garbage cleaning
rm(list = ls(all = TRUE))

