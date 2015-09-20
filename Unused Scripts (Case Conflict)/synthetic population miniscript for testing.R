# ************************************************************************************************
#                  England's synthetic population
# ************************************************************************************************

## Note to myshefl. I build synth pop in layers, attempting to recreate causality. This is innovative

# User input
prop <- 0.02  # Set propotion (prop) of the populations to be synthesized (set 1 for total population)
iterations <- 8 # Number of synthetic population to produce
number.of.cores <- 4 # Number of cores to be used ONLY VALID FOR LINUX

# preample
gc()
dependencies <- function(x) {
    for (j in x) {
        # require returns T invisibly if it was able to load package
        if (!require(j, character.only = T)) {
            # If package was not able to be loaded then re-install
            install.packages(j, dependencies = T)
            # Load package after installing
            require(j, character.only = T)
        }
    }
}

# Then try/install packages...
dependencies(c("simPopulation",
               "data.table",
               "dplyr", 
               "Hmisc",
               "doParallel",
               "compiler"))



# Get Dropbox folder.  Automaticly define of my dropbox folder in windows Manually define on linux
options(warn = 1)

if (Sys.info()[1] == "Linux") {
    setwd(paste("/home/", system("whoami", T), "/Dropbox/PhD", sep = "", collapse = ""))
} else {
    get.dropbox.folder <- function() {
        if (!require(RCurl)) 
            stop("You need to install RCurl package.")
        if (Sys.info()["sysname"] != "Windows") 
            stop("Currently, "get.dropbox.folder" works for Windows only. Sorry.")
        db.file <- paste(Sys.getenv("APPDATA"), "\\Dropbox\\host.db", sep = "")
        base64coded <- readLines(db.file, warn = F)[2]
        base64(base64coded, encode = F)
    }
    setwd(paste(get.dropbox.folder(), "/PhD", sep = ""))
    rm(get.dropbox.folder)
}

# Define end() function to beep end print a message
if (Sys.info()[1] == "Linux") {
    end <- function(...){
        cat("All done! \a\n")}
} else {
    end <- function(...){
        for(i in 3){
            system("rundll32 user32.dll,MessageBeep -1")
            Sys.sleep(.5)
        }
        cat("All done! \a\n")
    }
}

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

# Import datasets (house hold and individual files)
load(file="./Datasets/Health Survey for England/2011/hse2011ai.RData")
HSE2011original <- clear.labels(HSE.2011)
rm(HSE.2011)
HSE2011original <- data.table(HSE2011original, key="age")

load(file="./Datasets/Health Survey for England/2011/hse2011ah.RData")
HSE2011hh <- clear.labels(HSE2011hh)
HSE2011hh <- data.table(HSE2011hh, key="age")

load(file="./Datasets/Health Survey for England/2012/hse2012ai.RData")
HSE2012original <- clear.labels(HSE2012original)
HSE2012original <- data.table(HSE2012original, key="age")

load(file="./Datasets/Health Survey for England/2010/hse10ai.RData")
HSE2010original <- clear.labels(HSE2010original)
HSE2010original <- data.table(HSE2010original, key="age")

load(file="./Datasets/Health Survey for England/2008/hse08ai.RData")
HSE2008original <- clear.labels(HSE2008original)
HSE2008original <- data.table(HSE2008original, key="age")

load(file="./Datasets/Health Survey for England/2007/hse07ai.RData")
HSE2007original <- clear.labels(HSE2007original)
HSE2007original <- data.table(HSE2007original, key="age")

#breaks <- c(0, seq(15, 75, 20), 130)
#HSE2011hh[, ageCat20 :=  cut(age, breaks = breaks, 
#                             include.lowest = T, 
#                             ordered_result = T)]
#HSE2011original[, ageCat20 := cut(age, breaks = breaks, 
#                                  include.lowest = T,
#                                  ordered_result = T)]

breaks                   <- c(0, 1, seq(5, 85, 5), 130)
labels                   <- c("<1   ", "01-04", "05-09",
                              "10-14", "15-19", "20-24", 
                              "25-29", "30-34", "35-39", 
                              "40-44", "45-49", "50-54",
                              "55-59", "60-64", "65-69",
                              "70-74", "75-79", "80-84", "85+")
HSE2011original[, agegroup := cut(age, breaks = breaks, 
                                  labels = labels, 
                                  include.lowest = T, 
                                  right = F, 
                                  ordered_result = T)]
HSE2012original[, agegroup := cut(age, breaks = breaks, 
                                  labels = labels, 
                                  include.lowest = T, 
                                  right = F, 
                                  ordered_result = T)]
HSE2010original[, agegroup := cut(age, breaks = breaks, 
                                  labels = labels, 
                                  include.lowest = T, 
                                  right = F, 
                                  ordered_result = T)]
HSE2008original[, agegroup := cut(age, breaks = breaks, 
                                  labels = labels, 
                                  include.lowest = T, 
                                  right = F, 
                                  ordered_result = T)]

HSE2011original[eqv5 < 0, eqv5 := NA]
HSE2011original[is.na(porftvg) == T & age < 5, porftvg := 99]  # code ages of 0-5 as 99 = not  applicable
HSE2011original[is.na(totalwug) == T & age < 16, totalwug := 0]  # code ages of 0-15 as non drinkers in the totalwug
HSE2011original[is.na(cigdyal) == T & age < 16, cigdyal := 0]  # code ages of 0-15 as non smokers
HSE2011original[is.na(cigst1) == T & age < 16, cigst1 := 1]  # code ages of 0-15 as never smokers
HSE2011original[is.na(endsmoke) == T & (cigst1 == 1 | cigst1 == 4), endsmoke := 0]  # code non ex smokers as 0
HSE2011original[is.na(segment) == T & age < 16, segment := 99]  # code ages of 0-15 as not applicable= 99
setnames(HSE2011original, "hhsize", "hsize")
agegroup.fn("HSE2011original")
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


HSE2012original[eqv5 < 0, eqv5 := NA]
HSE2012original[is.na(porftvg) == T & age < 5, porftvg := 99]  # code ages of 0-5 as 99 = not  applicable
HSE2012original[is.na(totalwug) == T & age < 16, totalwug := 0]  # code ages of 0-15 as non drinkers in the totalwug
HSE2012original[is.na(cigdyal) == T & age < 16, cigdyal := 0]  # code ages of 0-15 as non smokers
HSE2012original[is.na(cigst1) == T & age < 16, cigst1 := 1]  # code ages of 0-15 as never smokers
HSE2012original[is.na(endsmoke) == T & (cigst1 == 1 | cigst1 == 4), endsmoke := 0]  # code non ex smokers as 0
HSE2012original[is.na(segment) == T & age < 16, segment := 99]  # code ages of 0-15 as not applicable= 99
setnames(HSE2012original, "hhsize", "hsize")
agegroup.fn("HSE2012original")
# Calculate actual statins prescription and overwrite "lipid" variable
HSE2012original[, lipid := 0]
for (i in 1:9) {
    nam <- paste("medbi0", i, sep="")
    man <- paste("medbia", i, sep="")
    HSE2012original[get(nam) == 21201 & get(man) == 1, lipid := 1 ]
}
for (i in 10:22) {
    nam <- paste("medbi", i, sep="")
    man <- paste("medbia", i, sep="")
    HSE2012original[get(nam) == 21201 & get(nam) == 1, lipid := 1 ]
}
HSE2012original[statina == 1, lipid := 1] # incluse OTC statins
HSE2012original[, lipid := as.factor(lipid)]


# Rescale weights of the household files (page 25 of HSE2011-Methods-and-docs.pdf)
t <- HSE2011hh[, sum(wt.hhld)]  # calculate the sum of original weights
# pop2010 <- 51818267 # Mid 2010 adjusted England's population (page 25 of
# HSE2011-Methods-and-docs.pdf)
pop2010 <- 53107200  # Mid 2011 England's population (not adjusted) from ONS. I will use this for compatibility under the assumption that institutionalised population above 65 (excluded from original HSE) has the same characteristics as the rest of the population.
d <- pop2010 * prop/t
HSE2011hh[, wt.hhld := wt.hhld * d]

# Recreate SHA information for household from the individual's file
#Temp = copy(HSE2011original[, list(hserial, sha)])
Temp <- unique(HSE2011original[, list(hserial, sha)], by="hserial")  # Keep only unique values
HSE2011hh <- merge(HSE2011hh, Temp, by = "hserial")
rm(Temp)

# Create household size (by summing adults, children and neonates)
HSE2011hh[,hsize := adults + children + infants]

SPOPtest <- simStructure(HSE2011hh, 
                         hid = "hserial", 
                         w = "wt.hhld", 
                         strata = "sha", 
                         hsize = "hsize", 
                         pid = "pserial", 
                         additional = c("age", "sex"), 
                         method = "multinom", 
                         keep = F)

SPOPtest <- data.table(SPOPtest, key="age")

# Create unique ID
SPOPtest[, id := c(1:.N)]

# Remove NA (missing age)
SPOPtest <- SPOPtest[is.na(age) == F]

# Create categorical age var, etc
breaks <- c(0, seq(15, 75, 20), 130)
SPOPtest[, ageCat20 := cut(age, breaks = breaks, 
                           include.lowest = T, 
                           ordered_result = T)]

agegroup.fn("SPOPtest")

# Create wt.hhld in individual file to be compatible with graphs
# HSE2011original$wt.hhld <- HSE2011original$wt.int*d

# Add qimd (quantile of index of multiple deprivation) variable (I started from it because no NAs)
basic <- c("age", "sex", "hsize")
additional <- c("qimd")

SPOPtest <- simCategorical(HSE2011original, SPOPtest, 
                           w = "wt.int", 
                           strata = "sha", 
                           basic = basic, 
                           additional = additional, 
                           method = "multinom")


# Start using qimd for strata from now on (5= more deprived) and add more SEC indicators
HSE2011 = copy(HSE2011original[is.na(eqv5) == F])
basic <- c("agegroup", "sex")
additional <- c("eqv5")

SPOPtest <- simCategorical(HSE2011, SPOPtest, 
                           w = "wt.int", 
                           strata = "qimd", 
                           basic = basic, 
                           additional = additional, 
                           method = "multinom", 
                           MaxNWts = 4000)
if (SPOPtest[is.na(eqv5) == T, .N] >0) {
    SPOPtest <- SPOPtest[is.na(eqv5) == F,] # Remove NA
}

spMosaic(c("eqv5", "qimd"), "wt.int", HSE2011, SPOPtest)
spMosaic(c("sex", "qimd"), "wt.int", HSE2011, SPOPtest)
spMosaic(c("agegroup", "qimd"), "wt.int", HSE2011, SPOPtest)

HSE2011 = copy(HSE2011original[is.na(hpnssec8) == F,])
basic <- c("agegroup", "sex", "eqv5")
additional <- c("hpnssec8")
SPOPtest <- simCategorical(HSE2011, SPOPtest, 
                           w = "wt.int", 
                           strata = "qimd", 
                           basic = basic, 
                           additional = additional, 
                           method = "multinom", 
                           MaxNWts = 4000)
if (SPOPtest[is.na(hpnssec8) == T, .N] > 0) {
    SPOPtest <- SPOPtest[is.na(hpnssec8) == F,] # Remove NA
}

spMosaic(c("hpnssec8", "qimd"), "wt.int", HSE2011, SPOPtest)
spMosaic(c("hpnssec8", "sex"), "wt.int", HSE2011, SPOPtest)
spMosaic(c("hpnssec8", "agegroup"), "wt.int", HSE2011, SPOPtest)

# For Fruit and Veg porftvg will be used (maximum 1 portion of juice, pulses or dried fruit
# contributed to the total portions in porfv(continous var)) porftvg was measured for ages >=5 For
# ages <5 I will code them as 99 = non applicable
HSE2011 = copy(HSE2011original)  # initialise HSE2011
basic <- c("agegroup", "sex", "hpnssec8", "eqv5")
additional <- c("porftvg")
SPOPtest <- simCategorical(HSE2011, SPOPtest, 
                           w = "wt.int", 
                           strata = "qimd", 
                           basic = basic, 
                           additional = additional, 
                           method = "multinom", 
                           MaxNWts = 4000)
if (SPOPtest[is.na(porftvg) == T, .N] > 0) {
    SPOPtest <- SPOPtest[is.na(porftvg) == F,] # Remove NA
}

spMosaic(c("porftvg", "agegroup"), "wt.int", HSE2011, SPOPtest)

#  For fruit portions
HSE2011 = copy(HSE2011original)  # initialise HSE2011
HSE2011[, frtpor := cut(frtpor, breaks = c(0, 0.1:8.1, 30), 
                        labels = c(0:9), include.lowest = T, 
                        right = F, 
                        ordered_result = T)]
levels(HSE2011$frtpor) <- c(levels(HSE2011$frtpor), "99")
HSE2011[is.na(frtpor)==T & age < 5, frtpor := as.factor("99")]  # code ages of 0-4 as 99 = non applicable
basic <- c("agegroup", "sex", "porftvg")
additional <- "frtpor"
SPOPtest <- simCategorical(HSE2011, SPOPtest, 
                           w = "wt.int", 
                           strata = "qimd", 
                           basic = basic, 
                           additional = additional, 
                           method = "multinom", 
                           MaxNWts = 6000)
if (SPOPtest[is.na(frtpor) == T, .N] > 0) {
    SPOPtest <- SPOPtest[is.na(frtpor) == F,] # Remove NA
}

HSE2011[, summary(frtpor)]
SPOPtest[, summary(frtpor)]

spMosaic(c("frtpor", "porftvg"), "wt.int", HSE2011, SPOPtest)

# For smoking history cigst1 will be used (D) Cigarette Smoking Status - Never/Ex-reg/Ex-occ/Current
# cigst1 has been measured in ages >=16. I will code all ages below 16 as never smokers
HSE2011 = copy(HSE2011original)  # initialise HSE2011
basic <- c("agegroup", "sex", "hpnssec8" )
additional <- "cigst1"
SPOPtest <- simCategorical(HSE2011, SPOPtest, 
                           w = "wt.int", 
                           strata = "qimd", 
                           basic = basic, 
                           additional = additional, 
                           method = "multinom", 
                           MaxNWts = 4000)
if (SPOPtest[is.na(cigst1) == T, .N] > 0) {
    SPOPtest <- SPOPtest[is.na(cigst1) == F,] # Remove NA
}

spMosaic(c("cigst1", "qimd"), "wt.int", HSE2011, SPOPtest)

# For Smoking cigdyal will be used ((D) Number of cigarettes smoke a day - inc. non-smokers)
# (measured for ages <=16) I will code ages < 16 as 0 (ie non smokers) I will recode into levels by 1
# cigar and aggregate more than 40 cigars into 20. To be named cigdyalCat
HSE2011 = copy(HSE2011original)
HSE2011[cigdyal > 40, cigdyal := 40]  # code more than 40 cigars as 40
breaks <- c(0, seq(0.1, 39.1, 1), 40.1)
HSE2011[, cigdyalCat := cut(cigdyal, breaks = breaks, 
                            labels = c(0:40), 
                            include.lowest = T, 
                            ordered_result = T)]
limit <- list(cigst1 = list("1" = "0", "2" = "0", "3" = "0"))
censor <- list("0" = list(cigst1 = "4"))
basic <- c("agegroup", "sex", "hpnssec8", "cigst1" )
additional <- "cigdyalCat"
SPOPtest <- simCategorical(HSE2011, SPOPtest, 
                           w = "wt.int", 
                           strata = "qimd", 
                           basic = basic, 
                           additional = additional, 
                           method = "multinom", 
                           limit = limit,
                           censor = censor,
                           MaxNWts = 4000)
if (SPOPtest[is.na(cigdyalCat) == T, .N] > 0) {
    SPOPtest <- SPOPtest[is.na(cigdyalCat) == F,] # Remove NA
}

HSE2011[, cigdyalCat := as.numeric(as.character(cigdyalCat))]
SPOPtest[, cigdyalCat := as.numeric(as.character(cigdyalCat))]
spCdfplot(x="cigdyalCat", weights="wt.int", cond = "cigst1", dataS=HSE2011, dataP=SPOPtest)

# For (would you) Like to give up smoking (Applicable only to smokers) I will use givupsk (1=yes
# 2=no) I will recode all non-smokers to 99= non smoker
HSE2011 = copy(HSE2011original)
HSE2011[is.na(givupsk) & cigst1 != 4, givupsk := 99] # code non smokers as no smoker = 99
HSE2011[cigdyal > 40, cigdyal := 40]  # code more than 40 cigars as 40
breaks <- c(0, seq(0.1, 39.1, 1), 40.1)
HSE2011[, cigdyalCat := cut(cigdyal, breaks = breaks, 
                            labels = c(0:40), 
                            include.lowest = T, 
                            ordered_result = T)]
limit <- list(cigdyalCat = list("0" = "99"))
censor <- list("99" = list(cigst1 = "4"))
basic <- c("agegroup", "sex", "hpnssec8", "cigst1", "cigdyalCat" )
additional <- "givupsk"
SPOPtest <- simCategorical(HSE2011, SPOPtest, 
                           w = "wt.int", 
                           strata = "qimd", 
                           basic = basic, 
                           additional = additional, 
                           method = "multinom", 
                           limit = limit,
                           censor = censor,
                           MaxNWts = 4000)
if (SPOPtest[is.na(givupsk) == T, .N] > 0) {
    SPOPtest <- SPOPtest[is.na(givupsk) == F,] # Remove NA
}

spMosaic(c("givupsk", "cigst1"), "wt.int", HSE2011, SPOPtest)

# For How long ago did you stop smoking cigarettes? (Applicable only to ex-smokers) I will use endsmoke
HSE2011 = copy(HSE2011original)
basic <- c("agegroup", "sex", "hpnssec8", "cigst1")
additional <- "endsmoke"
limit <- list(cigst1 = list("1" = "0", "4" = "0"))
SPOPtest <- simCategorical(HSE2011, SPOPtest, 
                           w = "wt.int", 
                           strata = "qimd", 
                           basic = basic, 
                           additional = additional, 
                           method = "multinom", 
                           limit = limit,
                           MaxNWts = 6000)
if (SPOPtest[is.na(endsmoke) == T, .N] > 0) {
    SPOPtest <- SPOPtest[is.na(endsmoke) == F,] # Remove NA
}

HSE2011[, endsmoke := as.numeric(as.character(endsmoke))]
SPOPtest[, endsmoke := as.numeric(as.character(endsmoke))]
spCdfplot(x="endsmoke", weights="wt.int", cond = "cigst1", dataS=HSE2011, dataP=SPOPtest)

# For passive smoking expsmok will be used (Number of hours/week exposed to others' smoke (c+sc)) To
# be recoded into a categorical variable (expsmokCat)
HSE2011 = copy(HSE2011original)
breaks <- c(0, seq(0.1, 99.1, 10), max(HSE2011$expsmok, na.rm = T))
HSE2011[, expsmokCat := cut(expsmok, breaks = breaks,
                            labels = c(0:10),
                            include.lowest = T, 
                            ordered_result = T)]
basic <- c("agegroup", "sex", "hpnssec8", "cigst1" )
additional <- "expsmokCat"
SPOPtest <- simCategorical(HSE2011, SPOPtest, 
                           w = "wt.int", 
                           strata = "qimd", 
                           basic = basic, 
                           additional = additional, 
                           method = "multinom", 
                           MaxNWts = 4000)
if (SPOPtest[is.na(expsmokCat) == T, .N] > 0) {
    SPOPtest <- SPOPtest[is.na(expsmokCat) == F,] # Remove NA
}

HSE2011[, expsmokCat := as.numeric(as.character(expsmokCat))]
SPOPtest[, expsmokCat := as.numeric(as.character(expsmokCat))]
spCdfplot(x="expsmokCat", weights="wt.int", cond = "cigst1", dataS=HSE2011, dataP=SPOPtest)

# Prepare HSE2011 to inform the synth pop about behaviours/personalities.
# For Health foundations lifestyle segment I will use segment (1=Hedonistic Immortal, 2=Live for
# Today, 3=Unconfident Fatalist, 4=Health Conscious Realist, 5=Balanced Compensator) segment been
# estimated for ages >=16.  I will recode ages <16 as 99= non-applicable
HSE2011 = copy(HSE2011original)
basic <- c("agegroup", "sex", "hpnssec8", "eqv5")
additional <- c("segment")
limit <- list(agegroup = list("<1   " = "99", "01-04" = "99", "05-09" = "99", "10-14" = "99"))
SPOPtest <- simCategorical(HSE2011, SPOPtest, 
                           w = "wt.int", 
                           strata = "qimd", 
                           basic = basic, 
                           additional = additional,
                           limit = limit,
                           method = "multinom", 
                           MaxNWts = 4000)
if (SPOPtest[is.na(segment) == T, .N] > 0) {
    SPOPtest <- SPOPtest[is.na(segment) == F,] # Remove NA
}

HSE2011[age>17, summary(as.factor(segment))]
SPOPtest[as.numeric(as.character(age))>17, summary(as.factor(segment))]
spMosaic(c("segment", "agegroup"), "wt.int", HSE2011, SPOPtest)
spMosaic(c("segment"), "wt.int", HSE2011, SPOPtest)

# Used medication
# lipid: use statin over last 7 days
HSE2011 = copy(HSE2011original)
basic <- c("agegroup", "sex", "hpnssec8")
additional <- c("lipid")
SPOPtest <- simCategorical(HSE2011, SPOPtest, 
                           w = "wt.nurse", 
                           strata = "qimd", 
                           basic = basic, 
                           additional = additional, 
                           method = "multinom", 
                           MaxNWts = 4000)
if (SPOPtest[is.na(lipid) == T, .N] > 0) {
    SPOPtest <- SPOPtest[is.na(lipid) == F,] # Remove NA
}

HSE2011[, summary(as.factor(lipid))]
SPOPtest[, summary(as.factor(lipid))]

spMosaic(c("lipid"), "wt.nurse", HSE2011, SPOPtest)
spMosaic(c("lipid", "segment"), "wt.nurse", HSE2011, SPOPtest)
spMosaic(c("lipid", "sex"), "wt.nurse", HSE2011, SPOPtest)
spMosaic(c("lipid", "agegroup"), "wt.nurse", HSE2011, SPOPtest)

# bpmedc: (D) Whether taking drugs affecting blood pressure
HSE2011[is.na(bpmedc)==T, bpmedc := 0]
basic <- c("agegroup", "sex", "hpnssec8", "lipid")
additional <- c("bpmedc")
SPOPtest <- simCategorical(HSE2011, SPOPtest, 
                           w = "wt.nurse", 
                           strata = "qimd", 
                           basic = basic, 
                           additional = additional, 
                           method = "multinom", 
                           MaxNWts = 6000)
if (SPOPtest[is.na(bpmedc) == T, .N] > 0) {
    SPOPtest <- SPOPtest[is.na(bpmedc) == F,] # Remove NA
}

spMosaic(c("bpmedc"), "wt.nurse", HSE2011, SPOPtest)
spMosaic(c("bpmedc", "sex"), "wt.nurse", HSE2011, SPOPtest)
spMosaic(c("bpmedc", "agegroup"), "wt.nurse", HSE2011, SPOPtest)

# For Redmeat (from HSE2008) Two vars are coding frequency of red meat consumption credmeat and
# redmeatb. I will aggregate to redmeat
# 1 = 6 or more times a week, 2 = 3-5 times a week, 3 = 1-2 times a week, 4 = Less than once a week, 5 = Rarely or never 
HSE2008 = copy(HSE2008original)
HSE2008[is.na(redmeatb) == F, redmeat := redmeatb]
HSE2008[is.na(credmeat) == F, redmeat := credmeat]
# Redmeat was not available for ages <2 I will code age < 2 as 3 = 1 to 2 times a week (not fully apply because I use agegroups for synthesis)
HSE2008[is.na(redmeat) == T & age < 2, redmeat := 3]  #code ages of 0-1 as 3 = 1 to 2 times a week
HSE2008[eqv5 < 0, eqv5 := NA]
basic <- c("agegroup", "sex", "hpnssec8", "eqv5")
additional <- c("redmeat")
SPOPtest <- simCategorical(HSE2008, SPOPtest, 
                           w = "wt.int", 
                           strata = "qimd", 
                           basic = basic, 
                           additional = additional, 
                           method = "multinom", 
                           MaxNWts = 6000)
if (SPOPtest[is.na(redmeat) == T, .N] > 0) {
    SPOPtest <- SPOPtest[is.na(redmeat) == F,] # Remove NA
}

spMosaic(c("redmeat"), "wt.int", HSE2008, SPOPtest)
spMosaic(c("redmeat", "sex"), "wt.int", HSE2008, SPOPtest)
spMosaic(c("redmeat", "agegroup"), "wt.int", HSE2008, SPOPtest)

breaks <- c(min(HSE2008$age, na.rm = T), 
            seq(15, 75, 20), 
            max(HSE2008$age, na.rm = T))
HSE2008[, ageCat20 := cut(age, breaks = breaks,
                          include.lowest = T, 
                          ordered_result = T)]

# For physical activity (from HSE2008) useable t59su06 for age >= 16 | (D) 1= 3x30 vig AND 5x30 mod,
# 2= 3x30 vig only, 3= 5x30 mod only, 4= Lower but active, 5= Inactive usable chpa08 for ages 2-15
# (D) Summary: Meet child PA recommendations (0 = Low, 1 = Med- 60mins+ on 3-6 days, 2 = Med-
# 30-59mins on all 7 days, 3 = High- 60mins+ on all 7 days ) I wil recode chpa8 to t59su06 (0=4, 1=3,
# 2=2, 3=1) I wil recode ages 0-1 as 4= Lower but active

# I wil recode chpa08 to t59su06 (0=4, 1=3, 2=2, 3=1)
HSE2008[is.na(t59su06) == T & chpa08 == 0, t59su06 := 4]
HSE2008[is.na(t59su06) == T & chpa08 == 1, t59su06 := 3]
HSE2008[is.na(t59su06) == T & chpa08 == 2, t59su06 := 2]
HSE2008[is.na(t59su06) == T & chpa08 == 3, t59su06 := 1]
# I wil recode ages 0-1 as 4= Lower but active
HSE2008[is.na(t59su06) == T & age < 2, t59su06 := 4]
basic <- c("agegroup", "sex", "hpnssec8", "eqv5")
additional <- c("t59su06")
SPOPtest <- simCategorical(HSE2008, SPOPtest, 
                           w = "wt.int", 
                           strata = "qimd", 
                           basic = basic, 
                           additional = additional, 
                           method = "multinom", 
                           MaxNWts = 6000)
if (SPOPtest[is.na(t59su06) == T, .N] > 0) {
    SPOPtest <- SPOPtest[is.na(t59su06) == F,] # Remove NA
}

spMosaic(c("t59su06"), "wt.int", HSE2008, SPOPtest)
spMosaic(c("t59su06", "sex"), "wt.int", HSE2008, SPOPtest)
spMosaic(c("t59su06", "agegroup"), "wt.int", HSE2008, SPOPtest)

# For BMI I will use bmival (measured for for ages >=2) as continous variable
HSE2011 = copy(HSE2011original)
agegroup.fn("HSE2011")
basic <- c("agegroup", "sex", "eqv5", "porftvg")
additional <- c("bmival")
limit <- list(agegroup = list("<1   " = 0))
SPOPtest <- simContinuous(HSE2011, SPOPtest, 
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
if (SPOPtest[is.na(bmival) == T, .N] > 0) {
    SPOPtest <- SPOPtest[is.na(bmival) == F,] # Remove NA
}

agegroup.fn("SPOPtest")
spCdfplot(x ='bmival', weights = 'wt.nurse', cond = c("sex"), 
          dataS = HSE2011, dataP = SPOPtest)
spCdfplot(x ='bmival', weights = 'wt.nurse', cond = c("qimd"), 
          dataS = HSE2011, dataP = SPOPtest, aprox = c(T,T))
spCdfplot(x ='bmival', weights = 'wt.nurse', cond = c("group"), 
          dataS = HSE2011[between(as.numeric(as.character(age)), 20, 79)], dataP = SPOPtest[between(as.numeric(as.character(age)), 20, 79)], aprox = c(T,T))

# For diabetes I will use diabtyper : (D) Type of diabetes {revised} weighted with wt.int 
# 1 = Diagnosed aged 35+ and/or not treated with insulin, 2 = Not diabetic, 3 = Diagnosed before the age of 35 and treated with insulin
breaks <- c(8.34, 14.4, 16.2, 17.7, 20.8, 24.2, 26.9, 30.2, 33.4, 36.9, 42.3, Inf)
HSE2011[, bmivalCat := cut(bmival, breaks = breaks,
                           include.lowest = T, 
                           ordered_result = T)]
HSE2011[age < 16, diabtyper := 2]
basic <- c("agegroup", "sex", "hpnssec8", "bmivalCat")
additional <- c("diabtyper")
SPOPtest <- simCategorical(HSE2011, SPOPtest, 
                           w = "wt.int", 
                           strata = "qimd", 
                           basic = basic, 
                           additional = additional, 
                           method = "multinom", 
                           MaxNWts = 6000)
if (SPOPtest[is.na(diabtyper) == T, .N] > 0) {
    SPOPtest <- SPOPtest[is.na(diabtyper) == F,] # Remove NA
}

spMosaic(c("diabtyper"), "wt.int", HSE2011, SPOPtest)
spMosaic(c("diabtyper", "sex"), "wt.int", HSE2011, SPOPtest)
spMosaic(c("diabtyper", "agegroup"), "wt.int", HSE2011, SPOPtest)

# Try to capture undiagnosed diabetes using 
# diabtotr: (D) Total diabetes from blood sample or doctor diagnosis (excluding pregnancy-only diabetes)
# 1 =  No diabetes, 2 = Doctor diagnosed diabetes and/or HbAlc>=6.5
basic <- c("agegroup", "sex", "bmivalCat", "diabtyper")
additional <- c("diabtotr")
HSE2011[age <= 16, diabtotr := 1]
limit <- list(diabtyper = list("1" = "2", "2" = "1", "3" = "2"))
censor <- list("1" = list(diabtyper = "1", diabtyper = "3"))
SPOPtest <- simCategorical(HSE2011, SPOPtest, 
                           w = "wt.blood", 
                           strata = "qimd", 
                           basic = basic, 
                           additional = additional, 
                           method = "multinom",
                           limit = limit,
                           censor = censor,
                           MaxNWts = 6000)
SPOPtest[diabtyper == "1" & diabtotr == "1", diabtotr := "2"]
if (SPOPtest[is.na(diabtotr) == T, .N] > 0) {
    SPOPtest <- SPOPtest[is.na(diabtotr) == F,] # Remove NA
}

spMosaic(c("diabtotr"), "wt.blood", HSE2011, SPOPtest)
spMosaic(c("diabtotr", "sex"), "wt.blood", HSE2011, SPOPtest)
spMosaic(c("diabtotr", "diabtyper"), "wt.blood", HSE2011, SPOPtest)

# Calculate packyears
HSE2011 = copy(HSE2011original)
HSE2011[cigst1 == 4, packyears := (age - startsmk) * cigdyal / 20]  # Years smoking for smokers
HSE2011[cigst1 == 3, packyears := smokyrs * numsmok / 20]  # Years smoked for ex smokers
HSE2011[cigst1 == 2, packyears := smokyrs * 0.5 / 20]  # Years smoked for non-regular ex smokers (numsmok not available)
HSE2011[cigst1 == 1, packyears := 0]  # Years smoked for non smokers
HSE2011[packyears < 0, packyears := 0]
HSE2011[is.na(HSE2011[, "packyears"]) == F & HSE2011[, "packyears"] < 0, "packyears"] <- NA
HSE2011[cigdyal > 40, cigdyal := 40]  # code more than 40 cigars as 40
breaks <- c(0, seq(0.1, 39.1, 1), 40.1)
HSE2011[, cigdyalCat := cut(cigdyal, breaks = breaks, 
                            labels = c(0:40), 
                            include.lowest = T, 
                            ordered_result = T)]
basic <- c("agegroup", "sex", "cigdyalCat", "cigst1")
additional <- c("packyears")
limit <- list(cigst1 = list("1" = "0"))
SPOPtest <- simContinuous(HSE2011, SPOPtest, 
                          w = "wt.int",
                          strata = "qimd",
                          basic = basic,
                          additional = additional, 
                          method = "multinom",
                          zeros = T,
                          limit = limit, 
                          gpd = F,
                          MaxNWts = 4000,
                          keep = T)
if (SPOPtest[is.na(packyears) == T, .N] > 0) {
    SPOPtest <- SPOPtest[is.na(packyears) == F,] # Remove NA
}

SPOPtest[as.numeric(as.character(age))>15 & packyears>(as.numeric(as.character(age)) - 15) * 3, packyears := (as.numeric(as.character(age)) - 15) * 3]
SPOPtest[as.numeric(as.character(age))<16, packyears := 0]

spCdfplot(x ='packyears', weights = 'wt.int', cond = c("group"), 
          dataS = HSE2011[between(as.numeric(as.character(age)), 20, 79)], dataP = SPOPtest[between(as.numeric(as.character(age)), 20, 79)], aprox = c(T,T))

# Calculate numsmok (number of cigarette smoked for ex smokers)
breaks <- c(0, seq(1,80,5), Inf)
HSE2011[, packyearsCat := cut(packyears, breaks = breaks, 
                              include.lowest = T, 
                              ordered_result = T)]
SPOPtest[, packyearsCat := cut(packyears, breaks = breaks, 
                               include.lowest = T, 
                               ordered_result = T)]
HSE2011[cigst1 != "3", numsmok := 0]
basic <- c("agegroup", "sex", "hpnssec8", "eqv5", "packyearsCat", "cigst1")
additional <- c("numsmok")
limit <- list(cigst1 = list("1" = 0, "2" = 0, "4" = 0))
SPOPtest <- simCategorical(HSE2011, SPOPtest, 
                           w = "wt.int",
                           strata = "qimd",
                           basic = basic,
                           additional = additional, 
                           method = "multinom",
                           limit = limit,
                           MaxNWts = 4000)
if (SPOPtest[is.na(numsmok) == T, .N] > 0) {
    SPOPtest <- SPOPtest[is.na(numsmok) == F,] # Remove NA
}

spMosaic(c("numsmok"), "wt.int", HSE2011, SPOPtest)

# For systolic BP (wt.nurse) I will use omsysval ((D) Omron Valid Mean Systolic BP) omsysval was
# measured in ages >=5
HSE2011 = copy(HSE2011original)
breaks <- c(8.34, 14.4, 16.2, 17.7, 20.8, 24.2, 26.9, 30.2, 33.4, 36.9, 42.3, Inf)
HSE2011[, bmivalCat := cut(bmival, breaks = breaks,
                           include.lowest = T, 
                           ordered_result = T)]
basic <- c("agegroup", "sex", "hpnssec8", "bmivalCat", "bpmedc")
additional <- c("omsysval")
SPOPtest <- simContinuous(HSE2011, SPOPtest, 
                          w = "wt.nurse", 
                          strata = "qimd", 
                          basic = basic, 
                          additional = additional, 
                          method = "multinom", 
                          equidist = F, 
                          zeros = F, 
                          gpd = T, 
                          MaxNWts = 6000, 
                          keep = T)
if (SPOPtest[is.na(omsysval) == T, .N] > 0) {
    SPOPtest <- SPOPtest[is.na(omsysval) == F,] # Remove NA
}

spCdfplot(x ='omsysval', weights = 'wt.nurse', cond = c("group"), 
          dataS = HSE2011[between(as.numeric(as.character(age)), 20, 79)], dataP = SPOPtest[between(as.numeric(as.character(age)), 20, 79)], aprox = c(T,T))

# For diastolic BP (wt.nurse) I will use omdiaval ((D) Omron Valid Mean Systolic BP) omdiaval was
# measured in ages >= 5
breaks <- c(77.5, 90.5, 100, 105, 111, 120, 129, 140, 150, 160, 176, Inf)
HSE2011[, omsysvalCat := cut(omsysval, breaks = breaks,
                             include.lowest = T, 
                             ordered_result = T)]
basic <- c("agegroup", "sex", "hpnssec8", "bmivalCat", "qimd", "bpmedc")
additional <- c("omdiaval")
SPOPtest <- simContinuous(HSE2011, SPOPtest, 
                          w = "wt.nurse", 
                          strata = "omsysvalCat", 
                          basic = basic, 
                          additional = additional, 
                          method = "multinom", 
                          equidist = F, 
                          zeros = F, 
                          gpd = T, 
                          MaxNWts = 6000, 
                          keep = F)
if (SPOPtest[is.na(omdiaval) == T, .N] > 0) {
    SPOPtest <- SPOPtest[is.na(omdiaval) == F,] # Remove NA
}

spCdfplot(x ='omdiaval', weights = 'wt.nurse', cond = c("group"), 
          dataS = HSE2011[between(as.numeric(as.character(age)), 20, 79)], dataP = SPOPtest[between(as.numeric(as.character(age)), 20, 79)], aprox = c(T,T))

# This method produces diastolic values that are higher than the systolic ones (around 5 individuals)
SPOPtest[(as.numeric(omsysval) - as.numeric(omdiaval)) < 10, omdiaval := as.numeric(omdiaval) - 25] # correction

# For total Chol (wt.blood) I will use cholval1: (D) Valid Cholesterol Result (incl those on lld)
# cholval1 was measured in ages >=16
basic <- c("agegroup", "sex", "bmivalCat", "lipid")
additional <- c("cholval1")
SPOPtest <- simContinuous(HSE2011, SPOPtest, 
                          w = "wt.blood", 
                          strata = "qimd", 
                          basic = basic, 
                          additional = additional, 
                          method = "multinom", 
                          equidist = F, 
                          zeros = F, 
                          gpd = T, 
                          MaxNWts = 6000, 
                          keep = T)
if (SPOPtest[is.na(cholval1) == T, .N] > 0) {
    SPOPtest <- SPOPtest[is.na(cholval1) == F,] # Remove NA
}

SPOPtest[between(as.numeric(as.character(age)), ageL,ageH), summary(cholval1)]
SPOP2011[between(as.numeric(as.character(age)), ageL,ageH), summary(cholval)]

spCdfplot(x ='cholval1', weights = 'wt.blood', cond = c("qimd"), 
          dataS = HSE2011[between(as.numeric(as.character(age)), 20, 79)], dataP = SPOPtest[between(as.numeric(as.character(age)), 20, 79)])
spCdfplot(x ='cholval1', weights = 'wt.blood', cond = c("qimd"), 
          dataS = HSE2011, dataP = SPOPtest)
spCdfplot(x ='cholval1', weights = 'wt.blood', cond = c("group"), 
          dataS = HSE2011[between(as.numeric(as.character(age)), 20, 79)], dataP = SPOPtest[between(as.numeric(as.character(age)), 20, 79)])
spCdfplot(x ='cholval1', weights = 'wt.blood', cond = c("sex"), 
          dataS = HSE2011[between(as.numeric(as.character(age)), 20, 79)], dataP = SPOPtest[between(as.numeric(as.character(age)), 20, 79)], aprox = c(T,T))

# CVD prevalence
# I will use cvdcon: (D) IHD/stroke but no IHD/None (1 = IHD, 2 = Stroke but no IHD, 3 = None of these) 
# I will not use redmeat and physical exercise as independent variables because they were not included in HSE2011.
# In the future I might use statmatch to inform HSE2011 with these variables from previous surveys 
breaks <- c(1.7, 2.9, 3.5, 3.8, 4.2, 4.9, 5.4, 6.1, 6.7, 7.1, 8, Inf)
HSE2011[, cholval1Cat := cut(cholval1, breaks = breaks,
                             include.lowest = T, 
                             ordered_result = T)]
HSE2011[is.na(cvdcon) == T & age < 16, cvdcon := 3]  # code ages of 0-15 as non having CVD
HSE2011[age <= 16, diabtyper := 2]
HSE2011[is.na(bpmedc)==T, bpmedc := 0]

basic <- c("agegroup", "sex", "bmivalCat", "lipid", "bpmedc", "cigst1", "diabtyper", "omsysvalCat", "cholval1Cat")
additional <- c("cvdcon")
limit <- list(agegroup = list("<1   " = "3", "01-04" = "3", "05-09" = "3",
                              "10-14" = "3", "15-19" = "3", "20-24" = "3")) # I set this because otherwise it produces very high prevalence of CVD
SPOPtest <- simCategorical(HSE2011, SPOPtest, 
                           w = "wt.blood", 
                           strata = "qimd", 
                           basic = basic, 
                           additional = additional, 
                           method = "multinom",
                           limit = limit,
                           MaxNWts = 6000)
if (SPOPtest[is.na(cvdcon) == T, .N] > 0) {
    SPOPtest <- SPOPtest[is.na(cvdcon) == F,] # Remove NA
}

spMosaic(c("cvdcon"), "wt.blood", HSE2011, SPOPtest)

# Salt consumption (from HSE2010) and weighted from Sodium Survey England 2011
# Tanaka's equation: 24-h urinary Na excretion (mEq/day) = 21.98 x {UNa/(UCr x 10) x (-2.04 x age + 14.89 x
# weight (kg) + 16.14 x height (cm) - 2244.45)}^ 0.392 Figure 3
# NOTE: units of urine measuraments is mmol/L for Na, K, Cr but Tanaka's formula uses mg/L (I will concert in the formula)
# 23 mg Na = 1 mmol Na or 1 mEq Na (1 mEq of Na = 58.5 mg of NaCl)
# Tanaka's method (more reliable)
HSE2010 =copy(HSE2010original)
HSE2010[is.na(bpmedc)==T, bpmedc := 0]
setnames(HSE2010, "imd2007", "qimd")
breaks <- c(77.5, 90.5, 100, 105, 111, 120, 129, 140, 150, 160, 176, Inf)
HSE2010[, omsysvalCat := cut(omsysval, breaks = breaks,
                             include.lowest = T, 
                             ordered_result = T)]
HSE2010[, Nameq := 21.98 * ((-2.04 * age + 14.89 * wtval + 16.14 * htval - 2244.45) * sodium/(creatin *113))^0.392] # Tanaka's equation
HSE2010[, Nagr := Nameq * 23/1000]
HSE2010[, salt := Nameq * 58.5/1000]
basic <- c("agegroup", "sex", "bpmedc", "omsysvalCat")
additional <- c("salt")
SPOPtest <- simContinuous(HSE2010, SPOPtest, 
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
if (SPOPtest[is.na(salt) == T, .N] > 0) {
    SPOPtest <- SPOPtest[is.na(salt) == F,] # Remove NA
}

agegroup.fn("HSE2010")
spCdfplot(x ='salt', weights = 'wt.urine', cond = c("qimd"), 
          dataS = HSE2010[between(as.numeric(as.character(age)), 20, 79)], dataP = SPOPtest[between(as.numeric(as.character(age)), 20, 79)])
spCdfplot(x ='salt', weights = 'wt.urine', cond = c("qimd"), 
          dataS = HSE2010, dataP = SPOPtest)
spCdfplot(x ='salt', weights = 'wt.urine', cond = c("group"), 
          dataS = HSE2010[between(as.numeric(as.character(age)), 20, 79)], dataP = SPOPtest[between(as.numeric(as.character(age)), 20, 79)])
spCdfplot(x ='salt', weights = 'wt.urine', cond = c("sex"), 
          dataS = HSE2010[between(as.numeric(as.character(age)), 20, 79)], dataP = SPOPtest[between(as.numeric(as.character(age)), 20, 79)], aprox = c(T,T))

# Corrections for salt
SPOPtest[, saltWt := salt]
SPOPtest[, age := as.numeric(as.character(age))]

# define function for distribution matching
match <- function (p, e, age1, age2, gender) {
    d <- SPOPtest[saltWt>0 & sex == gender & between(age, age1, age2), quantile(saltWt, probs = p)]
    names(e) <- names(d)
    f <- e/d
    names(f) <- names(d)
    
    for (k in 1:2) { # will be repeated 2 times
        for (l in 9:1) {
            SPOPtest[saltWt <= d[l] & sex == gender & between(age, age1, age2), saltWt := saltWt * f[l]]
            d <- SPOPtest[saltWt>0 & sex == gender & between(age, age1, age2), quantile(saltWt, probs = p)]
            f <- e/d
        }
    }
    return(SPOPtest)
}
# Men 19-34 (16-34)
p <- c(0.025, 0.15, 0.3, 0.44, 0.5, 0.61, 0.76, 0.95, 0.975)  # Known percentiles probabilities
e <- c(4.2, 5.3, 7, 8.8, 9.3, 10.5, 12.3, 15.8, 17.3)  # salt from Sodium Survey England 2011 tables 9 and 10
age1 <- 16
age2 <- 34
gender <- 1
match(p, e, age1, age2, gender)  # Call function to perform scalling

# Women 19-34 (16-34)
p <- c(0.025, 0.12, 0.23, 0.5, 0.52, 0.77, 0.91, 0.94, 0.975)  # Known percentiles probabilities
e <- c(2.8, 3.5, 5.3, 7, 7, 8.8, 10.5, 12.3, 15.2)  # salt from Sodium Survey England 2011 tables 9 and 10
age1 <- 16
age2 <- 34
gender <- 2
match(p, e, age1, age2, gender)  # Call function to perform scalling

# Men 35-49
p <- c(0.025, 0.06, 0.26, 0.39, 0.5, 0.6, 0.77, 0.94, 0.975)  # Known percentiles probabilities
e <- c(4.3, 5.3, 7, 8.8, 9.7, 10.5, 12.3, 15.8, 18.8)  # salt from Sodium Survey England 2011 tables 9 and 10
age1 <- 35
age2 <- 49
gender <- 1
match(p, e, age1, age2, gender)  # Call function to perform scalling

# Women 35-49
p <- c(0.025, 0.3, 0.5, 0.66, 0.78, 0.9, 0.975, 0.98, 0.99)  # Known percentiles probabilities
e <- c(3.4, 5.3, 6.1, 7, 8.8, 10.5, 12.1, 12.3, 15.8)  # salt from Sodium Survey England 2011 tables 9 and 10
age1 <- 35
age2 <- 49
gender <- 2
match(p, e, age1, age2, gender)  # Call function to perform scalling

# Men 50-64 (50-80)
p <- c(0.025, 0.21, 0.38, 0.5, 0.62, 0.81, 0.9, 0.97, 0.975)  # Known percentiles probabilities
e <- c(3.1, 5.3, 7, 7.8, 8.8, 10.5, 12.3, 15.8, 18)  # salt from Sodium Survey England 2011 tables 9 and 10
age1 <- 50
age2 <- 80
gender <- 1
match(p, e, age1, age2, gender)  # Call function to perform scalling

# Women 50-64 (50-80)
p <- c(0.025, 0.09, 0.33, 0.5, 0.6, 0.82, 0.95, 0.96, 0.975)  # Known percentiles probabilities
e <- c(2.6, 3.5, 5.3, 6.3, 7, 8.8, 10.5, 12.3, 12.7)  # salt from Sodium Survey England 2011 tables 9 and 10
age1 <- 50
age2 <- 80
gender <- 2
match(p, e, age1, age2, gender)  # Call function to perform scalling


# Finalise SPOPtest
SPOPtest[, sex := as.factor(sex)]
SPOPtest[, cigdyalCat := as.numeric(as.character(cigdyalCat))]
SPOPtest[, endsmoke := as.numeric(as.character(endsmoke))]
SPOPtest[, frtpor := as.numeric(as.character(frtpor))]
SPOPtest[, hsize := as.numeric(as.character(hsize))]
SPOPtest[, porftvg := as.numeric(as.character(porftvg))]
SPOPtest[, cigdyalCat := as.numeric(as.character(cigdyalCat))]
SPOPtest[, numsmok := as.numeric(as.character(numsmok))]
SPOPtest[, packyearsCat := NULL]
#SPOPtest[, bmival := round(bmival,1)]
setkey(SPOPtest, age, sex, agegroup, qimd)

# Return datatable
#save(SPOPtest, file = "./SynthPop/SPOPtest.RData")

end()

# Garbage cleaning
#rm(list = ls(all = T))

