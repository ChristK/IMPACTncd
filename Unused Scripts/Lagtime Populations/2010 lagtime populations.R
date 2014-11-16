# ************************************************************************************************
#                  England's synthetic population
# ************************************************************************************************


# User input
year <- 2010
prop <- 0.01  # Set propotion (prop) of the populations to be synthesized (set 1 for total population)
pop <- 52642500  # Mid year population
iterations <- 1 # Number of synthetic population to produce
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
            stop("Currently, 'get.dropbox.folder' works for Windows only. Sorry.")
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

# Import datasets (household and individual files)
load(file="./Datasets/Health Survey for England/2010/hse10ai.RData")
HSE2010original <- clear.labels(HSE2010original)
HSE <- data.table(filter(HSE2010original, samptype==1), key="age")
HSE =copy(HSE[age > 15,])
rm(HSE2010original)

breaks                   <- c(0, 1, seq(5, 85, 5), 130)
labels                   <- c("<1   ", "01-04", "05-09",
                              "10-14", "15-19", "20-24", 
                              "25-29", "30-34", "35-39", 
                              "40-44", "45-49", "50-54",
                              "55-59", "60-64", "65-69",
                              "70-74", "75-79", "80-84", "85+")
HSE[, agegroup := cut(age, breaks = breaks, 
                      labels = labels, 
                      include.lowest = T, 
                      right = F, 
                      ordered_result = T)]

HSE[is.na(endsmoke) == T & (cigst1 == 1 | cigst1 == 4), endsmoke := 0]  # code non ex smokers as 0
setnames(HSE, c("hhsize", "imd2007"), c("hsize", "qimd"))

# Rescale weights of the household files (page 25 of HSE2011-Methods-and-docs.pdf)
t <- HSE[, sum(wt.hhld)]  # calculate the sum of original weights
d <- pop * prop/t
HSE[, wt.hhld := wt.hhld * d]


# Create household structure of the synthetic population
SPOP <- simStructure(HSE, 
                     hid = "hserial", 
                     w = "wt.hhld", 
                     strata = "sha", 
                     pid = "pserial", 
                     additional = c("age", "sex"), 
                     method = "multinom", 
                     keep = F)

SPOP <- data.table(SPOP, key="age")

# Create unique ID
SPOP[, id := c(1:.N)]

# Remove NA (missing age)
SPOP <- SPOP[is.na(age) == F]

# Create categorical age var, etc
breaks                   <- c(0, 1, seq(5, 85, 5), 130)
labels                   <- c("<1   ", "01-04", "05-09",
                              "10-14", "15-19", "20-24", 
                              "25-29", "30-34", "35-39", 
                              "40-44", "45-49", "50-54",
                              "55-59", "60-64", "65-69",
                              "70-74", "75-79", "80-84", "85+")
SPOP[, agegroup := cut(age, breaks = breaks, 
                       labels = labels, 
                       include.lowest = T, 
                       right = F, 
                       ordered_result = T)]

# Add qimd (quantile of index of multiple deprivation) variable (I started from it because no NAs)
basic <- c("age", "sex", "hsize")
additional <- c("qimd")

SPOP <- simCategorical(HSE, SPOP, 
                       w = "wt.int", 
                       strata = "sha", 
                       basic = basic, 
                       additional = additional, 
                       method = "multinom")


# Start using qimd for strata from now on (5= more deprived) and add more SEC indicators
basic <- c("agegroup", "sex")
additional <- c("eqv5")

SPOP <- simCategorical(HSE, SPOP, 
                       w = "wt.int", 
                       strata = "qimd", 
                       basic = basic, 
                       additional = additional, 
                       method = "multinom", 
                       MaxNWts = 4000)
if (SPOP[is.na(eqv5) == T, .N] >0) {
    SPOP <- SPOP[is.na(eqv5) == F,] # Remove NA
}

basic <- c("agegroup", "sex", "eqv5")
additional <- c("hpnssec8")
SPOP <- simCategorical(HSE, SPOP, 
                       w = "wt.int", 
                       strata = "qimd", 
                       basic = basic, 
                       additional = additional, 
                       method = "multinom", 
                       MaxNWts = 4000)
if (SPOP[is.na(hpnssec8) == T, .N] > 0) {
    SPOP <- SPOP[is.na(hpnssec8) == F,] # Remove NA
}

# For Fruit and Veg porftvg will be used (maximum 1 portion of juice, pulses or dried fruit
# contributed to the total portions in porfv(continous var)) porftvg was measured for ages >=5 For
# ages <5 I will code them as 99 = non applicable
basic <- c("agegroup", "sex", "hpnssec8", "eqv5")
additional <- c("porftvg")
SPOP <- simCategorical(HSE, SPOP, 
                       w = "wt.int", 
                       strata = "qimd", 
                       basic = basic, 
                       additional = additional, 
                       method = "multinom", 
                       MaxNWts = 4000)
if (SPOP[is.na(porftvg) == T, .N] > 0) {
    SPOP <- SPOP[is.na(porftvg) == F,] # Remove NA
}

#  For fruit portions
HSE[, frtporCat := cut(frtpor, breaks = c(0, 0.1:8.1, 30), 
                       labels = c(0:9), include.lowest = T, 
                       right = F, 
                       ordered_result = T)]
basic <- c("agegroup", "sex", "porftvg")
additional <- "frtporCat"
SPOP <- simCategorical(HSE, SPOP, 
                       w = "wt.int", 
                       strata = "qimd", 
                       basic = basic, 
                       additional = additional, 
                       method = "multinom", 
                       MaxNWts = 6000)
setnames(SPOP, "frtporCat", "frtpor")
if (SPOP[is.na(frtpor) == T, .N] > 0) {
    SPOP <- SPOP[is.na(frtpor) == F,] # Remove NA
}

# For smoking history cigst1 will be used (D) Cigarette Smoking Status - Never/Ex-reg/Ex-occ/Current
# cigst1 has been measured in ages >=16. I will code all ages below 16 as never smokers
basic <- c("agegroup", "sex", "hpnssec8")
additional <- "cigst1"
SPOP <- simCategorical(HSE, SPOP, 
                       w = "wt.int", 
                       strata = "qimd", 
                       basic = basic, 
                       additional = additional, 
                       method = "multinom", 
                       MaxNWts = 4000)
if (SPOP[is.na(cigst1) == T, .N] > 0) {
    SPOP <- SPOP[is.na(cigst1) == F,] # Remove NA
}

# For Smoking cigdyal will be used ((D) Number of cigarettes smoke a day - inc. non-smokers)
# (measured for ages <=16) I will code ages < 16 as 0 (ie non smokers) I will recode into levels by 1
# cigar and aggregate more than 40 cigars into 20. To be named cigdyalCat
HSE[cigdyal > 40, cigdyal := 40]  # code more than 40 cigars as 40
breaks <- c(0, seq(0.1, 39.1, 1), 40.1)
HSE[, cigdyalCat := cut(cigdyal, breaks = breaks, 
                        labels = c(0:40), 
                        include.lowest = T, 
                        ordered_result = T)]
limit <- list(cigst1 = list("1" = "0", "2" = "0", "3" = "0"))
censor <- list("0" = list(cigst1 = "4"))
basic <- c("agegroup", "sex", "hpnssec8", "cigst1" )
additional <- "cigdyalCat"
SPOP <- simCategorical(HSE, SPOP, 
                       w = "wt.int", 
                       strata = "qimd", 
                       basic = basic, 
                       additional = additional, 
                       method = "multinom", 
                       limit = limit,
                       censor = censor,
                       MaxNWts = 4000)
if (SPOP[is.na(cigdyalCat) == T, .N] > 0) {
    SPOP <- SPOP[is.na(cigdyalCat) == F,] # Remove NA
}


# For How long ago did you stop smoking cigarettes? (Applicable only to ex-smokers) I will use endsmoke
basic <- c("agegroup", "sex", "hpnssec8", "cigst1")
additional <- "endsmoke"
limit <- list(cigst1 = list("1" = "0", "4" = "0"))
SPOP <- simCategorical(HSE, SPOP, 
                       w = "wt.int", 
                       strata = "qimd", 
                       basic = basic, 
                       additional = additional, 
                       method = "multinom", 
                       limit = limit,
                       MaxNWts = 6000)
if (SPOP[is.na(endsmoke) == T, .N] > 0) {
    SPOP <- SPOP[is.na(endsmoke) == F,] # Remove NA
}

# For passive smoking expsmok will be used (Number of hours/week exposed to others' smoke (c+sc)) To
# be recoded into a categorical variable (expsmokCat)
breaks <- c(0, seq(0.1, 99.1, 10), max(HSE$expsmok, na.rm = T))
HSE[, expsmokCat := cut(expsmok, breaks = breaks,
                        labels = c(0:10),
                        include.lowest = T, 
                        ordered_result = T)]
basic <- c("agegroup", "sex", "hpnssec8", "cigst1" )
additional <- "expsmokCat"
SPOP <- simCategorical(HSE, SPOP, 
                       w = "wt.int", 
                       strata = "qimd", 
                       basic = basic, 
                       additional = additional, 
                       method = "multinom", 
                       MaxNWts = 4000)
if (SPOP[is.na(expsmokCat) == T, .N] > 0) {
    SPOP <- SPOP[is.na(expsmokCat) == F,] # Remove NA
}


# For BMI I will use bmival (measured for for ages >=2) as continous variable
basic <- c("agegroup", "sex", "hpnssec8", "eqv5", "porftvg")
additional <- c("bmival")
limit <- list(agegroup = list("<1   " = 0))
SPOP <- simContinuous(HSE, SPOP, 
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
if (SPOP[is.na(bmival) == T, .N] > 0) {
    SPOP <- SPOP[is.na(bmival) == F,] # Remove NA
}

# Diabetes
breaks <- SPOP[, min(bmival), by= list(bmivalCat)]
breaks <- breaks$V1
breaks <- c(breaks, Inf)
breaks <- sort(breaks)
HSE[, bmivalCat := cut(bmival, breaks = breaks,
                       include.lowest = T, 
                       ordered_result = T)]
HSE[diabete2 == 2, diabtotr := 1]
HSE[diabete2 == 1 | glyhbval > 6.5, diabtotr := 2]

basic <- c("agegroup", "sex", "bmivalCat")
additional <- c("diabtotr")
SPOP <- simCategorical(HSE, SPOP, 
                       w = "wt.int", 
                       strata = "qimd", 
                       basic = basic, 
                       additional = additional, 
                       method = "multinom", 
                       MaxNWts = 6000)
if (SPOP[is.na(diabtotr) == T, .N] > 0) {
    SPOP <- SPOP[is.na(diabtotr) == F,] # Remove NA
}

# Calculate packyears
HSE[cigst1 == 4, packyears := (age - startsmk) * cigdyal / 20]  # Years smoking for smokers
HSE[cigst1 == 3, packyears := smokyrs * numsmok / 20]  # Years smoked for ex smokers
HSE[cigst1 == 2, packyears := smokyrs * 0.5 / 20]  # Years smoked for non-regular ex smokers (numsmok not available)
HSE[cigst1 == 1, packyears := 0]  # Years smoked for non smokers
HSE[packyears < 0, packyears := 0]
HSE[cigdyal > 40, cigdyal := 40]  # code more than 40 cigars as 40
breaks <- c(0, seq(0.1, 39.1, 1), 40.1)
HSE[, cigdyalCat := cut(cigdyal, breaks = breaks, 
                        labels = c(0:40), 
                        include.lowest = T, 
                        ordered_result = T)]
basic <- c("agegroup", "sex", "cigdyalCat", "cigst1")
additional <- c("packyears")
limit <- list(cigst1 = list("1" = "0"))
SPOP <- simContinuous(HSE, SPOP, 
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
if (SPOP[is.na(packyears) == T, .N] > 0) {
    SPOP <- SPOP[is.na(packyears) == F,] # Remove NA
}

# Calculate numsmok (number of cigarette smoked for ex smokers)
breaks <- c(0, seq(1,80,5), Inf)
HSE[, packyearsCat := cut(packyears, breaks = breaks, 
                          include.lowest = T, 
                          ordered_result = T)]
SPOP[, packyearsCat := cut(packyears, breaks = breaks, 
                           include.lowest = T, 
                           ordered_result = T)]
HSE[cigst1 != "3", numsmok := 0]
basic <- c("agegroup", "sex", "hpnssec8", "eqv5", "packyearsCat", "cigst1")
additional <- c("numsmok")
limit <- list(cigst1 = list("1" = 0, "2" = 0, "4" = 0))
SPOP <- simCategorical(HSE, SPOP, 
                       w = "wt.int",
                       strata = "qimd",
                       basic = basic,
                       additional = additional, 
                       method = "multinom",
                       limit = limit,
                       MaxNWts = 4000)
if (SPOP[is.na(numsmok) == T, .N] > 0) {
    SPOP <- SPOP[is.na(numsmok) == F,] # Remove NA
}

# For systolic BP (wt.nurse) I will use omsysval ((D) Omron Valid Mean Systolic BP) omsysval was
# measured in ages >=5
basic <- c("agegroup", "sex", "hpnssec8", "bmivalCat")
additional <- c("omsysval")
SPOP <- simContinuous(HSE, SPOP, 
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
if (SPOP[is.na(omsysval) == T, .N] > 0) {
    SPOP <- SPOP[is.na(omsysval) == F,] # Remove NA
}


# For total Chol (wt.blood) I will use cholval1: (D) Valid Cholesterol Result (incl those on lld)
# cholval1 was measured in ages >=16
basic <- c("agegroup", "sex", "porftvg", "bmivalCat")
additional <- c("cholval1")
SPOP <- simContinuous(HSE, SPOP, 
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
if (SPOP[is.na(cholval1) == T, .N] > 0) {
    SPOP <- SPOP[is.na(cholval1) == F,] # Remove NA
}

# Finalise SPOP
SPOP[, age := as.numeric(as.character(age))]
SPOP[, sex := as.factor(sex)]
SPOP[, cigdyalCat := as.numeric(as.character(cigdyalCat))]
SPOP[, endsmoke := as.numeric(as.character(endsmoke))]
SPOP[, frtpor := as.numeric(as.character(frtpor))]
SPOP[, hsize := as.numeric(as.character(hsize))]
SPOP[, porftvg := as.numeric(as.character(porftvg))]
SPOP[, cigdyalCat := as.numeric(as.character(cigdyalCat))]
SPOP[, numsmok := as.numeric(as.character(numsmok))]
SPOP[, packyearsCat := NULL]
#SPOP[, bmival := round(bmival,1)]
setkey(SPOP, age, sex, agegroup, qimd)

# Return datatable
save(SPOP, file = paste("./Models/IMPACTncd/Lagtime Populations/SPOP", year, ".RData", sep= ""))

end()

# Garbage cleaning
rm(list = ls(all = T))

