# preample
gc()

if (Sys.info()[1] == "Linux") {
    setwd(paste("/home/", 
                system("whoami", T), 
                "/Dropbox/PhD", 
                sep = "", 
                collapse = ""))
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
dependencies(c("data.table", 
               "dplyr", 
               "Hmisc",
               "survey",
               "reshape2",
               "ggplot2", 
               "stringr", 
               "compiler"))


# Define function to sample rows from a dataframe
sample.df <- function(x, size, replace = F, prob = NULL) {
    stopifnot(is.data.frame(x))
    x.indexes <- sample.int(nrow(x), size, replace, prob)
    x[x.indexes, ]
}

# Define operator %!in%, meaning !%in%
'%!in%' <- function(x,y)!('%in%'(x,y))

# Define outersect. Like setdiff but symmetrical. I.e. setdiff(a,b) is not the same as setdiff(b,a). outersect solve this by calculating both
outersect <- function(x, y, ...) {
    big.vec <- c(x, y, ...)
    duplicates <- big.vec[duplicated(big.vec)]
    setdiff(big.vec, unique(duplicates))
}

# Define function to clear labels form SPSS labelled imports
clear.labels <- function(x) {
    if(is.list(x)) {
        for(i in 1 : length(x)) class(x[[i]]) <- setdiff(class(x[[i]]), 'labelled') 
        for(i in 1 : length(x)) attr(x[[i]],"label") <- NULL
    }
    else {
        class(x) <- setdiff(class(x), "labelled")
        attr(x, "label") <- NULL
    }
    return(invisible(x))
}

## Peto method for the impact of smoking 

# For year 2011
Peto.method <- fread("./Cancer Statistics/2011 cases of lung cancer.csv", 
                     sep = ",", 
                     header = T, 
                     stringsAsFactors = T)
setnames(Peto.method, c("site", "sex", "<1   ", "01-04", "05-09", "10-14", "15-19", 
                        "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                        "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"))

Peto.method[, site := str_trim(site, side = "both")]
Peto.method = copy(melt(Peto.method, id.vars = c("site", "sex"),
                        variable.name = "agegroup", 
                        value.name = "observed.lung.cancer.incidence"))
Peto.method[, site := NULL]
setkey(Peto.method, sex, agegroup)

ONSpop2011temp <- fread("./Cancer Statistics/ONSpopulation2011.csv", 
                        sep = ",", 
                        header = T, 
                        stringsAsFactors = T)
setnames(ONSpop2011temp, c("site", "sex", "<1   ", "01-04", "05-09", "10-14", "15-19", 
                           "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                           "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"))

ONSpop2011temp = copy(melt(ONSpop2011temp, id.vars = c("site", "sex"),
                        variable.name = "agegroup", 
                        value.name = "population"))
ONSpop2011temp[, site := NULL]
setkey(ONSpop2011temp, sex, agegroup)

Peto.method = copy(ONSpop2011temp[Peto.method])
rm(ONSpop2011temp)

# rates for non smokers were taken from CPSII 
# 1 Thun MJ, Henley SJ, Burns D, et al. Lung Cancer Death Rates in Lifelong Nonsmokers. JNCI J Natl Cancer Inst 2006;98:691–9. doi:10.1093/jnci/djj187
# Table II (Rates smoothed with Poisson regression used for younger ages)

Peto.method[, "non.smoker.rate":= c(0, 0, 0, 0, 0, 0, 0, 0, 1.5, 2.3, 3.5, 5.5, 7, 14.2, 19.8, 34.4, 44.0, 80.3, 80.3,
                                    0, 0, 0, 0, 0, 0, 0, 0, 2, 2.9, 4.1, 6.3, 7.8, 13.3, 17.2, 29.9, 36.3, 52.8, 52.8)]
Peto.method[, expected.lung.cancer.incidence := round(population * non.smoker.rate / 100000)]
Peto.method[, paf.lung.ca := (observed.lung.cancer.incidence - expected.lung.cancer.incidence) / observed.lung.cancer.incidence]
for (j in seq_len(ncol(Peto.method))) set(Peto.method, which(is.na(Peto.method[[j]])), j, 0)  # replaces NA with 0
Peto.method[paf.lung.ca<0 | paf.lung.ca==1, paf.lung.ca :=0] # replaces negative paf.lung.ca with 0

# use sex and age specific RR of lung cancer(CHD,Stroke) for current smokers from 
# Thun MJ, Myers DG, Day-Lally C, et al. Age and the exposure-response relationships between cigarette smoking and premature death in Cancer Prevention Study II.
# Shopland D: Changes in cigarette-related disease risks and their implications for prevention and control, 
# National Institutes of Health, Bethesda, Maryland 1997;:383–413.
# Peto.method[, "rr.lung.ca":= c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 7, 21.1, 39, 31.3, 27, 26, 21.5, 13.8, 13.8,
#                                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 22.1, 11.3, 16.6, 14.3, 17.1, 10.2, 12.3, 7.3, 7.3)]

Peto.method[, "rr.lung.ca":= c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, rep(21.3,9),
                               1, 1, 1, 1, 1, 1, 1, 1, 1, 1, rep(12.5, 9))] # from Thun MJ, Apicella LF, Henley S. Smoking vs other risk factors as the cause of smoking-attributable deaths: Confounding in the courtroom. JAMA 2000;284:706–12. doi:10.1001/jama.284.6.706



# from the formula PAF = P*(rr-1)/ (1+ P(rr-1)), I solve for P to calculate the "notional" prevalence of smoking
Peto.method[, Pe:= paf.lung.ca/((rr.lung.ca-1)*(1-paf.lung.ca))]
Peto.method[Pe<0, Pe :=0] # replaces negative Pe with 0

Peto.method[, "rr.chd":= c(1, 1, 1, 1, 1, 1, 1, 5.51, 5.51, 5.51, 3.04, 3.04, 3.04, 1.88, 1.88, 1.44, 1.44, 1.05, 1.05,
                           1, 1, 1, 1, 1, 1, 1, 2.26, 2.26, 2.26, 3.78, 3.78, 3.78, 2.53, 2.53, 1.68, 1.68, 1.38, 1.38)] #from Ezzati M, Henley SJ, Thun MJ, Lopez AD. Role of Smoking in Global and Regional 
# Cardiovascular Mortality. Circulation. 2005 Jul 26;112(4):489–97.
# Table 1 Model B

Peto.method[, "rr.stroke":= c(1, 1, 1, 1, 1, 1, 1, 3.12, 3.12, 3.12, 3.12, 3.12, 3.12, 1.87, 1.87, 1.39, 1.39, 1.05, 1.05,
                              1, 1, 1, 1, 1, 1, 1, 4.61, 4.61, 4.61, 4.61, 4.61, 4.61, 2.81, 2.81, 1.95, 1.95, 1, 1)] # as above. for ages <45 set arbitrarily as 45-59. for ages above 80 was less than 1. arbitrarily to 1

# Calculate paf for CHD
Peto.method[, tobaccopaf.chd := Pe*(rr.chd-1)/(1+Pe*(rr.chd-1))]
# Calculate paf for stroke
Peto.method[, tobaccopaf.stroke := Pe*(rr.stroke-1)/(1+Pe*(rr.stroke-1))]
for (j in seq_len(ncol(Peto.method))) set(Peto.method, which(is.na(Peto.method[[j]])), j, 0)  # replaces NA with 0
for (j in seq_len(ncol(Peto.method))) set(Peto.method, which(Peto.method[[j]]==Inf), j, 0)  # replaces Inf with 0


chdsmokepaf = copy(Peto.method[, list(sex, agegroup, tobaccopaf.chd)])
setnames(chdsmokepaf, "tobaccopaf.chd", "tobaccopaf")
write.csv(chdsmokepaf, file = "./CVD Statistics/petochdsmokepaf.csv", row.names = F)

# draw example of smoothered PAF
chdsmokepaf[tobaccopaf > 0, smoothed := predict(loess(tobaccopaf~c(1:.N))), by="sex"]# smooth PAF
chdsmokepaf[, plot(tobaccopaf, col=c("blue", "red")[sex], main = "PAF of tobacco on CHD")]
chdsmokepaf[, lines(smoothed,  lwd=2)]

strokesmokepaf = copy(Peto.method[, list(sex, agegroup, tobaccopaf.stroke)])
setnames(strokesmokepaf, "tobaccopaf.stroke", "tobaccopaf")
write.csv(strokesmokepaf, file = "./CVD Statistics/petostrokesmokepaf.csv", row.names = F)

# draw example of smoothered PAF
strokesmokepaf[tobaccopaf > 0, smoothed := predict(loess(tobaccopaf~c(1:.N))), by="sex"]# smooth PAF
strokesmokepaf[, plot(tobaccopaf, col=c("blue", "red")[sex], main = "PAF of tobacco on stroke")]
strokesmokepaf[, lines(smoothed,  lwd=2)]

C34smokepaf2011 = copy(Peto.method[, list(sex, agegroup, paf.lung.ca)])
setnames(C34smokepaf2011, "paf.lung.ca", "tobaccopaf")
write.csv(C34smokepaf2011, file = "./Cancer Statistics/c34tobaccopaf.csv", row.names = F)

# draw example of smoothered PAF
C34smokepaf2011[tobaccopaf > 0, smoothed := predict(loess(tobaccopaf~c(1:.N))), by="sex"]# smooth PAF
C34smokepaf2011[, plot(tobaccopaf, col=c("blue", "red")[sex], main = "PAF of tobacco on lung cancer")]
C34smokepaf2011[, lines(smoothed,  lwd=2)]

#*********************************************************************************************************#

# Traditional method for tobacco PAF 
## Work on calculating paf for other factors. In order to do this I need to calculate their prevalence in 2006 
load(file="./Datasets/Health Survey for England/2006/hse06ai.RData") # load HSE2006
HSE <- setDT(clear.labels(HSE))
HSE <-HSE[samptype!=3,] # remove boost sample of children and convert to datatable
HSE[, cholval1 := cholval1 + 0.1] # adjust for different equipment since 2011 HSE used for the synthetic population
HSE[,age:= age + 5] # to compansate for the lag. eg. 30yo in 2011 is 25 in 2006
breaks <- c(-Inf, 1, seq(5, 85, 5), +Inf)
labels <- c("<1   ", "01-04", "05-09", "10-14", "15-19", "20-24", 
            "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
            "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")

HSE[, agegroup := cut(age, 
                      breaks = breaks, 
                      labels = labels, 
                      include.lowest = T, 
                      right = F, 
                      ordered_result = T)]
setkey(HSE, agegroup)

HSE.srv <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=HSE)

chdsmokepaf <-as.data.table(svytable(~agegroup + sex + cigst1, HSE.srv, exclude=NULL, na.action=na.pass))
chdsmokepaf[, sum := sum(N), by=list(agegroup, sex)] # count number of people in each agegroup by sex (denominator for prevalence)
chdsmokepaf[, Pe  := N/sum] # Calculate prevalence by agegroup and sex
chdsmokepaf[, `:=` (N = NULL, sum = NULL)]
write.csv(chdsmokepaf[Pe != 0,], file = "./Models/IMPACTncd/Exposure/TOBpreval2006.csv", row.names = F) # create csv with SBP prevalence

# RR for tobacco from Ezzati M, Henley SJ, Thun MJ, Lopez AD. 
# Role of Smoking in Global and Regional Cardiovascular Mortality. 
# Circulation. 2005 Jul 26;112(4):489–97.
# Table 1 Model B
chdsmokepaf[, rr := 1]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("30-34", "35-39","40-44") & sex == "1", rr := 5.51]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("45-49", "50-54","55-59") & sex == "1", rr :=3.04]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("60-64", "65-69") & sex == "1", rr := 1.88]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("70-74", "75-79") & sex == "1", rr := 1.44]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("30-34", "35-39","40-44") & sex == "2", rr := 2.26]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("45-49", "50-54","55-59") & sex == "2", rr :=3.78]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("60-64", "65-69") & sex == "2", rr := 2.53]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("70-74", "75-79") & sex == "2", rr := 1.68]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("80-84", "85+") & sex == "2", rr := 1.38]

# RR for ex-smokers from Huxley RR, Woodward M. 
# Cigarette smoking as a risk factor for coronary heart disease in women 
# compared with men: a systematic review and meta-analysis of prospective cohort studies. 
# The Lancet. 2011 Oct 14;378(9799):1297–305. 
# Appendix webfigure 8
chdsmokepaf[cigst1 == "3" & agegroup %!in% c("<1   ", "01-04", "05-09", "10-14", "15-19", "20-24", "25-29") & sex == "1" , rr := 1.25]
chdsmokepaf[cigst1 == "3" & agegroup %!in% c("<1   ", "01-04", "05-09", "10-14", "15-19", "20-24", "25-29") & sex == "2" , rr := 1.20]

# Calculate PAF for CHD
chdsmokepaf[, tobaccopaf := sum(Pe*(rr-1), na.rm = T)/(sum(Pe*(rr-1), na.rm = T) + 1), by = list(agegroup, sex)]
chdsmokepaf[, group := paste(agegroup, sex, sep="")]
setkey(chdsmokepaf, group)
chdsmokepaf <- unique(chdsmokepaf, by="group")
chdsmokepaf[, `:=` (cigst1 = NULL, Pe = NULL, rr = NULL, group = NULL)]
setkey(chdsmokepaf, sex, agegroup)
write.csv(chdsmokepaf, file = "./Models/IMPACTncd/CVD Statistics/chdsmokepaf.csv", row.names = F)

# draw example of smoothered PAF
chdsmokepaf[tobaccopaf > 0, smoothed := predict(loess(tobaccopaf~c(1:.N))), by = "sex"] # smooth PAF
chdsmokepaf[, plot(tobaccopaf, col=c("blue", "red")[sex], main = "PAF of tobacco on CHD")]
chdsmokepaf[, lines(smoothed,  lwd=2)]

# Stroke
chdsmokepaf <-as.data.table(svytable(~agegroup + sex + cigst1, HSE.srv, exclude=NULL, na.action=na.pass))
chdsmokepaf[, sum := sum(N), by=list(agegroup, sex)] # count number of people in each agegroup by sex (denominator for prevalence)
chdsmokepaf[, Pe  := N/sum] # Calculate prevalence by agegroup and sex
chdsmokepaf[, `:=` (N = NULL, sum = NULL)]

# Calculate PAF for stroke
# RR for tobacco from Ezzati M, Henley SJ, Thun MJ, Lopez AD. 
# Role of Smoking in Global and Regional Cardiovascular Mortality. 
# Circulation. 2005 Jul 26;112(4):489–97.
# Table 1 Model B
chdsmokepaf[, rr := 1]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("30-34", "35-39","40-44") & sex == "1", rr := 3.12] # arbitrary
chdsmokepaf[cigst1 == "4" & agegroup %in% c("45-49", "50-54","55-59") & sex == "1", rr := 3.12]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("60-64", "65-69") & sex == "1", rr := 1.87]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("70-74", "75-79") & sex == "1", rr := 1.39]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("80-84", "85+") & sex == "1", rr := 1.05]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("30-34", "35-39","40-44") & sex == "2", rr := 4.61]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("45-49", "50-54","55-59") & sex == "2", rr := 4.61]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("60-64", "65-69") & sex == "2", rr := 2.81]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("70-74", "75-79") & sex == "2", rr := 1.95]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("80-84", "85+") & sex == "2", rr := 1]

# ex-smokers
# Wolf PA, D’Agostino RB, Kannel WB, Bonita R, Belanger AJ. 
# Cigarette smoking as a risk factor for stroke: The framingham study. 
# JAMA 1988;259:1025–9. 
# Stroke risk decreased significantly by two years and was at the level of nonsmokers
# by five years after cessation of cigarette smoking.
chdsmokepaf[, tobaccopaf := sum(Pe*(rr-1), na.rm = T)/(sum(Pe*(rr-1), na.rm = T) + 1), by = list(agegroup, sex)]
chdsmokepaf[, group := paste(agegroup, sex, sep="")]
setkey(chdsmokepaf, group)
chdsmokepaf <- unique(chdsmokepaf, by="group")
chdsmokepaf[, `:=` (cigst1 = NULL, Pe = NULL, rr = NULL, group = NULL)]
setkey(chdsmokepaf, sex, agegroup)
write.csv(chdsmokepaf, file = "./Models/IMPACTncd/CVD Statistics/strokesmokepaf.csv", row.names = F)

# ************************************************************** ETS *****************************************************************

# Count <-data.table(data.frame(svytable(~agegroup+sex, HSE.srv, exclude=NULL, na.action=na.pass)))
# Count[, sum:=sum(Freq), by=agegroup]

# Calculate prevelence of Environmental Tobacco Smoking (expsm) (for ages 13 and above. Includes current smokers)
HSE[expsm <0, expsm := NA]
HSE[expsm >0, expsm := 1]
HSE[cigsta3==1, expsm := NA] # Set current smokers as NA
HSE.srv <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=HSE)

# tbl <- svytable(~agegroup + sex + expsm, HSE.srv, exclude=NULL, na.action=na.pass)
ETS <-data.table(data.frame(svytable(~agegroup + sex + expsm, HSE.srv, exclude=NULL, na.action=na.pass)))
ETS[, sum:=sum(Freq), by=list(agegroup, sex)] # count number of people in each agegroup by sex (denominator for prevalence)
ETS[, Pe := Freq/sum]
ETS[, `:=` (Freq = NULL, sum = NULL)]
write.csv(ETS[expsm == "1",], file = "./Models/IMPACTncd/Exposure/ETSpreval2006.csv", row.names = F)

# Calculate PAF of ETS to lung ca
# RR from IARC Working Group on the Evaluation of Carcinogenic Risks to Humans, World Health Organization, International Agency for Research on Cancer.
# Tobacco smoke and involuntary smoking. Lyon, France; Geneva: IARC Press.; Distributed by IARC Press and the World Health Organization Distribution
# and Sales; 2004. Table 2.10 p 1270
ETS = copy(ETS[expsm == "1",])
ETS[, rr := c(1, 1, 1, 1, 1, rep(1.37, 14),
              1, 1, 1, 1, 1, rep(1.24, 14))]
ETS[is.na(Pe),  `:=` (Pe = 0, se.Pe = 0)]
ETS[, etspaf := Pe*(rr-1)/(1+Pe*(rr-1))]
temp = copy(ETS)
temp[, `:=` (Pe = NULL, rr = NULL, expsm = NULL)]
write.csv(temp, file = "./Models/IMPACTncd/Cancer Statistics/c34etspaf.csv", row.names = F)

# draw example of smoothered PAF
ETS[etspaf > 0, smoothed := predict(loess(etspaf~c(1:.N))), by="sex"]# smooth PAF
ETS[, plot(etspaf, col=c("blue", "red")[sex], main = "PAF of ETS on lung cancer")]
ETS[, lines(smoothed,  lwd=2)]

# Calculate PAF of ETS for CHD
# RR from He J, Vupputuri S, Allen K, et al. 
# Passive Smoking and the Risk of Coronary Heart Disease — A Meta-Analysis of Epidemiologic Studies. 
# New England Journal of Medicine 1999;340:920–6. doi:10.1056/NEJM199903253401204
# Fully adjusted (10 studies)
ETS[, rr := c(rep(1, 7), rep(1.26, 12),
              rep(1, 7), rep(1.26, 12))]
ETS[, etspaf := Pe*(rr-1)/(1+Pe*(rr-1))]
ETS[, smoothed := NULL]
temp = copy(ETS)
temp[, `:=` (Pe = NULL, rr = NULL, expsm = NULL, se.Pe = NULL)]
write.csv(temp, file = "./Models/IMPACTncd/CVD Statistics/chdetspaf.csv", row.names = F)

# draw example of smoothered PAF
ETS[etspaf > 0, smoothed := predict(loess(etspaf~c(1:.N))), by="sex"]# smooth PAF
ETS[, plot(etspaf, col=c("blue", "red")[sex], main = "PAF of ETS on CHD")]
ETS[, lines(smoothed,  lwd=2)]

# Calculate PAF of ETS for stroke
# RR from Oono IP, Mackay DF, Pell JP. Meta-analysis of the association between secondhand smoke exposure and stroke. 
# J Public Health 2011;33:496–502. doi:10.1093/pubmed/fdr025

ETS[, rr := c(rep(1, 7), rep(1.25, 12),
              rep(1, 7), rep(1.25, 12))]
ETS[, etspaf := Pe*(rr-1)/(1+Pe*(rr-1))]
temp = copy(ETS)
temp[, `:=` (Pe = NULL, rr = NULL, expsm = NULL, se.Pe = NULL)]
ETS[, smoothed := NULL]
write.csv(temp, file = "./Models/IMPACTncd/CVD Statistics/strokeetspaf.csv", row.names = F)

# draw example of smoothered PAF
ETS[etspaf > 0, smoothed := predict(loess(etspaf~c(1:.N))), by="sex"]# smooth PAF
ETS[, plot(etspaf, col=c("blue", "red")[sex], main = "PAF of ETS on stroke")]
ETS[, lines(smoothed,  lwd=2)]

# ************************************************************** SBP *****************************************************************
# Calculate PAF of SBP (omsysval) for CHD and stroke
HSE[, omsysval.usual := round(omsysval,1)] 

# Calculate prevalence of SBP by agegroup and sex
HSE.srv <- svydesign(id=~psu, strata=~cluster, weights=~wt.nurse, nest=F, data=HSE) # change weighting
SBP <-data.table(data.frame(svytable(~agegroup + sex + omsysval.usual, HSE.srv, exclude=NULL, na.action=na.pass)))
SBP[, omsysval.usual := as.numeric(as.character(omsysval.usual))]
SBP[, sum := sum(Freq), by=list(agegroup, sex)] # count number of people in each agegroup by sex (denominator for prevalence)
SBP[, Pe  := Freq/sum] # Calculate prevalence by agegroup and sex
SBP[, `:=` (Freq = NULL, sum = NULL)]
write.csv(SBP[Pe != 0,], file = "./Models/IMPACTncd/Exposure/SBPpreval2006.csv", row.names = F) # create csv with SBP prevalence

# Calculate RR for CHD. Optimal SBP level at 115mmHg and RR(HR) of dying from CHD was taken from 
# "Age-specific relevance of
# usual blood pressure to vascular mortality: 
# a meta-analysis of individual data for one million adults in 61 prospective studies. 
# The Lancet. 2002 Dec 14;360(9349):1903–1913" 
# Figure 5
SBP[, rr := 1]
SBP[sex == 1 & agegroup  %in% c("30-34", "35-39", "40-44", "45-49"),  # assign a rr based on SBP/age/sex
    rr := 0.50^((115 - omsysval.usual)/20)]
SBP[sex == 2 & agegroup  %in% c("30-34", "35-39", "40-44", "45-49"), 
    rr := 0.40^((115 - omsysval.usual)/20)]
SBP[sex == 1 & agegroup  %in% c("50-54", "55-59"), rr := 0.50^((115 - omsysval.usual)/20)] 
SBP[sex == 2 & agegroup  %in% c("50-54", "55-59"), rr := 0.49^((115 - omsysval.usual)/20)] 
SBP[sex == 1 & agegroup  %in% c("60-64", "65-69"), rr := 0.55^((115 - omsysval.usual)/20)] 
SBP[sex == 2 & agegroup  %in% c("60-64", "65-69"), rr := 0.50^((115 - omsysval.usual)/20)] 
SBP[sex == 1 & agegroup  %in% c("70-74", "75-79"), rr := 0.62^((115 - omsysval.usual)/20)] 
SBP[sex == 2 & agegroup  %in% c("70-74", "75-79"), rr := 0.55^((115 - omsysval.usual)/20)] 
SBP[sex == 1 & agegroup  %in% c("80-84", "85+"  ), rr := 0.69^((115 - omsysval.usual)/20)] 
SBP[sex == 2 & agegroup  %in% c("80-84", "85+"  ), rr := 0.64^((115 - omsysval.usual)/20)] 
SBP[rr < 1, rr := 1] # Set rr<1, as 1

# Calculate PAF by agegroup/sex for CHD
SBP[, sbppaf := sum(Pe*(rr-1), na.rm = T)/(sum(Pe*(rr-1), na.rm = T) + 1), by = list(agegroup, sex)]
SBP[, group := paste(agegroup, sex, sep="")]
setkey(SBP, group)
SBP = unique(SBP, by="group")
SBP[, `:=` (omsysval.usual = NULL, Pe = NULL, rr = NULL, group = NULL)]
setkey(SBP, sex, agegroup)
write.csv(SBP, file = "./Models/IMPACTncd/CVD Statistics/chdsbppaf.csv", row.names = F)

# draw example of smoothered PAF
SBP[sbppaf > 0, smoothed := predict(loess(sbppaf~c(1:.N))), by="sex"]# smooth PAF
SBP[, plot(sbppaf, col=c("blue", "red")[sex], main = "PAF of SBP on CHD")]
SBP[, lines(smoothed,  lwd=2)]

# Calculate PAF of SBP for stroke (do the same as above)
SBP <-data.table(data.frame(svytable(~agegroup + sex + omsysval.usual, HSE.srv, exclude=NULL, na.action=na.pass)))
SBP[, omsysval.usual := as.numeric(as.character(omsysval.usual))]
SBP[, sum := sum(Freq), by=list(agegroup, sex)] # count number of people in each agegroup by sex (denominator for prevalence)
SBP[, Pe  := Freq/sum] # Calculate prevalence by agegroup and sex
SBP[, `:=` (Freq = NULL, sum = NULL)]

# Calculate RR for stroke. Optimal SBP level at 115mmHg and RR(HR) of dying from stroke was taken from "Age-specific relevance of usual blood pressure to 
# vascular mortality: a meta-analysis of individual data for one million adults in 61 prospective studies. 
# The Lancet. 2002 Dec 14;360(9349):1903–1913" 
# Figure 3
SBP[, rr := 1]
SBP[sex == 1 & agegroup  %in% c("30-34", "35-39", "40-44", "45-49"),  # assign a rr based on SBP/age/sex
    rr := 0.33^((115 - omsysval.usual)/20)]
SBP[sex == 2 & agegroup  %in% c("30-34", "35-39", "40-44", "45-49"), 
    rr := 0.41^((115 - omsysval.usual)/20)]
SBP[sex == 1 & agegroup  %in% c("50-54", "55-59"), rr := 0.34^((115 - omsysval.usual)/20)] 
SBP[sex == 2 & agegroup  %in% c("50-54", "55-59"), rr := 0.45^((115 - omsysval.usual)/20)] 
SBP[sex == 1 & agegroup  %in% c("60-64", "65-69"), rr := 0.41^((115 - omsysval.usual)/20)] 
SBP[sex == 2 & agegroup  %in% c("60-64", "65-69"), rr := 0.47^((115 - omsysval.usual)/20)] 
SBP[sex == 1 & agegroup  %in% c("70-74", "75-79"), rr := 0.48^((115 - omsysval.usual)/20)] 
SBP[sex == 2 & agegroup  %in% c("70-74", "75-79"), rr := 0.53^((115 - omsysval.usual)/20)] 
SBP[sex == 1 & agegroup  %in% c("80-84", "85+"  ), rr := 0.68^((115 - omsysval.usual)/20)] 
SBP[sex == 2 & agegroup  %in% c("80-84", "85+"  ), rr := 0.65^((115 - omsysval.usual)/20)] 
SBP[rr < 1, rr := 1] # Set rr<1, as 1

# Calculate PAF by agegroup/sex for stroke
SBP[, sbppaf := sum(Pe*(rr-1), na.rm = T)/(sum(Pe*(rr-1), na.rm = T) + 1), by = list(agegroup, sex)]
SBP[, group := paste(agegroup, sex, sep="")]
setkey(SBP, group)
SBP = unique(SBP, by="group")
SBP[, `:=` (omsysval.usual = NULL, Pe = NULL, rr = NULL, group = NULL)]
setkey(SBP, sex, agegroup)
write.csv(SBP, file = "./Models/IMPACTncd/CVD Statistics/strokesbppaf.csv", row.names = F)

# draw example of smoothered PAF
SBP[sbppaf > 0, smoothed := predict(loess(sbppaf~c(1:.N))), by="sex"]# smooth PAF
SBP[, plot(sbppaf, col=c("blue", "red")[sex], main = "PAF of SBP on stroke")]
SBP[, lines(smoothed,  lwd=2)]

# ************************************************************** CHOL *****************************************************************
# Calculate PAF of chol on CHD
HSE[, cholval1.usual := round(cholval1,2)]
HSE.srv <- svydesign(id=~psu, strata=~cluster, weights=~wt.blood, nest=F, data=HSE)

CHOL <-data.table(data.frame(svytable(~agegroup + sex + cholval1.usual, HSE.srv, exclude=NULL, na.action=na.pass)))
CHOL[, cholval1.usual := as.numeric(as.character(cholval1.usual))]
CHOL[, sum := sum(Freq), by=list(agegroup, sex)] # count number of people in each agegroup by sex (denominator for prevalence)
CHOL[, Pe  := Freq/sum] # Calculate prevalence by agegroup and sex
CHOL[, `:=` (Freq = NULL, sum = NULL)]
write.csv(CHOL[Pe != 0,], file = "./Models/IMPACTncd/Exposure/CHOLpreval2006.csv", row.names = F) # create csv with SBP prevalence

# Calculate RR for CHD. Optimal chol level at 3.8 mmol/L and RR(HR) of dying from CHD was taken from "Blood cholesterol and 
# vascular mortality by age, sex, and blood pressure: a meta-analysis of individual data from 61 prospective studies 
# with 55.000 vascular deaths. The Lancet. 2007 Dec 7;370(9602):1829–39. 
# Appendix Webtable 6  fully adjusted
CHOL[, rr := 1]
CHOL[agegroup  %in% c("30-34", "35-39", "40-44", "45-49"), 
                                         rr := 0.49^(3.8 - cholval1.usual)]
CHOL[agegroup  %in% c("50-54", "55-59"), rr := 0.62^(3.8 - cholval1.usual)] 
CHOL[agegroup  %in% c("60-64", "65-69"), rr := 0.74^(3.8 - cholval1.usual)] 
CHOL[agegroup  %in% c("70-74", "75-79"), rr := 0.84^(3.8 - cholval1.usual)] 
CHOL[agegroup  %in% c("80-84", "85+"  ), rr := 0.87^(3.8 - cholval1.usual)] 
CHOL[rr < 1, rr := 1] # Set rr<1, as 1

# Calculate PAF by agegroup/sex for CHD
CHOL[, cholpaf := sum(Pe*(rr-1), na.rm = T)/(sum(Pe*(rr-1), na.rm = T) + 1), by = list(agegroup, sex)]
CHOL[, group := paste0(agegroup, sex)]
setkey(CHOL, group)
CHOL <- unique(CHOL, by="group")
CHOL[, `:=` (cholval1.usual = NULL, Pe = NULL, rr = NULL, group = NULL)]
setkey(CHOL, sex, agegroup)
write.csv(CHOL, file = "./Models/IMPACTncd/CVD Statistics/chdcholpaf.csv", row.names = F)

# draw example of smoothered PAF
CHOL[cholpaf > 0, smoothed := predict(loess(cholpaf~c(1:.N))), by="sex"] # smooth PAF
CHOL[, plot(cholpaf, col=c("blue", "red")[sex], main = "PAF of chol on CHD")]
CHOL[, lines(smoothed,  lwd=2)]

# Calculate PAF by agegroup/sex for stroke
CHOL <-data.table(data.frame(svytable(~agegroup + sex + cholval1.usual, HSE.srv, exclude=NULL, na.action=na.pass)))
CHOL[, cholval1.usual := as.numeric(as.character(cholval1.usual))]
CHOL[, sum := sum(Freq), by=list(agegroup, sex)] # count number of people in each agegroup by sex (denominator for prevalence)
CHOL[, Pe  := Freq/sum] # Calculate prevalence by agegroup and sex
CHOL[, `:=` (Freq = NULL, sum = NULL)]

# Calculate RR for stroke. Optimal chol level at 3.8 mmol/L and RR(HR) of dying from CHD was taken from "Blood cholesterol and 
# vascular mortality by age, sex, and blood pressure: a meta-analysis of individual data from 61 prospective studies 
# with 55.000 vascular deaths. The Lancet. 2007 Dec 7;370(9602):1829–39. 
# Figure 4 (for total stroke. I used only significant HR's) 
CHOL[, rr := 1]
CHOL[sex == 1 & agegroup  %in% c("30-34", "35-39", "40-44", "45-49"), 
     rr := 0.9^(3.8 - cholval1.usual)]
CHOL[sex == 2 & agegroup  %in% c("30-34", "35-39", "40-44", "45-49"), 
     rr := 0.9^(3.8 - cholval1.usual)]
CHOL[sex == 1 & agegroup  %in% c("50-54", "55-59"), rr := 0.9^(3.8 - cholval1.usual)] 
CHOL[sex == 2 & agegroup  %in% c("50-54", "55-59"), rr := 0.9^(3.8 - cholval1.usual)] 
CHOL[rr < 1, rr := 1] # Set rr<1, as 1

# Calculate PAF by agegroup/sex for stroke
CHOL[, cholpaf := sum(Pe*(rr-1), na.rm = T)/(sum(Pe*(rr-1), na.rm = T) + 1), by = list(agegroup, sex)]
CHOL[, group := paste(agegroup, sex, sep="")]
setkey(CHOL, group)
CHOL = unique(CHOL, by="group")
CHOL[, `:=` (cholval1.usual = NULL, Pe = NULL, rr = NULL, group = NULL)]
setkey(CHOL, sex, agegroup)
write.csv(CHOL, file = "./Models/IMPACTncd/CVD Statistics/strokecholpaf.csv", row.names = F)

# draw example of smoothered PAF
CHOL[cholpaf > 0, smoothed := predict(loess(cholpaf~c(1:.N))), by="sex"] # smooth PAF
CHOL[, plot(cholpaf, col=c("blue", "red")[sex], main = "PAF of chol on stroke")]
CHOL[, lines(smoothed,  lwd=2)]

# ************************************************************** BMI *****************************************************************
# Calculate PAF of BMI on CHD
HSE[, bmival := round(bmival, 2)] # round BMI in 2 digits
HSE.srv <- svydesign(id=~psu, strata=~cluster, weights=~wt.nurse, nest=F, data=HSE)

BMI <-data.table(data.frame(svytable(~agegroup + sex + bmival, HSE.srv, exclude=NULL, na.action=na.pass)))
BMI[, bmival := as.numeric(as.character(bmival))]
BMI[, sum := sum(Freq), by=list(agegroup, sex)] # count number of people in each agegroup by sex (denominator for prevalence)
BMI[, Pe  := Freq/sum] # Calculate prevalence by agegroup and sex
BMI[, `:=` (Freq = NULL, sum = NULL)]
write.csv(BMI[Pe != 0,], file = "./Models/IMPACTncd/Exposure/BMIpreval2006.csv", row.names = F) # create csv with SBP prevalence

# Calculate RR for CHD. Optimal BMI level at 20 Kg/m^2 and RR(HR) of CHD/stroke (both fatal and non fatal) 
# was taken from "The Emerging Risk Factors Collaboration. 
# Separate and combined associations of body-mass index and abdominal adiposity with cardiovascular disease:
# collaborative analysis of 58 prospective studies.
# The Lancet 2011;377:1085–95. doi:10.1016/S0140-6736(11)60105-0
# Table 1 (Adjusted for age, sex, smoking status, systolic blood pressure, history of diabetes, and total and HDL cholesterol)
BMI[, rr := 1.11^((bmival - 20) / 4.56)]
BMI[rr < 1, rr := 1] # Set rr<1, as 1

# Calculate PAF for CHD
BMI[, bmipaf := sum(Pe*(rr-1), na.rm = T)/(sum(Pe*(rr-1), na.rm = T) + 1), by = list(agegroup, sex)]
BMI[, group := paste(agegroup, sex, sep="")]
setkey(BMI, group)
BMI = unique(BMI, by="group")
BMI[, `:=` (bmival = NULL, Pe = NULL, rr = NULL, group = NULL)]
setkey(BMI, sex, agegroup)
write.csv(BMI, file = "./Models/IMPACTncd/CVD Statistics/chdbmipaf.csv", row.names = F)

# draw example of smoothered PAF
BMI[bmipaf > 0, smoothed := predict(loess(bmipaf~c(1:.N))), by = "sex"] # smooth PAF
BMI[, plot(bmipaf, col=c("blue", "red")[sex], main = "PAF of BMI on CHD")]
BMI[, lines(smoothed,  lwd=2)]

# Calculate PAF for ischaemic stroke # NEED TO adjust somehow for total stroke
BMI <-data.table(data.frame(svytable(~agegroup + sex +  bmival, HSE.srv, exclude=NULL, na.action=na.pass)))
BMI[, bmival := as.numeric(as.character(bmival))]
BMI[, sum := sum(Freq), by=list(agegroup, sex)] # count number of people in each agegroup by sex (denominator for prevalence)
BMI[, Pe  := Freq/sum] # Calculate prevalence by agegroup and sex
BMI[, `:=` (Freq = NULL, sum = NULL)]
BMI[,rr := 1.06^((bmival - 20) / 4.56)]
BMI[rr < 1, rr := 1] # Set rr<1, as 1

# Calculate PAF by agegroup/sex for stroke
BMI[, bmipaf := sum(Pe*(rr-1), na.rm = T)/(sum(Pe*(rr-1), na.rm = T) + 1), by = list(agegroup, sex)]
BMI[, group := paste(agegroup, sex, sep="")]
setkey(BMI, group)
BMI = unique(BMI, by="group")
BMI[, `:=` (bmival = NULL, Pe = NULL, rr = NULL, group = NULL)]
setkey(BMI, sex, agegroup)
write.csv(BMI, file = "./Models/IMPACTncd/CVD Statistics/strokebmipaf.csv", row.names = F)

# draw example of smoothered PAF
BMI[bmipaf > 0, smoothed := predict(loess(bmipaf~c(1:.N))), by="sex"] # smooth PAF
BMI[, plot(bmipaf, col=c("blue", "red")[sex], main = "PAF of BMI on stroke")]
BMI[, lines(smoothed,  lwd=2)]

# ************************************************************** DIABETES *****************************************************************
# svyciprop(~I(diabete2 == 1), HSE.srv, method="li") # CI for proportions examble
HSE[is.na(diabete2) == T, diabete2 := 99] # Convert NAs to 99 = NA
HSE[, diabete2 := factor(diabete2)]
HSE.srv <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=HSE)
DIAB1 <- data.table(svyby(~diabete2, ~agegroup+sex, HSE.srv, svymean))
DIAB1[, `:=` (diabete22 = NULL, diabete299 = NULL, se.diabete22 = NULL, se.diabete299 = NULL)]
setnames(DIAB1, c("diabete21", "se.diabete21"), c("Pe", "se.Pe"))
write.csv(DIAB1, file = "./Models/IMPACTncd/Exposure/DIABdiagpreval2006.csv", row.names = F)

# Above is prevalence for diagnosed diab. 
# Next I'll calculate the pravalence of undiagnosed + diagnosed
HSE[diabete2 != "1" & glyhbval > 6.5, diabete2 := "1"]
HSE.srv <- svydesign(id=~psu, strata=~cluster, weights=~wt.blood, nest=F, data=HSE) # If I use wt.blood I get different results
DIAB2 <- data.table(svyby(~diabete2, ~agegroup + sex, HSE.srv, svymean, drop.empty.groups = F))
DIAB2[, `:=` (diabete22 = NULL, diabete299 = NULL, se.diabete22 = NULL, se.diabete299 = NULL)]
setnames(DIAB2, c("diabete21", "se.diabete21"), c("Pe", "se.Pe"))
write.csv(DIAB2, file = "./Models/IMPACTncd/Exposure/DIABundiagpreval2006.csv", row.names = F)

# Calculate PAF of diabetes for CHD
# RR from The Emerging Risk Factors Collaboration. Diabetes mellitus, fasting blood glucose concentration, and risk of vascular disease: a collaborative 
# meta-analysis of 102 prospective studies. The Lancet 2010;375:2215–22. doi:10.1016/S0140-6736(10)60484-9
# figure 2 (HRs were adjusted for age, smoking status, body-mass index, and  systolic blood pressure)
DIAB2[, rr := c(rep(1,7), rep(2.51,6), rep(2.01,2), rep(1.78,4),
                rep(1,7), rep(2.51,6), rep(2.01,2), rep(1.78,4))]
DIAB2[is.na(Pe),  `:=` (Pe = 0, se.Pe = 0)]
DIAB2[, diabpaf := Pe*(rr-1)/(1+Pe*(rr-1))]
temp = copy(DIAB2)
temp[, `:=` (Pe = NULL, rr = NULL, se.Pe = NULL)]
write.csv(temp, file = "./Models/IMPACTncd/CVD Statistics/chddiabpaf.csv", row.names = F)

# draw example of smoothered PAF
DIAB2[diabpaf > 0, smoothed := predict(loess(diabpaf~c(1:.N))), by="sex"]# smooth PAF
DIAB2[, plot(diabpaf, col=c("blue", "red")[sex], main = "PAF of diabetes on CHD")]
DIAB2[, lines(smoothed,  lwd=2)]

# Calculate PAF of diabetes for stroke
# RR as above for ISCHAEMIC STROKE
DIAB2[, rr := c(rep(1,7), rep(3.74,6), rep(2.06,2), rep(1.80,4),
                rep(1,7), rep(3.74,6), rep(2.06,2), rep(1.80,4))]
DIAB2[, diabpaf := Pe*(rr-1)/(1+Pe*(rr-1))]
temp = copy(DIAB2)
temp[, `:=` (Pe = NULL, rr = NULL, se.Pe = NULL)]
write.csv(temp, file = "./Models/IMPACTncd/CVD Statistics/strokediabpaf.csv", row.names = F)

# draw example of smoothered PAF
DIAB2[diabpaf > 0, smoothed := predict(loess(diabpaf~c(1:.N))), by="sex"]# smooth PAF
DIAB2[, plot(diabpaf, col=c("blue", "red")[sex], main = "PAF of diabetes on Isch Stroke")]
DIAB2[, lines(smoothed,  lwd=2)]

# ************************************************************** F & V *****************************************************************
HSE[, porfv := round(porfv, 0)]
HSE[porfv >10, porfv := 10] # Set max effect of 10 portions a day
HSE.srv <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=HSE)
FV <-data.table(data.frame(svytable(~agegroup + sex + porftvg, HSE.srv, exclude=NULL, na.action=na.pass)))
FV[, porftvg := as.numeric(as.character(porftvg))]
FV[, sum := sum(Freq), by=list(agegroup, sex)] # count number of people in each agegroup by sex (denominator for prevalence)
FV[, Pe  := Freq/sum] # Calculate prevalence by agegroup and sex
FV[, `:=` (Freq = NULL, sum = NULL)]
write.csv(FV[Pe != 0,], file = "./Models/IMPACTncd/Exposure/FVpreval2006.csv", row.names = F) # create csv with SBP prevalence

# Calculate RR for CHD. From Dauchet L, Amouyel P, Hercberg S, Dallongeville J. 
# Fruit and Vegetable Consumption and Risk of Coronary Heart Disease: 
# A Meta-Analysis of Cohort Studies. J Nutr. 2006 Oct 1;136(10):2588–93. 
FV[, rr := 0.96 ^ (porftvg)]

# Calculate PAF for CHD
FV[, fvpaf := sum(Pe*(rr-1), na.rm = T)/(sum(Pe*(rr-1), na.rm = T) + 1), by = list(agegroup, sex)]
FV[, group := paste(agegroup, sex, sep="")]
setkey(FV, group)
FV = unique(FV, by="group")
FV[, `:=` (porftvg = NULL, Pe = NULL, rr = NULL, group = NULL)]
setkey(FV, sex, agegroup)
write.csv(FV, file = "./Models/IMPACTncd/CVD Statistics/chdfvpaf.csv", row.names = F)

# draw example of smoothered PAF
FV[fvpaf < 0, smoothed := predict(loess(fvpaf~c(1:.N))), by = "sex"] # smooth PAF
FV[, plot(fvpaf, col=c("blue", "red")[sex], main = "PAF of F&V on CHD")]
FV[, lines(smoothed,  lwd=2)]

# Calculate PAF for stroke
FV <-data.table(data.frame(svytable(~agegroup + sex + porftvg, HSE.srv, exclude = NULL, na.action = na.pass)))
FV[, porftvg := as.numeric(as.character(porftvg))]
FV[, sum := sum(Freq), by=list(agegroup, sex)] # count number of people in each agegroup by sex (denominator for prevalence)
FV[, Pe  := Freq/sum] # Calculate prevalence by agegroup and sex
FV[, `:=` (Freq = NULL, sum = NULL)]

# Calculate RR for stroke. From Dauchet L, Amouyel P, Dallongeville J. Fruit and vegetable consumption and risk of stroke A meta-analysis of cohort studies. Neurology. 2005 Oct 25;65(8):1193–7. 
# To avoid negative PAF an optimal level of F&V has to be set arbitrarily. I set it to 10 
FV[, rr := 0.95 ^ porftvg]

# Calculate PAF for CHD
FV[, fvpaf := sum(Pe*(rr-1), na.rm = T)/(sum(Pe*(rr-1), na.rm = T) + 1), by = list(agegroup, sex)]
FV[, group := paste(agegroup, sex, sep="")]
setkey(FV, group)
FV = unique(FV, by="group")
FV[, `:=` (porftvg = NULL, Pe = NULL, rr = NULL, group = NULL)]
setkey(FV, sex, agegroup)
write.csv(FV, file = "./Models/IMPACTncd/CVD Statistics/strokefvpaf.csv", row.names = F)

# draw example of smoothered PAF
FV[fvpaf < 0, smoothed := predict(loess(fvpaf~c(1:.N))), by = "sex"] # smooth PAF
FV[, plot(fvpaf, col=c("blue", "red")[sex], main = "PAF of F&V on stroke")]
FV[, lines(smoothed,  lwd=2)]



# HSE[is.na(porfv) == T, porfv := 99] # Convert NAs to 99 = NA
# HSE[, porfv := ordered(porfv)]
# HSE.srv <- svydesign(id= ~psu, strata = ~cluster, weights = ~wt.int, nest = F, data = HSE)
# FV <- data.table(svyby(~porfv, ~agegroup + sex, HSE.srv, svymean))
# FV2 <- melt(FV, 
#             id.vars = c("sex", "agegroup"), 
#             measure.vars = c(3:100), 
#             value.name = "Pe")






#Garbage collection
rm(temp)
