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

# Preamble ----------------------------------------------------------------
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
               "compiler",
               "truncnorm",
               "rriskDistributions",
               "mc2d"))

# define function for stochastic RR 
stochRR <- cmpfun(function(n = .N, m, ci, multip=1) { # lognormal
  if (m < 1) {
    a = -Inf
    b = 0
  } else {
    a = 0
    b = Inf
  }
  ifelse(m == ci, rr <- rep(log(m), n), rr <- rtruncnorm(n = n, a = a, b = b, mean = log(m), sd = abs(log(m) - log(ci))/1.96) * multip)
  return(exp(rr))  
}
)

# define function for stochastic RR
stochRRabov1 <- cmpfun(function(n = .N, m, ci) { # lognormal
  rr <- exp(rtruncnorm(n, 0, Inf, log(m), abs(log(m) - log(ci))/1.96))
  return(rr)  
}
)

stochRRbelow1 <- cmpfun(function(n = .N, m, ci) { # lognormal
  rr <- exp(rtruncnorm(n, -Inf, 0, log(m), abs(log(m) - log(ci))/1.96))
  return(rr)  
}
)

stochRRnorm <- cmpfun(function(n = .N, m, ci, multip=1) { # normal distr DIFFERENT THAN THE MAIN MODEL stochRRnorm
  if (m < 1) {
    a = 0
    b = 1
  } else {
    a = 1
    b = Inf
  }
  ifelse(m == ci, rr <- rep(m, n), rr <- rtruncnorm(n = n, a = a, b = b, mean = m, sd = abs(m - ci)/1.96) * multip)
  return(rr)  
}
)

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

# PAF estimation ----------------------------------------------------------

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
HSE[is.na(cigst1), cigst1 := 99]
HSE[, cigst1 := factor(cigst1)]
HSE.srv <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=HSE)

chdsmokepaf <-as.data.table(svyby(~cigst1, ~agegroup + sex , HSE.srv,  svymean, na.rm = T, 
                                  vartype = c("se"),
                                  drop.empty.groups = F))
setnames(chdsmokepaf, paste0("cigst1", c(1:4, 99)), paste0(c(1:4, NA)))
setnames(chdsmokepaf, paste0("se.cigst1", c(1:4, 99)), paste0(c(1:4, NA)))
setkey(chdsmokepaf, agegroup, sex)

chdsmokepaf = copy(merge(melt(chdsmokepaf[, 1:6, with =F], c(1,2), variable.name = "cigst1", value.name = "Pe"),
                         melt(chdsmokepaf[, c(1:2, 8:11), with =F], c(1,2), variable.name = "cigst1", value.name = "se"), 
                         by = c("agegroup", "sex", "cigst1")))


write.csv(chdsmokepaf[Pe > 0,], file = "./Models/IMPACTncd/Exposure/TOBpreval2006.csv", row.names = F) # create csv with SBP prevalence

# RR for tobacco from Ezzati M, Henley SJ, Thun MJ, Lopez AD. 
# Role of Smoking in Global and Regional Cardiovascular Mortality. 
# Circulation. 2005 Jul 26;112(4):489–97.
# Table 1 Model B
chdsmokepaf[, `:=` (rr = 1, ci  = 1)]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("30-34", "35-39","40-44") & sex == "1",  `:=` (rr = 5.51, ci  = 12.25)]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("45-49", "50-54","55-59") & sex == "1", `:=` (rr = 3.04, ci  = 3.48)]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("60-64", "65-69") & sex == "1", `:=` (rr = 1.88, ci  = 2.08)]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("70-74", "75-79") & sex == "1", `:=` (rr = 1.44, ci  = 1.63)]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("30-34", "35-39","40-44") & sex == "2", `:=` (rr = 2.26, ci  = 6.14)]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("45-49", "50-54","55-59") & sex == "2", `:=` (rr = 3.78, ci  = 4.62)]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("60-64", "65-69") & sex == "2", `:=` (rr = 2.53, ci  = 2.87)]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("70-74", "75-79") & sex == "2", `:=` (rr = 1.68, ci  = 1.93)]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("80-84", "85+") & sex == "2", `:=` (rr = 1.38, ci  = 1.77)]

# RR for ex-smokers from Huxley RR, Woodward M. 
# Cigarette smoking as a risk factor for coronary heart disease in women 
# compared with men: a systematic review and meta-analysis of prospective cohort studies. 
# The Lancet. 2011 Oct 14;378(9799):1297–305. 
# Appendix webfigure 8
chdsmokepaf[cigst1 == "3" & agegroup %!in% c("<1   ", "01-04", "05-09", "10-14", "15-19", "20-24", "25-29") & sex == "1" , `:=` (rr = 1.25, ci  = 1.32)]
chdsmokepaf[cigst1 == "3" & agegroup %!in% c("<1   ", "01-04", "05-09", "10-14", "15-19", "20-24", "25-29") & sex == "2" , `:=` (rr = 1.20, ci  = 1.34)]

# Calculate PAF for CHD
chdsmokepaf[, tobaccopaf := mean(rtruncnorm(n = .N * 1e3, a = 0, mean = Pe, sd = se) * (stochRR(.N * 1e3, rr, ci) - 1), na.rm = T), by = .(agegroup, sex, cigst1)]

chdsmokepaf[, tobaccopaf := sum(tobaccopaf, na.rm = T) / (1 + sum(tobaccopaf, na.rm = T)), by = .(agegroup, sex)]

chdsmokepaf =copy(unique(chdsmokepaf, by=c("agegroup", "sex")))
chdsmokepaf[, `:=` (cigst1 = NULL, Pe = NULL, rr = NULL, ci = NULL, se = NULL)]
setkey(chdsmokepaf, sex, agegroup)

write.csv(chdsmokepaf, file = "./Models/IMPACTncd/CVD Statistics/chdsmokepaf.csv", row.names = F)

# draw example of smoothered PAF
chdsmokepaf[tobaccopaf > 0, smoothed := predict(loess(tobaccopaf~c(1:.N))), by = "sex"] # smooth PAF
chdsmokepaf[, plot(tobaccopaf, col=sex, main = "PAF of tobacco on CHD")]
chdsmokepaf[, lines(smoothed,  lwd=2)]

# Stroke
chdsmokepaf <-as.data.table(svyby(~cigst1, ~agegroup + sex , HSE.srv,  svymean, na.rm = T, 
                                  vartype = c("se"),
                                  drop.empty.groups = F))
setnames(chdsmokepaf, paste0("cigst1", c(1:4, 99)), paste0(c(1:4, NA)))
setnames(chdsmokepaf, paste0("se.cigst1", c(1:4, 99)), paste0(c(1:4, NA)))
setkey(chdsmokepaf, agegroup, sex)

chdsmokepaf = copy(merge(melt(chdsmokepaf[, 1:6, with =F], c(1,2), variable.name = "cigst1", value.name = "Pe"),
                         melt(chdsmokepaf[, c(1:2, 8:11), with =F], c(1,2), variable.name = "cigst1", value.name = "se"), 
                         by = c("agegroup", "sex", "cigst1")))

# Calculate PAF for stroke
# RR for tobacco from Ezzati M, Henley SJ, Thun MJ, Lopez AD. 
# Role of Smoking in Global and Regional Cardiovascular Mortality. 
# Circulation. 2005 Jul 26;112(4):489–97.
# Table 1 Model B
chdsmokepaf[, `:=` (rr = 1, ci  = 1)]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("30-34", "35-39","40-44") & sex == "1", `:=` (rr = 3.12, ci  = 4.64)] # arbitrary
chdsmokepaf[cigst1 == "4" & agegroup %in% c("45-49", "50-54","55-59") & sex == "1", `:=` (rr = 3.12, ci  = 4.64)]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("60-64", "65-69") & sex == "1", `:=` (rr = 1.87, ci  = 2.44)]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("70-74", "75-79") & sex == "1", `:=` (rr = 1.39, ci  = 1.77)]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("30-34", "35-39","40-44") & sex == "2", `:=` (rr = 4.61, ci  = 6.37)] # arbitrary
chdsmokepaf[cigst1 == "4" & agegroup %in% c("45-49", "50-54","55-59") & sex == "2", `:=` (rr = 4.61, ci  = 6.37)]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("60-64", "65-69") & sex == "2", `:=` (rr = 2.81, ci  = 3.58)]
chdsmokepaf[cigst1 == "4" & agegroup %in% c("70-74", "75-79") & sex == "2", `:=` (rr = 1.95, ci  = 2.45)]

# ex-smokers
# Wolf PA, D’Agostino RB, Kannel WB, Bonita R, Belanger AJ. 
# Cigarette smoking as a risk factor for stroke: The framingham study. 
# JAMA 1988;259:1025–9. 
# Stroke risk decreased significantly by two years and was at the level of nonsmokers
# by five years after cessation of cigarette smoking.
chdsmokepaf[, tobaccopaf := mean(rtruncnorm(n = .N * 1e3, a = 0, mean = Pe, sd = se) * (stochRR(.N * 1e3, rr, ci) - 1), na.rm = T), by = .(agegroup, sex, cigst1)]

chdsmokepaf[, tobaccopaf := sum(tobaccopaf, na.rm = T) / (1 + sum(tobaccopaf, na.rm = T)), by = .(agegroup, sex)]

chdsmokepaf =copy(unique(chdsmokepaf, by=c("agegroup", "sex")))
chdsmokepaf[, `:=` (cigst1 = NULL, Pe = NULL, rr = NULL, ci = NULL, se = NULL)]
setkey(chdsmokepaf, sex, agegroup)

write.csv(chdsmokepaf, file = "./Models/IMPACTncd/CVD Statistics/strokesmokepaf.csv", row.names = F)


# PAF for gastric cancer --------------------------------------------------

setkey(HSE, agegroup)
HSE[is.na(cigst1), cigst1 := 99]
HSE[, cigst1 := factor(cigst1)]
# cigarette-years = cigarettes smoke per day * years of smoking
HSE[, `:=` (cigyears4 = 0)]
HSE[cigst1 == "4" & startsmk < 90 & cigdyal < 90, 
    cigyears4 := cigdyal * (age -5 -startsmk)] #  startsmk == 97 means never smoked regurarly
HSE[cigst1 == "3" & numsmok < 90 & smokyrs <90,
    cigyears4 := as.numeric(numsmok * smokyrs)]
HSE[cigst1 == "2" & smokyrs <90,
    cigyears4 := as.numeric(0.5 * smokyrs)] # assume non regular smokers smok 0.5 cigaretes/day
HSE[, cigyears4 := factor(round(cigyears4, -1))]

HSE[, endsmoke2 := cut(endsmoke, 
                       breaks = quantile(endsmoke,
                                         c(0, .25, .50, .75, 1),
                                         na.rm = T), 
                       include.lowest = T, 
                       right = F, 
                       ordered_result = T)]
HSE[, endsmoke3 := 0]
HSE[cigst1 %in% c("2", "3"),
    endsmoke3 := mean(endsmoke),
    by = endsmoke2]
HSE[is.na(endsmoke), endsmoke :=0]
HSE.srv <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=HSE)

ttt <- as.data.table(svytable(~agegroup + cigyears4 + endsmoke + cigst1 + sex,
                              HSE.srv))
ttt[, Pe := N/sum(N, na.rm = T), by = .(agegroup, sex)]
ttt[, cigyears4 := as.numeric(as.character(cigyears4))]
ttt[, endsmoke := as.numeric(as.character(endsmoke))]

ttt[, sum(Pe), by = agegroup]
ttt[, sum(N), by = cigst1]

# RR for tobacco from González CA, et al. Smoking and the risk of gastric cancer in the 
# European Prospective Investigation Into Cancer and Nutrition (EPIC). Int J Cancer 2003;107:629–34. 
# "the HR were very similar (HR of the log-2 of cigarette-years 1.040 in males and 1.040 in females),
# both statistically significant". I assume CI from 1 to 1.08

ttt[, `:=` (rr = 1, ci  = 1)]
ttt[cigst1 %in% c("2", "3", "4") & Pe > 0, `:=` (rr = 1.04^log2(cigyears4), ci  = 1.08^log2(cigyears4))]
# reduce risk for ex smokers, from the same study Table IV. 
# will not use ci from table IV because I use mean (endsmoke)
ttt[cigst1 %in% c("2", "3") & endsmoke > 0,  `:=` (
  rr = rr * 0.961^log2(endsmoke), 
  ci = ci * 0.961^log2(endsmoke))]
#ttt[cigst1 %in% c("2", "3") & endsmoke>25, `:=` (rr = 1, ci = 1)] # no risk for those quit smoking 25 years ago
ttt[rr < 1, `:=` (rr = 1, ci = 1)]
ttt[cigst1=="4", summary(rr)]
ttt[cigst1=="3", summary(rr)]

# Calculate PAF for c16
ttt[rr > 1 , tobpaf := Pe * rr, 
    by = .(agegroup, cigyears4, endsmoke, cigst1, sex)]
ttt[is.na(tobpaf), tobpaf := 0]
ttt[cigyears4 == 0, tobpaf := Pe]
ttt[, tobpaf := 1 - 1 /sum(tobpaf, na.rm = T), by = .(agegroup, sex)]

ttt =copy(unique(ttt, by=c("agegroup", "sex")))
ttt[, `:=` (cigyears4 = NULL, endsmoke = NULL, cigst1 = NULL,  Pe = NULL, rr = NULL, ci = NULL, se = NULL, N = NULL)]
ttt[tobpaf < 0, tobpaf := 0]
setkey(ttt, sex, agegroup)

write.csv(ttt, 
          file = "./Models/IMPACTncd/Cancer Statistics/c16tobpaf.csv",
          row.names = F)

# draw example of smoothered PAF
ttt[tobpaf > 0, smoothed := predict(loess(tobpaf~c(1:.N))), by = "sex"] # smooth PAF
ttt[, plot(tobpaf, col=c("blue", "red")[as.numeric(sex)], main = "PAF of tob on C16")]
ttt[, lines(smoothed, lwd=2)]

# ************************************************************** ETS *****************************************************************

# Count <-data.table(data.frame(svytable(~agegroup+sex, HSE.srv, exclude=NULL, na.action=na.pass)))
# Count[, sum:=sum(Freq), by=agegroup]

# Calculate prevelence of Environmental Tobacco Smoking (expsm) (for ages 13 and above. Includes current smokers)
HSE[expsm <0, expsm := 99]
HSE[expsm >0, expsm := 1]
HSE[cigsta3==1, expsm := 0] # Set current smokers as NA
HSE[is.na(expsm), expsm := 99]
HSE[, expsm := factor(expsm)]
HSE.srv <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=HSE)

# tbl <- svytable(~agegroup + sex + expsm, HSE.srv, exclude=NULL, na.action=na.pass)
ETS <-as.data.table(svyby(~expsm, ~agegroup + sex , HSE.srv,  svymean, na.rm = T, 
                          vartype = c("se"),
                          drop.empty.groups = F))
setnames(ETS, paste0("expsm", c(0:1, 99)), paste0(c(0:1, NA)))
setnames(ETS, paste0("se.expsm", c(0:1, 99)), paste0(c(0:1, NA)))
setkey(ETS, agegroup, sex)

ETS = copy(merge(melt(ETS[, 1:4, with =F], c(1,2), variable.name = "expsm", value.name = "Pe"),
                 melt(ETS[, c(1:2, 6:7), with =F], c(1,2), variable.name = "expsm", value.name = "se"), 
                 by = c("agegroup", "sex", "expsm")))

write.csv(ETS[expsm == "1",], file = "./Models/IMPACTncd/Exposure/ETSpreval2006.csv", row.names = F)

# Calculate PAF of ETS to lung ca NEEDS STOCHASTIC UPDATE AS CHD AND STROKE
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
ETS <-as.data.table(svyby(~expsm, ~agegroup + sex , HSE.srv,  svymean, na.rm = T, 
                          vartype = c("se"),
                          drop.empty.groups = F))
setnames(ETS, paste0("expsm", c(0:1, 99)), paste0(c(0:1, NA)))
setnames(ETS, paste0("se.expsm", c(0:1, 99)), paste0(c(0:1, NA)))
setkey(ETS, agegroup, sex)
ETS = copy(merge(melt(ETS[, 1:4, with =F], c(1,2), variable.name = "expsm", value.name = "Pe"),
                 melt(ETS[, c(1:2, 6:7), with =F], c(1,2), variable.name = "expsm", value.name = "se"), 
                 by = c("agegroup", "sex", "expsm")))

ETS[, `:=` (rr = 1, ci  = 1)]
ETS[expsm == "1" & agegroup %!in% c("<1   ", "01-04", "05-09", "10-14", "15-19", "20-24", "25-29"), `:=` (rr = 1.26, ci  = 1.38)]

ETS[, etspaf := mean(rtruncnorm(n = .N * 1e3, a = 0, mean = Pe, sd = se) * (stochRR(.N * 1e3, rr, ci) - 1), na.rm = T), by = .(agegroup, sex, expsm)]

ETS[, etspaf := sum(etspaf, na.rm = T) / (1 + sum(etspaf, na.rm = T)), by = .(agegroup, sex)]

ETS =copy(unique(ETS, by=c("agegroup", "sex")))
ETS[, `:=` (expsm = NULL, Pe = NULL, rr = NULL, ci = NULL, se = NULL)]
setkey(ETS, sex, agegroup)
write.csv(ETS[etspaf > 0,], file = "./Models/IMPACTncd/CVD Statistics/chdetspaf.csv", row.names = F)

# draw example of smoothered PAF
ETS[etspaf > 0, smoothed := predict(loess(etspaf~c(1:.N))), by="sex"]# smooth PAF
ETS[, plot(etspaf, col=c("blue", "red")[sex], main = "PAF of ETS on CHD")]
ETS[, lines(smoothed,  lwd=2)]

# Calculate PAF of ETS for stroke
# RR from Oono IP, Mackay DF, Pell JP. Meta-analysis of the association between secondhand smoke exposure and stroke. 
# J Public Health 2011;33:496–502. doi:10.1093/pubmed/fdr025
ETS <-as.data.table(svyby(~expsm, ~agegroup + sex , HSE.srv,  svymean, na.rm = T, 
                          vartype = c("se"),
                          drop.empty.groups = F))
setnames(ETS, paste0("expsm", c(0:1, 99)), paste0(c(0:1, NA)))
setnames(ETS, paste0("se.expsm", c(0:1, 99)), paste0(c(0:1, NA)))
setkey(ETS, agegroup, sex)
ETS = copy(merge(melt(ETS[, 1:4, with =F], c(1,2), variable.name = "expsm", value.name = "Pe"),
                 melt(ETS[, c(1:2, 6:7), with =F], c(1,2), variable.name = "expsm", value.name = "se"), 
                 by = c("agegroup", "sex", "expsm")))

ETS[, `:=` (rr = 1, ci  = 1)]
ETS[expsm == "1" & agegroup %!in% c("<1   ", "01-04", "05-09", "10-14", "15-19", "20-24", "25-29"), `:=` (rr = 1.25, ci  = 1.38)]

ETS[, etspaf := mean(rtruncnorm(n = .N * 1e3, a = 0, mean = Pe, sd = se) * (stochRR(.N * 1e3, rr, ci) - 1), na.rm = T), by = .(agegroup, sex, expsm)]

ETS[, etspaf := sum(etspaf, na.rm = T) / (1 + sum(etspaf, na.rm = T)), by = .(agegroup, sex)]

ETS =copy(unique(ETS, by=c("agegroup", "sex")))
ETS[, `:=` (expsm = NULL, Pe = NULL, rr = NULL, ci = NULL, se = NULL)]
setkey(ETS, sex, agegroup)
write.csv(ETS[etspaf > 0,], file = "./Models/IMPACTncd/CVD Statistics/strokeetspaf.csv", row.names = F)

# draw example of smoothered PAF
ETS[etspaf > 0, smoothed := predict(loess(etspaf~c(1:.N))), by="sex"]# smooth PAF
ETS[, plot(etspaf, col=c("blue", "red")[sex], main = "PAF of ETS on stroke")]
ETS[, lines(smoothed,  lwd=2)]

# ************************************************************** SBP *****************************************************************
# Calculate PAF of SBP (omsysval) for CHD and stroke
HSE[, omsysval.usual := round(omsysval,1)] 
HSE[omsysval.usual > 200, omsysval.usual := 200]
HSE[is.na(omsysval.usual), omsysval.usual := 999]
HSE[, omsysval.usual := factor(omsysval.usual)]
HSE.srv <- svydesign(id=~psu, strata=~cluster, weights=~wt.nurse, nest=F, data=HSE) # change weighting

# Calculate prevalence of SBP by agegroup and sex
SBP <-as.data.table(svyby(~omsysval.usual, ~agegroup + sex , HSE.srv,  svymean, na.rm = T, 
                          vartype = c("se"),
                          drop.empty.groups = F))
PREVAL = copy(SBP)

tt <- HSE[, levels(omsysval.usual)]
setnames(SBP, paste0("omsysval.usual", tt), paste0(c(tt[tt!="999"], NA)))
setnames(SBP, paste0("se.omsysval.usual", tt), paste0(c(tt[tt!="999"], NA)))
setkey(SBP, agegroup, sex)

SBP = copy(merge(melt(SBP[, 1:(length(tt) + 2 - 1), with =F], c(1,2), variable.name = "omsysval.usual", value.name = "Pe"),
                 melt(SBP[, c(1:2, (length(tt) + 2):(2 * length(tt) + 1)), with =F], c(1,2), variable.name = "omsysval.usual", value.name = "se"), 
                 by = c("agegroup", "sex", "omsysval.usual")))

write.csv(SBP[Pe > 0,], file = "./Models/IMPACTncd/Exposure/SBPpreval2006.csv", row.names = F) # create csv with SBP prevalence

# Calculate RR for CHD. Optimal SBP level at 115mmHg and RR(HR) of dying from CHD was taken from 
# "Age-specific relevance of
# usual blood pressure to vascular mortality: 
# a meta-analysis of individual data for one million adults in 61 prospective studies. 
# The Lancet. 2002 Dec 14;360(9349):1903–1913" 
# Figure 5
SBP[, omsysval.usual := as.numeric(as.character(omsysval.usual))]

SBP[, `:=` (rr = 1, ci  = 1)]
SBP[sex == 1 & agegroup  %in% c("30-34", "35-39", "40-44", "45-49"),  # extrapolated
    `:=` (rr = 0.50^((115 - omsysval.usual)/20), ci = 0.54^((115 - omsysval.usual)/20))]
SBP[sex == 2 & agegroup  %in% c("30-34", "35-39", "40-44", "45-49"), 
    `:=` (rr = 0.40^((115 - omsysval.usual)/20), ci = 0.49^((115 - omsysval.usual)/20))]
SBP[sex == 1 & agegroup  %in% c("50-54", "55-59"), `:=` (rr = 0.50^((115 - omsysval.usual)/20), ci = 0.52^((115 - omsysval.usual)/20))] 
SBP[sex == 2 & agegroup  %in% c("50-54", "55-59"), `:=` (rr = 0.49^((115 - omsysval.usual)/20), ci = 0.54^((115 - omsysval.usual)/20))] 
SBP[sex == 1 & agegroup  %in% c("60-64", "65-69"), `:=` (rr = 0.55^((115 - omsysval.usual)/20), ci = 0.57^((115 - omsysval.usual)/20))] 
SBP[sex == 2 & agegroup  %in% c("60-64", "65-69"), `:=` (rr = 0.50^((115 - omsysval.usual)/20), ci = 0.53^((115 - omsysval.usual)/20))] 
SBP[sex == 1 & agegroup  %in% c("70-74", "75-79"), `:=` (rr = 0.62^((115 - omsysval.usual)/20), ci = 0.64^((115 - omsysval.usual)/20))] 
SBP[sex == 2 & agegroup  %in% c("70-74", "75-79"), `:=` (rr = 0.55^((115 - omsysval.usual)/20), ci = 0.58^((115 - omsysval.usual)/20))] 
SBP[sex == 1 & agegroup  %in% c("80-84", "85+"  ), `:=` (rr = 0.69^((115 - omsysval.usual)/20), ci = 0.73^((115 - omsysval.usual)/20))] 
SBP[sex == 2 & agegroup  %in% c("80-84", "85+"  ), `:=` (rr = 0.64^((115 - omsysval.usual)/20), ci = 0.68^((115 - omsysval.usual)/20))] 
SBP[rr < 1, `:=` (rr = 1, ci  = 1)] # Set rr<1, as 1

# Calculate PAF by agegroup/sex for CHD
SBP[, sbppaf := mean(rtruncnorm(n = .N * 1e3, a = 0, mean = Pe, sd = se) * (stochRR(.N * 1e3, rr, ci) - 1), na.rm = T), by = .(agegroup, sex, omsysval.usual)]

SBP[, sbppaf := sum(sbppaf, na.rm = T) / (1 + sum(sbppaf, na.rm = T)), by = .(agegroup, sex)]

SBP =copy(unique(SBP, by=c("agegroup", "sex")))
SBP[, `:=` (omsysval.usual = NULL, Pe = NULL, rr = NULL, ci = NULL, se = NULL)]
setkey(SBP, sex, agegroup)

write.csv(SBP, file = "./Models/IMPACTncd/CVD Statistics/chdsbppaf.csv", row.names = F)

# draw example of smoothered PAF
SBP[sbppaf > 0, smoothed := predict(loess(sbppaf~c(1:.N))), by="sex"] # smooth PAF
SBP[, plot(sbppaf, col=c("blue", "red")[sex], main = "PAF of SBP on CHD")]
SBP[, lines(smoothed,  lwd=2)]

# Calculate PAF of SBP for stroke (do the same as above)
SBP = copy(PREVAL)
tt <- HSE[, levels(omsysval.usual)]
setnames(SBP, paste0("omsysval.usual", tt), paste0(c(tt[tt!="999"], NA)))
setnames(SBP, paste0("se.omsysval.usual", tt), paste0(c(tt[tt!="999"], NA)))
setkey(SBP, agegroup, sex)

SBP = copy(merge(melt(SBP[, 1:(length(tt) + 2 - 1), with =F], c(1,2), variable.name = "omsysval.usual", value.name = "Pe"),
                 melt(SBP[, c(1:2, (length(tt) + 2):(2 * length(tt) + 1)), with =F], c(1,2), variable.name = "omsysval.usual", value.name = "se"), 
                 by = c("agegroup", "sex", "omsysval.usual")))

# Calculate RR for stroke. Optimal SBP level at 115mmHg and RR(HR) of dying from stroke was taken from "Age-specific relevance of usual blood pressure to 
# vascular mortality: a meta-analysis of individual data for one million adults in 61 prospective studies. 
# The Lancet. 2002 Dec 14;360(9349):1903–1913" 
# Figure 3
SBP[, omsysval.usual := as.numeric(as.character(omsysval.usual))]

SBP[, `:=` (rr = 1, ci  = 1)]
SBP[sex == 1 & agegroup  %in% c("30-34", "35-39", "40-44", "45-49"),  # extrapolated
    `:=` (rr = 0.33^((115 - omsysval.usual)/20), ci = 0.38^((115 - omsysval.usual)/20))]
SBP[sex == 2 & agegroup  %in% c("30-34", "35-39", "40-44", "45-49"), 
    `:=` (rr = 0.41^((115 - omsysval.usual)/20), ci = 0.49^((115 - omsysval.usual)/20))]
SBP[sex == 1 & agegroup  %in% c("50-54", "55-59"), `:=` (rr = 0.34^((115 - omsysval.usual)/20), ci = 0.37^((115 - omsysval.usual)/20))] 
SBP[sex == 2 & agegroup  %in% c("50-54", "55-59"), `:=` (rr = 0.45^((115 - omsysval.usual)/20), ci = 0.50^((115 - omsysval.usual)/20))] 
SBP[sex == 1 & agegroup  %in% c("60-64", "65-69"), `:=` (rr = 0.41^((115 - omsysval.usual)/20), ci = 0.44^((115 - omsysval.usual)/20))] 
SBP[sex == 2 & agegroup  %in% c("60-64", "65-69"), `:=` (rr = 0.47^((115 - omsysval.usual)/20), ci = 0.51^((115 - omsysval.usual)/20))] 
SBP[sex == 1 & agegroup  %in% c("70-74", "75-79"), `:=` (rr = 0.48^((115 - omsysval.usual)/20), ci = 0.51^((115 - omsysval.usual)/20))] 
SBP[sex == 2 & agegroup  %in% c("70-74", "75-79"), `:=` (rr = 0.53^((115 - omsysval.usual)/20), ci = 0.56^((115 - omsysval.usual)/20))] 
SBP[sex == 1 & agegroup  %in% c("80-84", "85+"  ), `:=` (rr = 0.68^((115 - omsysval.usual)/20), ci = 0.75^((115 - omsysval.usual)/20))] 
SBP[sex == 2 & agegroup  %in% c("80-84", "85+"  ), `:=` (rr = 0.65^((115 - omsysval.usual)/20), ci = 0.71^((115 - omsysval.usual)/20))] 
SBP[rr < 1, `:=` (rr = 1, ci  = 1)] # Set rr<1, as 1

# Calculate PAF by agegroup/sex for stroke
SBP[, sbppaf := mean(rtruncnorm(n = .N * 1e3, a = 0, mean = Pe, sd = se) * (stochRR(.N * 1e3, rr, ci) - 1), na.rm = T), by = .(agegroup, sex, omsysval.usual)]

SBP[, sbppaf := sum(sbppaf, na.rm = T) / (1 + sum(sbppaf, na.rm = T)), by = .(agegroup, sex)]

SBP =copy(unique(SBP, by=c("agegroup", "sex")))
SBP[, `:=` (omsysval.usual = NULL, Pe = NULL, rr = NULL, ci = NULL, se = NULL)]
setkey(SBP, sex, agegroup)
write.csv(SBP, file = "./Models/IMPACTncd/CVD Statistics/strokesbppaf.csv", row.names = F)

# draw example of smoothered PAF
SBP[sbppaf > 0, smoothed := predict(loess(sbppaf~c(1:.N))), by="sex"]# smooth PAF
SBP[, plot(sbppaf, col=c("blue", "red")[sex], main = "PAF of SBP on stroke")]
SBP[, lines(smoothed,  lwd=2)]

# ************************************************************** CHOL *****************************************************************
# Calculate PAF of chol on CHD
HSE[, cholval1.usual := round(cholval1,2)]
HSE[cholval1.usual > 12, cholval1.usual := 12]
HSE[is.na(cholval1.usual), cholval1.usual := 999]
HSE[, cholval1.usual := factor(cholval1.usual)]

HSE.srv <- svydesign(id=~psu, strata=~cluster, weights=~wt.blood, nest=F, data=HSE)

CHOL <-as.data.table(svyby(~cholval1.usual, ~agegroup + sex , HSE.srv,  svymean, na.rm = T, 
                           vartype = c("se"),
                           drop.empty.groups = F))
PREVAL = copy(CHOL)

tt <- HSE[, levels(cholval1.usual)]
setnames(CHOL, paste0("cholval1.usual", tt), paste0(c(tt[tt!="999"], NA)))
setnames(CHOL, paste0("se.cholval1.usual", tt), paste0(c(tt[tt!="999"], NA)))
setkey(CHOL, agegroup, sex)

CHOL = copy(merge(melt(CHOL[, 1:(length(tt) + 2 - 1), with =F], c(1,2), variable.name = "cholval1.usual", value.name = "Pe"),
                  melt(CHOL[, c(1:2, (length(tt) + 2):(2 * length(tt) + 1)), with =F], c(1,2), variable.name = "cholval1.usual", value.name = "se"), 
                  by = c("agegroup", "sex", "cholval1.usual")))

write.csv(CHOL[Pe > 0,], file = "./Models/IMPACTncd/Exposure/CHOLpreval2006.csv", row.names = F) # create csv with SBP prevalence

# Calculate RR for CHD. Optimal chol level at 3.8 mmol/L and RR(HR) of dying from CHD was taken from "Blood cholesterol and 
# vascular mortality by age, sex, and blood pressure: a meta-analysis of individual data from 61 prospective studies 
# with 55.000 vascular deaths. The Lancet. 2007 Dec 7;370(9602):1829–39. 
# Appendix Webtable 6  fully adjusted
CHOL[, cholval1.usual := as.numeric(as.character(cholval1.usual))]

CHOL[, `:=` (rr = 1, ci  = 1)]
CHOL[agegroup  %in% c("30-34", "35-39", "40-44", "45-49"), 
     `:=` (rr = 0.49^(3.8 - cholval1.usual), ci = 0.52^(3.8 - cholval1.usual))]
CHOL[agegroup  %in% c("50-54", "55-59"), `:=` (rr = 0.62^(3.8 - cholval1.usual), ci = 0.65^(3.8 - cholval1.usual))] 
CHOL[agegroup  %in% c("60-64", "65-69"), `:=` (rr = 0.74^(3.8 - cholval1.usual), ci = 0.76^(3.8 - cholval1.usual))] 
CHOL[agegroup  %in% c("70-74", "75-79"), `:=` (rr = 0.84^(3.8 - cholval1.usual), ci = 0.86^(3.8 - cholval1.usual))] 
CHOL[agegroup  %in% c("80-84", "85+"  ), `:=` (rr = 0.87^(3.8 - cholval1.usual), ci = 0.90^(3.8 - cholval1.usual))] 
CHOL[rr < 1, `:=` (rr = 1, ci  = 1)] # Set rr<1, as 1

# Calculate PAF by agegroup/sex for CHD
CHOL[, cholpaf := mean(rtruncnorm(n = .N * 1e3, a = 0, mean = Pe, sd = se) * (stochRR(.N * 1e3, rr, ci) - 1), na.rm = T), by = .(agegroup, sex, cholval1.usual)]

CHOL[, cholpaf := sum(cholpaf, na.rm = T) / (1 + sum(cholpaf, na.rm = T)), by = .(agegroup, sex)]

CHOL =copy(unique(CHOL, by=c("agegroup", "sex")))
CHOL[, `:=` (cholval1.usual = NULL, Pe = NULL, rr = NULL, ci = NULL, se = NULL)]
setkey(CHOL, sex, agegroup)

write.csv(CHOL, file = "./Models/IMPACTncd/CVD Statistics/chdcholpaf.csv", row.names = F)
# draw example of smoothered PAF
CHOL[cholpaf > 0, smoothed := predict(loess(cholpaf~c(1:.N))), by="sex"] # smooth PAF
CHOL[, plot(cholpaf, col=c("blue", "red")[sex], main = "PAF of chol on CHD")]
CHOL[, lines(smoothed,  lwd=2)]

# Calculate PAF by agegroup/sex for stroke
CHOL = copy(PREVAL)
tt <- HSE[, levels(cholval1.usual)]
setnames(CHOL, paste0("cholval1.usual", tt), paste0(c(tt[tt!="999"], NA)))
setnames(CHOL, paste0("se.cholval1.usual", tt), paste0(c(tt[tt!="999"], NA)))
setkey(CHOL, agegroup, sex)

CHOL = copy(merge(melt(CHOL[, 1:(length(tt) + 2 - 1), with =F], c(1,2), variable.name = "cholval1.usual", value.name = "Pe"),
                  melt(CHOL[, c(1:2, (length(tt) + 2):(2 * length(tt) + 1)), with =F], c(1,2), variable.name = "cholval1.usual", value.name = "se"), 
                  by = c("agegroup", "sex", "cholval1.usual")))

# Calculate RR for stroke. Optimal chol level at 3.8 mmol/L and RR(HR) of dying from CHD was taken from "Blood cholesterol and 
# vascular mortality by age, sex, and blood pressure: a meta-analysis of individual data from 61 prospective studies 
# with 55.000 vascular deaths. The Lancet. 2007 Dec 7;370(9602):1829–39. 
# Table 2 (for total stroke. I used only significant HR's). Not adjusted for SBP. When adjusted for SBP then becomes
# non significant. This contranticts evidence from trials. see discussion of above paper
CHOL[, cholval1.usual := as.numeric(as.character(cholval1.usual))]

CHOL[, `:=` (rr = 1, ci  = 1)]
CHOL[agegroup  %in% c("40-44", "45-49"), `:=` (rr = 0.87^(3.8 - cholval1.usual), ci = 1^(3.8 - cholval1.usual))] 
CHOL[agegroup  %in% c("50-54", "55-59"), `:=` (rr = 0.91^(3.8 - cholval1.usual), ci = 0.97^(3.8 - cholval1.usual))] 
CHOL[agegroup  %in% c("60-64", "65-69"), `:=` (rr = 0.93^(3.8 - cholval1.usual), ci = 0.97^(3.8 - cholval1.usual))] 
CHOL[rr < 1, `:=` (rr = 1, ci  = 1)] # Set rr<1, as 1

# Calculate PAF by agegroup/sex for stroke
CHOL[, cholpaf := mean(rtruncnorm(n = .N * 1e3, a = 0, mean = Pe, sd = se) * (stochRR(.N * 1e3, rr, ci) - 1), na.rm = T), by = .(agegroup, sex, cholval1.usual)]

CHOL[, cholpaf := sum(cholpaf, na.rm = T) / (1 + sum(cholpaf, na.rm = T)), by = .(agegroup, sex)]

CHOL =copy(unique(CHOL, by=c("agegroup", "sex")))
CHOL[, `:=` (cholval1.usual = NULL, Pe = NULL, rr = NULL, ci = NULL, se = NULL)]
setkey(CHOL, sex, agegroup)
write.csv(CHOL, file = "./Models/IMPACTncd/CVD Statistics/strokecholpaf.csv", row.names = F)

# draw example of smoothered PAF
CHOL[cholpaf > 0, smoothed := predict(loess(cholpaf~c(1:.N))), by="sex"] # smooth PAF
CHOL[, plot(cholpaf, col=c("blue", "red")[sex], main = "PAF of chol on stroke")]
CHOL[, lines(smoothed,  lwd=2)]

# ************************************************************** BMI *****************************************************************
# Calculate PAF of BMI on CHD
HSE[, bmival := round(bmival, 1)] # round BMI in 1 digit
HSE[is.na(bmival), bmival := 999]
HSE[, bmival := factor(bmival)]
HSE.srv <- svydesign(id=~psu, strata=~cluster, weights=~wt.nurse, nest=F, data=HSE)

BMI <- as.data.table(svyby(~bmival, ~agegroup + sex , HSE.srv,  svymean, na.rm = T, 
                           vartype = c("se"),
                           drop.empty.groups = F))
PREVAL = copy(BMI)

tt <- HSE[, levels(bmival)]
setnames(BMI, paste0("bmival", tt), paste0(c(tt[tt!="999"], NA)))
setnames(BMI, paste0("se.bmival", tt), paste0(c(tt[tt!="999"], NA)))
setkey(BMI, agegroup, sex)

BMI = copy(merge(melt(BMI[, 1:(length(tt) + 2 - 1), with =F], c(1,2), variable.name = "bmival", value.name = "Pe"),
                 melt(BMI[, c(1:2, (length(tt) + 2):(2 * length(tt) + 1)), with =F], c(1,2), variable.name = "bmival", value.name = "se"), 
                 by = c("agegroup", "sex", "bmival")))
write.csv(BMI[Pe > 0,], file = "./Models/IMPACTncd/Exposure/BMIpreval2006.csv", row.names = F) # create csv with SBP prevalence

# Calculate RR for CHD. Optimal BMI level at 20 Kg/m^2 and RR(HR) of CHD/stroke (both fatal and non fatal) 
# was taken from "The Emerging Risk Factors Collaboration. 
# Separate and combined associations of body-mass index and abdominal adiposity with cardiovascular disease:
# collaborative analysis of 58 prospective studies.
# The Lancet 2011;377:1085–95. doi:10.1016/S0140-6736(11)60105-0
# Table 1 (Adjusted for age, sex, smoking status, systolic blood pressure, history of diabetes, and total and HDL cholesterol)
# and figure 2 for age specific gradient
1.11 * 1.41 / 1.29
1.17 * 1.41 / 1.29
1.11 * 1.23 / 1.29
1.17 * 1.23 / 1.29
1.11 * 1.12 / 1.29
1.17 * 1.12 / 1.29
BMI[, bmival := as.numeric(as.character(bmival))]

BMI[, `:=` (rr = 1, ci  = 1)]

BMI[agegroup  %in% c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59"), 
    `:=` (rr = 1.213256^((bmival - 20) / 4.56), ci = 1.278837^((bmival - 20) / 4.56))]
BMI[agegroup  %in% c("60-64", "65-69"), 
    `:=` (rr = 1.058372^((bmival - 20) / 4.56), ci = 1.115581^((bmival - 20) / 4.56))]
BMI[rr < 1, `:=` (rr = 1, ci  = 1)] # Set rr<1, as 1

# Calculate PAF for CHD
BMI[, bmipaf := mean(rtruncnorm(n = .N * 1e3, a = 0, mean = Pe, sd = se) * (stochRR(.N * 1e3, rr, ci) - 1), na.rm = T), by = .(agegroup, sex, bmival)]

BMI[, bmipaf := sum(bmipaf, na.rm = T) / (1 + sum(bmipaf, na.rm = T)), by = .(agegroup, sex)]

BMI =copy(unique(BMI, by=c("agegroup", "sex")))
BMI[, `:=` (bmival = NULL, Pe = NULL, rr = NULL, ci = NULL, se = NULL)]
setkey(BMI, sex, agegroup)

write.csv(BMI, file = "./Models/IMPACTncd/CVD Statistics/chdbmipaf.csv", row.names = F)

# draw example of smoothered PAF
BMI[bmipaf > 0, smoothed := predict(loess(bmipaf~c(1:.N))), by = "sex"] # smooth PAF
BMI[, plot(bmipaf, col=c("blue", "red")[sex], main = "PAF of BMI on CHD")]
BMI[, lines(smoothed,  lwd=2)]

# Calculate PAF for ischaemic stroke # from BHF stroke statistics tables 2.2 and 2.3 isch stroke / haem stroke incidence is more than 10 to 1
BMI = copy(PREVAL)
tt <- HSE[, levels(bmival)]
setnames(BMI, paste0("bmival", tt), paste0(c(tt[tt!="999"], NA)))
setnames(BMI, paste0("se.bmival", tt), paste0(c(tt[tt!="999"], NA)))
setkey(BMI, agegroup, sex)

BMI = copy(merge(melt(BMI[, 1:(length(tt) + 2 - 1), with =F], c(1,2), variable.name = "bmival", value.name = "Pe"),
                 melt(BMI[, c(1:2, (length(tt) + 2):(2 * length(tt) + 1)), with =F], c(1,2), variable.name = "bmival", value.name = "se"), 
                 by = c("agegroup", "sex", "bmival")))

BMI[, bmival := as.numeric(as.character(bmival))]
1.06*1.34/1.20
1.13*1.34/1.20
1.06*1.22/1.20
1.13*1.22/1.20

BMI[, `:=` (rr = 1, ci  = 1)]

BMI[agegroup  %in% c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59"), 
    `:=` (rr = 1.183667^((bmival - 20) / 4.56), ci = 1.261833^((bmival - 20) / 4.56))]
BMI[agegroup  %in% c("60-64", "65-69"), 
    `:=` (rr = 1.077667^((bmival - 20) / 4.56), ci = 1.148833^((bmival - 20) / 4.56))]
BMI[rr < 1, `:=` (rr = 1, ci  = 1)] # Set rr<1, as 1

BMI[rr < 1, `:=` (rr = 1, ci  = 1)] # Set rr<1, as 1

# Calculate PAF by agegroup/sex for stroke
BMI[, bmipaf := mean(rtruncnorm(n = .N * 1e3, a = 0, mean = Pe, sd = se) * (stochRR(.N * 1e3, rr, ci) - 1), na.rm = T), by = .(agegroup, sex, bmival)]

BMI[, bmipaf := sum(bmipaf, na.rm = T) / (1 + sum(bmipaf, na.rm = T)), by = .(agegroup, sex)]

BMI =copy(unique(BMI, by=c("agegroup", "sex")))
BMI[, `:=` (bmival = NULL, Pe = NULL, rr = NULL, ci = NULL, se = NULL)]
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
setnames(DIAB2, c("diabete21", "se.diabete21"), c("Pe", "se"))
write.csv(DIAB2, file = "./Models/IMPACTncd/Exposure/DIABundiagpreval2006.csv", row.names = F)

# Calculate PAF of diabetes for CHD
# RR from The Emerging Risk Factors Collaboration. Diabetes mellitus, fasting blood glucose concentration, and risk of vascular disease: a collaborative 
# meta-analysis of 102 prospective studies. The Lancet 2010;375:2215–22. doi:10.1016/S0140-6736(10)60484-9
# figure 1 and 2 (HRs were adjusted for age, smoking status, body-mass index, and  systolic blood pressure)

DIAB2[, `:=` (rr = 1, ci  = 1)]
DIAB2[agegroup  %in% c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59"), `:=` (rr = 2.51, ci = 2.80)]
DIAB2[agegroup  %in% c("60-64", "65-69"), `:=` (rr = 2.01, ci = 2.26)]
DIAB2[agegroup  %in% c("70-74", "75-79", "80-84", "85+"), `:=` (rr = 1.78, ci = 2.05)]

DIAB2[, diabpaf := mean(rtruncnorm(n = .N * 1e3, a = 0, mean = Pe, sd = se) * (stochRR(.N * 1e3, rr, ci) - 1), na.rm = T), by = .(agegroup, sex)]

DIAB2[, diabpaf := sum(diabpaf, na.rm = T) / (1 + sum(diabpaf, na.rm = T)), by = .(agegroup, sex)]

DIAB2 =copy(unique(DIAB2, by=c("agegroup", "sex")))
DIAB2[, `:=` (Pe = NULL, rr = NULL, ci = NULL, se = NULL)]
setkey(DIAB2, sex, agegroup)

write.csv(DIAB2, file = "./Models/IMPACTncd/CVD Statistics/chddiabpaf.csv", row.names = F)

# draw example of smoothered PAF
DIAB2[diabpaf > 0, smoothed := predict(loess(diabpaf~c(1:.N))), by="sex"]# smooth PAF
DIAB2[, plot(diabpaf, col=c("blue", "red")[sex], main = "PAF of diabetes on CHD")]
DIAB2[, lines(smoothed,  lwd=2)]

# Calculate PAF of diabetes for stroke
# RR as above for ISCHAEMIC STROKE
DIAB2 <- data.table(svyby(~diabete2, ~agegroup + sex, HSE.srv, svymean, drop.empty.groups = F))
DIAB2[, `:=` (diabete22 = NULL, diabete299 = NULL, se.diabete22 = NULL, se.diabete299 = NULL)]
setnames(DIAB2, c("diabete21", "se.diabete21"), c("Pe", "se"))

DIAB2[, `:=` (rr = 1, ci  = 1)]
DIAB2[agegroup  %in% c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59"), `:=` (rr = 3.74, ci = 4.58)]
DIAB2[agegroup  %in% c("60-64", "65-69"), `:=` (rr = 2.06, ci = 2.58)]
DIAB2[agegroup  %in% c("70-74", "75-79", "80-84", "85+"), `:=` (rr = 1.80, ci = 2.27)]

DIAB2[, diabpaf := mean(rtruncnorm(n = .N * 1e3, a = 0, mean = Pe, sd = se) * (stochRR(.N * 1e3, rr, ci) - 1), na.rm = T), by = .(agegroup, sex)]

DIAB2[, diabpaf := sum(diabpaf, na.rm = T) / (1 + sum(diabpaf, na.rm = T)), by = .(agegroup, sex)]

DIAB2 =copy(unique(DIAB2, by=c("agegroup", "sex")))
DIAB2[, `:=` (Pe = NULL, rr = NULL, ci = NULL, se = NULL)]
setkey(DIAB2, sex, agegroup)

write.csv(DIAB2, file = "./Models/IMPACTncd/CVD Statistics/strokediabpaf.csv", row.names = F)

# draw example of smoothered PAF
DIAB2[diabpaf > 0, smoothed := predict(loess(diabpaf~c(1:.N))), by="sex"]# smooth PAF
DIAB2[, plot(diabpaf, col=c("blue", "red")[sex], main = "PAF of diabetes on Isch Stroke")]
DIAB2[, lines(smoothed,  lwd=2)]

# ************************************************************** F & V *****************************************************************
HSE[porftvg == 0, porftvg := 1]
HSE[, porftvg := porftvg - 1]
HSE[is.na(porftvg), porftvg := 999]
HSE[, porftvg := factor(porftvg)]
HSE.srv <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=HSE)

FV <- as.data.table(svyby(~porftvg, ~agegroup + sex , HSE.srv,  svymean, na.rm = T, 
                          vartype = c("se"),
                          drop.empty.groups = F))
PREVAL = copy(FV)

tt <- HSE[, levels(porftvg)]
setnames(FV, paste0("porftvg", tt), paste0(c(tt[tt!="999"], NA)))
setnames(FV, paste0("se.porftvg", tt), paste0(c(tt[tt!="999"], NA)))
setkey(FV, agegroup, sex)

FV = copy(merge(melt(FV[, 1:(length(tt) + 2 - 1), with =F], c(1,2), variable.name = "porftvg", value.name = "Pe"),
                melt(FV[, c(1:2, (length(tt) + 2):(2 * length(tt) + 1)), with =F], c(1,2), variable.name = "porftvg", value.name = "se"), 
                by = c("agegroup", "sex", "porftvg")))

write.csv(FV[Pe > 0,], file = "./Models/IMPACTncd/Exposure/FVpreval2006.csv", row.names = F) # create csv with SBP prevalence

# Calculate RR for CHD. From Dauchet L, Amouyel P, Hercberg S, Dallongeville J. 
# Fruit and Vegetable Consumption and Risk of Coronary Heart Disease: 
# A Meta-Analysis of Cohort Studies. J Nutr. 2006 Oct 1;136(10):2588–93. 
# Portions were considered as 106 gr instead of 80gr(HSE). This is not a problem because
# eg porftvg = 2 means between 2 and 3 portions
# 8 portions were considered to have the maximum effect
FV[, porftvg := as.numeric(as.character(porftvg))]
FV[, `:=` (rr = 1, ci  = 1)]
FV[agegroup %!in% c("<1   ", "01-04", "05-09", "10-14", "15-19", "20-24", "25-29"), `:=` (rr = 0.96^-(8-porftvg), ci  = 0.99^-(8-porftvg))]
FV[rr < 1, `:=` (rr = 1, ci  = 1)] # Set rr<1, as 1

# Calculate PAF for CHD
FV[, fvpaf := mean(rtruncnorm(n = .N * 1e3, a = 0, mean = Pe, sd = se) * (stochRR(.N * 1e3, rr, ci) - 1), na.rm = T), by = .(agegroup, sex, porftvg)]

FV[, fvpaf := sum(fvpaf, na.rm = T) / (1 + sum(fvpaf, na.rm = T)), by = .(agegroup, sex)]

FV =copy(unique(FV, by=c("agegroup", "sex")))
FV[, `:=` (porftvg = NULL, Pe = NULL, rr = NULL, ci = NULL, se = NULL)]
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
# To avoid negative PAF an optimal level of F&V has to be set arbitrarily. I set it to 8 
FV = copy(PREVAL)

tt <- HSE[, levels(porftvg)]
setnames(FV, paste0("porftvg", tt), paste0(c(tt[tt!="999"], NA)))
setnames(FV, paste0("se.porftvg", tt), paste0(c(tt[tt!="999"], NA)))
setkey(FV, agegroup, sex)

FV = copy(merge(melt(FV[, 1:(length(tt) + 2 - 1), with =F], c(1,2), variable.name = "porftvg", value.name = "Pe"),
                melt(FV[, c(1:2, (length(tt) + 2):(2 * length(tt) + 1)), with =F], c(1,2), variable.name = "porftvg", value.name = "se"), 
                by = c("agegroup", "sex", "porftvg")))

FV[, porftvg := as.numeric(as.character(porftvg))]
FV[, `:=` (rr = 1, ci  = 1)]
FV[agegroup %!in% c("<1   ", "01-04", "05-09", "10-14", "15-19", "20-24", "25-29"), `:=` (rr = 0.95^-(8-porftvg), ci  = 0.97^-(8-porftvg))]
FV[rr < 1, `:=` (rr = 1, ci  = 1)] # Set rr<1, as 1

# Calculate PAF for CHD
FV[, fvpaf := mean(rtruncnorm(n = .N * 1e3, a = 0, mean = Pe, sd = se) * (stochRR(.N * 1e3, rr, ci) - 1), na.rm = T), by = .(agegroup, sex, porftvg)]

FV[, fvpaf := sum(fvpaf, na.rm = T) / (1 + sum(fvpaf, na.rm = T)), by = .(agegroup, sex)]

FV =copy(unique(FV, by=c("agegroup", "sex")))
FV[, `:=` (porftvg = NULL, Pe = NULL, rr = NULL, ci = NULL, se = NULL)]
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


# PAF for PA
chd.mediator <- round(mean(1.39/1.64, 1.56/1.79, 1.20/1.43, 1.12/1.32, 1.52/1.85, 1.05/1.22, 2.63/2.94, 1.35/1.45), 2) # CHD adjusted/unadjusted RR 
# from table 10.13 p85 WHO CRA. For stroke I will use the same because not enough available studies

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

HSE[is.na(a30t06g), a30t06g := 99]
HSE[, a30t06g := factor(a30t06g)]

HSE.srv <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=HSE)

# 1 = low, 2 = medium, 3 = high PA
# svymean(~a30t06g, subset(HSE.srv, age >15),  na.rm = T)
chdpapaf <-as.data.table(svyby(~a30t06g, ~agegroup + sex , HSE.srv,  svymean, na.rm = T, 
                               vartype = c("se"),
                               drop.empty.groups = F))
setnames(chdpapaf, paste0("a30t06g", c(1:3, 99)), paste0(c(1:3, NA)))
setnames(chdpapaf, paste0("se.a30t06g", c(1:3, 99)), paste0(c(1:3, NA)))
setkey(chdpapaf, agegroup, sex)
chdpapaf = copy(merge(melt(chdpapaf[, 1:5, with =F], c(1,2), variable.name = "a30t06g", value.name = "Pe"),
                      melt(chdpapaf[, c(1:2, 7:9), with =F], c(1,2), variable.name = "a30t06g", value.name = "se"), 
                      by = c("agegroup", "sex", "a30t06g")))

write.csv(chdpapaf[Pe > 0,], file = "./Models/IMPACTncd/Exposure/PApreval2006.csv", row.names = F) # create csv with SBP prevalence

# RR for PA 1. WHO | Comparative Quantification of Health Risks [Internet]. 
# WHO [cited 2014 Jan 30];Available from: http://www.who.int/publications/cra/en/
# Table 10.19 (with adjustment for measurement error)
chdpapaf[, `:=` (rr = 1, ci  = 1)]
chdpapaf[a30t06g == "1" & agegroup %in% c("30-34", "35-39", "40-44", "45-49", "50-54","55-59", "60-64", "65-69"), `:=` (rr = 1.71, ci  = 1.85)]
chdpapaf[a30t06g == "1" & agegroup %in% c("70-74", "75-79"), `:=` (rr = 1.50, ci  = 1.61)]
chdpapaf[a30t06g == "1" & agegroup %in% c("80-84", "85+"),`:=` (rr = 1.30, ci  = 1.41)]
chdpapaf[a30t06g == "2" & agegroup %in% c("30-34", "35-39", "40-44", "45-49", "50-54","55-59", "60-64", "65-69"), `:=` (rr = 1.44, ci  = 1.62)]
chdpapaf[a30t06g == "2" & agegroup %in% c("70-74", "75-79"), `:=` (rr = 1.31, ci  = 1.48)]
chdpapaf[a30t06g == "2" & agegroup %in% c("80-84", "85+"), `:=` (rr = 1.20, ci  = 1.35)]
chdpapaf[rr<1, `:=` (rr=1, ci=1)]

# Calculate PAF for CHD
chdpapaf[, papaf := mean(rtruncnorm(n = .N * 1e3, a = 0, mean = Pe, sd = se) * (stochRR(.N * 1e3, rr, ci, chd.mediator) - 1), na.rm = T), by = .(agegroup, sex, a30t06g)]

chdpapaf[, papaf := sum(papaf, na.rm = T) / (1 + sum(papaf, na.rm = T)), by = .(agegroup, sex)]

chdpapaf =copy(unique(chdpapaf, by=c("agegroup", "sex")))
chdpapaf[, `:=` (a30t06g = NULL, Pe = NULL, rr = NULL, ci = NULL, se = NULL)]
setkey(chdpapaf, sex, agegroup)
write.csv(chdpapaf, file = "./Models/IMPACTncd/CVD Statistics/chdpapaf.csv", row.names = F)

# draw example of smoothered PAF
chdpapaf[papaf > 0, smoothed := predict(loess(papaf~c(1:.N))), by = "sex"] # smooth PAF
chdpapaf[, plot(papaf, col=c("blue", "red")[sex], main = "PAF of tobacco on CHD")]
chdpapaf[, lines(smoothed,  lwd=2)]

# Stroke
stroke.mediator <- round(mean(1.8/2, 1.2/3.7, 1.10/1.10, 1.34/1.49, 2.89/3.05, 1.47/1.6, 1.20/1.45, 1.89/2.27, 0.83/1.05), 2) # CHD adjusted/unadjusted RR 

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

HSE[is.na(a30t06g), a30t06g := 99]
HSE[, a30t06g := factor(a30t06g)]

HSE.srv <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=HSE)

# 1 = low, 2 = medium, 3 = high PA
# svymean(~a30t06g, subset(HSE.srv, age >15),  na.rm = T)
chdpapaf <-as.data.table(svyby(~a30t06g, ~agegroup + sex , HSE.srv,  svymean, na.rm = T, 
                               vartype = c("se"),
                               drop.empty.groups = F))
setnames(chdpapaf, paste0("a30t06g", c(1:3, 99)), paste0(c(1:3, NA)))
setnames(chdpapaf, paste0("se.a30t06g", c(1:3, 99)), paste0(c(1:3, NA)))
setkey(chdpapaf, agegroup, sex)
chdpapaf = copy(merge(melt(chdpapaf[, 1:5, with =F], c(1,2), variable.name = "a30t06g", value.name = "Pe"),
                      melt(chdpapaf[, c(1:2, 7:9), with =F], c(1,2), variable.name = "a30t06g", value.name = "se"), 
                      by = c("agegroup", "sex", "a30t06g")))

# RR for PA 1. WHO | Comparative Quantification of Health Risks [Internet]. 
# WHO [cited 2014 Jan 30];Available from: http://www.who.int/publications/cra/en/
# Table 10.20 (with adjustment for measurement error)
# Only signifficant HR were considered
chdpapaf[, `:=` (rr = 1, ci  = 1)]
chdpapaf[a30t06g == "1" & agegroup %in% c("30-34", "35-39", "40-44", "45-49", "50-54","55-59", "60-64", "65-69"), `:=` (rr = 1.53, ci  = 1.79)]
chdpapaf[a30t06g == "1" & agegroup %in% c("70-74", "75-79"), `:=` (rr = 1.38, ci  = 1.6)]
chdpapaf[a30t06g == "1" & agegroup %in% c("80-84", "85+"),`:=` (rr = 1.24, ci  = 1.45)]
chdpapaf[rr<1, `:=` (rr=1, ci=1)]

# Calculate PAF for CHD
chdpapaf[, papaf := mean(rtruncnorm(n = .N * 1e3, a = 0, mean = Pe, sd = se) * (stochRR(.N * 1e3, rr, ci, stroke.mediator) - 1), na.rm = T), by = .(agegroup, sex, a30t06g)]

chdpapaf[, papaf := sum(papaf, na.rm = T) / (1 + sum(papaf, na.rm = T)), by = .(agegroup, sex)]

chdpapaf =copy(unique(chdpapaf, by=c("agegroup", "sex")))
chdpapaf[, `:=` (a30t06g = NULL, Pe = NULL, rr = NULL, ci = NULL, se = NULL)]
setkey(chdpapaf, sex, agegroup)
write.csv(chdpapaf, file = "./Models/IMPACTncd/CVD Statistics/strokepapaf.csv", row.names = F)

# 10-year lag -------------------------------------------------------------

## Work on calculating paf for other factors. In order to do this I need to calculate their prevalence in 2001
load(file="./Datasets/Health Survey for England/2001/hse01ai.RData") # load HSE2001
HSE <- setDT(clear.labels(HSE2001original))
HSE[, age:= age + 10] # to compansate for the lag. eg. 30yo in 2011 is 20 in 2001
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
HSE[is.na(cigst1), cigst1 := 99]
HSE[, cigst1 := factor(cigst1)]
# cigarette-years = cigarettes smoke per day * years of smoking
HSE[, `:=` (cigyears4 = 0)]
HSE[cigst1 == "4" & startsmk < 90 & cigdyal < 90, 
    cigyears4 := cigdyal * (age -10 - startsmk)] #  startsmk == 97 means never smoked regurarly
HSE[cigst1 == "3" & numsmok < 90 & smokyrs <90,
    cigyears4 := as.numeric(numsmok * smokyrs)]
HSE[cigst1 == "2" & smokyrs <90,
    cigyears4 := as.numeric(0.5 * smokyrs)] # assume non regular smokers smok 0.5 cigaretes/day
HSE[, cigyears4 := factor(round(cigyears4, -1))]

HSE[, endsmoke2 := cut(endsmoke, 
                       breaks = quantile(endsmoke,
                                         c(0, .25, .50, .75, 1),
                                         na.rm = T), 
                       include.lowest = T, 
                       right = F, 
                       ordered_result = T)]
HSE[, endsmoke3 := 0]
HSE[cigst1 %in% c("2", "3"),
    endsmoke3 := mean(endsmoke),
    by = endsmoke2]
HSE[is.na(endsmoke), endsmoke :=0]
HSE.srv <- svydesign(id=~area, strata=NULL, weights=NULL, nest=T, data=HSE)

# c16smokepaf <- as.data.table(svyby(~cigst1, ~agegroup + sex , HSE.srv,  svymean, na.rm = T, 
#                                    vartype = c("se"),
#                                    drop.empty.groups = F))
# setnames(c16smokepaf, paste0("cigst1", c(1:4, 99)), paste0(c(1:4, NA)))
# setnames(c16smokepaf, paste0("se.cigst1", c(1:4, 99)), paste0(c(1:4, NA)))
# setkey(c16smokepaf, agegroup, sex)
# 
# c16smokepaf = copy(merge(melt(c16smokepaf[, 1:6, with =F], c(1,2), variable.name = "cigst1", value.name = "Pe"),
#                          melt(c16smokepaf[, c(1:2, 8:11), with =F], c(1,2), variable.name = "cigst1", value.name = "se"), 
#                          by = c("agegroup", "sex", "cigst1")))
# 
# 
# write.csv(c16smokepaf[Pe > 0,], file = "./Models/IMPACTncd/Exposure/TOBpreval2001.csv", row.names = F) # create csv with SBP prevalence

ttt <- as.data.table(svytable(~agegroup + cigyears4 + endsmoke + cigst1 + sex,
                              HSE.srv))
ttt[, Pe := N/sum(N, na.rm = T), by = .(agegroup, sex)]
ttt[, cigyears4 := as.numeric(as.character(cigyears4))]
ttt[, endsmoke := as.numeric(as.character(endsmoke))]

ttt[, sum(Pe), by = agegroup]
ttt[, sum(N), by = cigst1]

# RR for tobacco from González CA, et al. Smoking and the risk of gastric cancer in the 
# European Prospective Investigation Into Cancer and Nutrition (EPIC). Int J Cancer 2003;107:629–34. 
# "the HR were very similar (HR of the log-2 of cigarette-years 1.040 in males and 1.040 in females),
# both statistically significant". I assume CI from 1 to 1.08

ttt[, `:=` (rr = 1, ci  = 1)]
ttt[cigst1 %in% c("2", "3", "4") & Pe > 0, `:=` (rr = 1.04^log2(cigyears4), ci  = 1.08^log2(cigyears4))]
# reduce risk for ex smokers, from the same study Table IV. 
# will not use ci from table IV because I use mean (endsmoke)
ttt[cigst1 %in% c("2", "3") & endsmoke > 0,  `:=` (
  rr = rr * 0.961^log2(endsmoke), 
  ci = ci * 0.961^log2(endsmoke))]
ttt[rr < 1, `:=` (rr = 1, ci = 1)]
ttt[cigst1=="4", summary(rr)]
ttt[cigst1=="3", summary(rr)]

# Calculate PAF for c16
ttt[rr > 1 , tobpaf := Pe * rr, 
    by = .(agegroup, cigyears4, endsmoke, cigst1, sex)]
ttt[is.na(tobpaf), tobpaf := 0]
ttt[cigyears4 == 0, tobpaf := Pe]
ttt[, tobpaf := 1 -  1 /sum(tobpaf, na.rm = T), by = .(agegroup, sex)]

ttt =copy(unique(ttt, by=c("agegroup", "sex")))
ttt[, `:=` (cigyears4 = NULL, endsmoke = NULL, cigst1 = NULL,  Pe = NULL, rr = NULL, ci = NULL, se = NULL, N = NULL)]
ttt[tobpaf < 0, tobpaf := 0]
setkey(ttt, sex, agegroup)

write.csv(ttt, 
          file = "./Models/IMPACTncd/Cancer Statistics/c16tobpaf.csv",
          row.names = F)

# draw example of smoothered PAF
ttt[tobpaf > 0, smoothed := predict(loess(tobpaf~c(1:.N))), by = "sex"] # smooth PAF
ttt[, plot(tobpaf, col=c("blue", "red")[as.numeric(sex)], main = "PAF of tob on C16")]
ttt[, lines(smoothed, lwd=2)]


# C16 F&V -----------------------------------------------------------------

HSE[porftvg == 0, porftvg := 1]
HSE[, porftvg := porftvg - 1]
HSE[is.na(porftvg), porftvg := 999]
HSE[, porftvg := factor(porftvg)]
HSE.srv <- svydesign(id=~area, strata=NULL, weights=NULL, nest=T, data=HSE)

FV <- as.data.table(svyby(~porftvg, ~agegroup + sex , HSE.srv,  svymean, na.rm = T, 
                          vartype = c("se"),
                          drop.empty.groups = F))
PREVAL = copy(FV)

tt <- HSE[, levels(porftvg)]
setnames(FV, paste0("porftvg", tt), paste0(c(tt[tt!="999"], NA)))
setnames(FV, paste0("se.porftvg", tt), paste0(c(tt[tt!="999"], NA)))
setkey(FV, agegroup, sex)

FV = copy(merge(melt(FV[, 1:(length(tt) + 2 - 1), with =F], c(1,2), variable.name = "porftvg", value.name = "Pe"),
                melt(FV[, c(1:2, (length(tt) + 2):(2 * length(tt) + 1)), with =F], c(1,2), variable.name = "porftvg", value.name = "se"), 
                by = c("agegroup", "sex", "porftvg")))

write.csv(FV[Pe > 0,], file = "./Models/IMPACTncd/Exposure/FVpreval2001.csv", row.names = F) # create csv with SBP prevalence

# Calculate RR for C16 from Comparative quantification of health risks [Internet].
# Geneva: World Health Organisation; 2004. 
# Chapter 9 table 9.28 (p100 in pdf)

FV[, porftvg := as.numeric(as.character(porftvg))]
FV[, `:=` (rr = 1, ci  = 1)]
FV[agegroup %in% unique(agegroup.fn(15:69)), `:=` (rr = 0.94^-(8-porftvg), ci  = 1^-(8-porftvg))]
FV[agegroup %in% unique(agegroup.fn(70:79)), `:=` (rr = 0.95^-(8-porftvg), ci  = 1^-(8-porftvg))]
FV[agegroup %in% unique(agegroup.fn(80:99)), `:=` (rr = 0.97^-(8-porftvg), ci  = 1^-(8-porftvg))]
FV[rr < 1, `:=` (rr = 1, ci  = 1)] # Set rr<1, as 1
# upper ci reduced to 1 from 1.03 etc. Otherwise it creates bias by increasing the effect of FV
# because of the truncated distribution (the mean is )
# Calculate PAF for CHD
FV[, fvpaf := mean(rtruncnorm(n = .N * 1e3, a = 0, mean = Pe, sd = se) * (stochRRabov1(.N * 1e3, rr, ci)), na.rm = T), by = .(agegroup, sex, porftvg)] # This distorts results because of the wide ci crossing 1 and the truncation of the distrbution
# FV[, fvpaf := Pe * rr, by = .(agegroup, sex, porftvg)]

FV[, fvpaf := 1 - 1 / sum(fvpaf, na.rm = T), by = .(agegroup, sex)]

FV =copy(unique(FV, by=c("agegroup", "sex")))
FV[, `:=` (porftvg = NULL, Pe = NULL, rr = NULL, ci = NULL, se = NULL)]
setkey(FV, sex, agegroup)
write.csv(FV, file = "./Models/IMPACTncd/Cancer Statistics/c16fvpaf.csv", row.names = F)

# draw example of smoothered PAF
FV[fvpaf < 0, smoothed := predict(loess(fvpaf~c(1:.N))), by = "sex"] # smooth PAF
FV[, plot(fvpaf, col=c("blue", "red")[sex], main = "PAF of F&V on CHD")]
FV[, lines(smoothed,  lwd=2)]

# For salt
# define distributions from 24h urine from 2001 (table 4.1, 4.2)

# Men 19-24 
p <- c(0,	0,	0.09,	0.33,	0.46,	0.6,	0.75,	0.96,	1,	0.02,	0.37,	0.81,	1,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 14.0, 15.8, 3, 6, 9, 15, 18, 10.6, 6, 16.6) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=7, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
get.triang.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt <- get.triang.par(p, e, show.output=F, plot=F, tol=0.001, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 
HSE[between(age, 19, 24) & sex == 1, salt := rtriang(.N, tt[1], tt[2], tt[3])]

# Men 25-34 
p <- c(0.06,	0.12,	0.27,	0.33,	0.48,	0.59,	0.7,	0.78,	0.05,	0.2,	0.34,	0.57,	0.73,	0.89,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 14.0, 15.8, 3, 6, 9, 12, 15, 18, 10.9, 2.2, 22.3) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=5, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
get.triang.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt <- get.triang.par(p, e, show.output=F, plot=F, tol=0.001, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 
HSE[between(age, 25, 34) & sex == 1, salt := rtriang(.N, tt[1], tt[2], tt[3])]

# Men 35-49 
p <- c(0.02,	0.09,	0.19,	0.36,	0.51,	0.6,	0.74,	0.82,	0.02,	0.13,	0.39,	0.58,	0.8,	0.91,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 14.0, 15.8, 3, 6, 9, 12, 15, 18, 10.2, 2.4, 22.1) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=6, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
get.triang.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt <- get.triang.par(p, e, show.output=F, plot=F, tol=0.001, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 
HSE[between(age, 35, 49) & sex == 1, salt := rtriang(.N, tt[1], tt[2], tt[3])]

# Men 50-64 (50 - 100) 
p <- c(0.08,	0.15,	0.27,	0.42,	0.54,	0.67,	0.77,	0.85,	0.05,	0.18,	0.42,	0.65,	0.83,	0.91,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 14.0, 15.8, 3, 6, 9, 12, 15, 18, 10.1, 2.1, 21.2) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=5, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
get.triang.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt <- get.triang.par(p, e, show.output=F, plot=F, tol=0.001, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 
HSE[between(age, 50, 64) & sex == 1, salt := rtriang(.N, tt[1], tt[2], tt[3])]
HSE[between(age, 65, 110) & sex == 1, salt := rtriang(.N, tt[1], tt[2]-1, tt[3]-1)] # reduce salt by age

# Women 19-24 
p <- c(0.04,	0.17,	0.34,	0.65,	0.7,	0.84,	0.88,	0.9,	0.66,	0.84,	0.92,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 14.0, 15.8, 9, 12, 18, 7.6, 1.7, 23.2) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=25, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
get.lnorm.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt <- get.lnorm.par(p, e, show.output=F, plot=F, tol=0.001, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 
HSE[between(age, 19, 24) & sex == 2, salt := rlnorm(.N, tt[1], tt[2])]

# Women 25-34 
p <- c(0.05,	0.25,	0.41,	0.57,	0.74,	0.85,	0.91,	0.95,	0.06,	0.29,	0.59,	0.81,	0.92,	0.97,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 14.0, 15.8, 3, 6, 9, 12, 15, 18, 8, 1.9, 22.2) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=7, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
get.chisq.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt <- get.chisq.par(p, e, show.output=F, plot=F, tol=0.001, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 
HSE[between(age, 25, 34) & sex == 2, salt := rchisq(.N, tt[1])]

# Women 35-49 
p <- c(0.05,	0.2,	0.43,	0.67,	0.8,	0.87,	0.92,	0.97,	0.05,	0.31,	0.68,	0.85,	0.96,	1,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 14.0, 15.8, 3, 6, 9, 12, 15, 18, 7.6, 2.6, 16.2) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=6, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
get.gamma.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt <- get.gamma.par(p, e, show.output=F, plot=F, tol=0.001, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 
HSE[between(age, 35, 49) & sex == 2, salt := rgamma(.N, tt[1], tt[2])]

# Women 50-64 (50 - 100) 
p <- c(0.12,	0.28,	0.5,	0.68,	0.84,	0.92,	0.94,	0.98,	0.07,	0.38,	0.69,	0.91,	0.96,	0.99,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 14.0, 15.8, 3, 6, 9, 12, 15, 18, 7, 2.3, 15.7) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=8, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
get.gamma.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt <- get.gamma.par(p, e, show.output=F, plot=F, tol=0.001, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 
HSE[between(age, 50, 64) & sex == 2, salt := rgamma(.N, tt[1], tt[2])]
HSE[between(age, 65, 110) & sex == 2, salt := rgamma(.N, tt[1]-0.5, tt[2])]
# Calculate PAF of salt on gastric cancer
HSE[salt < 1, salt := 1]
HSE[between(age, 19, 64) & sex ==1, summary(salt)] #vs 11.0 +- 0.4
HSE[between(age, 19, 64) & sex ==2, summary(salt)] #vs 8.1 +- 0.32

HSE[, salt := round(salt, 1)] # round salt in 1 digit
HSE[is.na(salt), salt := 999]
HSE[, salt := factor(salt)]
HSE.srv <- svydesign(id=~area, strata=NULL, weights=NULL, nest=T, data=HSE)

SALT <- as.data.table(svyby(~salt, ~agegroup + sex , HSE.srv, 
                            svymean, na.rm = T, 
                            vartype = c("se"),
                            drop.empty.groups = F))
PREVAL = copy(SALT)

tt <- HSE[, levels(salt)]
setnames(SALT, paste0("salt", tt), paste0(c(tt[tt!="999"], NA)))
setnames(SALT, paste0("se.salt", tt), paste0(c(tt[tt!="999"], NA)))
setkey(SALT, agegroup, sex)

SALT = copy(merge(melt(SALT[, 1:(length(tt) + 2 - 1), with =F], c(1,2), variable.name = "salt", value.name = "Pe"),
                 melt(SALT[, c(1:2, (length(tt) + 2):(2 * length(tt) + 1)), with =F], c(1,2), variable.name = "salt", value.name = "se"), 
                 by = c("agegroup", "sex", "salt")))
write.csv(SALT[Pe > 0,], file = "./Models/IMPACTncd/Exposure/SALTpreval2001.csv", row.names = F) # create csv with SBP prevalence

# Calculate RR for C16. From World Cancer Research Fund, 
# American Institute for Cancer Research. Food, nutrition, 
# physical activity, and the prevention of cancer:
# a global perspective. Washington, DC: WCRF/AICR; 2007.
# p145. RR 1.08 (1 - 1.17) per 1gr/day of salt. Risk starts from
# 3gr/day (around reference category of the 2 cohort studies) 
# I will artificially decrease it for older ages  

SALT[, salt := as.numeric(as.character(salt))]

SALT[, `:=` (rr = 1, ci  = 1)]

SALT[agegroup %in% unique(agegroup.fn(20:69)),
     `:=` (rr = 1.08^(salt - 3), ci = 1^(salt - 3))]
SALT[agegroup %in% unique(agegroup.fn(70:79)),
     `:=` (rr = 1.06^(salt - 3), ci = 1^(salt - 3))]
SALT[agegroup %in% unique(agegroup.fn(80:89)),
     `:=` (rr = 1.04^(salt - 3), ci = 1^(salt - 3))]
SALT[rr < 1, `:=` (rr = 1, ci  = 1)] # Set rr<1, as 1

# Calculate PAF for CHD
SALT[, saltpaf := mean(rtruncnorm(n = .N * 1e3, a = 0, mean = Pe, sd = se) * stochRRabov1(.N * 1e3, rr, ci), na.rm = T), by = .(agegroup, sex, salt)]

SALT[, saltpaf := 1 - 1 / sum(saltpaf, na.rm = T), by = .(agegroup, sex)]

SALT = copy(unique(SALT, by=c("agegroup", "sex")))
SALT[, `:=` (salt = NULL, Pe = NULL, rr = NULL, ci = NULL, se = NULL)]
setkey(SALT, sex, agegroup)

write.csv(SALT, file = "./Models/IMPACTncd/Cancer Statistics/c16saltpaf.csv", row.names = F)

# draw example of smoothered PAF
SALT[saltpaf > 0, smoothed := predict(loess(saltpaf~c(1:.N))), by = "sex"] # smooth PAF
SALT[, plot(saltpaf, col=c("blue", "red")[sex], main = "PAF of salt on CHD")]
SALT[, lines(smoothed,  lwd=2)]


#**************************************************************************************************************
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

#Garbage collection
rm(temp)
