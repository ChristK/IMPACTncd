#cmpfile("./validation.R")
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


# preample ------------------------------------------
# if (exists("scenarios.list") &
#     "current trends.R" %in% scenarios.list) {
#   
if (Sys.info()[1] == "Linux") {
  if (system("whoami", T ) == "mdxasck2") {
    setwd("~/IMPACTncd/")
    clusternumber <- ifelse(clusternumber > 30, 30, clusternumber)  # overwrites previous if <60
  } else {
    setwd(paste("/home/", 
                system("whoami", T), 
                "/Dropbox/PhD/Models/IMPACTncd/", 
                sep = "", 
                collapse = ""))
  }
} else if (Sys.info()[1] == "Darwin") {
  setwd("/Volumes/home/dropbox/PhD/Models/IMPACTncd/")
} else {
  get.dropbox.folder <- function() {
    if (!require(RCurl)) 
      stop("You need to install RCurl package.")
    if (Sys.info()["sysname"] != "Windows") 
      stop("Currently, 'get.dropbox.folder' works for Windows and Linux only. Sorry.")
    db.file <- paste(Sys.getenv("LOCALAPPDATA"), "\\Dropbox\\host.db", sep = "")
    base64coded <- readLines(db.file, warn = F)[2]
    base64(base64coded, encode = F)
  }
  clusternumber <- 1L
  
  setwd(paste0(get.dropbox.folder(), "/PhD/Models/IMPACTncd/"))
  dir.dataset <- (paste0(get.dropbox.folder(), "/PhD/Datasets/"))
}

ext <- ".pdf"
dir <- "./Output/Validation/"

#  if (!exists("yearstoproject")) {
tt <- readLines("./Output/simulation parameters.txt")

yearstoproject <- as.integer(substring(tt[[grep(glob2rx("Years to project = *"), tt)]], 19))
ageL <- as.integer(substring(tt[[grep(glob2rx("ageL = *"), tt)]], 7))
ageH <- as.integer(substring(tt[[grep(glob2rx("ageH = *"), tt)]], 7))
Fertility.Assumption <- as.character(substring(tt[[grep(glob2rx("Fertility assumption = *"), tt)]], 24))

numberofiterations <- as.integer(substring(tt[[grep(glob2rx("Number of iterations = *"), tt)]], 24))

diseasestoexclude <- list()
for (jjj in 1:length(grep(glob2rx("diseases = *"), tt))) {
  diseasestoexclude[[jjj]] <- substring(tt[[grep(glob2rx("diseases = *"), tt)[[jjj]]]], 12)
}
diseasestoexclude <- unlist(diseasestoexclude)

init.year <- as.integer(substring(tt[[grep(glob2rx("First year of the simulation = *"), tt)]], 31))
n <- as.integer(substring(tt[[grep(glob2rx("Sample size = *"), tt)]], 14))
cvd.lag <- as.numeric(substring(tt[[grep(glob2rx("cvd.lag = *"), tt)]], 10))
cancer.lag <- as.numeric(substring(tt[[grep(glob2rx("cancer.lag = *"), tt)]], 13))
export.graphs <- F
fatality.annual.improvement.chd    <- as.numeric(substring(tt[[grep(glob2rx("CHD annual fatality improvement = *"), tt)]], 35))*100
fatality.annual.improvement.stroke <- as.numeric(substring(tt[[grep(glob2rx("Stroke annual fatality improvement = *"), tt)]], 38))*100
fatality.annual.improvement.c34    <- as.numeric(substring(tt[[grep(glob2rx("Lung cancer annual fatality improvement = *"), tt)]], 43))*100
fatality.annual.improvement.c16    <- as.numeric(substring(tt[[grep(glob2rx("Gastric cancer annual fatality improvement = *"), tt)]], 46))*100
fatality.sec.gradient.chd          <- as.numeric(substring(tt[[grep(glob2rx("CHD fatality gradient = *"), tt)]], 25))*100
fatality.sec.gradient.stroke       <- as.numeric(substring(tt[[grep(glob2rx("Stroke fatality gradient = *"), tt)]], 28))*100
fatality.sec.gradient.c34          <- as.numeric(substring(tt[[grep(glob2rx("Lung cancer fatality gradient = *"), tt)]], 33))*100
fatality.sec.gradient.c16          <- as.numeric(substring(tt[[grep(glob2rx("Gastric cancer fatality gradient = *"), tt)]], 36))*100
pop.fraction                       <- as.numeric(substring(tt[[grep(glob2rx("Population fraction = *"), tt)]], 23))
paired <- substring(tt[[grep(glob2rx("Paired = *"), tt)]], 10)
paired <- ifelse(paired == "TRUE", TRUE, FALSE)

rm(jjj, tt)
# }

require(compiler)
loadcmp(file = "./initialisation.Rc")
cvd.lag    <- round(1 + cvd.lag * 9) # binom mean = n*p
cancer.lag <- round(1 + cancer.lag * 9) # binom mean = n*p

#load("C:/Users/ckyprid/Documents/IMPACTncd outputs/riskfactors.RData")
#load("./Output/RF/highrisk.RData")
if (!exists("pop.abs")) load("./Output/RF/population.structure.RData")
#load("./Output/Other/life.exp0.RData")
#load("./Output/Other/life.exp65.RData")
#load("./Output/Other/hlife.exp.RData")
#load("./Output/Other/other.mortality.RData")
if (!exists("chd.burden") & "CHD" %in% diseasestoexclude) load("./Output/CHD/chd.burden.RData")
if (!exists("stroke.burden") & "stroke" %in% diseasestoexclude) load("./Output/Stroke/stroke.burden.RData")
if (!exists("c34.burden") & "C34" %in% diseasestoexclude) load("./Output/Lung ca/c34.burden.RData")
if (!exists("c16.burden") & "C16" %in% diseasestoexclude) load("./Output/Gastric ca/c16.burden.RData")
#load("./Output/Graphs.tbl//Graphs.tbl.rda")
if (!exists("Tables")) load("./Output/Tables/Tables.rda")
load(file = "./Lagtimes/HSE.ts.RData")
HSE.ts2 = copy(HSE.ts)

loadcmp(file = "./post simulation functions.Rc")

# population.actual <- fread("./Population/population.struct.csv",  header = T)[year == paste0(init.year), ]
# population.actual[, pct := round(as.numeric(n) * pop / sum(pop))]
# pop.fraction <- n / population.actual[, sum(pop)] # 53107200 is the total mid 2011 population of England (52642600 for 2010)

pd <- position_dodge(0.0) 
# The palette with black:
cbbPalette <- c(#"#000000", 
  "#E69F00", 
  "#56B4E9", 
  "#009E73", 
  #"#F0E442", 
  #"#0072B2", 
  "#D55E00", 
  "#CC79A7")

require(Cairo)
if (Sys.info()[1] == "Windows") Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.16/bin/gswin64c.exe")
require(extrafont)
# font_import() # to be run only once in each system
#loadfonts(device = "win", quiet = T)
loadfonts()
#fonttable()
require(scales)
#require(ggthemes)
require(RColorBrewer)

MC.mean <- function(m, ...) {
  return(list(mean = mean(m, na.rm = T),
              lui = quantile(m, probs = 0.025, na.rm = T),
              uui = quantile(m, probs = 0.975, na.rm = T)))
}

# MC.mean <- function(m, ...) {
#   return(list(mean = median(m, na.rm = T),
#               lui = mad(m, na.rm = T, low = T),
#               uui = mad(m, na.rm = T, high = T)))
# }

clonedt <- function(DT, times = numberofiterations) {
  l <- sample(list(DT), times, T)
  rbindlist(l, idcol = T)
  return(rbindlist(l, idcol = T))
}

ggsave.mine <- function(x, y, ...) {
  if (ext == ".tiff") {
    ggsave(x, y,  compression = "lzw", family = "Calibri", dpi = 600,  antialias = "subpixel",    width = 11.69/1.5, height = 8.27/2, type = "cairo", pointsize = 8)
  }
  
  if (ext == ".pdf") {
    ggsave(x, y, width = 11.69, height = 8.27, device = cairo_pdf)
  }
}

sy <- ifelse (init.year == 2006, 2001, 2006)

mortqimd <- readRDS("./Validation/mortqimd.rds")

# Load ONS deaths
deathsONS <- readRDS("./Validation/deaths.rds")
deathsONS <- deathsONS[agegroup %in% unique(agegroup.fn(30:84)),
                       .(ndths = sum(ndths)), 
                       by = .(year, sex, icd3digits)]

# Recode vascular dementia F01 to I67 for 2011 onwards due overcome ONS icd software changes
deathsONS[icd3digits == "F01", icd3digits := "I67"]
deathsONS[sex == "1", sex := "Men"]
deathsONS[sex == "2", sex := "Women"]
deathsONS[, sex := factor(sex)]

# appendix RF trends
# riskfactors[, scenario := factor(scenario, 
#                                  levels = scn.levels,
#                                  labels = scn.names,
#                                  ordered = T)]

qimd_labeller <- labeller(qimd = c("1" = "QIMD 1",
                                   "2" = "QIMD 2",
                                   "3" = "QIMD 3",
                                   "4" = "QIMD 4",
                                   "5" = "QIMD 5"))

mcjobs <- vector("list", 37)

# Rename scenarios --------------------------------------------------------
scn.levels <- c(
  "current trends"
)
scn.names <- c(
  "Current Policy"
)
if (exists("chd.burden")) {
  chd.burden[, scenario := factor(scenario, 
                                  levels = scn.levels,
                                  labels = scn.names,
                                  ordered = T)]
}
if (exists("stroke.burden")) {
  stroke.burden[, scenario := factor(scenario, 
                                     levels = scn.levels,
                                     labels = scn.names,
                                     ordered = T)]
}
if (exists("c16.burden")) {
  c16.burden[, scenario := factor(scenario, 
                                  levels = scn.levels,
                                  labels = scn.names,
                                  ordered = T)]
}
if (exists("c34.burden")) {
  c34.burden[, scenario := factor(scenario, 
                                  levels = scn.levels,
                                  labels = scn.names,
                                  ordered = T)]
}
# other.mortality[, scenario := factor(scenario, 
#                                      levels = scn.levels,
#                                      labels = scn.names,
#                                      ordered = T)]
# hlife.exp[, scenario := factor(scenario, 
#                                levels = scn.levels,
#                                labels = scn.names,
#                                ordered = T)]
# life.exp0[, scenario := factor(scenario, 
#                                levels = scn.levels,
#                                labels = scn.names,
#                                ordered = T)]
# life.exp65[, scenario := factor(scenario, 
#                                 levels = scn.levels,
#                                 labels = scn.names,
#                                 ordered = T)]
pop.abs[, scenario := factor(scenario, 
                             levels = scn.levels,
                             labels = scn.names,
                             ordered = T)]
# highrisk[, scenario := factor(scenario, 
#                               levels = scn.levels,
#                               labels = scn.names,
#                               ordered = T)]
lapply(Tables, function(x) {
  x[, scenario := factor(scenario, 
                         levels = scn.levels,
                         labels = scn.names,
                         ordered = T)]
}
)
x <- Tables$hle.S[, levels(scenario)]

# CHD validation ----------------------------------------------------------
#load("./Output/CHD/chd.burden.RData")
#load(file="./Validation/kirk.RData") # chd.drates
mcjobs[[1]] <- function () {
  if (exists("chd.burden")) {
    chd.drates <- fread("./Validation/BAMP 02-13 model 14-2112 proj with CI.csv")
    
    setnames(chd.drates, c("year", "agegroup", "sex", "qimd", "mean", "lui", "uui"))
    chd.drates[, Model:="BAMP"]
    chd.drates[, sex:= factor(sex, c("M", "F"), c("Men", "Women"))]
    chd.drates[, `:=` (lui = mean, uui = mean)] # ignore BAMP CI
    
    chd.mort = copy(chd.burden[group=="SAQ" & scenario == "Current Policy",])
    chd.mort = copy(chd.mort[agegroup!="30-34",])
    chd.mort[agegroup %in% c("35-39", "40-44"), agegroup := "35-44"]
    chd.mort[agegroup %in% c("45-49", "50-54"), agegroup := "45-54"]
    chd.mort[agegroup %in% c("55-59", "60-64"), agegroup := "55-64"]
    chd.mort[agegroup %in% c("65-69", "70-74"), agegroup := "65-74"]
    chd.mort[agegroup %in% c("75-79", "80-84"), agegroup := "75-84"]
    chd.mort <- chd.mort[, .(pop = sum(pop),
                             chd.mortality = sum(chd.mortality)),
                         by = .(agegroup, sex, qimd, scenario, mc, year)]
    
    grp <- c("year", "scenario", "sex", "agegroup", "qimd")
    
    tt <- mortqimd[icd=="I20-I25" & agegroup %in% unique(agegroup.fn(30:84))]
    tt = copy(tt[agegroup!="30-34",])
    tt[agegroup %in% c("35-39", "40-44"), agegroup := "35-44"]
    tt[agegroup %in% c("45-49", "50-54"), agegroup := "45-54"]
    tt[agegroup %in% c("55-59", "60-64"), agegroup := "55-64"]
    tt[agegroup %in% c("65-69", "70-74"), agegroup := "65-74"]
    tt[agegroup %in% c("75-79", "80-84"), agegroup := "75-84"]
    tt <- tt[, .(pop = sum(pop),
                 chd.mortality = sum(deaths)),
             by = .(agegroup, sex, qimd, year)]
    tt[, `:=` (Model = "Observed", mean = chd.mortality / pop)]
    
    chd.mort <- chd.mort[,
                         MC.mean(chd.mortality/pop),
                         by = grp]
    
    chd.mort[, `:=` (Model= "IMPACTNCD", scenario = NULL)]
    chd.mort <- rbind(chd.mort, chd.drates, tt, fill = T)
    chd.mort <- chd.mort[agegroup!="85+" & between(year, 2002, init.year + yearstoproject)]
    
    chd.men <- ggplot(chd.mort[sex=="Men",],
                      aes(x=year, y=mean*100000, colour=Model, ymin = 0)) + 
      geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.05, position=pd, alpha=3/5) + 
      #geom_smooth(method=lm, se=F) +
      geom_line(position=pd,size = 2, alpha=3/5) +
      facet_grid(agegroup ~ qimd, scales="free", labeller = qimd_labeller) +
      ylab("Mortality per 100,000") + scale_x_continuous(name="Year") + 
      scale_colour_discrete(breaks = c("BAMP", "IMPACTNCD", "Observed"),
                            labels = c("BAMP         ", expression(IMPACT[NCD]),
                                       "Observed  ")) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
      #ggtitle("CHD Mortality Validation (Men)") +
      theme(text = element_text(family="Calibri", size = 12)) +
      theme(plot.title = element_text(family="Calibri", face="bold", size=16))+ 
      theme(strip.text.x = element_text(size = 10, face = "bold"),
            strip.text.y = element_text(size = 10, face = "bold"),
            strip.background = element_rect(colour="purple", fill="#CCCCFF"))
    
    
    chd.women <- ggplot(chd.mort[sex=="Women",],
                        aes(x=year, y=mean*100000, colour=Model, ymin = 0)) + 
      geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.05, position=pd, alpha=3/5) + 
      #geom_smooth(method=lm, se=F) +
      geom_line(position=pd,size = 2, alpha=3/5) +
      facet_grid(agegroup ~ qimd, scales="free", labeller = qimd_labeller) +
      ylab("Mortality per 100,000") + scale_x_continuous(name="Year") + 
      scale_colour_discrete(breaks = c("BAMP", "IMPACTNCD", "Observed"),
                            labels = c("BAMP         ", expression(IMPACT[NCD]),
                                       "Observed  ")) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
      #ggtitle("CHD Mortality Validation (Women)") +
      theme(text = element_text(family="Calibri", size = 12)) +
      theme(plot.title = element_text(family="Calibri", face="bold", size=16))+ 
      theme(strip.text.x = element_text(size = 10, face = "bold"),
            strip.text.y = element_text(size = 10, face = "bold"),
            strip.background = element_rect(colour="purple", fill="#CCCCFF"))
    
    #print(chd.men)
    #print(chd.women) 
    
    
    # ggsave("./Validation/validation men chd.pdf", chd.men, units = "in", device=cairo_pdf, family="Calibri", antialias = "subpixel", dpi=600, width = 11.69, height = 8.27)
    # ggsave("./Validation/validation women chd.pdf", chd.women, units = "in", device=cairo_pdf, family="Calibri", antialias = "subpixel", dpi=600, width = 11.69, height = 8.27)
    chd.mort = copy(chd.burden[group=="SAQ" & scenario == "Current Policy", ])
    
    
    chd.mort <- chd.mort[, .(pop = sum(pop),
                             chd.mortality = sum(chd.mortality)),
                         by = .(sex, scenario, mc, year)]
    
    tt <- mortqimd[icd=="I20-I25" & agegroup %in% unique(agegroup.fn(30:84))]
    tt <- tt[, .(pop = sum(pop),
                 deaths = sum(deaths)),
             by = .(sex, year)]
    tt[, `:=` (grp = "Observed", icd = NULL, cause = NULL, mean = deaths/pop)]
    
    grp <- c("year", "sex")
    
    chd.mort <- chd.mort[,
                         MC.mean(chd.mortality/pop),
                         by = grp]
    chd.mort[, `:=` (grp= "IMPACTNCD")]
    
    ttt <- rbind(chd.mort, tt, fill = T)
    
    chd.sum <- ggplot(ttt[between(year, 2002, 2018),],
                      aes(x=year, y=mean*100000, colour=grp, ymin = 0)) + 
      geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.05, position=pd, alpha=3/5)  +
      geom_line(position=pd, size = 1, alpha = 3/5) +
      facet_grid(. ~ sex, scales="free", labeller=qimd_labeller) +
      ylab("Mortality per 100,000") + 
      scale_x_continuous(name="Year", breaks = c(2002, 2006, 2010, 2014, 2018)) + 
      scale_colour_discrete(breaks = c("Observed", "IMPACTNCD"),
                            labels = c("Observed   ", expression(bold("IMPACT"["NCD"])))) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5, size = 12)) + 
      theme(axis.text.y  = element_text(angle=0, size = 10)) + 
      #ggtitle("chd Mortality Validation (Men)") +
      theme(text = element_text(family="Calibri", face = "bold", size = 12)) +
      theme(plot.title = element_text(family="Calibri", size=14)) + 
      theme(legend.title = element_blank(),  
            legend.text = element_text(family="Calibri", size = 14, face = "bold"),
            legend.key.size = unit(2.5, "lines")) +
      theme(strip.text.x = element_text(size = 12, face = "bold"),
            strip.text.y = element_text(size = 12, face = "bold", angle=0),
            strip.background = element_rect(colour="purple", fill="#CCCCFF"))
    
    ggsave.mine(paste0(dir, "chd sum", ext), chd.sum)
    ggsave.mine(paste0(dir, "chd men", ext), chd.men)
    ggsave.mine(paste0(dir, "chd women", ext), chd.women)
  }
}

# Stroke validation -------------------------------------------------------
#load("./Output/Stroke/stroke.burden.RData")
mcjobs[[2]] <- function () {
  if (exists("stroke.burden")) {
    load(file="./Validation/stroke.drates.RData")
    stroke.drates[, Model:="BAMP"]
    stroke.drates[, `:=` (lui = mean, uui = mean)] # ignore BAMP CI
    
    stroke.mort = copy(stroke.burden[group=="SAQ" & scenario == "Current Policy",])
    stroke.mort = copy(stroke.mort[agegroup!="30-34",])
    stroke.mort[agegroup %in% c("35-39", "40-44"), agegroup := "35-44"]
    stroke.mort[agegroup %in% c("45-49", "50-54"), agegroup := "45-54"]
    stroke.mort[agegroup %in% c("55-59", "60-64"), agegroup := "55-64"]
    stroke.mort[agegroup %in% c("65-69", "70-74"), agegroup := "65-74"]
    stroke.mort[agegroup %in% c("75-79", "80-84"), agegroup := "75-84"]
    stroke.mort <- stroke.mort[, .(pop = sum(pop),
                                   stroke.mortality = sum(stroke.mortality)),
                               by = .(agegroup, sex, qimd, scenario, mc, year)]
    
    grp <- c("year", "scenario", "sex", "agegroup", "qimd")
    
    tt <- mortqimd[icd=="I60-I69" & agegroup %in% unique(agegroup.fn(30:84))]
    tt = copy(tt[agegroup!="30-34",])
    tt[agegroup %in% c("35-39", "40-44"), agegroup := "35-44"]
    tt[agegroup %in% c("45-49", "50-54"), agegroup := "45-54"]
    tt[agegroup %in% c("55-59", "60-64"), agegroup := "55-64"]
    tt[agegroup %in% c("65-69", "70-74"), agegroup := "65-74"]
    tt[agegroup %in% c("75-79", "80-84"), agegroup := "75-84"]
    tt <- tt[, .(pop = sum(pop),
                 stroke.mortality = sum(deaths)),
             by = .(agegroup, sex, qimd, year)]
    tt[, `:=` (Model = "Observed", mean = stroke.mortality / pop)]
    
    stroke.mort <- stroke.mort[,
                               MC.mean(stroke.mortality/pop),
                               by = grp]
    
    stroke.mort[, `:=` (Model= "IMPACTNCD", scenario = NULL)]
    stroke.mort <- rbind(stroke.mort,stroke.drates, tt, fill=T)
    stroke.mort <- stroke.mort[agegroup!="85+" & between(year, 2002, init.year + yearstoproject)]
    
    stroke.men <- ggplot(stroke.mort[sex=="Men",],
                         aes(x=year, y=mean*100000, colour=Model, ymin = 0)) + 
      geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.05, position = pd, alpha=3/5) +
      #geom_smooth(method=lm, se=F) +
      geom_line(position=pd,size = 2, alpha=3/5) +
      facet_grid(agegroup ~ qimd, scales="free", labeller = qimd_labeller) +
      ylab("Mortality per 100,000") + scale_x_continuous(name="Year") + 
      scale_colour_discrete(breaks = c("BAMP", "IMPACTNCD", "Observed"),
                            labels = c("BAMP         ", expression(IMPACT[NCD]),
                                       "Observed  ")) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
      #ggtitle("Stroke Mortality Validation (Men)") +
      theme(text = element_text(family="Calibri", size = 12)) +
      theme(plot.title = element_text(family="Calibri", face="bold", size=16))+ 
      theme(strip.text.x = element_text(size = 10, face = "bold"),
            strip.text.y = element_text(size = 10, face = "bold"),
            strip.background = element_rect(colour="purple", fill="#CCCCFF"))
    
    
    stroke.women <- ggplot(stroke.mort[sex=="Women",],
                           aes(x=year, y=mean*100000, colour=Model, ymin = 0)) + 
      geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.05, position=pd, alpha=3/5) +
      #geom_smooth(method=lm, se=F) +
      geom_line(position=pd,size = 2, alpha=3/5) +
      facet_grid(agegroup ~ qimd, scales="free", labeller = qimd_labeller) +
      ylab("Mortality per 100,000") + scale_x_continuous(name="Year") + 
      scale_colour_discrete(breaks = c("BAMP", "IMPACTNCD", "Observed"),
                            labels = c("BAMP         ", expression(IMPACT[NCD]),
                                       "Observed  ")) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
      #ggtitle("Stroke Mortality Validation (Women)") +
      theme(text = element_text(family="Calibri", size = 12)) +
      theme(plot.title = element_text(family="Calibri", face="bold", size=16))+ 
      theme(strip.text.x = element_text(size = 10, face = "bold"),
            strip.text.y = element_text(size = 10, face = "bold"),
            strip.background = element_rect(colour="purple", fill="#CCCCFF"))
    
    #print(stroke.men)
    #print(stroke.women) 
    
    
    # ggsave("./Validation/validation men stroke.pdf", stroke.men, units = "in", device=cairo_pdf, family="Calibri", antialias = "subpixel", dpi=1200, width = 11.69, height = 8.27)
    # ggsave("./Validation/validation women stroke.pdf", stroke.women, units = "in", device=cairo_pdf, family="Calibri", antialias = "subpixel", dpi=1200, width = 11.69, height = 8.27)
    stroke.mort = copy(stroke.burden[group=="SAQ" & scenario == "Current Policy",])
    
    
    stroke.mort <- stroke.mort[, .(pop = sum(pop),
                                   stroke.mortality = sum(stroke.mortality)),
                               by = .(sex, scenario, mc, year)]
    
    tt <- mortqimd[icd=="I60-I69" & agegroup %in% unique(agegroup.fn(30:84))]
    tt <- tt[, .(pop = sum(pop),
                 deaths = sum(deaths)),
             by = .(sex, year)]
    tt[, `:=` (grp = "Observed", icd = NULL, cause = NULL, mean = deaths/pop)]
    
    grp <- c("year", "sex")
    
    stroke.mort <- stroke.mort[,
                               MC.mean(stroke.mortality/pop),
                               by = grp]
    stroke.mort[, `:=` (grp= "IMPACTNCD")]
    
    ttt <- rbind(stroke.mort, tt, fill = T)
    
    stroke.sum <- ggplot(ttt[between(year, 2002, 2018),],
                         aes(x=year, y=mean*100000, colour=grp, ymin = 0)) + 
      geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.05, position=pd, alpha=3/5)  +
      geom_line(position=pd, size = 1, alpha = 3/5) +
      facet_grid(. ~ sex, scales="free", labeller=qimd_labeller) +
      ylab("Mortality per 100,000") + 
      scale_x_continuous(name="Year", breaks = c(2002, 2006, 2010, 2014, 2018)) + 
      scale_colour_discrete(breaks = c("Observed", "IMPACTNCD"),
                            labels = c("Observed   ", expression(bold("IMPACT"["NCD"])))) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5, size = 12)) + 
      theme(axis.text.y  = element_text(angle=0, size = 10)) + 
      #ggtitle("stroke Mortality Validation (Men)") +
      theme(text = element_text(family="Calibri", face = "bold", size = 12)) +
      theme(plot.title = element_text(family="Calibri", size=14)) + 
      theme(legend.title = element_blank(),  
            legend.text = element_text(family="Calibri", size = 14, face = "bold"),
            legend.key.size = unit(2.5, "lines")) +
      theme(strip.text.x = element_text(size = 12, face = "bold"),
            strip.text.y = element_text(size = 12, face = "bold", angle=0),
            strip.background = element_rect(colour="purple", fill="#CCCCFF"))
    
    ggsave.mine(paste0(dir, "stroke sum", ext), stroke.sum)
    ggsave.mine(paste0(dir, "stroke men", ext), stroke.men)
    ggsave.mine(paste0(dir, "stroke women", ext), stroke.women)
  }
}

# Lung cancer validation -----------------------------------------------
#load("./Output/Lung ca/c34.burden.RData")
mcjobs[[36]] <- function () {
  if (exists("c34.burden")) {
    load(file="./Validation/c34.drates.RData")
    c34.drates[, Model:="BAMP"]
    c34.drates[, `:=` (lui = mean, uui = mean)] # ignore BAMP CI
    
    c34.mort = copy(c34.burden[group=="SAQ" & scenario == "Current Policy",])
    c34.mort = copy(c34.mort[agegroup!="30-34",])
    c34.mort[agegroup %in% c("35-39", "40-44"), agegroup := "35-44"]
    c34.mort[agegroup %in% c("45-49", "50-54"), agegroup := "45-54"]
    c34.mort[agegroup %in% c("55-59", "60-64"), agegroup := "55-64"]
    c34.mort[agegroup %in% c("65-69", "70-74"), agegroup := "65-74"]
    c34.mort[agegroup %in% c("75-79", "80-84"), agegroup := "75-84"]
    c34.mort <- c34.mort[, .(pop = sum(pop),
                             c34.mortality = sum(c34.mortality)),
                         by = .(agegroup, sex, qimd, scenario, mc, year)]
    
    grp <- c("year", "scenario", "sex", "agegroup", "qimd")
    
    
    c34.mort <- c34.mort[,
                         MC.mean(c34.mortality/pop),
                         by = grp]
    
    c34.mort[, `:=` (Model= "IMPACTNCD", scenario = NULL)]
    
    tt <- mortqimd[icd=="C33-C34" & agegroup %in% unique(agegroup.fn(30:84))]
    tt = copy(tt[agegroup!="30-34",])
    tt[agegroup %in% c("35-39", "40-44"), agegroup := "35-44"]
    tt[agegroup %in% c("45-49", "50-54"), agegroup := "45-54"]
    tt[agegroup %in% c("55-59", "60-64"), agegroup := "55-64"]
    tt[agegroup %in% c("65-69", "70-74"), agegroup := "65-74"]
    tt[agegroup %in% c("75-79", "80-84"), agegroup := "75-84"]
    tt <- tt[, .(pop = sum(pop),
                 c34.mortality = sum(deaths)),
             by = .(agegroup, sex, qimd, year)]
    tt[, `:=` (Model = "Observed", mean = c34.mortality / pop)]
    
    c34.mort <- rbind(c34.mort,c34.drates, tt, fill=T)
    c34.mort <- c34.mort[agegroup!="85+" & between(year, 2002, init.year + yearstoproject)]
    
    c34.men <- ggplot(c34.mort[sex=="Men",],
                      aes(x=year, y=mean*100000, colour=Model, ymin = 0)) + 
      #geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.05, position=pd, alpha=3/5) +
      #geom_smooth(method=lm, se=F) +
      geom_line(position=pd,size= 2, alpha=3/5) +
      facet_grid(agegroup ~ qimd, labeller=qimd_labeller, scales="free") +
      ylab("Mortality per 100,000") + scale_x_continuous(name="Year") + 
      scale_colour_discrete(breaks = c("BAMP", "IMPACTNCD", "Observed"),
                            labels = c("BAMP         ", expression(IMPACT[NCD]),
                                       "Observed  ")) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
      ggtitle("Lung Cancer Mortality Validation (Men)") +
      theme(text = element_text(family="Calibri", size = 12)) +
      theme(plot.title = element_text(family="Calibri", face="bold", size=34))+ 
      theme(strip.text.x = element_text(size = 10, face = "bold"),
            strip.text.y = element_text(size = 10, face = "bold"),
            strip.background = element_rect(colour="purple", fill="#CCCCFF"))
    
    
    c34.women <- ggplot(c34.mort[sex=="Women",],
                        aes(x=year, y=mean*100000, colour=Model, ymin = 0)) + 
      #geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.05, position=pd, alpha=3/5) +
      #geom_smooth(method=lm, se=F) +
      geom_line(position=pd,size = 2, alpha=3/5) +
      facet_grid(agegroup ~ qimd,  labeller=qimd_labeller, scales="free") +
      ylab("Mortality per 100,000") + scale_x_continuous(name="Year") + 
      scale_colour_discrete(breaks = c("BAMP", "IMPACTNCD", "Observed"),
                            labels = c("BAMP         ", expression(IMPACT[NCD]),
                                       "Observed  ")) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
      ggtitle("Lung Cancer Mortality Validation (Women)") +
      theme(text = element_text(family="Calibri", size = 12)) +
      theme(plot.title = element_text(family="Calibri", face="bold", size=34))+ 
      theme(strip.text.x = element_text(size = 10, face = "bold"),
            strip.text.y = element_text(size = 10, face = "bold"),
            strip.background = element_rect(colour="purple", fill="#CCCCFF"))
    
    #print(c34.men)
    #print(c34.women) 
    
    # ggsave("./Validation/validation men c34.pdf", c34.men, units = "in", device=cairo_pdf, family="Calibri", antialias = "subpixel", dpi=1200, width = 11.69, height = 8.27)
    # ggsave("./Validation/validation women c34.pdf", c34.women, units = "in", device=cairo_pdf, family="Calibri", antialias = "subpixel", dpi=1200, width = 11.69, height = 8.27)
    
    
    c34.incid = copy(c34.burden[group=="SAQ" & scenario == "Current Policy",])
    
    c34.incid <- c34.incid[, .(pop.mod = sum(pop),
                               cases.mod = sum(c34.incidence)),
                           by = .(agegroup, sex, scenario, mc, year)]
    
    c34cases <- fread("./Cancer Statistics/C34cases.csv")
    
    c34cases[, `:=` (grp = "Observed", site = NULL)]
    
    tt <- mortqimd[icd=="C33-C34" & agegroup %in% unique(agegroup.fn(30:84)), 
                   .(pop = sum(pop)), by = .(year, sex, agegroup)]
    
    c34cases[tt[between(year, 2006, 2013)], pop := pop, on = c("year", "agegroup", "sex")]
    c34cases[, mean := cases / pop]
    grp <- c("year", "sex", "agegroup")
    
    c34.incid <- c34.incid[,
                           MC.mean(cases.mod/pop.mod),
                           by = grp]
    c34.incid[tt[between(year, 2006, 2013)], pop := pop, on = c("year", "agegroup", "sex")]
    
    c34.incid[, `:=` (grp = "IMPACTNCD", cases = mean * pop,
                      lui.pop = lui * pop, uui.pop = uui * pop)]
    
    ttt <- rbind(c34.incid, c34cases, fill = T)[agegroup %in% unique(agegroup.fn(30:84)), ]
    
    c34.incid.a <- ggplot(ttt[between(year, 2006, 2013),],
                          aes(x=year, y=cases, colour=grp)) + 
      geom_line(position=pd, size = 1, alpha = 3/5) +
      #geom_errorbar(aes(ymin= lui.pop, ymax = uui.pop), width=.05, position=pd, alpha=3/5) +
      #geom_smooth(method=lm, se=F) +
      facet_grid(sex ~ agegroup, scales="free") +
      ylab("Lung cancer cases") + 
      scale_x_continuous(name="Year", breaks = c(2006, 2009, 2013)) + 
      scale_colour_discrete(breaks = c("Observed", "IMPACTNCD"),
                            labels = c("Observed   ", expression(bold("IMPACT"["NCD"])))) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5, size = 12)) + 
      theme(axis.text.y  = element_text(angle=0, size = 10)) + 
      #ggtitle("c34 Mortality Validation (Men)") +
      theme(text = element_text(family="Calibri", face = "bold", size = 16)) +
      theme(plot.title = element_text(family = "Calibri", size = 16)) + 
      theme(legend.title = element_blank(),  
            legend.text = element_text(family = "Calibri", size = 16, face = "bold"),
            legend.key.size = unit(2.5, "lines")) +
      theme(strip.text.x = element_text(size = 16, face = "bold"),
            strip.text.y = element_text(size = 16, face = "bold", angle=-90),
            strip.background = element_rect(colour="purple", fill="#CCCCFF"))
    #print(c34.incid.a)
    
    c34.incid.a.rates <- ggplot(ttt[between(year, 2006, 2013),],
                                aes(x=year, y=mean * 1e5, colour=grp)) + 
      geom_line(position=pd, size = 1, alpha = 3/5) +
      #geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.05, position=pd, alpha=3/5) +
      #geom_smooth(method=lm, se=F) +
      facet_grid(sex ~ agegroup, scales="free") +
      ylab("Lung cancer incidence per 100,000") + 
      scale_x_continuous(name="Year", breaks = c(2006, 2009, 2013)) + 
      scale_colour_discrete(breaks = c("Observed", "IMPACTNCD"),
                            labels = c("Observed   ", expression(bold("IMPACT"["NCD"])))) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5, size = 12)) + 
      theme(axis.text.y  = element_text(angle=0, size = 10)) + 
      #ggtitle("c34 Mortality Validation (Men)") +
      theme(text = element_text(family="Calibri", face = "bold", size = 16)) +
      theme(plot.title = element_text(family = "Calibri", size = 16)) + 
      theme(legend.title = element_blank(),  
            legend.text = element_text(family = "Calibri", size = 16, face = "bold"),
            legend.key.size = unit(2.5, "lines")) +
      theme(strip.text.x = element_text(size = 16, face = "bold"),
            strip.text.y = element_text(size = 16, face = "bold", angle=-90),
            strip.background = element_rect(colour="purple", fill="#CCCCFF"))
    #print(c34.incid.a.rates)
    
    c34.incid = copy(c34.burden[group=="S" & scenario == "Current Policy",])
    
    c34.incid <- c34.incid[, .(pop.mod = sum(pop),
                               cases.mod = sum(c34.incidence)),
                           by = .(sex, scenario, mc, year)]
    
    c34cases <- fread("./Cancer Statistics/C34cases.csv")
    
    c34cases[, `:=` (grp = "Observed", site = NULL)]
    
    tt <- mortqimd[icd=="C33-C34" & agegroup %in% unique(agegroup.fn(30:84)), 
                   .(pop = sum(pop)), by = .(year, sex, agegroup)]
    
    
    c34cases[tt[between(year, 2006, 2013)], pop := pop, on = c("year", "agegroup", "sex")]
    c34cases <- c34cases[agegroup %in% unique(agegroup.fn(30:84)), .(cases = sum(cases),
                                                                     pop = sum(pop)),
                         by = .(year, sex, grp)]
    c34cases[, mean := cases/pop]
    
    grp <- c("year", "sex")
    
    c34.incid <- c34.incid[,
                           MC.mean(cases.mod/pop.mod),
                           by = grp]
    c34.incid[c34cases[between(year, 2006, 2013)], pop := pop, on = c("year", "sex")]
    
    c34.incid[, `:=` (grp = "IMPACTNCD", cases = mean * pop,
                      lui.pop = lui * pop, uui.pop = uui * pop)]
    
    ttt <- rbind(c34.incid, c34cases, fill = T)
    
    c34.incid <- ggplot(ttt[between(year, 2006, 2013),],
                        aes(x=year, y=cases, colour=grp)) + 
      geom_line(position=pd, size = 1, alpha = 3/5) +
      #geom_errorbar(aes(ymin = lui.pop, ymax = uui.pop), width=.05, position=pd, alpha=3/5) +
      #geom_smooth(method=lm, se=F) +
      facet_grid(sex ~ .) +
      ylab("Lung cancer cases") + 
      scale_x_continuous(name="Year", breaks = c(2006, 2009, 2013)) + 
      scale_colour_discrete(breaks = c("Observed", "IMPACTNCD"),
                            labels = c("Observed   ", expression(bold("IMPACT"["NCD"])))) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5, size = 12)) + 
      theme(axis.text.y  = element_text(angle=0, size = 10)) + 
      #ggtitle("c34 Mortality Validation (Men)") +
      theme(text = element_text(family="Calibri", face = "bold", size = 16)) +
      theme(plot.title = element_text(family = "Calibri", size = 16)) + 
      theme(legend.title = element_blank(),  
            legend.text = element_text(family = "Calibri", size = 16, face = "bold"),
            legend.key.size = unit(2.5, "lines")) +
      theme(strip.text.x = element_text(size = 16, face = "bold"),
            strip.text.y = element_text(size = 16, face = "bold", angle=-90),
            strip.background = element_rect(colour="purple", fill="#CCCCFF"))
    #print(c34.incid)
    c34.incid.e <- c34.incid + geom_errorbar(aes(ymin = lui.pop, ymax = uui.pop), width=.05, position=pd, alpha=3/5)
    
    c34.incid.rates <- ggplot(ttt[between(year, 2006, 2013),],
                              aes(x=year, y=mean*1e5, colour=grp)) + 
      geom_line(position=pd, size = 1, alpha = 3/5) +
      #geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.05, position=pd, alpha=3/5) +
      #geom_smooth(method=lm, se=F) +
      facet_grid(sex ~ .) +
      ylab("Lung cancer incidence per 100,000") + 
      scale_x_continuous(name="Year", breaks = c(2006, 2009, 2013)) + 
      scale_colour_discrete(breaks = c("Observed", "IMPACTNCD"),
                            labels = c("Observed   ", expression(bold("IMPACT"["NCD"])))) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5, size = 12)) + 
      theme(axis.text.y  = element_text(angle=0, size = 10)) + 
      #ggtitle("c34 Mortality Validation (Men)") +
      theme(text = element_text(family="Calibri", face = "bold", size = 16)) +
      theme(plot.title = element_text(family = "Calibri", size = 16)) + 
      theme(legend.title = element_blank(),  
            legend.text = element_text(family = "Calibri", size = 16, face = "bold"),
            legend.key.size = unit(2.5, "lines")) +
      theme(strip.text.x = element_text(size = 16, face = "bold"),
            strip.text.y = element_text(size = 16, face = "bold", angle=-90),
            strip.background = element_rect(colour="purple", fill="#CCCCFF"))
    #print(c34.incid.rates)
    c34.incid.rates.e <- c34.incid.rates + geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.05, position=pd, alpha=3/5)
    
    ggsave.mine(paste0(dir, "lung ca incid", ext), c34.incid)
    ggsave.mine(paste0(dir, "lung ca incid.rates", ext), c34.incid.rates)
    ggsave.mine(paste0(dir, "lung ca incid error", ext), c34.incid.e)
    ggsave.mine(paste0(dir, "lung ca incid rates error", ext), c34.incid.rates.e)
    ggsave.mine(paste0(dir, "lung ca incid.a", ext), c34.incid.a)
    ggsave.mine(paste0(dir, "lung ca incid.a.rates", ext), c34.incid.a.rates)
    ggsave.mine(paste0(dir, "lung ca men",   ext), c34.men)
    ggsave.mine(paste0(dir, "lung ca women", ext), c34.women)
  }
}

# Gastric cancer validation -----------------------------------------------
#load("./Output/Gastric ca/c16.burden.RData")
mcjobs[[3]] <- function () {
  if (exists("c16.burden")) {
    load(file="./Validation/c16.drates.RData")
    c16.drates[, Model:="BAMP"]
    c16.drates[, `:=` (lui = mean, uui = mean)] # ignore BAMP CI
    
    c16.mort = copy(c16.burden[group=="SAQ" & scenario == "Current Policy",])
    c16.mort = copy(c16.mort[agegroup!="30-34",])
    c16.mort[agegroup %in% c("35-39", "40-44"), agegroup := "35-44"]
    c16.mort[agegroup %in% c("45-49", "50-54"), agegroup := "45-54"]
    c16.mort[agegroup %in% c("55-59", "60-64"), agegroup := "55-64"]
    c16.mort[agegroup %in% c("65-69", "70-74"), agegroup := "65-74"]
    c16.mort[agegroup %in% c("75-79", "80-84"), agegroup := "75-84"]
    c16.mort <- c16.mort[, .(pop = sum(pop),
                             c16.mortality = sum(c16.mortality)),
                         by = .(agegroup, sex, qimd, scenario, mc, year)]
    
    grp <- c("year", "scenario", "sex", "agegroup", "qimd")
    
    tt <- mortqimd[icd=="C16" & agegroup %in% unique(agegroup.fn(30:84))]
    tt = copy(tt[agegroup!="30-34",])
    tt[agegroup %in% c("35-39", "40-44"), agegroup := "35-44"]
    tt[agegroup %in% c("45-49", "50-54"), agegroup := "45-54"]
    tt[agegroup %in% c("55-59", "60-64"), agegroup := "55-64"]
    tt[agegroup %in% c("65-69", "70-74"), agegroup := "65-74"]
    tt[agegroup %in% c("75-79", "80-84"), agegroup := "75-84"]
    tt <- tt[, .(pop = sum(pop),
                 c16.mortality = sum(deaths)),
             by = .(agegroup, sex, qimd, year)]
    tt[, `:=` (Model = "Observed", mean = c16.mortality / pop)]
    
    c16.mort <- c16.mort[,
                         MC.mean(c16.mortality/pop),
                         by = grp]
    
    c16.mort[, `:=` (Model= "IMPACTNCD", scenario = NULL)]
    c16.mort <- rbind(c16.mort,c16.drates, tt, fill=T)
    c16.mort <- c16.mort[agegroup!="85+" & between(year, 2002, init.year + yearstoproject)]
    
    c16.men <- ggplot(c16.mort[sex=="Men",],
                      aes(x=year, y=mean*100000, colour=Model, ymin = 0)) + 
      #geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.05, position=pd, alpha=3/5) +
      geom_smooth(method=lm, se=F) +
      geom_line(position=pd,size = 2, alpha=3/5) +
      facet_grid(agegroup ~ qimd, labeller=qimd_labeller, scales="free") +
      ylab("Mortality per 100,000") + scale_x_continuous(name="Year") + 
      scale_colour_discrete(breaks = c("BAMP", "IMPACTNCD", "Observed"),
                            labels = c("BAMP         ", expression(IMPACT[NCD]),
                                       "Observed  ")) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
      ggtitle("Gastric Cancer Mortality Validation (Men)") +
      theme(text = element_text(family="Calibri", size = 12)) +
      theme(plot.title = element_text(family="Calibri", face="bold", size=16))+ 
      theme(strip.text.x = element_text(size = 10, face = "bold"),
            strip.text.y = element_text(size = 10, face = "bold"),
            strip.background = element_rect(colour="purple", fill="#CCCCFF"))
    
    
    c16.women <- ggplot(c16.mort[sex=="Women",],
                        aes(x=year, y=mean*100000, colour=Model, ymin = 0)) + 
      #geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.05, position=pd, alpha=3/5) +
      #geom_smooth(size = 2, alpha=1/4) +
      geom_line(position=pd,size = 2, alpha=3/5) +
      facet_grid(agegroup ~ qimd,  labeller=qimd_labeller, scales="free") +
      ylab("Mortality per 100,000") + scale_x_continuous(name="Year") + 
      scale_colour_discrete(breaks = c("BAMP", "IMPACTNCD", "Observed"),
                            labels = c("BAMP         ", expression(IMPACT[NCD]),
                                       "Observed  ")) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
      ggtitle("Gastric Cancer Mortality Validation (Women)") +
      theme(text = element_text(family="Calibri", size = 12)) +
      theme(plot.title = element_text(family="Calibri", face="bold", size=16))+ 
      theme(strip.text.x = element_text(size = 10, face = "bold"),
            strip.text.y = element_text(size = 10, face = "bold"),
            strip.background = element_rect(colour="purple", fill="#CCCCFF"))
    
    #print(c16.men)
    #print(c16.women) 
    
    
    # ggsave("./Validation/validation men c16.pdf", c16.men, units = "in", device=cairo_pdf, family="Calibri", antialias = "subpixel", dpi=1200, width = 11.69, height = 8.27)
    # ggsave("./Validation/validation women c16.pdf", c16.women, units = "in", device=cairo_pdf, family="Calibri", antialias = "subpixel", dpi=1200, width = 11.69, height = 8.27)
    
    c16.incid = copy(c16.burden[group=="SAQ" & scenario == "Current Policy",])
    
    c16.incid <- c16.incid[, .(pop.mod = sum(pop),
                               cases.mod = sum(c16.incidence)),
                           by = .(agegroup, sex, scenario, mc, year)]
    
    c16cases <- fread("./Cancer Statistics/C16cases.csv")
    
    c16cases[, `:=` (grp = "Observed", site = NULL)]
    
    tt <- mortqimd[icd=="C16" & agegroup %in% unique(agegroup.fn(30:84)), 
                   .(pop = sum(pop)), by = .(year, sex, agegroup)]
    
    c16cases[tt[between(year, 2006, 2013)], pop := pop, on = c("year", "agegroup", "sex")]
    c16cases[, mean := cases / pop]
    grp <- c("year", "sex", "agegroup")
    
    c16.incid <- c16.incid[,
                           MC.mean(cases.mod/pop.mod),
                           by = grp]
    c16.incid[tt[between(year, 2006, 2013)], pop := pop, on = c("year", "agegroup", "sex")]
    
    c16.incid[, `:=` (grp = "IMPACTNCD", cases = mean * pop,
                      lui.pop = lui * pop, uui.pop = uui * pop)]
    
    ttt <- rbind(c16.incid, c16cases, fill = T)[agegroup %in% unique(agegroup.fn(30:84)), ]
    
    c16.incid.a <- ggplot(ttt[between(year, 2006, 2013),],
                          aes(x=year, y=cases, colour=grp)) + 
      geom_line(position=pd, size = 1, alpha = 3/5) +
      #geom_errorbar(aes(ymin= lui.pop, ymax = uui.pop), width=.05, position=pd, alpha=3/5) +
      #geom_smooth(method=lm, se=F) +
      facet_grid(sex ~ agegroup, scales="free") +
      ylab("Gastric cancer cases") + 
      scale_x_continuous(name="Year", breaks = c(2006, 2009, 2013)) + 
      scale_colour_discrete(breaks = c("Observed", "IMPACTNCD"),
                            labels = c("Observed   ", expression(bold("IMPACT"["NCD"])))) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5, size = 12)) + 
      theme(axis.text.y  = element_text(angle=0, size = 10)) + 
      #ggtitle("c16 Mortality Validation (Men)") +
      theme(text = element_text(family="Calibri", face = "bold", size = 16)) +
      theme(plot.title = element_text(family = "Calibri", size = 16)) + 
      theme(legend.title = element_blank(),  
            legend.text = element_text(family = "Calibri", size = 16, face = "bold"),
            legend.key.size = unit(2.5, "lines")) +
      theme(strip.text.x = element_text(size = 16, face = "bold"),
            strip.text.y = element_text(size = 16, face = "bold", angle=-90),
            strip.background = element_rect(colour="purple", fill="#CCCCFF"))
    #print(c16.incid.a)
    
    c16.incid.a.rates <- ggplot(ttt[between(year, 2006, 2013),],
                                aes(x=year, y=mean * 1e5, colour=grp)) + 
      geom_line(position=pd, size = 1, alpha = 3/5) +
      #geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.05, position=pd, alpha=3/5) +
      #geom_smooth(method=lm, se=F) +
      facet_grid(sex ~ agegroup, scales="free") +
      ylab("Gastric cancer incidence per 100,000") + 
      scale_x_continuous(name="Year", breaks = c(2006, 2009, 2013)) + 
      scale_colour_discrete(breaks = c("Observed", "IMPACTNCD"),
                            labels = c("Observed   ", expression(bold("IMPACT"["NCD"])))) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5, size = 12)) + 
      theme(axis.text.y  = element_text(angle=0, size = 10)) + 
      #ggtitle("c16 Mortality Validation (Men)") +
      theme(text = element_text(family="Calibri", face = "bold", size = 16)) +
      theme(plot.title = element_text(family = "Calibri", size = 16)) + 
      theme(legend.title = element_blank(),  
            legend.text = element_text(family = "Calibri", size = 16, face = "bold"),
            legend.key.size = unit(2.5, "lines")) +
      theme(strip.text.x = element_text(size = 16, face = "bold"),
            strip.text.y = element_text(size = 16, face = "bold", angle=-90),
            strip.background = element_rect(colour="purple", fill="#CCCCFF"))
    #print(c16.incid.a.rates)
    
    c16.incid = copy(c16.burden[group=="S" & scenario == "Current Policy",])
    
    c16.incid <- c16.incid[, .(pop.mod = sum(pop),
                               cases.mod = sum(c16.incidence)),
                           by = .(sex, scenario, mc, year)]
    
    c16cases <- fread("./Cancer Statistics/C16cases.csv")
    
    c16cases[, `:=` (grp = "Observed", site = NULL)]
    
    tt <- mortqimd[icd=="C16" & agegroup %in% unique(agegroup.fn(30:84)), 
                   .(pop = sum(pop)), by = .(year, sex, agegroup)]
    
    
    c16cases[tt[between(year, 2006, 2013)], pop := pop, on = c("year", "agegroup", "sex")]
    c16cases <- c16cases[agegroup %in% unique(agegroup.fn(30:84)), .(cases = sum(cases),
                                                                     pop = sum(pop)),
                         by = .(year, sex, grp)]
    c16cases[, mean := cases/pop]
    
    grp <- c("year", "sex")
    
    c16.incid <- c16.incid[,
                           MC.mean(cases.mod/pop.mod),
                           by = grp]
    c16.incid[c16cases[between(year, 2006, 2013)], pop := pop, on = c("year", "sex")]
    
    c16.incid[, `:=` (grp = "IMPACTNCD", cases = mean * pop,
                      lui.pop = lui * pop, uui.pop = uui * pop)]
    
    ttt <- rbind(c16.incid, c16cases, fill = T)
    
    c16.incid <- ggplot(ttt[between(year, 2006, 2013),],
                        aes(x=year, y=cases, colour=grp)) + 
      geom_line(position=pd, size = 1, alpha = 3/5) +
      #geom_errorbar(aes(ymin = lui.pop, ymax = uui.pop), width=.05, position=pd, alpha=3/5) +
      #geom_smooth(method=lm, se=F) +
      facet_grid(sex ~ .) +
      ylab("Gastric cancer cases") + 
      scale_x_continuous(name="Year", breaks = c(2006, 2009, 2013)) + 
      scale_colour_discrete(breaks = c("Observed", "IMPACTNCD"),
                            labels = c("Observed   ", expression(bold("IMPACT"["NCD"])))) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5, size = 12)) + 
      theme(axis.text.y  = element_text(angle=0, size = 10)) + 
      #ggtitle("c16 Mortality Validation (Men)") +
      theme(text = element_text(family="Calibri", face = "bold", size = 16)) +
      theme(plot.title = element_text(family = "Calibri", size = 16)) + 
      theme(legend.title = element_blank(),  
            legend.text = element_text(family = "Calibri", size = 16, face = "bold"),
            legend.key.size = unit(2.5, "lines")) +
      theme(strip.text.x = element_text(size = 16, face = "bold"),
            strip.text.y = element_text(size = 16, face = "bold", angle=-90),
            strip.background = element_rect(colour="purple", fill="#CCCCFF"))
    #print(c16.incid)
    c16.incid.e <- c16.incid + geom_errorbar(aes(ymin = lui.pop, ymax = uui.pop), width=.05, position=pd, alpha=3/5)
    
    c16.incid.rates <- ggplot(ttt[between(year, 2006, 2013),],
                              aes(x=year, y=mean*1e5, colour=grp)) + 
      geom_line(position=pd, size = 1, alpha = 3/5) +
      #geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.05, position=pd, alpha=3/5) +
      #geom_smooth(method=lm, se=F) +
      facet_grid(sex ~ .) +
      ylab("Gastric cancer incidence per 100,000") + 
      scale_x_continuous(name="Year", breaks = c(2006, 2009, 2013)) + 
      scale_colour_discrete(breaks = c("Observed", "IMPACTNCD"),
                            labels = c("Observed   ", expression(bold("IMPACT"["NCD"])))) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5, size = 12)) + 
      theme(axis.text.y  = element_text(angle=0, size = 10)) + 
      #ggtitle("c16 Mortality Validation (Men)") +
      theme(text = element_text(family="Calibri", face = "bold", size = 16)) +
      theme(plot.title = element_text(family = "Calibri", size = 16)) + 
      theme(legend.title = element_blank(),  
            legend.text = element_text(family = "Calibri", size = 16, face = "bold"),
            legend.key.size = unit(2.5, "lines")) +
      theme(strip.text.x = element_text(size = 16, face = "bold"),
            strip.text.y = element_text(size = 16, face = "bold", angle=-90),
            strip.background = element_rect(colour="purple", fill="#CCCCFF"))
    #print(c16.incid.rates)
    c16.incid.rates.e <- c16.incid.rates + geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.05, position=pd, alpha=3/5)
    
    c16.mort = copy(c16.burden[group=="SAQ" & scenario == "Current Policy",])
    
    
    c16.mort <- c16.mort[, .(pop = sum(pop),
                             c16.mortality = sum(c16.mortality)),
                         by = .(sex, scenario, mc, year)]
    
    tt <- mortqimd[icd=="C16" & agegroup %in% unique(agegroup.fn(30:84))]
    tt <- tt[, .(pop = sum(pop),
                 deaths = sum(deaths)),
             by = .(sex, year)]
    tt[, `:=` (grp = "Observed", icd = NULL, cause = NULL, mean = deaths/pop)]
    
    grp <- c("year", "sex")
    
    c16.mort <- c16.mort[,
                         MC.mean(c16.mortality/pop),
                         by = grp]
    c16.mort[, `:=` (grp= "IMPACTNCD")]
    
    ttt <- rbind(c16.mort, tt, fill = T)
    
    c16.sum <- ggplot(ttt[between(year, 2002, 2018),],
                      aes(x=year, y=mean*100000, colour=grp, ymin = 0)) +
      geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.05, position=pd, alpha=3/5)  +
      geom_line(position=pd, size = 1, alpha = 3/5) +
      facet_grid(. ~ sex, scales="free", labeller=qimd_labeller) +
      ylab("Mortality per 100,000") + 
      scale_x_continuous(name="Year", breaks = c(2002, 2006, 2010, 2014, 2018)) + 
      scale_colour_discrete(breaks = c("Observed", "IMPACTNCD"),
                            labels = c("Observed   ", expression(bold("IMPACT"["NCD"])))) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5, size = 12)) + 
      theme(axis.text.y  = element_text(angle=0, size = 10)) + 
      #ggtitle("c16 Mortality Validation (Men)") +
      theme(text = element_text(family="Calibri", face = "bold", size = 12)) +
      theme(plot.title = element_text(family="Calibri", size=14)) + 
      theme(legend.title = element_blank(),  
            legend.text = element_text(family="Calibri", size = 14, face = "bold"),
            legend.key.size = unit(2.5, "lines")) +
      theme(strip.text.x = element_text(size = 12, face = "bold"),
            strip.text.y = element_text(size = 12, face = "bold", angle=0),
            strip.background = element_rect(colour="purple", fill="#CCCCFF"))
    
    ggsave.mine(paste0(dir, "gastric cancer sum", ext), c16.sum)
    ggsave.mine(paste0(dir, "gastric ca incid", ext), c16.incid)
    ggsave.mine(paste0(dir, "gastric ca incid.rates", ext), c16.incid.rates)
    ggsave.mine(paste0(dir, "gastric ca incid error", ext), c16.incid.e)
    ggsave.mine(paste0(dir, "gastric ca incid rates error", ext), c16.incid.rates.e)
    ggsave.mine(paste0(dir, "gastric ca incid.a", ext), c16.incid.a)
    ggsave.mine(paste0(dir, "gastric ca incid.a.rates", ext), c16.incid.a.rates)
    ggsave.mine(paste0(dir, "gastric ca men", ext), c16.men)
    ggsave.mine(paste0(dir, "gastric ca women", ext), c16.women)
  }
}

# Salt graph 20-64 ------------------------------------------------------------------
mcjobs[[4]] <- function () {
  pop.st <- pop.abs[, mean(pop), by = .(year, agegroup, sex, scenario, qimd)
                    ][,
                      list("pop" = sum(V1)), by = .(year, agegroup, sex, scenario)
                      ]
  
  dt = copy(Tables$salt.ca.SA[scenario == "Current Policy", ])
  dt <- merge(dt, pop.st, by = c("year", "agegroup", "sex", "scenario"), all.x = T)
  dt[, year := year.calag]
  dt <- dt[agegroup %in% unique(agegroup.fn((20 + cancer.lag):(64 + cancer.lag)))] # 20-64 in 2001 are 30-74 in 2011
  
  dt <- dt[, list("mean" = sum(mean * pop) / sum(pop),
                  "lui"  = sum(lui * pop) / sum(pop),
                  "uui"  = sum(uui * pop) / sum(pop)),
           by = .(sex, scenario, year)]
  
  
  # observed salt consumption
  fix.hplc <- 0.98 # correction for different methods of PABA estimation
  dt2 <- data.table("year" = rep(c(2001, 2006, 2008, 2011), 2),
                    "sex" = rep(c("Men", "Women"), each = 4),
                    "mean" = c(10.96 * fix.hplc,	10 * fix.hplc,
                               9.64 * fix.hplc, 9.3, 8.1 * fix.hplc,
                               7.54 * fix.hplc, 7.66 * fix.hplc,	6.84),
                    "lui" = c(10.39 * (fix.hplc - .01),	9.45 * (fix.hplc - .01),
                              9.18 * (fix.hplc - .01),	8.61, 7.76 * (fix.hplc - .01),
                              7.21 * (fix.hplc - .01),	7.21 * (fix.hplc - .01),	6.44),
                    "uui" = c(11.53 * (fix.hplc + .01),	10.56 * (fix.hplc + .01),
                              10.1 * (fix.hplc + .01),	9.99, 8.44 * (fix.hplc + .01),
                              7.88 * (fix.hplc + .01),	8.11 * (fix.hplc + .01),	7.23),
                    "scenario" = rep("Observed", 8)
  )
  
  dt <- rbind(dt, dt2, fill = T)
  
  salt <- 
    ggplot(dt[between(year, 2001, 2011)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_point(size = 2, alpha = 4/5) +
    geom_smooth(method=lm, se=F) +
    facet_grid(sex ~ .) +
    scale_x_continuous(name = "Year", breaks = c(2001:2012)) +
    scale_y_continuous(name = "Salt consumption (g)",
                       limits = c(6, 12),
                       breaks = c(6, 9, 12)) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Salt consumption by sex (ages 19 - 64)") +
    theme(text = element_text(family = "Calibri", size = 14)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 16)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  
  # print(salt)
  
  ggsave.mine(paste0(dir, "salt.s19", ext), salt)
}

# Salt graph agegroup 20-64 ------------------------------------------------------------------
mcjobs[[5]] <- function () {
  dt = copy(Tables$salt.ca.SA[scenario == "Current Policy", ])
  dt <- merge(dt, pop.abs, by = c("year", "agegroup", "sex", "scenario"), all.x = T)
  dt[, year := year.calag]
  dt <- dt[agegroup %in% unique(agegroup.fn((20 + cancer.lag):(64 + cancer.lag)))] # 20-64 in 2001 are 30-74 in 2011
  dt[agegroup %in% c("30-34", "35-39", "40-44"), agegroup := "19-34"]
  dt[agegroup %in% c("45-49", "50-54", "55-59"), agegroup := "35-49"]
  dt[agegroup %in% c("60-64", "65-69", "70-74"), agegroup := "50-64"]
  dt[, agegroup := factor(agegroup)]
  
  dt <- dt[, list("mean" = sum(mean * pop) / sum(pop),
                  "lui"  = sum(lui * pop) / sum(pop),
                  "uui"  = sum(uui * pop) / sum(pop)),
           by = .(sex, scenario, year, agegroup)]
  
  # 2001
  m.1.1.m <- weighted.mean(c(11.4, 11), c(152, 62) )
  s.1.1.m <- 5.79 / sqrt(152 + 62)
  m.1.2.m <- 11.1
  s.1.2.m <- 4.83 / sqrt(170)
  m.1.3.m <- 10.5
  s.1.3.m <- 4.95 / sqrt(183)
  m.1.1.w <- weighted.mean(c(9.1, 8.7), c(60, 129) )
  s.1.1.w <- 4.62 / sqrt(60 + 129)
  m.1.2.w <- 8
  s.1.2.w <- 3.42 / sqrt(203)
  m.1.3.w <- 7.5
  s.1.3.w <- 3.45 / sqrt(187)
  
  # 2006
  m.6.1.m <- weighted.mean(c(11, 9.9), c(26, 45) )
  s.6.1.m <- 4.06 / sqrt(26 + 45)
  m.6.2.m <- 10.1
  s.6.2.m <- 0.44
  m.6.3.m <- 10.2
  s.6.3.m <- 0.49
  m.6.1.w <- weighted.mean(c(7, 8.8), c(13, 49) )
  s.6.1.w <- 3.1 / sqrt(13 + 49)
  m.6.2.w <- 7.9
  s.6.2.w <- 0.31
  m.6.3.w <- 6.8
  s.6.3.w <- 0.33
  
  # 2008
  m.8.1.m <- weighted.mean(c(10.67, 10.16), c(9, 37) )
  s.8.1.m <- 4.42 / sqrt(9 + 37)
  m.8.2.m <- 9.5
  s.8.2.m <- 0.39
  m.8.3.m <- 9.3
  s.8.3.m <- 0.26
  m.8.1.w <- weighted.mean(c(10.01, 8.08), c(7, 54) )
  s.8.1.w <- 4.03 / sqrt(7 + 54)
  m.8.2.w <- 7.41
  s.8.2.w <- 0.23
  m.8.3.w <- 6.97
  s.8.3.w <- 0.22
  
  # 2011
  m.11.1.m <- 9.5
  s.11.1.m <- 0.6
  m.11.2.m <- 10
  s.11.2.m <- 0.4
  m.11.3.m <- 8.2
  s.11.3.m <- 0.4
  m.11.1.w <- 7.1
  s.11.1.w <- 0.5
  m.11.2.w <- 6.8
  s.11.2.w <- 0.3
  m.11.3.w <- 6.6
  s.11.3.w <- 0.3
  
  # observed salt consumption
  fix.hplc <- 0.98 # correction for different methods of PABA estimation
  dt2 <- data.table("year" = rep(c(2001, 2006, 2008, 2011), 6),
                    "sex"  = rep(c("Men", "Women"), each = 12),
                    "agegroup" = rep(c("19-34", "35-49", "50-64"), each = 4),
                    "mean" = c(m.1.1.m * fix.hplc,	m.6.1.m * fix.hplc,
                               m.8.1.m * fix.hplc,  m.11.1.m * fix.hplc,
                               m.1.2.m * fix.hplc,	m.6.2.m * fix.hplc,
                               m.8.2.m * fix.hplc,  m.11.2.m * fix.hplc,
                               m.1.3.m * fix.hplc,	m.6.3.m * fix.hplc,
                               m.8.3.m * fix.hplc,  m.11.3.m * fix.hplc,
                               m.1.1.w * fix.hplc,	m.6.1.w * fix.hplc,
                               m.8.1.w * fix.hplc,  m.11.1.w * fix.hplc,
                               m.1.2.w * fix.hplc,	m.6.2.w * fix.hplc,
                               m.8.2.w * fix.hplc,  m.11.2.w * fix.hplc,
                               m.1.3.w * fix.hplc,	m.6.3.w * fix.hplc,
                               m.8.3.w * fix.hplc,  m.11.3.w * fix.hplc
                    ),
                    "se" = c(s.1.1.m,	s.6.1.m,
                             s.8.1.m, s.11.1.m,
                             s.1.2.m,	s.6.2.m,
                             s.8.2.m, s.11.2.m,
                             s.1.3.m,	s.6.3.m,
                             s.8.3.m, s.11.3.m,
                             s.1.1.w,	s.6.1.w,
                             s.8.1.w, s.11.1.w,
                             s.1.2.w,	s.6.2.w,
                             s.8.2.w, s.11.2.w,
                             s.1.3.w,	s.6.3.w,
                             s.8.3.w, s.11.3.w
                    ),
                    "scenario" = rep("Observed", 24)
  )
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  
  dt <- rbind(dt, dt2, fill = T)
  rm(s.1.1.m,	s.6.1.m, s.8.1.m, s.11.1.m,
     s.1.2.m,	s.6.2.m, s.8.2.m, s.11.2.m,
     s.1.3.m,	s.6.3.m, s.8.3.m, s.11.3.m,
     s.1.1.w,	s.6.1.w, s.8.1.w, s.11.1.w,
     s.1.2.w,	s.6.2.w, s.8.2.w, s.11.2.w,
     s.1.3.w,	s.6.3.w, s.8.3.w, s.11.3.w,
     m.1.1.m,	 m.6.1.m,
     m.8.1.m, m.11.1.m,
     m.1.2.m,	 m.6.2.m,
     m.8.2.m, m.11.2.m,
     m.1.3.m,	 m.6.3.m,
     m.8.3.m, m.11.3.m,
     m.1.1.w,	 m.6.1.w,
     m.8.1.w, m.11.1.w,
     m.1.2.w,	 m.6.2.w,
     m.8.2.w, m.11.2.w,
     m.1.3.w,	 m.6.3.w,
     m.8.3.w, m.11.3.w)
  
  salt <- 
    ggplot(dt[between(year, 2001, 2011)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_point(size = 2, alpha = 4/5) +
    geom_smooth(method=lm, se=F) +
    facet_grid(sex ~ agegroup) +
    scale_x_continuous(name = "Year", breaks = c(2001:2012)) +
    scale_y_continuous(name = "Salt consumption (g)",
                       limits = c(5, 12),
                       breaks = c(6, 9, 12)) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Salt consumption by sex (ages 19 - 64)") +
    theme(text = element_text(family = "Calibri", size = 10)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 14)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  
  # print(salt)
  
  ggsave.mine(paste0(dir, "salt.s19.a", ext), salt)
}

# SBP validation ----------------------------------------------------------
mcjobs[[6]] <- function () {
  dt = copy(Tables$sbp.S[scenario == "Current Policy", ])
  dt[, year := year.cvdlag]
  
  HSE.ts = copy(HSE.ts2)
  HSE.ts[, age := age + cvd.lag]
  agegroup.fn(HSE.ts)
  HSE.ts.srv.nurse <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.nurse, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.nurse <- subset(HSE.ts.srv.nurse, age >= ageL & age <= ageH & wt.nurse > 0 & 
                               is.na(omsysval) == F)
  
  dt2 <- data.table(svyby(~omsysval, ~sex + year, HSE.ts.srv.nurse, svymean, na.rm = T))
  setnames(dt2, "omsysval", "mean")
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, year := year + 2011]
  dt2[, scenario := "Observed"]
  dt <- rbind(dt, dt2, fill = T)
  
  sbp <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      ) 
    ) +
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    facet_grid(sex ~ .) +
    scale_x_continuous(name = "Year", breaks = c(sy:2013)) +
    scale_y_continuous(name = "Systolic blood pressure (mmHg)",
                       limits = c(120, 135),
                       breaks = c(120, 125, 130, 135)) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Mean systolic blood pressure (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 14)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 16)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  
  # print(sbp)
  
  ggsave.mine(paste0(dir, "sbp", ext), sbp)
}

# SBP QIMD validation ----------------------------------------------------------
mcjobs[[7]] <- function () {
  dt = copy(Tables$sbp.SQ[scenario == "Current Policy", ])
  dt[, year := year.cvdlag]
  
  HSE.ts = copy(HSE.ts2)
  HSE.ts[, age := age + cvd.lag]
  agegroup.fn(HSE.ts)
  HSE.ts.srv.nurse <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.nurse, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.nurse <- subset(HSE.ts.srv.nurse, age >= ageL & age <= ageH & wt.nurse > 0 & 
                               !is.na(omsysval) & !is.na(qimd))
  
  dt2 <- data.table(svyby(~omsysval, ~sex + qimd + year, HSE.ts.srv.nurse, svymean, na.rm = T))
  setnames(dt2, "omsysval", "mean")
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, year := year + 2011]
  dt2[, scenario := "Observed"]
  dt <- rbind(dt, dt2, fill = T)
  
  sbp <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      ) 
    ) +
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    facet_grid(sex ~ qimd, labeller = qimd_labeller) +
    scale_x_continuous(name = "Year", breaks = c(sy:2013)) +
    scale_y_continuous(name = "Systolic blood pressure (mmHg)",
                       limits = c(115, 135),
                       breaks = c(115, 120, 125, 130, 135)) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Mean systolic blood pressure (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 14)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 16)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  
  # print(sbp)
  
  ggsave.mine(paste0(dir, "sbp.q", ext), sbp)
}

# SBP agegroup validation ----------------------------------------------------------
mcjobs[[8]] <- function () {
  dt = copy(Tables$sbp.SA[scenario == "Current Policy", ])
  dt[, year := year.cvdlag]
  dt <- dt[agegroup %in% unique(agegroup.fn(ageL:ageH)) ,]
  
  HSE.ts = copy(HSE.ts2)
  HSE.ts[, age := age + cvd.lag]
  agegroup.fn(HSE.ts)
  HSE.ts.srv.nurse <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.nurse, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.nurse <- subset(HSE.ts.srv.nurse, age >= ageL & age <= ageH & wt.nurse > 0 & 
                               !is.na(omsysval) & !is.na(agegroup))
  
  dt2 <- data.table(svyby(~omsysval, ~sex + agegroup + year, HSE.ts.srv.nurse, svymean, na.rm = T))
  setnames(dt2, "omsysval", "mean")
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, year := year + 2011]
  dt2[, scenario := "Observed"]
  dt <- rbind(dt, dt2, fill = T)
  
  sbp <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      ) 
    ) +
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    facet_grid(sex ~ agegroup) +
    scale_x_continuous(name = "Year", breaks = seq(sy, 2013, 3)) +
    scale_y_continuous(name = "Systolic blood pressure (mmHg)") + #,
    #limits = c(115, 135),
    #breaks = c(115, 120, 125, 130, 135)) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Mean systolic blood pressure (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 10)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 14)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  
  # print(sbp)
  
  ggsave.mine(paste0(dir, "sbp.a", ext), sbp)
}

# TC validation -----------------------------------------------------------
mcjobs[[9]] <- function () {
  dt = copy(Tables$tc.S[scenario == "Current Policy", ])
  dt[, year := year.cvdlag]
  
  HSE.ts = copy(HSE.ts2)
  HSE.ts[, age := age + cvd.lag]
  agegroup.fn(HSE.ts)
  HSE.ts.srv.blood <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.blood, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.blood <- subset(HSE.ts.srv.blood, age >= ageL & age <= ageH & wt.nurse > 0 & 
                               is.na(cholval1) == F)
  
  dt2 <- setnames(data.table(svyby(~cholval1, ~sex + year, HSE.ts.srv.blood, svymean, na.rm = T)), "cholval1", "mean")
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  
  tc <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    facet_grid(sex ~ .) +
    scale_x_continuous(name = "Year", breaks = c(sy:2013)) +
    scale_y_continuous(name = "Cholesterol (mmol/L)") +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Mean total cholesterol (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 14)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 16)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(tc)
  
  ggsave.mine(paste0(dir, "tc", ext), tc)
}

# TC QIMD validation -----------------------------------------------------------
mcjobs[[10]] <- function () {
  dt = copy(Tables$tc.SQ[scenario == "Current Policy", ])
  dt[, year := year.cvdlag]
  
  HSE.ts = copy(HSE.ts2)
  HSE.ts[, age := age + cvd.lag]
  agegroup.fn(HSE.ts)
  HSE.ts.srv.blood <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.blood, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.blood <- subset(HSE.ts.srv.blood, age >= ageL & age <= ageH & wt.nurse > 0 & 
                               is.na(cholval1) == F & !is.na(qimd))
  
  dt2 <- setnames(data.table(svyby(~cholval1, ~sex + qimd + year, HSE.ts.srv.blood, svymean, na.rm = T)), "cholval1", "mean")
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  
  tc <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    facet_grid(sex ~ qimd, labeller = qimd_labeller) +
    scale_x_continuous(name = "Year", breaks = c(sy:2013)) +
    scale_y_continuous(name = "Cholesterol (mmol/L)") +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Mean total cholesterol (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 14)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 16)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(tc)
  
  ggsave.mine(paste0(dir, "tc.q", ext), tc)
}

# TC agegroup validation -----------------------------------------------------------
mcjobs[[11]] <- function () {
  dt = copy(Tables$tc.SA[scenario == "Current Policy", ])
  dt[, year := year.cvdlag]
  dt <- dt[agegroup %in% unique(agegroup.fn(ageL:ageH)) ,]
  
  HSE.ts = copy(HSE.ts2)
  HSE.ts[, age := age + cvd.lag]
  agegroup.fn(HSE.ts)
  HSE.ts.srv.blood <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.blood, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.blood <- subset(HSE.ts.srv.blood, age >= ageL & age <= ageH & wt.nurse > 0 & 
                               is.na(cholval1) == F & !is.na(agegroup))
  
  dt2 <- setnames(data.table(svyby(~cholval1, ~sex + agegroup + year, HSE.ts.srv.blood, svymean, na.rm = T)), "cholval1", "mean")
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  
  tc <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    facet_grid(sex ~ agegroup) +
    scale_x_continuous(name = "Year", breaks = c(sy, 2010, 2013)) +
    scale_y_continuous(name = "Cholesterol (mmol/L)") +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Mean total cholesterol (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 10)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 14)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(tc)
  
  ggsave.mine(paste0(dir, "tc.a", ext), tc)
}

# BMI validation ----------------------------------------------------------
mcjobs[[12]] <- function () {
  # TODO bmi.cvd /bmi.ca
  dt = copy(Tables$bmi.cvd.S[scenario == "Current Policy", ])
  dt[, year := year.cvdlag]
  
  HSE.ts = copy(HSE.ts2)
  HSE.ts[, age := age + cvd.lag]
  agegroup.fn(HSE.ts)
  HSE.ts.srv.nurse <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.nurse, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.nurse <- subset(HSE.ts.srv.nurse, age >= ageL & age <= ageH & wt.nurse > 0 & 
                               is.na(bmival) == F)
  
  dt2 <- setnames(data.table(svyby(~bmival, ~sex + year, HSE.ts.srv.nurse, svymean, na.rm = T)), "bmival", "mean")
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  bmi <- 
    ggplot(dt[between(year, 2001, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    facet_grid(sex ~ .) +
    scale_x_continuous(name = "Year", breaks = c(2001:2013)) +
    scale_y_continuous(name = expression(
      paste("Body mass index (Kg/", m^bold("2"), ")"))
    ) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Mean body mass index (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 14)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 16)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(bmi)
  
  ggsave.mine(paste0(dir, "bmi", ext), bmi)
}

# BMI QIMD validation ----------------------------------------------------------
mcjobs[[13]] <- function () {
  # TODO bmi.cvd /bmi.ca
  dt = copy(Tables$bmi.cvd.SQ[scenario == "Current Policy", ])
  dt[, year := year.cvdlag]
  
  HSE.ts = copy(HSE.ts2)
  HSE.ts[, age := age + cvd.lag]
  agegroup.fn(HSE.ts)
  HSE.ts.srv.nurse <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.nurse, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.nurse <- subset(HSE.ts.srv.nurse, age >= ageL & age <= ageH & wt.nurse > 0 & 
                               is.na(bmival) == F & !is.na(qimd))
  
  dt2 <- setnames(data.table(svyby(~bmival, ~sex + qimd + year, HSE.ts.srv.nurse, svymean, na.rm = T)), "bmival", "mean")
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  bmi <- 
    ggplot(dt[between(year, 2001, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    facet_grid(sex ~ qimd, labeller = qimd_labeller) +
    scale_x_continuous(name = "Year", breaks = c(2001:2013)) +
    scale_y_continuous(name = expression(
      paste("Body mass index (Kg/", m^bold("2"), ")"))
    ) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Mean body mass index (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 14)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 16)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(bmi)
  
  ggsave.mine(paste0(dir, "bmi.q", ext), bmi)
}

# BMI agegroup validation ----------------------------------------------------------
mcjobs[[14]] <- function () {
  # TODO bmi.cvd /bmi.ca
  dt = copy(Tables$bmi.cvd.SA[scenario == "Current Policy", ])
  dt[, year := year.cvdlag]
  dt <- dt[agegroup %in% unique(agegroup.fn(ageL:ageH)) ,]
  
  HSE.ts = copy(HSE.ts2)
  HSE.ts[, age := age + cvd.lag]
  agegroup.fn(HSE.ts)
  HSE.ts.srv.nurse <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.nurse, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.nurse <- subset(HSE.ts.srv.nurse, age >= ageL & age <= ageH & wt.nurse > 0 & 
                               is.na(bmival) == F & !is.na(agegroup))
  
  dt2 <- setnames(data.table(svyby(~bmival, ~sex + agegroup + year, HSE.ts.srv.nurse, svymean, na.rm = T)), "bmival", "mean")
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  bmi <- 
    ggplot(dt[between(year, 2001, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    facet_grid(sex ~ agegroup) +
    scale_x_continuous(name = "Year", breaks = c(2001, 2006, 2013)) +
    scale_y_continuous(name = expression(
      paste("Body mass index (Kg/", m^bold("2"), ")"))
    ) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Mean body mass index (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 10)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 14)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(bmi)
  
  ggsave.mine(paste0(dir, "bmi.a", ext), bmi)
}

# Smoking cvd validation ------------------------------------------------------
mcjobs[[15]] <- function () {
  dt = copy(Tables$smoking.cvd.S[scenario == "Current Policy", ])
  dt[, year := year.cvdlag]
  
  HSE.ts = copy(HSE.ts2)
  HSE.ts[, age := age + cvd.lag]
  agegroup.fn(HSE.ts)
  HSE.ts.srv.int <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.int, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.int <- subset(HSE.ts.srv.int, age >= ageL & age <= ageH & wt.int > 0 & 
                             is.na(cigst1) == F)
  
  dt2 <- setnames(
    data.table(
      svyby(~I(cigst1 == 4), ~sex + year, HSE.ts.srv.int, svymean, na.rm = T)
    ), 
    c("I(cigst1 == 4)TRUE", "se.I(cigst1 == 4)TRUE"), c("mean", "se")
  )
  
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  smok <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_point(size = 2, alpha = 4/5) +
    geom_smooth(method=lm, se=F) +
    facet_grid(sex ~ .) +
    scale_x_continuous(name = "Year", breaks = c(sy:2013)) +
    scale_y_continuous(name="Prevalence",labels = percent_format()) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Active smoking prevalence (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 14)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 16)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(smok)
  
  ggsave.mine(paste0(dir, "smok.act.s.cvd", ext), smok)
  
  dt1 = copy(Tables$smoking.cvd.S[scenario == "Current Policy", ])
  dt2 = copy(Tables$nev.smoking.cvd.S[scenario == "Current Policy", ])
  
  dt <- dt1[dt2, on = c("year.cvdlag", "sex" ), `:=` (
    mean = 1 - mean - i.mean,
    lui  = 1 - lui  - i.lui,
    uui  = 1 - uui  - i.uui)]
  dt[, year := year.cvdlag]
  dt2 <- setnames(
    data.table(
      svyby(~I(cigst1 > 1 & cigst1 < 4), ~sex + year, HSE.ts.srv.int, svymean, na.rm = T)
    ), 
    c("I(cigst1 > 1 & cigst1 < 4)TRUE", "se.I(cigst1 > 1 & cigst1 < 4)TRUE"), c("mean", "se")
  )
  
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  smok <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_point(size = 2, alpha = 4/5) +
    geom_smooth(method=lm, se=F) +
    facet_grid(sex ~ .) +
    scale_x_continuous(name = "Year", breaks = c(sy:2013)) +
    scale_y_continuous(name="Prevalence",labels = percent_format()) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Ex smoking prevalence (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 14)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 16)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(smok)
  
  ggsave.mine(paste0(dir, "smok.ex.s.cvd", ext), smok)
}

# Smoking cvd QIMD validation ------------------------------------------------------
mcjobs[[16]] <- function () {
  dt = copy(Tables$smoking.cvd.SQ[scenario == "Current Policy", ])
  dt[, year := year.cvdlag]
  
  HSE.ts = copy(HSE.ts2)
  HSE.ts[, age := age + cvd.lag]
  agegroup.fn(HSE.ts)
  HSE.ts.srv.int <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.int, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.int <- subset(HSE.ts.srv.int, age >= ageL & age <= ageH & wt.int > 0 & 
                             is.na(cigst1) == F & !is.na(qimd))
  
  dt2 <- setnames(
    data.table(
      svyby(~I(cigst1 == 4), ~sex + qimd + year, HSE.ts.srv.int, svymean, na.rm = T)
    ), 
    c("I(cigst1 == 4)TRUE", "se.I(cigst1 == 4)TRUE"), c("mean", "se")
  )
  
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  smok <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_point(size = 2, alpha = 4/5) +
    geom_smooth(method=lm, se=F) +
    facet_grid(sex ~ qimd, labeller = qimd_labeller) +
    scale_x_continuous(name = "Year", breaks = c(sy:2013)) +
    scale_y_continuous(name="Prevalence",labels = percent_format()) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Active smoking prevalence (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 14)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 16)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(smok)
  
  ggsave.mine(paste0(dir, "smok.act.q.cvd", ext), smok)
  
  dt1 = copy(Tables$smoking.cvd.SQ[scenario == "Current Policy", ])
  dt2 = copy(Tables$nev.smoking.cvd.SQ[scenario == "Current Policy", ])
  dt <- dt1[dt2, on = c("year.cvdlag", "sex", "qimd" ), `:=` (
    mean = 1 - mean - i.mean,
    lui  = 1 - lui  - i.lui,
    uui  = 1 - uui  - i.uui)]
  
  dt[, year := year.cvdlag]
  
  dt2 <- setnames(
    data.table(
      svyby(~I(cigst1 > 1 & cigst1 < 4), ~sex + qimd + year, HSE.ts.srv.int, svymean, na.rm = T)
    ), 
    c("I(cigst1 > 1 & cigst1 < 4)TRUE", "se.I(cigst1 > 1 & cigst1 < 4)TRUE"), c("mean", "se")
  )
  
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  smok <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_point(size = 2, alpha = 4/5) +
    geom_smooth(method=lm, se=F) +
    facet_grid(sex ~ qimd, labeller = qimd_labeller) +
    scale_x_continuous(name = "Year", breaks = c(sy:2013)) +
    scale_y_continuous(name="Prevalence",labels = percent_format()) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Ex smoking prevalence (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 14)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 16)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(smok)
  
  ggsave.mine(paste0(dir, "smok.ex.q.cvd", ext), smok)
}

# Smoking cvd agegroup validation ------------------------------------------------------
mcjobs[[17]] <- function () {
  dt = copy(Tables$smoking.cvd.SA[scenario == "Current Policy", ])
  dt[, year := year.cvdlag]
  dt <- dt[agegroup %in% unique(agegroup.fn(20:ageH))]
  
  HSE.ts = copy(HSE.ts2)
  HSE.ts[, age := age + cvd.lag]
  agegroup.fn(HSE.ts)
  HSE.ts.srv.int <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.int, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.int <- subset(HSE.ts.srv.int, age >= 20L & age <= ageH & wt.int > 0 & 
                             is.na(cigst1) == F & !is.na(agegroup))
  
  dt2 <- setnames(
    data.table(
      svyby(~I(cigst1 == 4), ~sex + agegroup + year, HSE.ts.srv.int, svymean, na.rm = T)
    ), 
    c("I(cigst1 == 4)TRUE", "se.I(cigst1 == 4)TRUE"), c("mean", "se")
  )
  
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  smok <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    #geom_line(position = pd, size = 0.5, alpha = 4/4, se = F) +
    geom_point(size = 2, alpha = 4/5) +
    geom_smooth(method=lm, se=F) +
    facet_grid(sex ~ agegroup) +
    scale_x_continuous(name = "Year", breaks = seq(sy, 2013, 3)) +
    scale_y_continuous(name="Prevalence",labels = percent_format()) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Active smoking prevalence (ages 20 - 84)") +
    theme(text = element_text(family = "Calibri", size = 10)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 14)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(smok)
  
  ggsave.mine(paste0(dir, "smok.act.a.cvd", ext), smok)
  
  dt1 = copy(Tables$smoking.cvd.SA[scenario == "Current Policy", ])
  dt2 = copy(Tables$nev.smoking.cvd.SA[scenario == "Current Policy", ])
  dt <- dt1[dt2, on = c("year.cvdlag", "sex", "agegroup" ), `:=` (
    mean = 1 - mean - i.mean,
    lui  = 1 - lui  - i.lui,
    uui  = 1 - uui  - i.uui)]
  
  dt[, year := year.cvdlag]
  dt <- dt[agegroup %in% unique(agegroup.fn(20:ageH))]
  
   dt2 <- setnames(
    data.table(
      svyby(~I(cigst1 > 1 & cigst1 < 4), ~sex + agegroup + year, HSE.ts.srv.int, svymean, na.rm = T)
    ), 
    c("I(cigst1 > 1 & cigst1 < 4)TRUE", "se.I(cigst1 > 1 & cigst1 < 4)TRUE"), c("mean", "se")
  )
  
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  smok <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    #geom_line(position = pd, size = 0.5, alpha = 4/4, se = F) +
    geom_point(size = 2, alpha = 4/5) +
    geom_smooth(method=lm, se=F) +
    facet_grid(sex ~ agegroup) +
    scale_x_continuous(name = "Year", breaks = seq(sy, 2013, 3)) +
    scale_y_continuous(name="Prevalence",labels = percent_format()) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Ex smoking prevalence (ages 20 - 84)") +
    theme(text = element_text(family = "Calibri", size = 10)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 14)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(smok)
  
  ggsave.mine(paste0(dir, "smok.ex.a.cvd", ext), smok)
}

# Smoking cvd (never) validation ------------------------------------------------------
mcjobs[[18]] <- function () {
  dt = copy(Tables$nev.smoking.cvd.S[scenario == "Current Policy", ])
  dt[, year := year.cvdlag]
  
  HSE.ts = copy(HSE.ts2)
  HSE.ts[, age := age + cvd.lag]
  agegroup.fn(HSE.ts)
  HSE.ts.srv.int <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.int, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.int <- subset(HSE.ts.srv.int, age >= ageL & age <= ageH & wt.int > 0 & 
                             is.na(cigst1) == F)
  
  dt2 <- setnames(
    data.table(
      svyby(~I(cigst1 == 1), ~sex + year, HSE.ts.srv.int, svymean, na.rm = T)
    ), 
    c("I(cigst1 == 1)TRUE", "se.I(cigst1 == 1)TRUE"), c("mean", "se")
  )
  
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  smok <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    #geom_line(position = pd, size = 0.5, alpha = 4/4, se = F) +
    geom_point(size = 2, alpha = 4/5) +
    geom_smooth(method=lm, se=F) +
    facet_grid(sex ~ .) +
    scale_x_continuous(name = "Year", breaks = c(sy:2013)) +
    scale_y_continuous(name="Prevalence",labels = percent_format()) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Never smoking prevalence (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 14)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 16)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(smok)
  
  ggsave.mine(paste0(dir, "smok.nev.s.cvd", ext), smok)
}

# Smoking cvd QIMD (never)validation ------------------------------------------------------
mcjobs[[19]] <- function () {
  dt = copy(Tables$nev.smoking.cvd.SQ[scenario == "Current Policy", ])
  dt[, year := year.cvdlag]
  
  HSE.ts = copy(HSE.ts2)
  HSE.ts[, age := age + cvd.lag]
  agegroup.fn(HSE.ts)
  HSE.ts.srv.int <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.int, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.int <- subset(HSE.ts.srv.int, age >= ageL & age <= ageH & wt.int > 0 & 
                             is.na(cigst1) == F & !is.na(qimd))
  
  dt2 <- setnames(
    data.table(
      svyby(~I(cigst1 == 1), ~sex + qimd + year, HSE.ts.srv.int, svymean, na.rm = T)
    ), 
    c("I(cigst1 == 1)TRUE", "se.I(cigst1 == 1)TRUE"), c("mean", "se")
  )
  
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  smok <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    facet_grid(sex ~ qimd, labeller = qimd_labeller) +
    scale_x_continuous(name = "Year", breaks = c(sy:2013)) +
    scale_y_continuous(name="Prevalence",labels = percent_format()) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Never smoking prevalence (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 14)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 16)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(smok)
  
  ggsave.mine(paste0(dir, "smok.nev.q.cvd.", ext), smok)
}

# Smoking cvd agegroup (never) validation ------------------------------------------------------
mcjobs[[20]] <- function () {
  dt = copy(Tables$nev.smoking.cvd.SA[scenario == "Current Policy", ])
  dt[, year := year.cvdlag]
  dt <- dt[agegroup %in% unique(agegroup.fn(20:ageH)) ,]
  
  HSE.ts = copy(HSE.ts2)
  HSE.ts[, age := age + cvd.lag]
  agegroup.fn(HSE.ts)
  HSE.ts.srv.int <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.int, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.int <- subset(HSE.ts.srv.int, age >= 20 & age <= ageH & wt.int > 0 & 
                             is.na(cigst1) == F & !is.na(agegroup))
  
  dt2 <- setnames(
    data.table(
      svyby(~I(cigst1 == 1), ~sex + agegroup + year, HSE.ts.srv.int, svymean, na.rm = T)
    ), 
    c("I(cigst1 == 1)TRUE", "se.I(cigst1 == 1)TRUE"), c("mean", "se")
  )
  
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  smok <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    facet_grid(sex ~ agegroup) +
    scale_x_continuous(name = "Year", breaks = seq(sy, 2013, 3)) +
    scale_y_continuous(name="Prevalence",labels = percent_format()) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Never smoking prevalence (ages 20 - 84)") +
    theme(text = element_text(family = "Calibri", size = 10)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 14)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(smok)
  
  ggsave.mine(paste0(dir, "smok.nev.a.cvd", ext), smok)
}

# Smoking curr validation ------------------------------------------------------
mcjobs[[21]] <- function () {
  dt = copy(Tables$smoking.curr.S[scenario == "Current Policy", ])

  HSE.ts = copy(HSE.ts2)
  agegroup.fn(HSE.ts)
  HSE.ts.srv.int <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.int, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.int <- subset(HSE.ts.srv.int, age >= ageL & age <= ageH & wt.int > 0 & 
                             is.na(cigst1) == F)
  
  dt2 <- setnames(
    data.table(
      svyby(~I(cigst1 == 4), ~sex + year, HSE.ts.srv.int, svymean, na.rm = T)
    ), 
    c("I(cigst1 == 4)TRUE", "se.I(cigst1 == 4)TRUE"), c("mean", "se")
  )
  
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  smok <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    facet_grid(sex ~ .) +
    scale_x_continuous(name = "Year", breaks = c(sy:2013)) +
    scale_y_continuous(name="Prevalence",labels = percent_format()) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Active smoking prevalence (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 14)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 16)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(smok)
  
  ggsave.mine(paste0(dir, "smok.act.s.curr", ext), smok)
  
  dt1 = copy(Tables$smoking.curr.S[scenario == "Current Policy", ])
  dt2 = copy(Tables$nev.smoking.curr.S[scenario == "Current Policy", ])
  dt <- dt1[dt2, on = c("year", "sex" ), `:=` (
    mean = 1 - mean - i.mean,
    lui  = 1 - lui  - i.lui,
    uui  = 1 - uui  - i.uui)]

  
  dt2 <- setnames(
    data.table(
      svyby(~I(cigst1 > 1 & cigst1 < 4), ~sex + year, HSE.ts.srv.int, svymean, na.rm = T)
    ), 
    c("I(cigst1 > 1 & cigst1 < 4)TRUE", "se.I(cigst1 > 1 & cigst1 < 4)TRUE"), c("mean", "se")
  )
  
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  smok <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    facet_grid(sex ~ .) +
    scale_x_continuous(name = "Year", breaks = c(sy:2013)) +
    scale_y_continuous(name="Prevalence",labels = percent_format()) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Ex smoking prevalence (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 14)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 16)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(smok)
  
  ggsave.mine(paste0(dir, "smok.ex.s.curr", ext), smok)
}


# Smoking curr QIMD validation ------------------------------------------------------
mcjobs[[22]] <- function () {
  dt = copy(Tables$smoking.curr.SQ[scenario == "Current Policy", ])

  HSE.ts = copy(HSE.ts2)
  agegroup.fn(HSE.ts)
  HSE.ts.srv.int <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.int, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.int <- subset(HSE.ts.srv.int, age >= ageL & age <= ageH & wt.int > 0 & 
                             is.na(cigst1) == F & !is.na(qimd))
  
  dt2 <- setnames(
    data.table(
      svyby(~I(cigst1 == 4), ~sex + qimd + year, HSE.ts.srv.int, svymean, na.rm = T)
    ), 
    c("I(cigst1 == 4)TRUE", "se.I(cigst1 == 4)TRUE"), c("mean", "se")
  )
  
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  smok <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    facet_grid(sex ~ qimd, labeller = qimd_labeller) +
    scale_x_continuous(name = "Year", breaks = c(sy:2013)) +
    scale_y_continuous(name="Prevalence",labels = percent_format()) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Active smoking prevalence (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 14)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 16)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(smok)
  
  ggsave.mine(paste0(dir, "smok.act.q.curr", ext), smok)
  
  dt1 = copy(Tables$smoking.curr.SQ[scenario == "Current Policy", ])
  dt2 = copy(Tables$nev.smoking.curr.SQ[scenario == "Current Policy", ])
  dt <- dt1[dt2, on = c("year", "sex", "qimd" ), `:=` (
    mean = 1 - mean - i.mean,
    lui  = 1 - lui  - i.lui,
    uui  = 1 - uui  - i.uui)]

  dt2 <- setnames(
    data.table(
      svyby(~I(cigst1 > 1 & cigst1 < 4), ~sex + qimd + year, HSE.ts.srv.int, svymean, na.rm = T)
    ), 
    c("I(cigst1 > 1 & cigst1 < 4)TRUE", "se.I(cigst1 > 1 & cigst1 < 4)TRUE"), c("mean", "se")
  )
  
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  smok <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    facet_grid(sex ~ qimd, labeller = qimd_labeller) +
    scale_x_continuous(name = "Year", breaks = c(sy:2013)) +
    scale_y_continuous(name="Prevalence",labels = percent_format()) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Ex smoking prevalence (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 14)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 16)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(smok)
  
  ggsave.mine(paste0(dir, "smok.ex.q.curr", ext), smok)
  
}

# Smoking curr agegroup validation ------------------------------------------------------
mcjobs[[23]] <- function () {
  dt = copy(Tables$smoking.curr.SA[scenario == "Current Policy", ])
  dt <- dt[agegroup %in% unique(agegroup.fn(20:ageH)) ,]
  
  HSE.ts = copy(HSE.ts2)
  agegroup.fn(HSE.ts)
  HSE.ts.srv.int <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.int, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.int <- subset(HSE.ts.srv.int, age >= 20 & age <= ageH & wt.int > 0 & 
                             is.na(cigst1) == F & !is.na(agegroup))
  
  dt2 <- setnames(
    data.table(
      svyby(~I(cigst1 == 4), ~sex + agegroup + year, HSE.ts.srv.int, svymean, na.rm = T)
    ), 
    c("I(cigst1 == 4)TRUE", "se.I(cigst1 == 4)TRUE"), c("mean", "se")
  )
  
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  smok <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    facet_grid(sex ~ agegroup) +
    scale_x_continuous(name = "Year", breaks = seq(sy, 2013, 3)) +
    scale_y_continuous(name="Prevalence",labels = percent_format()) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Active smoking prevalence (ages 20 - 84)") +
    theme(text = element_text(family = "Calibri", size = 10)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 14)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(smok)
  
  ggsave.mine(paste0(dir, "smok.act.a.curr", ext), smok)
  
  dt1 = copy(Tables$smoking.curr.SA[scenario == "Current Policy", ])
  dt2 = copy(Tables$nev.smoking.curr.SA[scenario == "Current Policy", ])
  dt <- dt1[dt2, on = c("year", "sex", "agegroup"), `:=` (
    mean = 1 - mean - i.mean,
    lui  = 1 - lui  - i.lui,
    uui  = 1 - uui  - i.uui)]
  dt <- dt[agegroup %in% unique(agegroup.fn(20:ageH)) ,]
  
  dt2 <- setnames(
    data.table(
      svyby(~I(cigst1 > 1 & cigst1 < 4), ~sex + agegroup + year, HSE.ts.srv.int, svymean, na.rm = T)
    ), 
    c("I(cigst1 > 1 & cigst1 < 4)TRUE", "se.I(cigst1 > 1 & cigst1 < 4)TRUE"), c("mean", "se")
  )
  
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  smok <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    facet_grid(sex ~ agegroup) +
    scale_x_continuous(name = "Year", breaks = seq(sy, 2013, 3)) +
    scale_y_continuous(name="Prevalence",labels = percent_format()) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Ex smoking prevalence (ages 20 - 84)") +
    theme(text = element_text(family = "Calibri", size = 10)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 14)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(smok)
  
  ggsave.mine(paste0(dir, "smok.ex.a.curr", ext), smok)
  
}

# Smoking curr (never) validation ------------------------------------------------------
mcjobs[[24]] <- function () {
  dt = copy(Tables$nev.smoking.curr.S[scenario == "Current Policy", ])
  dt[, year := year]
  
  HSE.ts = copy(HSE.ts2)
  agegroup.fn(HSE.ts)
  HSE.ts.srv.int <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.int, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.int <- subset(HSE.ts.srv.int, age >= ageL & age <= ageH & wt.int > 0 & 
                             is.na(cigst1) == F)
  
  dt2 <- setnames(
    data.table(
      svyby(~I(cigst1 == 1), ~sex + year, HSE.ts.srv.int, svymean, na.rm = T)
    ), 
    c("I(cigst1 == 1)TRUE", "se.I(cigst1 == 1)TRUE"), c("mean", "se")
  )
  
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  smok <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    facet_grid(sex ~ .) +
    scale_x_continuous(name = "Year", breaks = c(sy:2013)) +
    scale_y_continuous(name="Prevalence",labels = percent_format()) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Never smoking prevalence (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 14)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 16)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(smok)
  
  ggsave.mine(paste0(dir, "smok.nev.s.curr", ext), smok)
}

# Smoking curr QIMD (never)validation ------------------------------------------------------
mcjobs[[25]] <- function () {
  dt = copy(Tables$nev.smoking.curr.SQ[scenario == "Current Policy", ])
  dt[, year := year]
  
  HSE.ts = copy(HSE.ts2)
  agegroup.fn(HSE.ts)
  HSE.ts.srv.int <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.int, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.int <- subset(HSE.ts.srv.int, age >= ageL & age <= ageH & wt.int > 0 & 
                             is.na(cigst1) == F & !is.na(qimd))
  
  dt2 <- setnames(
    data.table(
      svyby(~I(cigst1 == 1), ~sex + qimd + year, HSE.ts.srv.int, svymean, na.rm = T)
    ), 
    c("I(cigst1 == 1)TRUE", "se.I(cigst1 == 1)TRUE"), c("mean", "se")
  )
  
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  smok <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    facet_grid(sex ~ qimd, labeller = qimd_labeller) +
    scale_x_continuous(name = "Year", breaks = c(sy:2013)) +
    scale_y_continuous(name="Prevalence",labels = percent_format()) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Never smoking prevalence (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 14)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 16)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(smok)
  
  ggsave.mine(paste0(dir, "smok.nev.q.curr", ext), smok)
}

# Smoking curr agegroup (never) validation ------------------------------------------------------
mcjobs[[26]] <- function () {
  dt = copy(Tables$nev.smoking.curr.SA[scenario == "Current Policy", ])
  dt[, year := year]
  dt <- dt[agegroup %in% unique(agegroup.fn(20:ageH)) ,]
  
  HSE.ts = copy(HSE.ts2)
  agegroup.fn(HSE.ts)
  HSE.ts.srv.int <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.int, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.int <- subset(HSE.ts.srv.int, age >= 20 & age <= ageH & wt.int > 0 & 
                             is.na(cigst1) == F & !is.na(agegroup))
  
  dt2 <- setnames(
    data.table(
      svyby(~I(cigst1 == 1), ~sex + agegroup + year, HSE.ts.srv.int, svymean, na.rm = T)
    ), 
    c("I(cigst1 == 1)TRUE", "se.I(cigst1 == 1)TRUE"), c("mean", "se")
  )
  
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  smok <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    facet_grid(sex ~ agegroup) +
    scale_x_continuous(name = "Year", breaks = seq(sy, 2013, 3)) +
    scale_y_continuous(name="Prevalence",labels = percent_format()) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Never smoking prevalence (ages 20 - 84)") +
    theme(text = element_text(family = "Calibri", size = 10)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 14)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(smok)
  
  ggsave.mine(paste0(dir, "smok.nev.a.curr", ext), smok)
}

# Diabetes validation ----------------------------------------------------------
mcjobs[[27]] <- function () {
  dt = copy(Tables$diabetes.S[scenario == "Current Policy", ])
  dt[, year := year.cvdlag]
  
  HSE.ts = copy(HSE.ts2)
  HSE.ts[, age := age + cvd.lag]
  agegroup.fn(HSE.ts)
  HSE.ts.srv.blood <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.blood, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.blood <- subset(HSE.ts.srv.blood, age >= ageL & age <= ageH & wt.blood > 0 & 
                               is.na(diabtotr) == F)
  
  dt2 <- setnames(
    data.table(
      svyby(~I(diabtotr == 2), ~sex + year, HSE.ts.srv.blood, svymean, na.rm = T)
    ), 
    c("I(diabtotr == 2)TRUE", "se.I(diabtotr == 2)TRUE"), c("mean", "se")
  )
  
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  diab <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    facet_grid(sex ~ .) +
    scale_x_continuous(name = "Year", breaks = c(sy:2013)) +
    scale_y_continuous(name="Prevalence",labels = percent_format()) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Diabetes mellitus prevalence (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 14)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 16)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(diab)
  
  ggsave.mine(paste0(dir, "diab", ext), diab)
}

# Diabetes QIMD validation ----------------------------------------------------------
mcjobs[[28]] <- function () {
  dt = copy(Tables$diabetes.SQ[scenario == "Current Policy", ])
  dt[, year := year.cvdlag]
  
  HSE.ts = copy(HSE.ts2)
  HSE.ts[, age := age + cvd.lag]
  agegroup.fn(HSE.ts)
  HSE.ts.srv.blood <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.blood, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.blood <- subset(HSE.ts.srv.blood, age >= ageL & age <= ageH & wt.blood > 0 & 
                               is.na(diabtotr) == F & !is.na(qimd))
  
  dt2 <- setnames(
    data.table(
      svyby(~I(diabtotr == 2), ~sex + qimd + year, HSE.ts.srv.blood, svymean, na.rm = T)
    ), 
    c("I(diabtotr == 2)TRUE", "se.I(diabtotr == 2)TRUE"), c("mean", "se")
  )
  
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  diab <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    facet_grid(sex ~ qimd, labeller = qimd_labeller) +
    scale_x_continuous(name = "Year", breaks = c(sy:2013)) +
    scale_y_continuous(name="Prevalence",labels = percent_format()) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Diabetes mellitus prevalence (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 14)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 16)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(diab)
  
  ggsave.mine(paste0(dir, "diab.q", ext), diab)
}

# Diabetes agegroup validation ----------------------------------------------------------
mcjobs[[29]] <- function () {
  dt = copy(Tables$diabetes.SA[scenario == "Current Policy", ])
  dt[, year := year.cvdlag]
  dt <- dt[agegroup %in% unique(agegroup.fn(20:ageH)) ,]
  
  HSE.ts = copy(HSE.ts2)
  HSE.ts[, age := age + cvd.lag]
  agegroup.fn(HSE.ts)
  HSE.ts.srv.blood <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.blood, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.blood <- subset(HSE.ts.srv.blood, age >= 20 & age <= ageH & wt.blood > 0 & 
                               is.na(diabtotr) == F & !is.na(agegroup))
  
  dt2 <- setnames(
    data.table(
      svyby(~I(diabtotr == 2), ~sex + agegroup + year, HSE.ts.srv.blood, svymean, na.rm = T)
    ), 
    c("I(diabtotr == 2)TRUE", "se.I(diabtotr == 2)TRUE"), c("mean", "se")
  )
  
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  diab <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    facet_grid(sex ~ agegroup) +
    scale_x_continuous(name = "Year", breaks = seq(sy, 2013, 3)) +
    scale_y_continuous(name="Prevalence",labels = percent_format()) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Diabetes mellitus prevalence (ages 20 - 84)") +
    theme(text = element_text(family = "Calibri", size = 10)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 14)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(diab)
  
  ggsave.mine(paste0(dir, "diab.a", ext), diab)
}

# PA Validation -----------------------------------------------------------
mcjobs[[30]] <- function () {
  dt = copy(Tables$pa.S[scenario == "Current Policy", ])
  dt[, year := year.cvdlag]
  
  HSE.ts = copy(HSE.ts2)
  HSE.ts[, age := age + cvd.lag]
  agegroup.fn(HSE.ts)
  HSE.ts.srv.int <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.int, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.int <- subset(HSE.ts.srv.int, age >= ageL & age <= ageH & wt.int > 0 & 
                             is.na(a30to06m) == F)
  
  dt2 <- setnames(
    data.table(
      svyby(~I(a30to06m > 4), ~sex + year, HSE.ts.srv.int, svymean, na.rm = T)
    ), 
    c("I(a30to06m > 4)TRUE", "se.I(a30to06m > 4)TRUE"), c("mean", "se")
  )
  
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  pa <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    facet_grid(sex ~ .) +
    scale_x_continuous(name = "Year", breaks = c(sy:2013)) +
    scale_y_continuous(name="Prevalence",labels = percent_format()) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("More than 5 active days prevalence (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 14)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 16)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(pa)
  
  ggsave.mine(paste0(dir, "pa", ext), pa)
}

# PA QIMD Validation -----------------------------------------------------------
mcjobs[[31]] <- function () {
  dt = copy(Tables$pa.SQ[scenario == "Current Policy", ])
  dt[, year := year.cvdlag]
  
  HSE.ts = copy(HSE.ts2)
  HSE.ts[, age := age + cvd.lag]
  agegroup.fn(HSE.ts)
  HSE.ts.srv.int <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.int, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.int <- subset(HSE.ts.srv.int, age >= ageL & age <= ageH & wt.int > 0 & 
                             is.na(a30to06m) == F & !is.na(qimd))
  
  dt2 <- setnames(
    data.table(
      svyby(~I(a30to06m > 4), ~sex + qimd + year, HSE.ts.srv.int, svymean, na.rm = T)
    ), 
    c("I(a30to06m > 4)TRUE", "se.I(a30to06m > 4)TRUE"), c("mean", "se")
  )
  
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  pa <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    facet_grid(sex ~ qimd, labeller = qimd_labeller) +
    scale_x_continuous(name = "Year", breaks = c(sy:2013)) +
    scale_y_continuous(name="Prevalence",labels = percent_format()) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("More than 5 active days prevalence (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 14)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 16)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(pa)
  
  ggsave.mine(paste0(dir, "pa.q", ext), pa)
}

# PA agegroup Validation -----------------------------------------------------------
mcjobs[[32]] <- function () {
  dt = copy(Tables$pa.SA[scenario == "Current Policy", ])
  dt[, year := year.cvdlag]
  dt <- dt[agegroup %in% unique(agegroup.fn(ageL:ageH)) ,]
  
  HSE.ts = copy(HSE.ts2)
  HSE.ts[, age := age + cvd.lag]
  agegroup.fn(HSE.ts)
  HSE.ts.srv.int <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.int, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.int <- subset(HSE.ts.srv.int, age >= ageL & age <= ageH & wt.int > 0 & 
                             is.na(a30to06m) == F & !is.na(agegroup))
  
  dt2 <- setnames(
    data.table(
      svyby(~I(a30to06m > 4), ~sex + agegroup + year, HSE.ts.srv.int, svymean, na.rm = T)
    ), 
    c("I(a30to06m > 4)TRUE", "se.I(a30to06m > 4)TRUE"), c("mean", "se")
  )
  
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  pa <- 
    ggplot(dt[between(year, sy, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    facet_grid(sex ~ agegroup) +
    scale_x_continuous(name = "Year", breaks = seq(sy, 2013, 3)) +
    scale_y_continuous(name="Prevalence",labels = percent_format()) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("More than 5 active days prevalence (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 10)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 14)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(pa)
  
  ggsave.mine(paste0(dir, "pa.a", ext), pa)
}

# F&V validation ----------------------------------------------------------
mcjobs[[33]] <- function () {
  dt = copy(Tables$fv.ca.S[scenario == "Current Policy", ]) # need to use 
  # riskfactors to extract 10 year lag
  dt[, year := year.calag]
  
  HSE.ts = copy(HSE.ts2)
  HSE.ts[, age := age + cancer.lag]
  agegroup.fn(HSE.ts)
  HSE.ts.srv.int <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.int, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.int<- subset(HSE.ts.srv.int, age >= ageL & age <= ageH & wt.int > 0 & 
                            is.na(porftvg) == F & year > -10)
  
  dt2 <- setnames(
    data.table(
      svyby(~I(porftvg > 4), ~sex + year, HSE.ts.srv.int, svymean, na.rm = T)
    ),
    c("I(porftvg > 4)TRUE", "se.I(porftvg > 4)TRUE"), c("mean", "se")
  )
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  fv <- 
    ggplot(dt[between(year, 2001, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    facet_grid(sex ~ .) +
    scale_x_continuous(name = "Year", breaks = c(2001:2013)) +
    scale_y_continuous(name="Prevalence",labels = percent_format()) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("More than 5 portions of fruit & veg\nprevalence (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 14)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 16)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(fv)
  
  ggsave.mine(paste0(dir, "fv", ext), fv)
}

# F&V QIMD validation ----------------------------------------------------------
mcjobs[[34]] <- function () {
  dt = copy(Tables$fv.ca.SQ[scenario == "Current Policy", ]) # need to use 
  # riskfactors to extract 10 year lag
  dt[, year := year.calag]
  
  HSE.ts = copy(HSE.ts2)
  HSE.ts[, age := age + cancer.lag]
  agegroup.fn(HSE.ts)
  HSE.ts.srv.int <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.int, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.int<- subset(HSE.ts.srv.int, age >= ageL & age <= ageH & wt.int > 0 & 
                            is.na(porftvg) == F & year > -10 & !is.na(qimd))
  
  dt2 <- setnames(
    data.table(
      svyby(~I(porftvg > 4), ~sex + qimd + year, HSE.ts.srv.int, svymean, na.rm = T)
    ),
    c("I(porftvg > 4)TRUE", "se.I(porftvg > 4)TRUE"), c("mean", "se")
  )
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  fv <- 
    ggplot(dt[between(year, 2001, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    facet_grid(sex ~ qimd, labeller = qimd_labeller) +
    scale_x_continuous(name = "Year", breaks = c(2001:2013)) +
    scale_y_continuous(name="Prevalence",labels = percent_format()) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("More than 5 portions of fruit & veg\nprevalence (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 14)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 16)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(fv)
  
  ggsave.mine(paste0(dir, "fv.q", ext), fv)
}

# F&V agegroup validation ----------------------------------------------------------
mcjobs[[35]] <- function () {
  dt = copy(Tables$fv.ca.SA[scenario == "Current Policy", ]) # need to use 
  # riskfactors to extract 10 year lag
  dt[, year := year.calag]
  dt <- dt[agegroup %in% unique(agegroup.fn(ageL:ageH)) ,]
  
  HSE.ts = copy(HSE.ts2)
  HSE.ts[, age := age + cancer.lag]
  agegroup.fn(HSE.ts)
  HSE.ts.srv.int <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.int, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.int<- subset(HSE.ts.srv.int, age >= ageL & age <= ageH & wt.int > 0 & 
                            is.na(porftvg) == F & year > -10 & !is.na(agegroup))
  
  dt2 <- setnames(
    data.table(
      svyby(~I(porftvg > 4), ~sex + agegroup + year, HSE.ts.srv.int, svymean, na.rm = T)
    ),
    c("I(porftvg > 4)TRUE", "se.I(porftvg > 4)TRUE"), c("mean", "se")
  )
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  fv <- 
    ggplot(dt[between(year, 2001, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    facet_grid(sex ~ agegroup) +
    scale_x_continuous(name = "Year", breaks = c(2001, 2006, 2013)) +
    scale_y_continuous(name="Prevalence",labels = percent_format()) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("More than 5 portions of fruit & veg\nprevalence (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 14)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 16)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(fv)
  
  ggsave.mine(paste0(dir, "fv.a", ext), fv)
}

# Packyears ---------------------------------------------------------------
mcjobs[[37]] <- function () {
  dt = copy(Tables$packyears.S[scenario == "Current Policy", ])
  
  HSE.ts = copy(HSE.ts2)
  HSE.ts[startsmk == 97, startsmk := NA]
  HSE.ts[cigst1 == 4, smokyrs:= age - startsmk]
  HSE.ts[smokyrs <0, smokyrs := 0L]
  HSE.ts[, packyears := 0]
  HSE.ts[cigst1 == 4, packyears := cigdyal * smokyrs/20]
  HSE.ts[cigst1 == 3, packyears := numsmok * smokyrs/20]
  HSE.ts[cigst1 == 2, packyears := smokyrs/20]
  HSE.ts.srv.int <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.int, nest = F,
                              data = HSE.ts, check.strata = T)
  HSE.ts.srv.int <- subset(HSE.ts.srv.int, age >= ageL & age <= ageH & wt.int > 0 & 
                             is.na(packyears) == F)
  
  dt2 <- setnames(data.table(svyby(~packyears, ~sex + year, HSE.ts.srv.int, svymean, na.rm = T)), "packyears", "mean")
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  packyears <- 
    ggplot(dt[between(year, 2001, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    facet_grid(sex ~ .) +
    scale_x_continuous(name = "Year", breaks = c(2001:2013)) +
    scale_y_continuous(name = "Packyears") +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Packyears (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 14)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 16)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(packyears)
  
  ggsave.mine(paste0(dir, "packyears", ext), packyears)
  
  dt = copy(Tables$packyears.SQ[scenario == "Current Policy", ])
  
  HSE.ts.srv.int <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.int, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.int <- subset(HSE.ts.srv.int, age >= ageL & age <= ageH & wt.int > 0 & 
                             is.na(packyears) == F & !is.na(qimd))
  
  dt2 <- setnames(data.table(svyby(~packyears, ~sex + qimd + year, HSE.ts.srv.int, svymean, na.rm = T)), "packyears", "mean")
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  packyears.q <- 
    ggplot(dt[between(year, 2001, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    facet_grid(sex ~ qimd, labeller = qimd_labeller) +
    scale_x_continuous(name = "Year", breaks = c(2001:2013)) +
    scale_y_continuous(name = "Packyears") +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Packyears (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 14)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 16)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(packyears.q)
  
  ggsave.mine(paste0(dir, "packyears.q", ext), packyears.q)
  
  dt = copy(Tables$packyears.SA[scenario == "Current Policy", ])
  
  HSE.ts.srv.int <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.int, nest = F, data = HSE.ts, check.strata = T)
  HSE.ts.srv.int <- subset(HSE.ts.srv.int, age >= 20L & age <= ageH & wt.int > 0 & 
                             is.na(packyears) == F & !is.na(agegroup))
  
  dt2 <- setnames(data.table(svyby(~packyears, ~sex + agegroup + year, HSE.ts.srv.int, svymean, na.rm = T)), "packyears", "mean")
  dt2[, sex := factor(sex, 1:2, c("Men", "Women"))]
  dt2[, lui := mean - 1.96 * se]
  dt2[, uui := mean + 1.96 * se]
  dt2[, scenario := "Observed"]
  dt2[, year := year + 2011]
  dt <- rbind(dt, dt2, fill = T)
  
  packyears.a <- 
    ggplot(dt[between(year, 2001, 2012)],
           aes(x = year + 0.5, 
               y = mean,
               colour = scenario)) + 
    geom_errorbar(
      aes(
        ymin = lui, 
        ymax = uui
      )
    ) +
    geom_smooth(method=lm, se=F) +
    geom_point(size = 2, alpha = 4/5) +
    facet_grid(sex ~ agegroup) +
    scale_x_continuous(name = "Year", breaks = c(2001:2013)) +
    scale_y_continuous(name = "Packyears") +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Packyears (ages 30 - 84)") +
    theme(text = element_text(family = "Calibri", size = 14)) +
    theme(plot.title = element_text(family = "Calibri", face = "bold", size = 16)) + 
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.background = element_rect(colour = "purple", fill = "#CCCCFF")) +
    scale_fill_manual(values = cbbPalette,
                      name = "") +
    scale_colour_manual(values = cbbPalette,
                        name = "")
  # print(packyears.a)
  
  ggsave.mine(paste0(dir, "packyears.a", ext), packyears.a)
}


# Parallelisation ---------------------------------------------------------
mclapply(mcjobs,
         function(f) f(),
         mc.cores = clusternumber)
