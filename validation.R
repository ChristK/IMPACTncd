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


# preample ----------------------------------------------------------------
# if (exists("scenarios.list") &
#     "current trends.R" %in% scenarios.list) {
#   
if (Sys.info()[1] == "Linux") {
  if (system("whoami", T ) == "mdxasck2") {
    setwd("~/IMPACTncd/")
    clusternumber <- ifelse(clusternumber > 70, 70, clusternumber)  # overwrites previous if <60
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
    db.file <- paste(Sys.getenv("APPDATA"), "\\Dropbox\\host.db", sep = "")
    base64coded <- readLines(db.file, warn = F)[2]
    base64(base64coded, encode = F)
  }
  setwd(paste0(get.dropbox.folder(), "/PhD/Models/IMPACTncd/"))
  dir.dataset <- (paste0(get.dropbox.folder(), "/PhD/Datasets/"))
}

ext <- ".pdf"
dir <- "./Output/Validation/"

#  if (!exists("yearstoproject")) {
tt <- readLines("./Output/simulation parameters.txt")

yearstoproject <- as.numeric(substring(tt[[grep(glob2rx("Years to project = *"), tt)]], 19))
ageL <- as.numeric(substring(tt[[grep(glob2rx("ageL = *"), tt)]], 7))
ageH <- as.numeric(substring(tt[[grep(glob2rx("ageH = *"), tt)]], 7))
Fertility.Assumption <- as.character(substring(tt[[grep(glob2rx("Fertility assumption = *"), tt)]], 24))

numberofiterations <- 1

diseasestoexclude <- list()
for (jjj in 1:length(grep(glob2rx("diseases = *"), tt))) {
  diseasestoexclude[[jjj]] <- substring(tt[[grep(glob2rx("diseases = *"), tt)[[jjj]]]], 12)
}
diseasestoexclude <- unlist(diseasestoexclude)

init.year <- as.numeric(substring(tt[[grep(glob2rx("First year of the simulation = *"), tt)]], 31))
n <- as.numeric(substring(tt[[grep(glob2rx("Sample size = *"), tt)]], 14))
cvd.lag <- as.numeric(substring(tt[[grep(glob2rx("cvd.lag = *"), tt)]], 10))
cancer.lag <- as.numeric(substring(tt[[grep(glob2rx("cancer.lag = *"), tt)]], 13))
export.graphs <- F
fatality.annual.improvement.chd    <- as.numeric(substring(tt[[grep(glob2rx("CHD annual fatality improvement = *"), tt)]], 35))*100
fatality.annual.improvement.stroke <- as.numeric(substring(tt[[grep(glob2rx("Stroke annual fatality improvement = *"), tt)]], 38))*100
fatality.annual.improvement.c16    <- as.numeric(substring(tt[[grep(glob2rx("Gastric cancer annual fatality improvement = *"), tt)]], 46))*100
fatality.sec.gradient.chd          <- as.numeric(substring(tt[[grep(glob2rx("CHD fatality gradient = *"), tt)]], 25))*100
fatality.sec.gradient.stroke       <- as.numeric(substring(tt[[grep(glob2rx("Stroke fatality gradient = *"), tt)]], 28))*100
fatality.sec.gradient.c16          <- as.numeric(substring(tt[[grep(glob2rx("Gastric cancer fatality gradient = *"), tt)]], 36))*100
pop.fraction                       <- as.numeric(substring(tt[[grep(glob2rx("Population fraction = *"), tt)]], 23))
cleardirectories                   <- F

rm(jjj, tt)
# }

require(compiler)
loadcmp(file = "./initialisation.Rc")

#load("C:/Users/ckyprid/Documents/IMPACTncd outputs/riskfactors.RData")
#load("./Output/RF/highrisk.RData")
if (!exists("pop.abs")) load("./Output/RF/population.structure.RData")
#load("./Output/Other/life.exp0.RData")
#load("./Output/Other/life.exp65.RData")
#load("./Output/Other/hlife.exp.RData")
#load("./Output/Other/other.mortality.RData")
if (!exists("chd.burden")) load("./Output/CHD/chd.burden.RData")
if (!exists("stroke.burden")) load("./Output/Stroke/stroke.burden.RData")
if (!exists("c16.burden")) load("./Output/Gastric ca/c16.burden.RData")
#load("./Output/Graphs.tbl//Graphs.tbl.rda")
if (!exists("Tables")) load("./Output/Tables/Tables.rda")
load(file = "./Lagtimes/HSE.ts.RData")
HSE.ts2 = copy(HSE.ts)

loadcmp(file = "./post simulation functions.Rc")

# population.actual <- fread("./Population/population.struct.csv",  header = T)[year == paste0(init.year), ]
# population.actual[, pct := round(as.numeric(n) * pop / sum(pop))]
# pop.fraction <- n / population.actual[, sum(pop)] # 53107200 is the total mid 2011 population of England (52642600 for 2010)

cleardirectories <- F

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
if (Sys.info()[1] == "Windows") Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.15/bin/gswin64c.exe")
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

clonedt <- function(DT, times = 500) {
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

# Rename scenarios --------------------------------------------------------
scn.levels <- c(
  "current trends"
)
scn.names <- c(
  "Current Policy"
)

chd.burden[, scenario := factor(scenario, 
                                levels = scn.levels,
                                labels = scn.names,
                                ordered = T)]
stroke.burden[, scenario := factor(scenario, 
                                   levels = scn.levels,
                                   labels = scn.names,
                                   ordered = T)]
c16.burden[, scenario := factor(scenario, 
                                levels = scn.levels,
                                labels = scn.names,
                                ordered = T)]
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
x <- x[!x == "Current Policy"]

# appendix RF trends
# riskfactors[, scenario := factor(scenario, 
#                                  levels = scn.levels,
#                                  labels = scn.names,
#                                  ordered = T)]

qimd_labeller <- function(var, value){
  value <- as.character(value)
  if (var == "qimd") { 
    value[value == "1"] <- "QIMD 1"
    value[value == "2"] <- "QIMD 2"
    value[value == "3"] <- "QIMD 3"
    value[value == "4"] <- "QIMD 4"
    value[value == "5"] <- "QIMD 5"
  }
  return(value)
}

# CHD validation ----------------------------------------------------------
#load("./Output/CHD/chd.burden.RData")
#load(file="./Validation/kirk.RData") # chd.drates

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

chd.mort <- chd.mort[,
                     MC.mean(chd.mortality/pop),
                     by = grp]

chd.mort[, `:=` (Model= "IMPACTNCD", scenario = NULL)]
chd.mort <- rbind(chd.mort, chd.drates, fill = T)
chd.mort <- chd.mort[agegroup!="85+" & between(year, 2002, init.year + yearstoproject)]

chd.men <- ggplot(chd.mort[sex=="Men",],
                  aes(x=year, y=mean*100000, colour=Model, ymin = 0)) + 
  #geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.05, position=pd, alpha=3/5) + 
  #geom_smooth(size = 2, alpha=1/4) +
  geom_line(position=pd,size= 1, alpha=3/5) +
  facet_grid(agegroup ~ qimd, scales="free", labeller=qimd_labeller) +
  ylab("Mortality per 100,000") + scale_x_continuous(name="Year") + 
  scale_colour_discrete(breaks = c("BAMP", "IMPACTNCD"),
                        labels = c("BAMP         ", expression(IMPACT[NCD]))) + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
  ggtitle("CHD Mortality Validation (Men)") +
  theme(text = element_text(family="Calibri", size = 12)) +
  theme(plot.title = element_text(family="Calibri", face="bold", size=16))+ 
  theme(strip.text.x = element_text(size = 10, face = "bold"),
        strip.text.y = element_text(size = 10, face = "bold"),
        strip.background = element_rect(colour="purple", fill="#CCCCFF"))


chd.women <- ggplot(chd.mort[sex=="Women",],
                    aes(x=year, y=mean*100000, colour=Model, ymin = 0)) + 
  #geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.05, position=pd, alpha=3/5) + 
  geom_line(position=pd,size= 1, alpha=3/5) +
  facet_grid(agegroup ~ qimd, scales="free", labeller=qimd_labeller) +
  ylab("Mortality per 100,000") + scale_x_continuous(name="Year") + 
  scale_colour_discrete(breaks = c("BAMP", "IMPACTNCD"),
                        labels = c("BAMP         ", expression(IMPACT[NCD]))) + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
  ggtitle("CHD Mortality Validation (Women)") +
  theme(text = element_text(family="Calibri", size = 12)) +
  theme(plot.title = element_text(family="Calibri", face="bold", size=16))+ 
  theme(strip.text.x = element_text(size = 10, face = "bold"),
        strip.text.y = element_text(size = 10, face = "bold"),
        strip.background = element_rect(colour="purple", fill="#CCCCFF"))

#print(chd.men)
#print(chd.women) 


# ggsave("./Validation/validation men chd.pdf", chd.men, units = "in", device=cairo_pdf, family="Calibri", antialias = "subpixel", dpi=1200, width = 11.69, height = 8.27)
# ggsave("./Validation/validation women chd.pdf", chd.women, units = "in", device=cairo_pdf, family="Calibri", antialias = "subpixel", dpi=1200, width = 11.69, height = 8.27)

ggsave.mine(paste0(dir, "chd men", ext), chd.men)
ggsave.mine(paste0(dir, "chd women", ext), chd.women)

# Stroke validation -------------------------------------------------------
#load("./Output/Stroke/stroke.burden.RData")
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


stroke.mort <- stroke.mort[,
                           MC.mean(stroke.mortality/pop),
                           by = grp]

stroke.mort[, `:=` (Model= "IMPACTNCD", scenario = NULL)]
stroke.mort <- rbind(stroke.mort,stroke.drates, fill=T)
stroke.mort <- stroke.mort[agegroup!="85+" & between(year, 2002, init.year + yearstoproject)]

stroke.men <- ggplot(stroke.mort[sex=="Men",],
                     aes(x=year, y=mean*100000, colour=Model, ymin = 0)) + 
  #geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.05, position = pd, alpha=3/5) +
  #geom_smooth(size = 2, alpha=1/4) +
  geom_line(position=pd,size= 1, alpha=3/5) +
  facet_grid(agegroup ~ qimd, scales="free", labeller=qimd_labeller) +
  ylab("Mortality per 100,000") + scale_x_continuous(name="Year") + 
  scale_colour_discrete(breaks = c("BAMP", "IMPACTNCD"),
                        labels = c("BAMP         ", expression(IMPACT[NCD]))) + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
  ggtitle("Stroke Mortality Validation (Men)") +
  theme(text = element_text(family="Calibri", size = 12)) +
  theme(plot.title = element_text(family="Calibri", face="bold", size=16))+ 
  theme(strip.text.x = element_text(size = 10, face = "bold"),
        strip.text.y = element_text(size = 10, face = "bold"),
        strip.background = element_rect(colour="purple", fill="#CCCCFF"))


stroke.women <- ggplot(stroke.mort[sex=="Women",],
                       aes(x=year, y=mean*100000, colour=Model, ymin = 0)) + 
  #geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.05, position=pd, alpha=3/5) +
  #geom_smooth(size = 2, alpha=1/4) +
  geom_line(position=pd,size= 1, alpha=3/5) +
  facet_grid(agegroup ~ qimd, scales="free", labeller=qimd_labeller) +
  ylab("Mortality per 100,000") + scale_x_continuous(name="Year") + 
  scale_colour_discrete(breaks = c("BAMP", "IMPACTNCD"),
                        labels = c("BAMP         ", expression(IMPACT[NCD]))) + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
  ggtitle("Stroke Mortality Validation (Women)") +
  theme(text = element_text(family="Calibri", size = 12)) +
  theme(plot.title = element_text(family="Calibri", face="bold", size=16))+ 
  theme(strip.text.x = element_text(size = 10, face = "bold"),
        strip.text.y = element_text(size = 10, face = "bold"),
        strip.background = element_rect(colour="purple", fill="#CCCCFF"))

#print(stroke.men)
#print(stroke.women) 


# ggsave("./Validation/validation men stroke.pdf", stroke.men, units = "in", device=cairo_pdf, family="Calibri", antialias = "subpixel", dpi=1200, width = 11.69, height = 8.27)
# ggsave("./Validation/validation women stroke.pdf", stroke.women, units = "in", device=cairo_pdf, family="Calibri", antialias = "subpixel", dpi=1200, width = 11.69, height = 8.27)

ggsave.mine(paste0(dir, "stroke men", ext), stroke.men)
ggsave.mine(paste0(dir, "stroke women", ext), stroke.women)


# Gastric cancer validation -----------------------------------------------
#load("./Output/Gastric ca/c16.burden.RData")
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


c16.mort <- c16.mort[,
                     MC.mean(c16.mortality/pop),
                     by = grp]

c16.mort[, `:=` (Model= "IMPACTNCD", scenario = NULL)]
c16.mort <- rbind(c16.mort,c16.drates, fill=T)
c16.mort <- c16.mort[agegroup!="85+" & between(year, 2002, init.year + yearstoproject)]

c16.men <- ggplot(c16.mort[sex=="Men",],
                  aes(x=year, y=mean*100000, colour=Model, ymin = 0)) + 
  #geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.05, position=pd, alpha=3/5) +
  #geom_smooth(size = 2, alpha=1/4) +
  geom_line(position=pd,size= 1, alpha=3/5) +
  facet_grid(agegroup ~ qimd, labeller=qimd_labeller, scales="free") +
  ylab("Mortality per 100,000") + scale_x_continuous(name="Year") + 
  scale_colour_discrete(breaks = c("BAMP", "IMPACTNCD"),
                        labels = c("BAMP         ", expression(IMPACT[NCD]))) + 
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
  geom_line(position=pd,size= 1, alpha=3/5) +
  facet_grid(agegroup ~ qimd,  labeller=qimd_labeller, scales="free") +
  ylab("Mortality per 100,000") + scale_x_continuous(name="Year") + 
  scale_colour_discrete(breaks = c("BAMP", "IMPACTNCD"),
                        labels = c("BAMP         ", expression(IMPACT[NCD]))) + 
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

ggsave.mine(paste0(dir, "gastric ca men", ext), c16.men)
ggsave.mine(paste0(dir, "gastric ca women", ext), c16.women)


# Salt graph 20-64 ------------------------------------------------------------------
pop.st <- pop.abs[, mean(pop), by = .(year, agegroup, sex, scenario, qimd)
                  ][,
                    list("pop" = sum(V1)), by = .(year, agegroup, sex, scenario)
                    ]

dt = copy(Tables$salt.ca.SA[scenario == "Current Policy", ])
dt <- merge(dt, pop.st, by = c("year", "agegroup", "sex", "scenario"), all.x = T)
dt[, year := year - cancer.lag]
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
  facet_grid(sex ~ ., labeller = qimd_labeller) +
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

# Salt graph agegroup 20-64 ------------------------------------------------------------------
dt = copy(Tables$salt.ca.SA[scenario == "Current Policy", ])
dt <- merge(dt, pop.st, by = c("year", "agegroup", "sex", "scenario"), all.x = T)
dt[, year := year - cancer.lag]
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
  facet_grid(sex ~ agegroup, labeller = qimd_labeller) +
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

# SBP validation ----------------------------------------------------------
dt = copy(Tables$sbp.S[scenario == "Current Policy", ])
dt[, year := year - cvd.lag]

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
  #geom_line(position = pd, size = 0.5, alpha = 4/4, se = F) +
  geom_point(size = 2, alpha = 4/5) +
  facet_grid(sex ~ ., labeller = qimd_labeller) +
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

# SBP QIMD validation ----------------------------------------------------------
dt = copy(Tables$sbp.SQ[scenario == "Current Policy", ])
dt[, year := year - cvd.lag]

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
  #geom_line(position = pd, size = 0.5, alpha = 4/4, se = F) +
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

# SBP agegroup validation ----------------------------------------------------------
dt = copy(Tables$sbp.SA[scenario == "Current Policy", ])
dt[, year := year - cvd.lag]
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
  #geom_line(position = pd, size = 0.5, alpha = 4/4, se = F) +
  geom_point(size = 2, alpha = 4/5) +
  facet_grid(sex ~ agegroup, labeller = qimd_labeller) +
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

# TC validation -----------------------------------------------------------
dt = copy(Tables$tc.S[scenario == "Current Policy", ])
dt[, year := year - cvd.lag]

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
  
  #geom_line(position = pd, size = 0.5, alpha = 4/4, se = F) +
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

# TC QIMD validation -----------------------------------------------------------
dt = copy(Tables$tc.SQ[scenario == "Current Policy", ])
dt[, year := year - cvd.lag]

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
  
  #geom_line(position = pd, size = 0.5, alpha = 4/4, se = F) +
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

# TC agegroup validation -----------------------------------------------------------
dt = copy(Tables$tc.SA[scenario == "Current Policy", ])
dt[, year := year - cvd.lag]
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
  
  #geom_line(position = pd, size = 0.5, alpha = 4/4, se = F) +
  geom_point(size = 2, alpha = 4/5) +
  geom_errorbar(
    aes(
      ymin = lui, 
      ymax = uui
    )
  ) +
  facet_grid(sex ~ agegroup, labeller = qimd_labeller) +
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

# BMI validation ----------------------------------------------------------
# TODO bmi.cvd /bmi.ca
dt = copy(Tables$bmi.ca.S[scenario == "Current Policy", ])
dt[, year := year - cancer.lag]

HSE.ts = copy(HSE.ts2)
HSE.ts[, age := age + cancer.lag]
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
  #geom_line(position = pd, size = 0.5, alpha = 4/4, se = F) +
  geom_point(size = 2, alpha = 4/5) +
  facet_grid(sex ~ ., labeller = qimd_labeller) +
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

# BMI QIMD validation ----------------------------------------------------------
# TODO bmi.cvd /bmi.ca
dt = copy(Tables$bmi.ca.SQ[scenario == "Current Policy", ])
dt[, year := year - cancer.lag]

HSE.ts = copy(HSE.ts2)
HSE.ts[, age := age + cancer.lag]
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
  #geom_line(position = pd, size = 0.5, alpha = 4/4, se = F) +
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

# BMI agegroup validation ----------------------------------------------------------
# TODO bmi.cvd /bmi.ca
dt = copy(Tables$bmi.ca.SA[scenario == "Current Policy", ])
dt[, year := year - cancer.lag]
dt <- dt[agegroup %in% unique(agegroup.fn(ageL:ageH)) ,]

HSE.ts = copy(HSE.ts2)
HSE.ts[, age := age + cancer.lag]
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
  #geom_line(position = pd, size = 0.5, alpha = 4/4, se = F) +
  geom_point(size = 2, alpha = 4/5) +
  facet_grid(sex ~ agegroup, labeller = qimd_labeller) +
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

# Smoking validation ------------------------------------------------------
dt = copy(Tables$smoking.S[scenario == "Current Policy", ])
dt[, year := year - cvd.lag]

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
  #geom_line(position = pd, size = 0.5, alpha = 4/4, se = F) +
  geom_point(size = 2, alpha = 4/5) +
  facet_grid(sex ~ ., labeller = qimd_labeller) +
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

ggsave.mine(paste0(dir, "smok", ext), smok)

# Smoking QIMD validation ------------------------------------------------------
dt = copy(Tables$smoking.SQ[scenario == "Current Policy", ])
dt[, year := year - cvd.lag]

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
  #geom_line(position = pd, size = 0.5, alpha = 4/4, se = F) +
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

ggsave.mine(paste0(dir, "smok.q", ext), smok)

# Smoking agegroup validation ------------------------------------------------------
dt = copy(Tables$smoking.SA[scenario == "Current Policy", ])
dt[, year := year - cvd.lag]
dt <- dt[agegroup %in% unique(agegroup.fn(ageL:ageH)) ,]

HSE.ts = copy(HSE.ts2)
HSE.ts[, age := age + cvd.lag]
agegroup.fn(HSE.ts)
HSE.ts.srv.int <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.int, nest = F, data = HSE.ts, check.strata = T)
HSE.ts.srv.int <- subset(HSE.ts.srv.int, age >= ageL & age <= ageH & wt.int > 0 & 
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
  facet_grid(sex ~ agegroup, labeller = qimd_labeller) +
  scale_x_continuous(name = "Year", breaks = seq(sy, 2013, 3)) +
  scale_y_continuous(name="Prevalence",labels = percent_format()) +
  theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
  ggtitle("Active smoking prevalence (ages 30 - 84)") +
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

ggsave.mine(paste0(dir, "smok.a", ext), smok)

# Diabetes validation ----------------------------------------------------------
dt = copy(Tables$diabetes.S[scenario == "Current Policy", ])
dt[, year := year - cvd.lag]

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
  #geom_line(position = pd, size = 0.5, alpha = 4/4, se = F) +
  geom_point(size = 2, alpha = 4/5) +
  facet_grid(sex ~ ., labeller = qimd_labeller) +
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

# Diabetes QIMD validation ----------------------------------------------------------
dt = copy(Tables$diabetes.SQ[scenario == "Current Policy", ])
dt[, year := year - cvd.lag]

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
  #geom_line(position = pd, size = 0.5, alpha = 4/4, se = F) +
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

# Diabetes agegroup validation ----------------------------------------------------------
dt = copy(Tables$diabetes.SA[scenario == "Current Policy", ])
dt[, year := year - cvd.lag]
dt <- dt[agegroup %in% unique(agegroup.fn(ageL:ageH)) ,]

HSE.ts = copy(HSE.ts2)
HSE.ts[, age := age + cvd.lag]
agegroup.fn(HSE.ts)
HSE.ts.srv.blood <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.blood, nest = F, data = HSE.ts, check.strata = T)
HSE.ts.srv.blood <- subset(HSE.ts.srv.blood, age >= ageL & age <= ageH & wt.blood > 0 & 
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
  #geom_line(position = pd, size = 0.5, alpha = 4/4, se = F) +
  geom_point(size = 2, alpha = 4/5) +
  facet_grid(sex ~ agegroup, labeller = qimd_labeller) +
  scale_x_continuous(name = "Year", breaks = seq(sy, 2013, 3)) +
  scale_y_continuous(name="Prevalence",labels = percent_format()) +
  theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) + 
  ggtitle("Diabetes mellitus prevalence (ages 30 - 84)") +
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

# PA Validation -----------------------------------------------------------
dt = copy(Tables$pa.S[scenario == "Current Policy", ])
dt[, year := year - cvd.lag]

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
  #geom_line(position = pd, size = 0.5, alpha = 4/4, se = F) +
  geom_point(size = 2, alpha = 4/5) +
  facet_grid(sex ~ ., labeller = qimd_labeller) +
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

# PA QIMD Validation -----------------------------------------------------------
dt = copy(Tables$pa.SQ[scenario == "Current Policy", ])
dt[, year := year - cvd.lag]

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
  #geom_line(position = pd, size = 0.5, alpha = 4/4, se = F) +
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

# PA agegroup Validation -----------------------------------------------------------
dt = copy(Tables$pa.SA[scenario == "Current Policy", ])
dt[, year := year - cvd.lag]
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
  #geom_line(position = pd, size = 0.5, alpha = 4/4, se = F) +
  geom_point(size = 2, alpha = 4/5) +
  facet_grid(sex ~ agegroup, labeller = qimd_labeller) +
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

# F&V validation ----------------------------------------------------------
dt = copy(Tables$fv.ca.S[scenario == "Current Policy", ]) # need to use 
# riskfactors to extract 10 year lag
dt[, year := year - cancer.lag]

HSE.ts = copy(HSE.ts2)
HSE.ts[, age := age + cancer.lag]
agegroup.fn(HSE.ts)
HSE.ts.srv.int <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.int, nest = F, data = HSE.ts, check.strata = T)
HSE.ts.srv.int<- subset(HSE.ts.srv.int, age >= ageL & age <= ageH & wt.int > 0 & 
                          is.na(porftvg) == F & year > -10)

dt2 <- setnames(
  data.table(
    svyby(~I(porftvg) > 4, ~sex + year, HSE.ts.srv.int, svymean, na.rm = T)
  ),
  c("I(porftvg) > 4TRUE", "se.I(porftvg) > 4TRUE"), c("mean", "se")
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
  #geom_line(position = pd, size = 0.5, alpha = 4/4, se = F) +
  geom_point(size = 2, alpha = 4/5) +
  facet_grid(sex ~ ., labeller = qimd_labeller) +
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

# F&V QIMD validation ----------------------------------------------------------
dt = copy(Tables$fv.ca.SQ[scenario == "Current Policy", ]) # need to use 
# riskfactors to extract 10 year lag
dt[, year := year - cancer.lag]

HSE.ts = copy(HSE.ts2)
HSE.ts[, age := age + cancer.lag]
agegroup.fn(HSE.ts)
HSE.ts.srv.int <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.int, nest = F, data = HSE.ts, check.strata = T)
HSE.ts.srv.int<- subset(HSE.ts.srv.int, age >= ageL & age <= ageH & wt.int > 0 & 
                          is.na(porftvg) == F & year > -10 & !is.na(qimd))

dt2 <- setnames(
  data.table(
    svyby(~I(porftvg) > 4, ~sex + qimd + year, HSE.ts.srv.int, svymean, na.rm = T)
  ),
  c("I(porftvg) > 4TRUE", "se.I(porftvg) > 4TRUE"), c("mean", "se")
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
  #geom_line(position = pd, size = 0.5, alpha = 4/4, se = F) +
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

# F&V agegroup validation ----------------------------------------------------------
dt = copy(Tables$fv.ca.SA[scenario == "Current Policy", ]) # need to use 
# riskfactors to extract 10 year lag
dt[, year := year - cancer.lag]
dt <- dt[agegroup %in% unique(agegroup.fn(ageL:ageH)) ,]

HSE.ts = copy(HSE.ts2)
HSE.ts[, age := age + cancer.lag]
agegroup.fn(HSE.ts)
HSE.ts.srv.int <- svydesign(id = ~psu, strata = ~cluster, weights = ~wt.int, nest = F, data = HSE.ts, check.strata = T)
HSE.ts.srv.int<- subset(HSE.ts.srv.int, age >= ageL & age <= ageH & wt.int > 0 & 
                          is.na(porftvg) == F & year > -10 & !is.na(agegroup))

dt2 <- setnames(
  data.table(
    svyby(~I(porftvg) > 4, ~sex + agegroup + year, HSE.ts.srv.int, svymean, na.rm = T)
  ),
  c("I(porftvg) > 4TRUE", "se.I(porftvg) > 4TRUE"), c("mean", "se")
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
  #geom_line(position = pd, size = 0.5, alpha = 4/4, se = F) +
  geom_point(size = 2, alpha = 4/5) +
  facet_grid(sex ~ agegroup, labeller = qimd_labeller) +
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
# }
