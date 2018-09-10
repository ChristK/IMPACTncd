# preample ---------------------------
# require(MASS)
library(data.table)
library(compiler)
require(scales)
require(ggthemes)
require(RColorBrewer)
require(cowplot)

decline <-  "_lin" # or "_log"
save_dir <- paste0("~/pCloudDrive/My Papers/Salt/Responsibility Deal/model_outputs", decline,"/")

result_dir <- paste0("/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output", decline, "/")

transp_nam <- function(x) {
  names <- names(x)
  cbind(names, data.table::transpose(x))
}

agegroup.fn <- 
  function(x, lagtime = 0) {
    breaks <- c(0, 1, seq(5, 85, 5), Inf)
    labels <- c("<1   ", "01-04", "05-09",
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
        setorder(x, qimd, sex, agegroup)
        x[, group := rleid(qimd, sex, agegroup)]
        return(invisible(x))
      } else return(print("only datatables and vectors are eligible inputs"))
    }
  }


# Define function to split agegroups and create groups
agegroup.part <- 
  function(x, lagtime = 0) {
    breaks <- c(seq(20, 85, 5), Inf)
    labels <- c("20-24", "25-29", "30-34", 
                "35-39", "40-44", "45-49",
                "50-54", "55-59", "60-64",
                "65-69", "70-74", "75-79",
                "80-84", "85+")
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
        return(invisible(x))
      } else return(print("only datatables and vectors are eligible inputs"))
    }
  }

# Define operator %!in%, meaning !%in%
'%!in%' <- function(x,y)!('%in%'(x,y))


if (Sys.info()[1] == "Linux") {
  if (system("whoami", T) == "mdxasck2") {
    setwd("~/IMPACTncd/")
    # all.files <- list.files('./SynthPop', pattern = glob2rx('spop2011*.rds'),
    # full.names = T) spop.l <- lapply(all.files, readRDS) rm(all.files)
  } else {
    setwd("~/pCloudDrive/My Models/Responsibility deal/")
  }
} else {
  get.dropbox.folder <- function() {
    if (!require(RCurl)) 
      stop("You need to install RCurl package.")
    if (Sys.info()["sysname"] != "Windows") 
      stop("Currently, 'get.dropbox.folder' works for Windows and Linux only. Sorry...")
    db.file <- paste(Sys.getenv("APPDATA"), "\\Dropbox\\host.db", sep = "")
    base64coded <- readLines(db.file, warn = F)[2]
    base64(base64coded, encode = F)
  }
  setwd(paste0(get.dropbox.folder(), "/PhD/Models/IMPACTncd/"))
}

numberofiterations <- max(as.integer(
  list.dirs(
    path = paste0(result_dir, "responsibility deal"),
    full.names = F,
    recursive = F
  )
))

tt <- readLines(paste0(result_dir, "simulation parameters.txt"))
yearstoproject <- as.numeric(substring(tt[[grep(glob2rx("Years to project = *"), tt)]], 19))
ageL <- as.numeric(substring(tt[[grep(glob2rx("ageL = *"), tt)]], 7))
ageH <- as.numeric(substring(tt[[grep(glob2rx("ageH = *"), tt)]], 7))
Fertility.Assumption <- as.character(substring(tt[[grep(glob2rx("Fertility assumption = *"), tt)]], 24))

diseasestoexclude <- list()
for (jjj in 1:length(grep(glob2rx("diseases = *"), tt))) {
  diseasestoexclude[[jjj]] <- substring(tt[[grep(glob2rx("diseases = *"), tt)[[jjj]]]], 12)
}
diseasestoexclude <- unlist(diseasestoexclude)
paired <- substring(tt[[grep(glob2rx("Paired = *"), tt)]], 10)
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

tt <- readLines(paste0(result_dir, "simulation parameters.txt"))
cvd.lag <- as.numeric(substring(tt[[grep(glob2rx("cvd.lag = *"), tt)]], 10))
cancer.lag <- as.numeric(substring(tt[[grep(glob2rx("cancer.lag = *"), tt)]], 13))
lagtimes <- fread(paste0(result_dir, "lagtimes.csv"))
lagtimes[, mc := .I]
rm(jjj, tt)


load(paste0(result_dir, "RF/riskfactors.RData"))
salt <- riskfactors[is.na(qimd) & is.na(sex) & is.na(agegroup),
                    .(year, scenario, mc, pop, sbp.cvd.mean, salt.fsa.mean, salt.rd.mean, salt.diff.mean)]
rm(riskfactors)
#load("./Output/RF/salt.table.rda")
#salt <- readRDS("./Output/RF/salt.rds")
#load("./Output/RF/highrisk.RData")
load(paste0(result_dir, "RF/population.structure.RData"))
#load("./Output/Other/life.exp0.RData")
#load("./Output/Other/life.exp65.RData")
#load("./Output/Other/hlife.exp.RData")
load(paste0(result_dir, "Other/other.mortality.RData"))
load(paste0(result_dir, "CHD/chd.burden.RData"))
chd.burden    <- chd.burden[group == "SAQ"]
load(paste0(result_dir, "Stroke/stroke.burden.RData"))
stroke.burden <- stroke.burden[group == "SAQ"]
load(paste0(result_dir, "Gastric ca/c16.burden.RData"))
c16.burden    <- c16.burden[group == "SAQ"]
#load("./Output/Graphs.tbl/Graphs.tbl.rda")
load(paste0(result_dir, "Tables/Tables.rda"))
gc()
source(file = "./post simulation functions.R")

#population.actual <- fread("./Population/population.struct.csv",  header = T)[year == paste0(init.year), ]
#population.actual[, pct := round(as.numeric(n) * pop / sum(pop))]
#pop.fraction <- n / population.actual[, sum(pop)] # 53107200 is the total mid 2011 population of England (52642600 for 2010)

cleardirectories <- F

pd <- position_dodge(.3) 
# The palette with black:
cbbPalette <- c(#"#000000", 
  "#E69F00", 
  "#56B4E9", 
  "#009E73", 
  #"#F0E442", 
  #"#0072B2", 
  "#D55E00", 
  "#CC79A7")

clonedt <- function(DT, times = 500) {
  l <- sample(list(DT), times, T)
  rbindlist(l, idcol = T)
  return(rbindlist(l, idcol = T))
}

# Rename scenarios -------------------------------
scn.levels <- c("current trends",
                "responsibility deal")
scn.names <- c("FSA Led",
               "Responsibility Deal")


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
other.mortality[, scenario := factor(scenario,
                                     levels = scn.levels,
                                     labels = scn.names,
                                     ordered = T)]
pop.abs[, scenario := factor(scenario, 
                             levels = scn.levels,
                             labels = scn.names,
                             ordered = T)]
# lapply(Tables, function(x) {
#   x[, scenario := factor(scenario, 
#                          levels = scn.levels,
#                          labels = scn.names,
#                          ordered = T)]
# }
# )
# lapply(salt.table$ca, function(x) {
#   x[, scenario := factor(scenario, 
#                          levels = scn.levels,
#                          labels = scn.names,
#                          ordered = T)]
# }
# )
# lapply(salt.table$cvd, function(x) {
#   x[, scenario := factor(scenario, 
#                          levels = scn.levels,
#                          labels = scn.names,
#                          ordered = T)]
# }
# )
# x <- Tables$hle.S[, levels(scenario)]
# x <- x[!x == "FSA Led"]

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

# CPP/DPP files ---------------------------------------
chd.cpp <- chd.burden[group == "SAQ",
                      .(chd.incidence = chd.incidence,
                        chd.mortality = chd.mortality), 
                      by = .(year, scenario, sex, agegroup, qimd, mc)]
setkey(chd.cpp, year, scenario, sex, agegroup, qimd, mc)
stroke.cpp <- stroke.burden[group == "SAQ", 
                            .(stroke.incidence = stroke.incidence,
                              stroke.mortality = stroke.mortality), 
                            by = .(year, scenario, sex, agegroup, qimd, mc)]
setkey(stroke.cpp, year, scenario, sex, agegroup, qimd, mc)
c16.cpp <- c16.burden[group == "SAQ", list(c16.incidence = c16.incidence,
                                           c16.mortality = c16.mortality), 
                      by = .(year, scenario, sex, agegroup, qimd, mc)]
setkey(c16.cpp, year, scenario, sex, agegroup, qimd, mc)
## add here other diseases

# cvd cpp/dpp
cvd.cpp <- merge(chd.cpp, stroke.cpp, by = c("year", "scenario", "sex", "agegroup", "qimd", "mc"))
cvd.cpp <- merge(cvd.cpp, pop.abs, by = c("year", "scenario", "sex", "agegroup", "qimd", "mc"), all.x = T, all.y = F)
# cvd.cpp[, `:=` (cases = stroke.incidence,
#                 deaths = stroke.mortality)]
cvd.cpp[, `:=` (
  chd.cases = chd.incidence,
  chd.deaths = chd.mortality,
  stroke.cases = stroke.incidence,
  stroke.deaths = stroke.mortality,
  cvd.cases = chd.incidence + stroke.incidence,
  cvd.deaths = chd.mortality + stroke.mortality)] 
cvd.cpp[, c("chd.incidence", "stroke.incidence",
            "chd.mortality", "stroke.mortality") := NULL]
rm(chd.cpp, stroke.cpp)
# Gastric ca
setnames(c16.cpp, c("c16.incidence", "c16.mortality"),
         c("gca.cases", "gca.deaths"))

cvd.cpp[lagtimes, on = "mc", cvd_lagtimes := i.cvd.lag]
c16.cpp[lagtimes, on = "mc", cancer_lagtimes := i.cancer.lag]

cpp <- merge(cvd.cpp, c16.cpp, by = c("year", "scenario", "sex", "agegroup", "qimd", "mc"), all.x = T, all.y = T)
rm(cvd.cpp, c16.cpp)
cpp <- dcast(cpp,
             year + agegroup + sex + qimd + mc + cvd_lagtimes +
               cancer_lagtimes ~ scenario, fun.aggregate = NULL,
             value.var = c("realpop", "pop", "chd.cases", "chd.deaths",
                           "stroke.cases", "stroke.deaths", "cvd.cases",
                           "cvd.deaths", "gca.cases", "gca.deaths"))
gc()

cpp[, `:=` (
  n_fsa = `realpop_FSA Led`/`pop_FSA Led`,
  n_rd = `realpop_Responsibility Deal`/`pop_Responsibility Deal`)]

# upscale cases(deaths) to population 
cpp[, `:=` (
  `chd.cases_FSA Led` = n_fsa * `chd.cases_FSA Led`,
  `chd.deaths_FSA Led` = n_fsa * `chd.deaths_FSA Led`,
  `stroke.cases_FSA Led` = n_fsa * `stroke.cases_FSA Led`,
  `stroke.deaths_FSA Led` = n_fsa * `stroke.deaths_FSA Led`,
  `cvd.cases_FSA Led` = n_fsa * `cvd.cases_FSA Led`,
  `cvd.deaths_FSA Led` = n_fsa * `cvd.deaths_FSA Led`,
  `gca.cases_FSA Led` = n_fsa * `gca.cases_FSA Led`,
  `gca.deaths_FSA Led` = n_fsa * `gca.deaths_FSA Led`,
  `chd.cases_Responsibility Deal` =
    n_rd * `chd.cases_Responsibility Deal`,
  `chd.deaths_Responsibility Deal` = 
    n_rd * `chd.deaths_Responsibility Deal`,
  `stroke.cases_Responsibility Deal` = 
    n_rd * `stroke.cases_Responsibility Deal`,
  `stroke.deaths_Responsibility Deal` = 
    n_rd * `stroke.deaths_Responsibility Deal`,
  `cvd.cases_Responsibility Deal` = 
    n_rd * `cvd.cases_Responsibility Deal`,
  `cvd.deaths_Responsibility Deal` =
    n_rd * `cvd.deaths_Responsibility Deal`,
  `gca.cases_Responsibility Deal` =
    n_rd * `gca.cases_Responsibility Deal`,
  `gca.deaths_Responsibility Deal` =
    n_rd * `gca.deaths_Responsibility Deal`,
  n_fsa = NULL, n_rd = NULL)]

# Salt intake  -------------------------------------------
# proof that sodium keeps reducing after the age of 64
load("./Lagtimes/HSE.ts.RData")
setDT(HSE.ts)
HSE.ts[, scatter.smooth(sodium~age)]
HSE.ts[, .("mean sodium" = mean(sodium, na.rm = T)), keyby = age
       ][, plot(age, `mean sodium`)]


salt[year >= 2011, .(fsa = mean(salt.fsa.mean), rd = mean(salt.rd.mean), diff = mean(salt.diff.mean)),
     keyby = .(scenario, year)]
salt[year >= 2011, .(sbp = mean(sbp.cvd.mean)), keyby = .(scenario)]

tt <- 
  salt[, .("current trends" = mean(salt.fsa.mean),
           "responsibility deal" = mean(salt.rd.mean)),
       keyby = .(scenario, year)]
tt[scenario == "current trends", scenario := scn.names[1]]
tt[scenario == "responsibility deal", scenario := scn.names[2]]
tt[, salt := `current trends`]
tt[scenario == scn.names[2] & year >= 2011, salt := `responsibility deal`] 


gg <-
  ggplot(tt[between(year, 2006, 2025), ],
         aes(x = year, y = salt, 
             col = scenario, linetype = rev(scenario), fill = scenario)) +
  #geom_point(size = 1, alpha = 5/5, show.legend = F) +
  geom_line(size = 2, alpha = 5/5) +
  #geom_ribbon(alpha = 2/5, linetype = 0, show.legend = F) +
  geom_hline(aes(yintercept = 6), show.legend = F, linetype = 2) +
  scale_x_continuous(name = "Year", breaks = c(2006, 2011, 2018, 2025),
                     limits = c(2006, 2025), expand = c(0, 0, 0, 0.5)) +
  scale_y_continuous(name = "Mean salt consumption (g/d)", limits = c(0, 9),
                     expand = c(0, 0, 0, 0), breaks = c(0, 3, 6, 9)) +
  scale_linetype_discrete(guide = FALSE) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(
          margin = margin(r = 24, unit = "pt")))

gg <- gg + 
  annotate("rect", xmin = 2011, xmax = 2018, ymin = 0, ymax = 9, alpha  = 1/15, fill = "red") +
  annotate("rect", xmin = 2018, xmax = 2025, ymin = 0, ymax = 9, alpha  = 1/15, fill = "blue") +
  annotate("text", x = 2013, y = .5, label = "Period 1", colour = "black") +
  annotate("text", x = 2020, y = .5, label = "Period 2", colour = "black")


cowplot::ggsave("salt_scenarios2025.tiff", gg, path = save_dir, dpi = 600,
                compression = "lzw", scale = 2, width = 8, height = 6, units = "cm")

# Socioeconomic gradient in sodium did not change over time?
HSE.ts[, qimdf := factor(qimd, ordered = F)]
HSE.ts[, summary(glm(sodium~age + sex + qimdf*year, weights = wt.urine))] 

# gg <-
#   ggplot(tt[between(year, 2006, 2025), ],
#          aes(x = year, y = salt, 
#              col = scenario, linetype = rev(scenario), fill = scenario)) +
#   #geom_point(size = 1, alpha = 5/5, show.legend = F) +
#   geom_line(size = 1, alpha = 5/5) +
#   #geom_ribbon(alpha = 2/5, linetype = 0, show.legend = F) +
#   geom_hline(aes(yintercept = 6), show.legend = F, linetype = 2) +
#   scale_x_continuous(name = "Year", breaks = c(2010, 2025)) +
#   scale_y_continuous(name = "Mean salt consumption (g/d)",
#                      limits = c(0, 10), breaks = c(0, 3, 6, 9)) +
#   scale_linetype_discrete(guide = FALSE) +
#   theme(legend.title = element_blank(),
#         legend.position = "bottom",
#         legend.text = element_text(
#           margin = margin(r = 24, unit = "pt")))
# cowplot::ggsave("salt_scenarios2025.tiff", gg, path = save_dir, dpi = 600,
#                 compression = "lzw", scale = 2, width = 8, height = 6, units = "cm")

# Burden estimation 2019 - 202x --------------------------
ext <- function(hor, strata = c("qimd", NULL)) {
  pr <- c(0.5, 0.25, 0.75, 0.1, 0.9)
  str <- c("mc", strata)
  res1 <- 
    cpp[between(year, 2019 + cvd_lagtimes, hor + cvd_lagtimes), .(
      cvd.cases = round(sum(`cvd.cases_Responsibility Deal` - 
                              `cvd.cases_FSA Led`)),
      cvd.deaths = round(sum(`cvd.deaths_Responsibility Deal` - 
                               `cvd.deaths_FSA Led`)),
      chd.cases = round(sum(`chd.cases_Responsibility Deal` - 
                              `chd.cases_FSA Led`)),
      chd.deaths = round(sum(`chd.deaths_Responsibility Deal` - 
                               `chd.deaths_FSA Led`)),
      stroke.cases = round(sum(`stroke.cases_Responsibility Deal` - 
                                 `stroke.cases_FSA Led`)),
      stroke.deaths = round(sum(`stroke.deaths_Responsibility Deal` - 
                                  `stroke.deaths_FSA Led`))
    ),
    by = str][, lapply(.SD, quantile, probs = pr),
              keyby = strata,
              .SDcols = !"mc"][, transp_nam(.SD), by = strata]
  
  res2 <- 
    cpp[between(year, 2019 + cvd_lagtimes, hor + cvd_lagtimes), .(
      cvd.cases = sum(`cvd.cases_Responsibility Deal`) < 
        sum(`cvd.cases_FSA Led`),
      cvd.deaths = sum(`cvd.deaths_Responsibility Deal`) < 
        sum(`cvd.deaths_FSA Led`),
      chd.cases = sum(`chd.cases_Responsibility Deal`) < 
        sum(`chd.cases_FSA Led`),
      chd.deaths = sum(`chd.deaths_Responsibility Deal`) < 
        sum(`chd.deaths_FSA Led`),
      stroke.cases = sum(`stroke.cases_Responsibility Deal`) < 
        sum(`stroke.cases_FSA Led`),
      stroke.deaths = sum(`stroke.deaths_Responsibility Deal`) < 
        sum(`stroke.deaths_FSA Led`)
    ),
    by = str][, lapply(.SD, function(x) sum(x)/numberofiterations),
              keyby = strata,
              .SDcols = !"mc"][, transp_nam(.SD), by = strata]
  setnames(res2, "V1", "Ps")
  
  res3 <- 
    cpp[between(year, 2019 + cancer_lagtimes, hor + cancer_lagtimes), .(gca.cases = round(sum(`gca.cases_Responsibility Deal` - 
                                                                                                `gca.cases_FSA Led`)),
                                                                        gca.deaths = round(sum(`gca.deaths_Responsibility Deal` - 
                                                                                                 `gca.deaths_FSA Led`))
    ),
    by = str][, lapply(.SD, quantile, probs = pr),
              keyby = strata,
              .SDcols = !"mc"][, transp_nam(.SD), by = strata]
  
  res4 <- 
    cpp[between(year, 2019 + cancer_lagtimes, hor + cancer_lagtimes), 
        .(gca.cases = sum(`gca.cases_Responsibility Deal`) < 
            sum(`gca.cases_FSA Led`),
          gca.deaths = sum(`gca.deaths_Responsibility Deal`) < 
            sum(`gca.deaths_FSA Led`)
        ),
        by = str][, lapply(.SD, function(x) sum(x)/numberofiterations),
                  keyby = strata,
                  .SDcols = !"mc"][, transp_nam(.SD), by = strata]
  setnames(res4, "V1", "Ps")
  
  res5 <- merge(res1, res2, by = c("names", strata))
  res6 <- merge(res3, res4, by = c("names", strata))
  res <- rbind(res5, res6)
  setnames(res, paste0("V", 1:5), c("median", 0.25, 0.75, 0.1, 0.9))
  if (is.null(strata)) {
    res <- res[, lapply(.SD, signif, 2), by = names]
  } else {
    
    res7 <- 
      cpp[between(year, 2019 + cvd_lagtimes, hor + cvd_lagtimes), .(
        cvd.cases = 1e5 * sum(`cvd.cases_Responsibility Deal` -
                                `cvd.cases_FSA Led`) /
          sum(`realpop_Responsibility Deal`),
        cvd.deaths = 1e5 * sum(`cvd.deaths_Responsibility Deal` - 
                                 `cvd.deaths_FSA Led`) /
          sum(`realpop_Responsibility Deal`),
        chd.cases = 1e5 * sum(`chd.cases_Responsibility Deal` - 
                                `chd.cases_FSA Led`) /
          sum(`realpop_Responsibility Deal`),
        chd.deaths = 1e5 * sum(`chd.deaths_Responsibility Deal` - 
                                 `chd.deaths_FSA Led`) /
          sum(`realpop_Responsibility Deal`),
        stroke.cases = 1e5 * sum(`stroke.cases_Responsibility Deal` - 
                                   `stroke.cases_FSA Led`) /
          sum(`realpop_Responsibility Deal`),
        stroke.deaths = 1e5 * sum(`stroke.deaths_Responsibility Deal` - 
                                    `stroke.deaths_FSA Led`) /
          sum(`realpop_Responsibility Deal`)
      ),
      by = str][, lapply(.SD, quantile, probs = pr),
                keyby = strata,
                .SDcols = !"mc"][, transp_nam(.SD), by = strata]
    
    res8 <- 
      cpp[between(year, 2019 + cancer_lagtimes, hor + cancer_lagtimes), .(
        gca.cases = 1e5 * sum(`gca.cases_Responsibility Deal` -                                    `gca.cases_FSA Led`) /
          sum(`realpop_Responsibility Deal`),
        gca.deaths = 1e5 * sum(`gca.deaths_Responsibility Deal` - 
                                 `gca.deaths_FSA Led`) /
          sum(`realpop_Responsibility Deal`)
      ),
      by = str][, lapply(.SD, quantile, probs = pr),
                keyby = strata,
                .SDcols = !"mc"][, transp_nam(.SD), by = strata]
    res9 <- rbind(res7, res8)
    setnames(res9, paste0("V", 1:5),
             paste0("rate_", c("median", 0.25,
                               0.75, 0.1, 0.9)))
    res <- res[res9, on = c("names", strata)]
    
    res10 <- 
      cpp[between(year, 2019 + cvd_lagtimes, hor + cvd_lagtimes), .(
        cvd.cases = 1e5 * sum(`cvd.cases_Responsibility Deal` - 
                                `cvd.cases_FSA Led`) /
          sum(`cvd.cases_FSA Led`),
        cvd.deaths = 1e5 * sum(`cvd.deaths_Responsibility Deal` - 
                                 `cvd.deaths_FSA Led`) /
          sum(`cvd.deaths_FSA Led`),
        chd.cases = 1e5 * sum(`chd.cases_Responsibility Deal` - 
                                `chd.cases_FSA Led`) /
          sum(`chd.cases_FSA Led`),
        chd.deaths = 1e5 * sum(`chd.deaths_Responsibility Deal` - 
                                 `chd.deaths_FSA Led`) /
          sum(`chd.deaths_FSA Led`),
        stroke.cases = 1e5 * sum(`stroke.cases_Responsibility Deal` -                                    `stroke.cases_FSA Led`) /
          sum(`stroke.cases_FSA Led`),
        stroke.deaths = 1e5 * sum(`stroke.deaths_Responsibility Deal` - 
                                    `stroke.deaths_FSA Led`) /
          sum(`stroke.deaths_FSA Led`)
      ),
      by = str][, lapply(.SD, quantile, probs = pr),
                keyby = strata,
                .SDcols = !"mc"][, transp_nam(.SD), by = strata]
    
    res11 <- 
      cpp[between(year, 2019 + cvd_lagtimes, hor + cvd_lagtimes), .(
        gca.cases = 1e5 * sum(`gca.cases_Responsibility Deal` -                                    `gca.cases_FSA Led`) /
          sum(`gca.cases_FSA Led`),
        gca.deaths = 1e5 * sum(`gca.deaths_Responsibility Deal` - 
                                 `gca.deaths_FSA Led`) /
          sum(`gca.deaths_FSA Led`)
      ),
      by = str][, lapply(.SD, quantile, probs = pr),
                keyby = strata,
                .SDcols = !"mc"][, transp_nam(.SD), by = strata]
    res12 <- rbind(res10, res11)
    setnames(res12, paste0("V", 1:5),
             paste0("burden_", c("median", 0.25,
                                 0.75, 0.1, 0.9)))
    res <- res[res12, on = c("names", strata)]
    res <- res[names %like% "cases",
               lapply(.SD, signif, 2), by = c("names", strata)]
  }
  setkeyv(res, c("names", strata))
  return(res)
}
fwrite(ext(2025, NULL), paste0(save_dir, "results_2025.csv"))
# fwrite(ext(2025, NULL), paste0(save_dir, "results_2025.csv"))

fwrite(ext(2025, "qimd"), paste0(save_dir, "results_2025_qimd.csv"))
# fwrite(ext(2025, "qimd"), paste0(save_dir, "results_2025_qimd.csv"))

hor <- 2025
str <- c("mc", "qimd")
t1 <- cpp[between(year, 2019 + cvd_lagtimes, hor + cvd_lagtimes) & qimd == 1, .(
  cvd.cases = sum(`cvd.cases_Responsibility Deal` -
                    `cvd.cases_FSA Led`)
),
by = str]

t5 <- cpp[between(year, 2019 + cvd_lagtimes, hor + cvd_lagtimes) & qimd == 5, .(
  cvd.cases = sum(`cvd.cases_Responsibility Deal` -
                    `cvd.cases_FSA Led`)
),
by = str]

t1[t5, on = "mc", sum(cvd.cases < i.cvd.cases)/.N] # Prob of causing more cases in qimd 5 (deprived)


t1 <- cpp[between(year, 2019 + cvd_lagtimes, hor + cvd_lagtimes) & qimd == 1, .(
  cvd.cases = 1e5 * sum(`cvd.cases_Responsibility Deal` -
                          `cvd.cases_FSA Led`) /
    sum(`realpop_Responsibility Deal`)
),
by = str]

t5 <- cpp[between(year, 2019 + cvd_lagtimes, hor + cvd_lagtimes) & qimd == 5, .(
  cvd.cases = 1e5 * sum(`cvd.cases_Responsibility Deal` -
                          `cvd.cases_FSA Led`) /
    sum(`realpop_Responsibility Deal`)
),
by = str]

t1[t5, on = "mc", sum(cvd.cases<i.cvd.cases)/.N] # Prob of causing more cases/1e5 in qimd 5 (deprived)


t1 <- cpp[between(year, 2019 + cvd_lagtimes, hor + cvd_lagtimes) & qimd == 1, .(
  cvd.cases = 1e5 * sum(`cvd.cases_Responsibility Deal` -
                          `cvd.cases_FSA Led`) /
    sum(`cvd.cases_FSA Led`)
),
by = str]

t5 <- cpp[between(year, 2019 + cvd_lagtimes, hor + cvd_lagtimes) & qimd == 5, .(
  cvd.cases = 1e5 * sum(`cvd.cases_Responsibility Deal` -
                          `cvd.cases_FSA Led`) /
    sum(`cvd.cases_FSA Led`)
),
by = str]

t1[t5, on = "mc", sum(cvd.cases<i.cvd.cases)/.N] # Prob of causing more cases/1e5 baseline cases in qimd 5 (deprived)

t1 <- cpp[between(year, 2019 + cancer_lagtimes, hor + cancer_lagtimes) & qimd == 1, .(
  gca.cases = sum(`gca.cases_Responsibility Deal` -
                    `gca.cases_FSA Led`)
),
by = str]

t5 <- cpp[between(year, 2019 + cancer_lagtimes, hor + cancer_lagtimes) & qimd == 5, .(
  gca.cases = sum(`gca.cases_Responsibility Deal` -
                    `gca.cases_FSA Led`)
),
by = str]

t1[t5, on = "mc", sum(gca.cases<i.gca.cases)/.N] # Prob of causing more cases in qimd 5 (deprived)


t1 <- cpp[between(year, 2019 + cancer_lagtimes, hor + cancer_lagtimes) & qimd == 1, .(
  gca.cases = 1e5 * sum(`gca.cases_Responsibility Deal` -
                          `gca.cases_FSA Led`) /
    sum(`realpop_Responsibility Deal`)
),
by = str]

t5 <- cpp[between(year, 2019 + cancer_lagtimes, hor + cancer_lagtimes) & qimd == 5, .(
  gca.cases = 1e5 * sum(`gca.cases_Responsibility Deal` -
                          `gca.cases_FSA Led`) /
    sum(`realpop_Responsibility Deal`)
),
by = str]

t1[t5, on = "mc", sum(gca.cases<i.gca.cases)/.N] # Prob of causing more cases/1e5 in qimd 5 (deprived)


t1 <- cpp[between(year, 2019 + cancer_lagtimes, hor + cancer_lagtimes) & qimd == 1, .(
  gca.cases = 1e5 * sum(`gca.cases_Responsibility Deal` -
                          `gca.cases_FSA Led`) /
    sum(`gca.cases_FSA Led`)
),
by = str]

t5 <- cpp[between(year, 2019 + cancer_lagtimes, hor + cancer_lagtimes) & qimd == 5, .(
  gca.cases = 1e5 * sum(`gca.cases_Responsibility Deal` -
                          `gca.cases_FSA Led`) /
    sum(`gca.cases_FSA Led`)
),
by = str]

t1[t5, on = "mc", sum(gca.cases<i.gca.cases)/.N] # Prob of causing more cases/1e5 baseline cases in qimd 5 (deprived)


t1 <- cpp[between(year, 2019 + cvd_lagtimes, hor + cvd_lagtimes) & qimd == 1, .(
  cvd.cases = 1e5 * sum(`cvd.cases_Responsibility Deal` -
                          `cvd.cases_FSA Led`) /
    sum(`realpop_Responsibility Deal`)
),
by = str]

# Costs -------------------------------------------------------
# costs are 2018 £. discounted by 3.5% for years after 2018 and inflated by 3.5% for years before
# gastric costs not by deprivation because no gradient was observed in policy effect
disease_costs <- fread("./Economics/costs.csv")[, qimd := factor(qimd, ordered = T)]
# Stroke productivity cost for 2018 : £4575 (only applies to people aged under 65)
# CHD productivity cost for 2018: £4903 (only applies to people aged under 65)
# Gastric cancer productivity cost for 2018: £48124 (applies to gastric cancer deaths)

cpp[disease_costs[condition == "chd_exclusive" & year == 1], on = "qimd",
    `:=` (`cost.chd.cases_FSA Led` = `chd.cases_FSA Led` * costs_2018, 
          `cost.chd.cases_Responsibility Deal` = `chd.cases_Responsibility Deal` * costs_2018
    )]

chd.burden[lagtimes, on = "mc", cvd_lagtimes := i.cvd.lag]
stroke.burden[lagtimes, on = "mc", cvd_lagtimes := i.cvd.lag]
c16.burden[lagtimes, on = "mc", cancer_lagtimes := i.cancer.lag]

hor <- 2025
chd.cost <- chd.burden[group == "SAQ" & between(year, 2019 + cvd_lagtimes, hor + cvd_lagtimes),
                       .(chd.incidence,
                         chd.prevalence,
                         chd.mortality, pop), 
                       by = .(year, scenario, sex, agegroup, qimd, mc)]
setkey(chd.cost, year, scenario, sex, agegroup, qimd, mc)
chd.cost[pop.abs, on = c("year", "scenario", "sex", "agegroup", "qimd", "mc"), realpop := i.realpop]

stroke.cost <- stroke.burden[group == "SAQ" & between(year, 2019 + cvd_lagtimes, hor + cvd_lagtimes), 
                             .(stroke.incidence,
                               stroke.prevalence,
                               stroke.mortality), 
                             by = .(year, scenario, sex, agegroup, qimd, mc)]
setkey(stroke.cost, year, scenario, sex, agegroup, qimd, mc)

cvd.cost <- chd.cost[stroke.cost, on = c("year", "scenario", "sex", "agegroup", "qimd", "mc")]

tt1 <- disease_costs[condition == "chd_exclusive" & year == 1, .(qimd, chd.incidence.cost = costs_2018)]
tt2 <- disease_costs[condition == "chd_exclusive" & year == 2, .(qimd, chd.prevalence.cost = costs_2018)]
tt3 <- disease_costs[condition == "chd_death", .(qimd = factor(1:5, ordered = T), chd.mortality.cost = costs_2018)]
tt4 <- disease_costs[condition == "stroke_exclusive" & year == 1,
                     .(qimd, stroke.incidence.cost = costs_2018)]
tt5 <- disease_costs[condition == "stroke_exclusive" & year == 2,
                     .(qimd, stroke.prevalence.cost = costs_2018)]
tt6 <- disease_costs[condition == "stroke_death", .(qimd = factor(1:5, ordered = T),stroke.mortality.cost = costs_2018)]


tt <- tt1[tt2, on = "qimd"]
tt <- tt[tt3, on = "qimd"]
tt <- tt[tt4, on = "qimd"]
tt <- tt[tt5, on = "qimd"]
tt <- tt[tt6, on = "qimd"]

cvd.cost[tt, on = "qimd", `:=`
         (chd.incidence.cost = i.chd.incidence.cost,
           chd.prevalence.cost = i.chd.prevalence.cost,
           chd.mortality.cost = i.chd.mortality.cost,
           stroke.incidence.cost = i.stroke.incidence.cost,
           stroke.prevalence.cost = i.stroke.prevalence.cost,
           stroke.mortality.cost = i.stroke.mortality.cost,
           chd.productivity.cost = 0,
           stroke.productivity.cost = 0)]
cvd.cost[(agegroup %in% unique(agegroup.part(30:64))), `:=` 
         (chd.productivity.cost = 4903,
           stroke.productivity.cost = 4575)]

cvd.cost[, chd.med.cost := chd.incidence * chd.incidence.cost * realpop/pop +
           (chd.prevalence - chd.incidence - chd.mortality/2) * chd.prevalence.cost * realpop/pop + 
           chd.mortality * chd.mortality.cost * realpop/pop]
cvd.cost[, stroke.med.cost := stroke.incidence * stroke.incidence.cost * realpop/pop +
           (stroke.prevalence - stroke.incidence - stroke.mortality/2) * stroke.prevalence.cost * realpop/pop + 
           stroke.mortality * stroke.mortality.cost * realpop/pop]
cvd.cost[, cvd.med.cost := chd.med.cost + stroke.med.cost]
cvd.cost[, cvd.prod.cost := (chd.prevalence - chd.mortality/2) * chd.productivity.cost * realpop/pop+
           (stroke.prevalence - stroke.mortality/2) * stroke.productivity.cost * realpop/pop]
cvd.cost[, cvd.med.cost := cvd.med.cost * (1.035^(year - 2018))] # Discounting
cvd.cost[, cvd.prod.cost := cvd.prod.cost * (1.035^(year - 2018))] # Discounting

# Medical costs from CVD in million £
tt1 <- cvd.cost[scenario == scn.names[[1]], .(cvd.med.cost.fsa = sum(cvd.med.cost)), keyby = mc]
tt2 <- cvd.cost[scenario == scn.names[[2]], .(cvd.med.cost.rd  = sum(cvd.med.cost)), keyby = mc]
tt <- tt1[tt2, on = "mc"]
# signif(quantile(tt1[tt2, on = "mc", (cvd.med.cost.rd - cvd.med.cost.fsa)/1e6], c(0.5, 0.25, 0.75)), 2) # in million £
res <- as.data.table(c(cost = "cvd.med.cost (in million)", as.list(signif(quantile(tt1[tt2, on = "mc", (cvd.med.cost.rd - cvd.med.cost.fsa)/1e6], c(0.5, 0.25, 0.75)), 2)))) # in million £
# 50% 25% 75% 
# 180 120 240
# log
# 50% 25% 75% 
# 110  52 170 

# Productivity costs from CVD in million £
tt1 <- cvd.cost[scenario == scn.names[[1]], .(cvd.prod.cost.fsa = sum(cvd.prod.cost)), keyby = mc]
tt2 <- cvd.cost[scenario == scn.names[[2]], .(cvd.prod.cost.rd = sum(cvd.prod.cost)), keyby = mc]
tt <- tt[tt1, on = "mc"]
tt <- tt[tt2, on = "mc"]
# signif(quantile(tt1[tt2, on = "mc", (cvd.prod.cost.rd - cvd.prod.cost.fsa)/1e6], c(0.5, 0.25, 0.75)), 2) # in million £
res <- rbind(res, 
             as.data.table(c(cost = "cvd.prod.cost (in million)", as.list(signif(quantile(tt1[tt2, on = "mc", (cvd.prod.cost.rd - cvd.prod.cost.fsa)/1e6], c(0.5, 0.25, 0.75)), 2)))) )
# 50% 25% 75% 
# 90  43 140 
# log
# 50% 25% 75% 
# 61  10 110 
c16.cost <- c16.burden[group == "SAQ" & between(year, 2019 + cancer_lagtimes, hor + cancer_lagtimes),
                       .(c16.incidence,
                         c16.prevalence,
                         c16.mortality, pop), 
                       by = .(year, scenario, sex, agegroup, qimd, mc)]
setkey(c16.cost, year, scenario, sex, agegroup, qimd, mc)
c16.cost[pop.abs, on = c("year", "scenario", "sex", "agegroup", "qimd", "mc"), realpop := i.realpop]

tt1 <- disease_costs[condition == "gastric_cancer", .(qimd, gca.prevalence.cost = costs_2018)]
c16.cost[tt1, on = "qimd", `:=`
         (c16.prevalence.cost = i.gca.prevalence.cost,
           c16.productivity.cost = 0)]
c16.cost[(agegroup %in% unique(agegroup.part(30:64))), `:=` 
         (c16.productivity.cost = 48124.389)] # apply on deaths

c16.cost[, c16.med.cost := (c16.prevalence - c16.mortality/2) *
           c16.prevalence.cost * realpop/pop]
c16.cost[, c16.prod.cost := c16.mortality *
           c16.productivity.cost * realpop/pop]

c16.cost[, c16.med.cost := c16.med.cost * (1.035^(year - 2018))] # Discounting
c16.cost[, c16.prod.cost := c16.prod.cost * (1.035^(year - 2018))] # Discounting

# Medical costs from C16 in million £
tt1 <- c16.cost[scenario == scn.names[[1]], .(c16.med.cost.fsa = sum(c16.med.cost)), keyby = mc]
tt2 <- c16.cost[scenario == scn.names[[2]], .(c16.med.cost.rd  = sum(c16.med.cost)), keyby = mc]
tt <- tt[tt1, on = "mc"]
tt <- tt[tt2, on = "mc"]
# signif(quantile(tt1[tt2, on = "mc", (c16.med.cost.rd - c16.med.cost.fsa)/1e6], c(0.5, 0.25, 0.75)), 2) # in million £
res <- rbind(res, 
             as.data.table(c(cost = "c16.med.cost (in million)", as.list(signif(quantile(tt1[tt2, on = "mc", (c16.med.cost.rd - c16.med.cost.fsa)/1e6], c(0.5, 0.25, 0.75)), 2)))) )

# 50%   25%   75% 
# 67.0   9.6 110.0
# log
# 50% 25% 75% 
# 39 -13  92 

# Productivity costs from C16 in million £
tt1 <- c16.cost[scenario == scn.names[[1]], .(c16.prod.cost.fsa = sum(c16.prod.cost)), keyby = mc]
tt2 <- c16.cost[scenario == scn.names[[2]], .(c16.prod.cost.rd  = sum(c16.prod.cost)), keyby = mc]
tt <- tt[tt1, on = "mc"]
tt <- tt[tt2, on = "mc"]
# signif(quantile(tt1[tt2, on = "mc", (c16.prod.cost.rd - c16.prod.cost.fsa)/1e6], c(0.5, 0.25, 0.75)), 2) # in million £
res <- rbind(res, 
             as.data.table(c(cost = "c16.prod.cost (in million)", as.list(signif(quantile(tt1[tt2, on = "mc", (c16.prod.cost.rd - c16.prod.cost.fsa)/1e6], c(0.5, 0.25, 0.75)), 2)))) )

# 50%   25%   75% 
# 14   -14    43
# log
# 50% 25% 75% 
# 11 -21  40 

# total costs
# tt[, .(cvd.med.cost.rd + cvd.prod.cost.rd + c16.med.cost.rd + c16.prod.cost.rd -
#      cvd.med.cost.fsa - cvd.prod.cost.fsa - c16.med.cost.fsa - c16.prod.cost.fsa)][, signif(quantile(V1, c(0.5, 0.25, 0.75))/1e6, 2)]
res <- rbind(res, 
             as.data.table(c(cost = "total.cost (in million)", as.list(tt[, .(cvd.med.cost.rd + cvd.prod.cost.rd + c16.med.cost.rd + c16.prod.cost.rd -
                                                                                cvd.med.cost.fsa - cvd.prod.cost.fsa - c16.med.cost.fsa - c16.prod.cost.fsa)][, signif(quantile(V1, c(0.5, 0.25, 0.75))/1e6, 2)]))) )

# 50% 25% 75% 
# 350 250 460 
# log
# 50% 25% 75% 
# 220  99 340
res[, Ps := NA_real_]
res[cost == "total.cost (in million)", Ps :=  tt[, .(cvd.med.cost.rd + cvd.prod.cost.rd + c16.med.cost.rd + c16.prod.cost.rd -
                                                       cvd.med.cost.fsa - cvd.prod.cost.fsa - c16.med.cost.fsa - c16.prod.cost.fsa)][, sum(V1<=0)/.N]
    ]
#0.019
# log
#0.086

# Med costs
res <- rbind(res, 
             as.data.table(c(cost = "total.med.cost (in million)", as.list(tt[, .(cvd.med.cost.rd + c16.med.cost.rd  -
                                                                                    cvd.med.cost.fsa - c16.med.cost.fsa)][, signif(quantile(V1, c(0.5, 0.25, 0.75))/1e6, 2)]))), fill = T)

# 50% 25% 75% 
# 240 160 320  
# log
# 50% 25% 75% 
# 150  69 240 

# Prod costs
res <- rbind(res, 
             as.data.table(c(cost = "total.prod.cost (in million)", as.list(tt[, .(cvd.prod.cost.rd + c16.prod.cost.rd  -
                                                                                     cvd.prod.cost.fsa - c16.prod.cost.fsa)][, signif(quantile(V1, c(0.5, 0.25, 0.75))/1e6, 2)]))), fill = T)
# 50% 25% 75% 
# 110  50 160  
# log
# 50% 25% 75% 
# 71  16 130 
fwrite(res, paste0(save_dir, "costs_2025.csv"))
