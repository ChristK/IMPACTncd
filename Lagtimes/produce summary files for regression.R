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
               "compiler",
               "survey",
               "MASS",
               "glm2",
               "StatMatch",
               "pryr",
               "mc2d",
               "rriskDistributions",
               "quantreg",
               "foreach",
               "rms"))



# Get Dropbox folder.  Automaticly define of my dropbox folder in windows Manually define on linux
options(warn = 1)

if (Sys.info()[1] == "Linux") {
  if (system("whoami", T )== "mdxasck2") {
    setwd("~/IMPACTncd/")
    clusternumber <- ifelse (clusternumber<70, 70, clusternumber)  # overwrites previous if <60
  } else {
    setwd(paste("/home/", 
                system("whoami", T), 
                "/Dropbox/PhD/Models/IMPACTncd/", 
                sep = "", 
                collapse = ""))
  }
} else if (Sys.info()[1] == "Darwin") {
  setwd("/Users/chris/Dropbox/PhD/")
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

# Define function to calculate SD from svy objects (not necessary)
# svysd <- function(...) sqrt(coef(svyvar(...)))

# Define function to split agegroups and create groups
agegroup.fn <- function(x, lagtime = 0) {
  breaks                   <- c(0, 1, seq(5, 85, 5), 130)
  labels                   <- c("<1   ", "01-04", "05-09",
                                "10-14", "15-19", "20-24", 
                                "25-29", "30-34", "35-39", 
                                "40-44", "45-49", "50-54",
                                "55-59", "60-64", "65-69",
                                "70-74", "75-79", "80-84", "85+")
  if (is.numeric(x)) { 
    agegroup = cut(x - lagtime, 
                   breaks = breaks, 
                   labels = labels, 
                   include.lowest = T, 
                   right = F, 
                   ordered_result = T)
    return(invisible(agegroup))    
  } else {
    if (is.data.table(x)) {
      x[, agegroup := cut(as.numeric(as.character(age)) + lagtime, 
                          breaks = breaks, 
                          labels = labels, 
                          include.lowest = T, 
                          right = F, 
                          ordered_result = T)]
      x[, group := paste(qimd, sex, agegroup, sep='')]
      return(invisible(x))
    } else return(print("not eligible input"))
  }
}
options(survey.lonely.psu = "adjust") #Lonely PSU (center any single-PSU strata around the sample grand mean rather than the stratum mean)
#options(datatable.auto.index= F) # needed until bug is resolved

# load single datasets ----------------------------------------------------
# Import datasets (house hold and individual files)
load(file="./Datasets/Health Survey for England/2012/hse2012ai.RData")
HSE2012original <- clear.labels(HSE2012original)
HSE2012original <- data.table(HSE2012original, key="age")
agegroup.fn(HSE2012original)
#HSE.1.srv.blood <- svydesign(id=~psu, strata=~cluster, weights=~wt.blood, nest=F, data=HSE2012original[wt.blood>0], check.strata = T)
#HSE.1.srv.nurse <- svydesign(id=~psu, strata=~cluster, weights=~wt.nurse, nest=F, data=HSE2012original[wt.nurse>0], check.strata = T)

HSE2012 =copy(HSE2012original[, list(wt.int, wt.nurse, wt.blood, psu, cluster, age, agegroup, sex, group, qimd, bmival, cholval12, omsysval, diabtotr, cigst1, startsmk, endsmoke, numsmok, smokyrs, cigdyal, a30to06, sodiumval, potass, creatin, wt.urine, origin, hdlval12, bpmedd2, diabete2)])
HSE2012[, `:=`(year=1, porftvg = NA, frtpor = NA)]
setnames(HSE2012, "sodiumval", "sodium")
HSE2012[, psu := paste0(psu, "2012")]
HSE2012[, cluster := paste0(cluster, "2012")]
setnames(HSE2012, c("cholval12", "hdlval12", "bpmedd2"), c("cholval1", "hdlval1", "bpmedd"))

load(file="./Datasets/Health Survey for England/2011/hse2011ai.RData")
HSE2011original <- clear.labels(HSE.2011)
rm(HSE.2011)
HSE2011original <- data.table(HSE2011original, key="age")
agegroup.fn(HSE2011original)
#HSE0.srv.blood <- svydesign(id=~psu, strata=~cluster, weights=~wt.blood, nest=F, data=HSE2011original[wt.blood>0], check.strata = T)
#HSE0.srv.nurse <- svydesign(id=~psu, strata=~cluster, weights=~wt.nurse, nest=F, data=HSE2011original[wt.nurse>0], check.strata = T)
#HSE0.srv.int <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=HSE2011original[wt.nurse>0], check.strata = T)

HSE2011 =copy(HSE2011original[, list(wt.int, wt.nurse, wt.blood, psu, cluster, age, agegroup, sex, group, qimd, bmival, cholval1, omsysval, diabtotr, cigst1, startsmk, endsmoke, porftvg, frtpor, numsmok, smokyrs, cigdyal, origin, hdlval1,
                                     iregdef, bpmedd, diabete2)])
HSE2011[, `:=`(year=0, a30to06 = NA, sodium = NA, potass = NA, creatin = NA, wt.urine = 0)]
HSE2011[, psu := paste0(psu, "2011")]
HSE2011[, cluster := paste0(cluster, "2011")]

load(file="./Datasets/Health Survey for England/2010/hse10ai.RData")
HSE2010original <- setDT(clear.labels(HSE2010original))
HSE2010original <- filter(HSE2010original, samptype==1)
setnames(HSE2010original, "imd2007", "qimd")
HSE2010original[cholflag == 1, `:=` (cholval1 = cholval1 + 0.1, hdlval1 = hdlval1 - 0.1)]
HSE2010original[is.na(wt.nurse), wt.nurse := 0]
HSE2010original[is.na(wt.blood), wt.blood := 0]
agegroup.fn(HSE2010original)
HSE2010original[diabete2 == 2, diabtotr := 1]
HSE2010original[diabete2 == 1 | glyhbval > 6.5, diabtotr := 2]
#HSE1.srv.blood <- svydesign(id=~psu, strata=~cluster, weights=~wt.blood, nest=F, data=HSE2010original[wt.blood>0], check.strata = T)
#HSE1.srv.nurse <- svydesign(id=~psu, strata=~cluster, weights=~wt.nurse, nest=F, data=HSE2010original[wt.nurse>0], check.strata = T)
#HSE1.srv.int <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=HSE2010original[wt.int>0], check.strata = T)

HSE2010 =copy(HSE2010original[, list(wt.int, wt.nurse, wt.blood, psu, cluster, age, agegroup, sex, group, qimd, bmival, cholval1, omsysval, diabtotr, cigst1, startsmk, endsmoke, porftvg, frtpor, numsmok, smokyrs, cigdyal, sodival, potass, creatin, wt.urine, origin, hdlval1, kiddiag, bpmedd, diabete2)])
HSE2010[, `:=`(year=-1, a30to06 = NA)]
setnames(HSE2010, c("sodival"), c("sodium"))
HSE2010[, psu := paste0(psu, "2010")]
HSE2010[, cluster := paste0(cluster, "2010")]

load(file="./Datasets/Health Survey for England/2009/hse09ai.RData")
HSE2009original <- setDT(clear.labels(HSE2009original))
HSE2009original <- filter(HSE2009original, samptype==1)
setnames(HSE2009original, "imd2007" , "qimd")
HSE2009original[, `:=` (cholval1 = cholval1 + 0.1, hdlval1 = hdlval1 - 0.1)]
agegroup.fn(HSE2009original)
HSE2009original[diabete2 == 2, diabtotr := 1]
HSE2009original[diabete2 == 1 | glyhbval > 6.5, diabtotr := 2]
#HSE2.srv.blood <- svydesign(id=~psu, strata=~cluster, weights=~wt.blood, nest=F, data=HSE2009original[wt.blood>0], check.strata = T)
#HSE2.srv.nurse <- svydesign(id=~psu, strata=~cluster, weights=~wt.nurse, nest=F, data=HSE2009original[wt.nurse>0], check.strata = T)
#HSE2.srv.int <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=HSE2009original[wt.int>0], check.strata = T)

HSE2009 =copy(HSE2009original[, list(wt.int, wt.nurse, wt.blood, psu, cluster, age, agegroup, sex, group, qimd, bmival, cholval1, omsysval, diabtotr, cigst1, startsmk, endsmoke, porftvg, frtpor, numsmok, smokyrs, cigdyal, sodium, potass, creatin, wt.urine, origin, hdlval1, bpmedd, diabete2)])
HSE2009[, `:=`(year=-2, a30to06 = NA)]
HSE2009[, psu := paste0(psu, "2009")]
HSE2009[, cluster := paste0(cluster, "2009")]

load(file="./Datasets/Health Survey for England/2008/hse08ai.RData")
HSE2008original <- setDT(clear.labels(HSE2008original))
HSE2008original <- filter(HSE2008original, samptype==1) 
HSE2008original[, `:=` (cholval1 = cholval1 + 0.1, hdlval1 = hdlval1 - 0.1)]
agegroup.fn(HSE2008original)
#HSE3.srv.blood <- svydesign(id=~psu, strata=~cluster, weights=~wt.blood, nest=F, data=HSE2008original[wt.blood>0], check.strata = T)
#HSE3.srv.nurse <- svydesign(id=~psu, strata=~cluster, weights=~wt.nurse, nest=F, data=HSE2008original[wt.nurse>0], check.strata = T)
#HSE3.srv.int <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=HSE2008original[wt.int>0], check.strata = T)

HSE2008 =copy(HSE2008original[, list(wt.int, wt.nurse, wt.blood, psu, cluster, age, agegroup, sex, group, qimd, bmival, cholval1, omsysval,  cigst1, startsmk, endsmoke, porftvg, frtpor, numsmok, smokyrs, cigdyal, a30to06, origin, hdlval1,
                                     bpmedd)])
HSE2008[, `:=`(year=-3, diabtotr = NA, diabete2 = NA, sodium = NA, potass = NA, creatin = NA, wt.urine = 0)]
HSE2008[, psu := paste0(psu, "2008")]
HSE2008[, cluster := paste0(cluster, "2008")]


load(file="./Datasets/Health Survey for England/2007/hse07ai.RData")
HSE2007original <- setDT(clear.labels(HSE2007original))
HSE2007original <- filter(HSE2007original, samptype==1)
setnames(HSE2007original, c("imd2007", "area"), c("qimd", "psu"))
agegroup.fn(HSE2007original)
#HSE4.srv.nurse <- svydesign(id=~psu, strata=~cluster, weights=~wt.nurse, nest=F, data=HSE2007original[wt.nurse>0], check.strata = T)
#HSE4.srv.int <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=HSE2007original[wt.int>0], check.strata = T)

HSE2007 =copy(HSE2007original[, list(wt.int, wt.nurse,  psu, cluster, age, agegroup, sex, group, qimd, bmival, omsysval, cigst1, startsmk, endsmoke, porftvg, frtpor, numsmok, smokyrs, cigdyal, sodium, potass, creatin, bpmedd)])
HSE2007[, `:=`(year=-4, diabtotr = NA, diabete2 = NA, wt.blood = 1, cholval1 = NA, a30to06 = NA, wt.urine = wt.nurse)]
HSE2007[, psu := paste0(psu, "2007")]
HSE2007[, cluster := paste0(cluster, "2007")]

load(file="./Datasets/Health Survey for England/2006/hse06ai.RData")
HSE <- setDT(clear.labels(HSE))
HSE2006original <- filter(HSE, samptype!=3)
rm(HSE)
setnames(HSE2006original, "imd2004", "qimd")
HSE2006original[, `:=` (cholval1 = cholval1 + 0.1, hdlval1 = hdlval1 - 0.1)]
agegroup.fn(HSE2006original)
HSE2006original[diabete2 == 2, diabtotr := 1]
HSE2006original[diabete2 == 1 | glyhbval > 6.5, diabtotr := 2]
#HSE5.srv.blood <- svydesign(id=~psu, strata=~cluster, weights=~wt.blood, nest=F, data=HSE2006original[wt.blood>0], check.strata = T)
#HSE5.srv.nurse <- svydesign(id=~psu, strata=~cluster, weights=~wt.nurse, nest=F, data=HSE2006original[wt.nurse>0], check.strata = T)
#HSE5.srv.int <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=HSE2006original[wt.int>0], check.strata = T)

HSE2006 =copy(HSE2006original[, list(wt.int, wt.nurse, wt.blood, psu, cluster, age, agegroup, sex, group, qimd, bmival, cholval1, omsysval, diabtotr, cigst1, startsmk, endsmoke, porftvg, frtpor, numsmok, smokyrs, cigdyal, a30to06, sodium, potass, creatin, famcvd, hdlval1, bpmedd, diabete2)])
HSE2006[, `:=`(year=-5, wt.urine = wt.nurse)]
HSE2006[, psu := paste0(psu, "2006")]
HSE2006[, cluster := paste0(cluster, "2006")]

load(file="./Datasets/Health Survey for England/2005/hse05ai.RData") # Only individuals aged 65 and over were asked for a blood sample
HSE2005original <- setDT(clear.labels(HSE2005original))
HSE2005original <- filter(HSE2005original, samptype==1) 
setnames(HSE2005original, c("imd2004", "area", "wt.bldel"), c("qimd", "psu", "wt.blood"))
HSE2005original[, `:=` (cholval1 = cholval1 + 0.1, hdlval1 = hdlval1 - 0.1)]
agegroup.fn(HSE2005original)
HSE2005original[diabete2 == 2, diabtotr := 1]
HSE2005original[diabete2 == 1 | glyhbval > 6.5, diabtotr := 2]
#HSE6.srv.blood <- svydesign(id=~psu, strata=~cluster, weights=~wt.blood, nest=F, data=HSE2005original[wt.blood>0], check.strata = T)
#HSE6.srv.nurse <- svydesign(id=~psu, strata=~cluster, weights=~wt.nurse, nest=F, data=HSE2005original[wt.nurse>0], check.strata = T)
#HSE6.srv.int <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=HSE2005original[wt.int>0], check.strata = T)

HSE2005 =copy(HSE2005original[, list(wt.int, wt.nurse, wt.blood, psu, cluster, age, agegroup, sex, group, qimd, bmival, cholval1, omsysval, diabtotr, cigst1, startsmk, endsmoke, porftvg, frtpor, numsmok, smokyrs, cigdyal, sodium, potass,
                                     creatin, hdlval1, bpmedd, diabete2)])
HSE2005[, `:=`(year=-6, a30to06 = NA, wt.urine = wt.nurse)]
HSE2005[, psu := paste0(psu, "2005")]
HSE2005[, cluster := paste0(cluster, "2005")]

load(file="./Datasets/Health Survey for England/2004/hse04gpa.RData") # It seems to be a mess with the weighting for the core and boost sample
HSE2004original <- setDT(clear.labels(HSE2004original))
HSE2004original <- filter(HSE2004original, samptype==7)
setnames(HSE2004original, c("imd2004", "area"), c("qimd", "psu"))
HSE2004original[, `:=` (cholval1 = cholval1 + 0.1, hdlval1 = hdlval1 - 0.1)]
agegroup.fn(HSE2004original)
HSE2004original[diabete2 == 2, diabtotr := 1]
HSE2004original[diabete2 == 1 | glyhbval > 6.5, diabtotr := 2]
HSE2004original[, `:=` (wt.int = wt.int / 10)]
HSE2004original[, `:=` (wt.blood = wt.int, wt.nurse = wt.int)]
#HSE7.srv.blood <- svydesign(id=~psu, strata=~cluster, weights=~wt.blood, nest=F, data=HSE2004original[wt.blood>0], check.strata = T)
#HSE7.srv.nurse <- svydesign(id=~psu, strata=~cluster, weights=~wt.nurse, nest=F, data=HSE2004original[wt.nurse>0], check.strata = T)
#HSE7.srv.int <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=HSE2004original[wt.int>0], check.strata = T)

HSE2004 =copy(HSE2004original[, list(wt.int, wt.nurse, wt.blood, psu, cluster, age, agegroup, sex, group, qimd, bmival, cholval1, omsysval, diabtotr, cigst1, startsmk, endsmoke, porftvg, frtpor, numsmok, smokyrs, cigdyal, adtot30, sodium,
                                     potass, creatin, hdlval1, bpmedd, diabete2)])
setnames(HSE2004, "adtot30","a30to06")
HSE2004[, `:=`(year=-7, wt.urine = wt.nurse)]
HSE2004[, psu := paste0(psu, "2004")]
HSE2004[, cluster := paste0(cluster, "2004")]

load(file="./Datasets/Health Survey for England/2003/hse03ai.RData")
HSE2003original <- setDT(clear.labels(HSE2003original))
#HSE2003original <- filter(HSE2003original, samptype==1) #samples types are same from
#the statistics point of view. Sampletype 2 has more nurse measurements 
setnames(HSE2003original, c("imd2004", "area", "int.wt", "blood.wt", "nurse.wt"), c("qimd", "psu", "wt.int", "wt.blood", "wt.nurse"))
HSE2003original[, `:=` (cholval1 = cholval1 + 0.1, hdlval1 = hdlval1 - 0.1)]
agegroup.fn(HSE2003original)
HSE2003original[diabete2 == 2, diabtotr := 1]
HSE2003original[diabete2 == 1 | glyhbval > 6.5, diabtotr := 2]
#HSE8.srv.blood <- svydesign(id=~psu, strata=~cluster, weights=~wt.blood, nest=F, data=HSE2003original[wt.blood>0], check.strata = T)
#HSE8.srv.nurse <- svydesign(id=~psu, strata=~cluster, weights=~wt.nurse, nest=F, data=HSE2003original[wt.nurse>0], check.strata = T)
#HSE8.srv.int <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=HSE2003original[wt.int>0], check.strata = T)

HSE2003 =copy(HSE2003original[, list(wt.int, wt.nurse, wt.blood, psu, cluster, age, agegroup, sex, group, qimd, bmival, cholval1, omsysval, diabtotr, cigst1, startsmk, endsmoke, porftvg, frtpor, numsmok, smokyrs, cigdyal, adtot30, sodium, 
                                     potass, creatin, hdlval1, bpmedd, diabete2)])
setnames(HSE2003, "adtot30", "a30to06")
HSE2003[, `:=`(year=-8, wt.urine = wt.nurse)]
HSE2003[, psu := paste0(psu, "2003")]
HSE2003[, cluster := paste0(cluster, "2003")]

load(file="./Datasets/Health Survey for England/2002/hse02ai.RData")
HSE2002original <- setDT(clear.labels(HSE2002original))
setnames(HSE2002original, c("nimd", "area", "sysval", "adtot30"), c("qimd", "psu", "omsysval", "a30to06"))
HSE2002original[, `:=`(cluster = 2002, wt.blood = tablewt, wt.nurse = tablewt, wt.int = tablewt, wt.urine = tablewt)]
agegroup.fn(HSE2002original)
#HSE9.srv <- svydesign(id=~psu, strata=~cluster, weights = ~wt.int, nest=F, data=HSE2002original)

HSE2002 = copy(HSE2002original[, list(wt.int, wt.nurse, wt.blood, wt.urine, psu, cluster, age, agegroup, sex, group, qimd, bmival, omsysval, cigst1, startsmk, endsmoke, porftvg, frtpor, numsmok, smokyrs, cigdyal, a30to06, bpmedd)])
HSE2002[, `:=`(year=-9, cholval1 =NA, diabtotr = NA, diabete2 = NA, sodium = NA, potass = NA, creatin = NA)]
HSE2002[, psu := paste0(psu, "2002")]
HSE2002[, cluster := paste0(cluster, "2002")]

load(file="./Datasets/Health Survey for England/2001/hse01ai.RData")
HSE2001original <- setDT(clear.labels(HSE2001original))
setnames(HSE2001original, c("nimd", "area", "sysval"), c("qimd", "psu", "omsysval"))
HSE2001original[, `:=`(cluster = 2001, wt.blood =1, wt.nurse = 1, wt.int = 1, wt.urine = 1)]
agegroup.fn(HSE2001original)
#HSE10.srv <- svydesign(id=~psu, strata =~cluster, weights = ~wt.int, nest=F, data=HSE2001original, check.strata = T)

HSE2001 =copy(HSE2001original[, list(wt.int, wt.nurse, wt.blood, wt.urine, psu, cluster, age, agegroup, sex, group, qimd, bmival, omsysval, cigst1, startsmk, endsmoke, porftvg, frtpor, numsmok, smokyrs, cigdyal, bpmedd)])
HSE2001[, `:=`(year=-10, cholval1 =NA, diabtotr = NA, a30to06 = NA, sodium = NA, potass = NA, creatin = NA)]
HSE2001[, psu := paste0(psu, "2001")]
HSE2001[, cluster := paste0(cluster, "2001")]

HSE.ts <- rbind(HSE2012, HSE2011, HSE2010, HSE2009, HSE2008, HSE2007, HSE2006, HSE2005, HSE2004, HSE2003, HSE2002, HSE2001, 
                fill = T)
HSE.ts[, sex := factor(sex)]
HSE.ts[, qimd := ordered(qimd)]
HSE.ts[porftvg == 0, porftvg := 1]
HSE.ts[, porftvg := as.integer(porftvg - 1)]
HSE.ts[, frtpor := as.integer(frtpor)]
HSE.ts[frtpor > 8, frtpor := 8]
HSE.ts[, a30to06m := as.integer((a30to06/4))]
HSE.ts[origin <5, origin := 1] # 1 = white
HSE.ts[origin == 9, origin := 2] # 2 = indian
HSE.ts[origin == 10, origin := 3] # 3 = pakistani
HSE.ts[origin == 11, origin := 4] # 4 = bangladeshi
HSE.ts[between(origin, 5, 8), origin := 18] # 18 = other
HSE.ts[origin == 13, origin := 5] # 5 = other asian
HSE.ts[origin == 15, origin := 6] # 6 = black caribbean
HSE.ts[origin == 14, origin := 7] # 7 = black african
HSE.ts[origin == 12, origin := 8] # 8 = chinese
HSE.ts[origin > 8, origin := 9] # 9 = other
rm(list=ls(pattern="HSE20"))


# Impute PA
HSE.ts[, id := 1:.N]

A = copy(HSE.ts[is.na(a30to06m) == T & age >15 & is.na(agegroup)==F & is.na(sex)==F & is.na(qimd)==F, .(id, agegroup, sex, qimd, year, wt.int, psu, cluster)]) # missing pa to be imputed

B = copy(HSE.ts[age >15 & is.na(a30to06m)==F & is.na(agegroup)==F & is.na(sex)==F & is.na(qimd)==F, .(id, a30to06m, agegroup, sex, year, qimd, wt.int, psu, cluster)]) # 

B[, a30to06m := ordered(a30to06m)]
A[, agegroup := ordered(agegroup)]
B[, agegroup := ordered(agegroup)]

A.srv <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=A, check.strata = T)
B.srv <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=B, check.strata = T)

out.hz <- harmonize.x(svy.A=A.srv,
                      svy.B=B.srv,
                      x.tot=NULL,
                      form.x=~(year + agegroup+sex+qimd)^2-1, cal.method="linear")
A.srv =copy(out.hz$cal.A)
B.srv =copy(out.hz$cal.B)

tt <- comb.samples(A.srv,
                   B.srv, NULL, 
                   "id", "a30to06m", 
                   form.x=~(year + agegroup+sex+qimd)^2-1,
                   micro = T)

ttt <- cbind(A, tt$Z.A)
setnames(ttt, paste0("a30to06m", 1:8), paste0(0:7))

ttt <- melt(ttt, id=c("id", "agegroup", "sex", "qimd", "year", "wt.int", "psu", "cluster"), variable.name = "a30to06m.imp")
setkey(ttt, id, value)
ttt[value < 0, value := 0 ]
ttt[value > 1, value := 1 ]
ttt<- ttt[, sample_n(.SD, 1, weight = value), by = id]
HSE.ts = copy(merge(HSE.ts, ttt[,.(id, a30to06m.imp)], by = "id", all.x = T))
HSE.ts[, a30to06m.imp := as.integer(as.character(a30to06m.imp))]
HSE.ts[is.na(a30to06m.imp), a30to06m.imp := a30to06m]

# Impute porftvg (only consider 2011 to impute in 2012)
A = copy(HSE.ts[is.na(porftvg)==T & age >15 & is.na(agegroup)==F & is.na(sex)==F & is.na(qimd)==F, .(id, agegroup, sex, qimd, wt.int, psu, cluster)]) # missing pa to be imputed

B = copy(HSE.ts[age >15 & is.na(porftvg)==F & is.na(agegroup)==F & is.na(sex)==F & is.na(qimd)==F & year == 0, .(id, porftvg, agegroup, sex, qimd, wt.int, psu, cluster)]) # 

B[, porftvg := ordered(porftvg)]
A[, agegroup := ordered(agegroup)]
B[, agegroup := ordered(agegroup)]

A.srv <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=A, check.strata = T)
B.srv <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=B, check.strata = T)

out.hz <- harmonize.x(svy.A=A.srv,
                      svy.B=B.srv,
                      x.tot=NULL,
                      form.x=~-1+(agegroup+sex+qimd)^2, cal.method="linear")
A.srv =copy(out.hz$cal.A)
B.srv =copy(out.hz$cal.B)

tt <- comb.samples(A.srv,
                   B.srv, NULL, 
                   "id", "porftvg", 
                   form.x=~-1+(agegroup+sex+qimd)^2,
                   micro = T)

ttt <- cbind(A, tt$Z.A)
setnames(ttt, paste0("porftvg", 1:9), paste0(0:8))

ttt <- melt(ttt, id=c("id", "agegroup", "sex", "qimd", "wt.int", "psu", "cluster"), variable.name = "porftvg.imp")
setkey(ttt, id, value)
ttt[value < 0, value := 0 ]
ttt[value > 1, value := 1 ]
ttt <- ttt[, sample_n(.SD, 1, weight = value), by = id]
HSE.ts = copy(merge(HSE.ts, ttt[,.(id, porftvg.imp)], by = "id", all.x = T))
HSE.ts[, porftvg.imp := as.integer(as.character(porftvg.imp))]
HSE.ts[is.na(porftvg.imp), porftvg.imp := porftvg]

#HSE.ts.srv.int <- svydesign(id=~psu, strata =~cluster, weights = ~wt.int, nest=F, data=HSE.ts[wt.int>0,], check.strata = T)
#HSE.ts.srv.nurse <- svydesign(id=~psu, strata =~cluster, weights = ~wt.nurse, nest=F, data=HSE.ts[wt.nurse>0,], check.strata = T)
#HSE.ts.srv.blood <- svydesign(id=~psu, strata =~cluster, weights = ~wt.blood, nest=F, data=HSE.ts[wt.blood>0,], check.strata = T)
#save(HSE.ts, file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
#load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")

# Build Models

# Salt prepare file  -------------------------------------------------------------
load(file="./Lagtimes/HSE.ts.RData")
#load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")

# Salt
loadcmp(file="./salt.Rc")
#source(file="./Models/IMPACTncd/salt.R")
HSE.ts[bmival<16 & age>19, bmival := 16]
HSE.ts[bmival>50 & age>19, bmival := 50]
HSE.ts[is.na(wt.urine), wt.urine := 0]
HSE.ts[age>85, age:= 85]
# HSE.ts[age>15 & sex== "1", Na24 := Na24.men(.N, sodium, creatin, potass, bmival, age)]
# HSE.ts[age>15 & sex== "2", Na24 := Na24.women(.N, sodium, creatin, potass, bmival, age)]
# HSE.ts[, salt := Na24 * 58.5/1000]
# HSE.ts[, summary(salt)]

# ignore survey effects and match with 24h known distributions
HSE = copy(HSE.ts[between(age, 19, 64) & !is.na(qimd) & !is.na(sodium) & !is.na(bmival), ])
HSE <- rbindlist(sample(list(HSE), 1000, T),idcol = T)
HSE[age>15 & sex== "1", Na24 := Na24.men(.N, sodium, creatin, potass, bmival, age)]
HSE[age>15 & sex== "2", Na24 := Na24.women(.N, sodium, creatin, potass, bmival, age)]
HSE[, salt := Na24 * 58.5/1000]
# year 2001. I will use HSE2003 sample but 24h distribution from 2001. Year will be -10 (and not -8) 
#HSE[year == -8, year := -10]
tmp1 = copy(HSE[year == -8,])

tmp1[between(age, 19, 24) & sex == "1", grp := 1]
tmp1[between(age, 19, 24) & sex == "2", grp := 2]
tmp1[between(age, 25, 34) & sex == "1", grp := 3]
tmp1[between(age, 25, 34) & sex == "2", grp := 4]
tmp1[between(age, 35, 49) & sex == "1", grp := 5]
tmp1[between(age, 35, 49) & sex == "2", grp := 6]
tmp1[between(age, 50, 64) & sex == "1", grp := 7]
tmp1[between(age, 50, 64) & sex == "2", grp := 8]

tmp1[grp > 0, salt24h.rank := (frank(salt, na.last = F, ties.method="random")-1)/(.N - 1), by = grp]
setkey(tmp1, grp, salt24h.rank)

# define distributions from 24h urine from 2001 (table 4.1, 4.2)
# Men 19-24 
p <- c(0,	0,	0.09,	0.33,	0.46,	0.6,	0.75,	0.96,	1,	0.02,	0.37,	0.81,	1,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 14.0, 15.8, 3, 6, 9, 15, 18, 10.6, 6, 16.6) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p == 1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=7, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.triang.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt1 <- get.triang.par(p, e, show.output=F, plot=F, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 

# Women 19-24 
p <- c(0.04,	0.17,	0.34,	0.65,	0.7,	0.84,	0.88,	0.9,	0.66,	0.84,	0.92,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 14.0, 15.8, 9, 12, 18, 7.6, 1.7, 23.2) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=25, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.lnorm.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt2 <- get.lnorm.par(p, e, show.output=F, plot=F, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 


# Men 25-34 
p <- c(0.06,	0.12,	0.27,	0.33,	0.48,	0.59,	0.7,	0.78,	0.05,	0.2,	0.34,	0.57,	0.73,	0.89,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 14.0, 15.8, 3, 6, 9, 12, 15, 18, 10.9, 2.2, 22.3) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=5, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.triang.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt3 <- get.triang.par(p, e, show.output=F, plot=F, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 

# Women 25-34 
p <- c(0.05,	0.25,	0.41,	0.57,	0.74,	0.85,	0.91,	0.95,	0.06,	0.29,	0.59,	0.81,	0.92,	0.97,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 14.0, 15.8, 3, 6, 9, 12, 15, 18, 8, 1.9, 22.2) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=7, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.chisq.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt4 <- get.chisq.par(p, e, show.output=F, plot=F, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 

# Men 35-49 
p <- c(0.02,	0.09,	0.19,	0.36,	0.51,	0.6,	0.74,	0.82,	0.02,	0.13,	0.39,	0.58,	0.8,	0.91,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 14.0, 15.8, 3, 6, 9, 12, 15, 18, 10.2, 2.4, 22.1) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=6, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.triang.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt5 <- get.triang.par(p, e, show.output=F, plot=F, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 

# Women 35-49 
p <- c(0.05,	0.2,	0.43,	0.67,	0.8,	0.87,	0.92,	0.97,	0.05,	0.31,	0.68,	0.85,	0.96,	1,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 14.0, 15.8, 3, 6, 9, 12, 15, 18, 7.6, 2.6, 16.2) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=6, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.gamma.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt6 <- get.gamma.par(p, e, show.output=F, plot=F, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 

# Men 50-64 (50 - 100) 
p <- c(0.08,	0.15,	0.27,	0.42,	0.54,	0.67,	0.77,	0.85,	0.05,	0.18,	0.42,	0.65,	0.83,	0.91,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 14.0, 15.8, 3, 6, 9, 12, 15, 18, 10.1, 2.1, 21.2) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=5, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.triang.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt7 <- get.triang.par(p, e, show.output=F, plot=F, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 

# Women 50-64 (50 - 100) 
p <- c(0.12,	0.28,	0.5,	0.68,	0.84,	0.92,	0.94,	0.98,	0.07,	0.38,	0.69,	0.91,	0.96,	0.99,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 14.0, 15.8, 3, 6, 9, 12, 15, 18, 7, 2.3, 15.7) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=8, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.gamma.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt8 <- get.gamma.par(p, e, show.output=F, plot=F, tol=0.001, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 


tmp <- data.table(
  salt24h = c( rtriang(tmp1[grp == 1, .N], tt1[1], tt1[2], tt1[3]),
               rlnorm (tmp1[grp == 2, .N], tt2[1], tt2[2]),
               rtriang(tmp1[grp == 3, .N], tt3[1], tt3[2], tt3[3]),
               rchisq (tmp1[grp == 4, .N], tt4[1]),
               rtriang(tmp1[grp == 5, .N], tt5[1], tt5[2], tt5[3]),
               rgamma (tmp1[grp == 6, .N], tt6[1]  , tt6[2]),
               rtriang(tmp1[grp == 7, .N], tt7[1], tt7[2], tt7[3]),
               rgamma (tmp1[grp == 8, .N], tt8[1]  , tt8[2])
  ),
  grp = c(rep(1, tmp1[grp == 1, .N]),
          rep(2, tmp1[grp == 2, .N]),
          rep(3, tmp1[grp == 3, .N]),
          rep(4, tmp1[grp == 4, .N]),
          rep(5, tmp1[grp == 5, .N]),
          rep(6, tmp1[grp == 6, .N]),
          rep(7, tmp1[grp == 7, .N]),
          rep(8, tmp1[grp == 8, .N])
  )
) 

tmp[, salt24h.rank := (frank(salt24h, na.last = F, ties.method="random")-1)/(.N - 1), by = grp]
setkey(tmp, grp, salt24h.rank)
tmp1 <-   tmp[tmp1, roll = "nearest"]
tmp1[, `:=` (grp = NULL, salt24h.rank = NULL)]

# year 2006. for 24h urine participants were selected from HSE2005
tmp2 = copy(HSE[year == -5,])
tmp2[, year := -5]

tmp2[between(age, 19, 24) & sex == "1", grp := 1]
tmp2[between(age, 19, 24) & sex == "2", grp := 2]
tmp2[between(age, 25, 34) & sex == "1", grp := 3]
tmp2[between(age, 25, 34) & sex == "2", grp := 4]
tmp2[between(age, 35, 49) & sex == "1", grp := 5]
tmp2[between(age, 35, 49) & sex == "2", grp := 6]
tmp2[between(age, 50, 64) & sex == "1", grp := 7]
tmp2[between(age, 50, 64) & sex == "2", grp := 8]

tmp2[grp > 0, salt24h.rank := (frank(salt, na.last = F, ties.method="random")-1)/(.N - 1), by = grp]
setkey(tmp2, grp, salt24h.rank)

# define distributions from 24h urine from 2006
# Men 19-24 
p <- c(0,	0,	0,	0.35,	0.5, 0.66,	0.94,	0,	0,	0.31,	0.59,	0.94,	0.5,	0.025)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 15.8, 3, 6, 9, 15, 18, 10.4, 7.5) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=10, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.triang.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt1 <- get.triang.par(p, e, show.output=F, plot=F, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 

# Women 19-24 
p <- c(0,	0.15,	0.62,	1,	1,	1,	1,	0,	0.15,	1,	1,	1,	0.5,	0.025)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 15.8, 3, 6, 9, 15, 18, 6.6, 4.6) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=.95, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.norm.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt2 <- get.norm.par(p, e, show.output=F, plot=F, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 


# Men 25-34 
p <- c(0,	0,	0.24,	0.44,	0.63,	0.73,	0.95,	0,	0.15,	0.49,	0.73,	0.95,	0.95,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 15.8, 3, 6, 9, 12, 15, 18, 9.8, 5.2, 23.6) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=11, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.lnorm.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt3 <- get.lnorm.par(p, e, show.output=F, plot=F, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 

# Women 25-34 
p <- c(0.02,	0.1,	0.31,	0.51,	0.77,	0.86,	0.98,	0.02,	0.16,	0.56,	0.84,	0.98,	1,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 15.8, 3, 6, 9, 12, 15, 18, 8.7, 2.5, 16.8) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=6, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.gamma.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt4 <- get.gamma.par(p, e, show.output=F, plot=F, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 

# Men 35-49 
p <- c(0,	0.04,	0.28,	0.47,	0.61,	0.68,	0.94,	0,	0.12,	0.47,	0.66,	0.92,	0.97,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 15.8, 3, 6, 9, 12, 15, 18, 10.2, 2.4, 22.1) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=13, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.gamma.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt5 <- get.gamma.par(p, e, show.output=F, plot=F, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 

# Women 35-49 
p <- c(0.02,	0.18,	0.41,	0.66,	0.82,	0.95,	0.98,	0.01,	0.28,	0.69,	0.95,	0.98,	1,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 15.8, 3, 6, 9, 12, 15, 18, 7.6, 2.6, 16.2) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=6.25, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.gamma.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt6 <- get.gamma.par(p, e, show.output=F, plot=F, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 

# Men 50-64 (50 - 100) 
p <- c(0.02,	0.07,	0.21,	0.39,	0.59,	0.72,	0.92,	0.02,	0.11,	0.41,	0.71,	0.89,	0.96,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 15.8, 3, 6, 9, 12, 15, 18, 9.7, 2.7, 20.8) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=7.3, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.gamma.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt7 <- get.gamma.par(p, e, show.output=F, plot=F, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 

# Women 50-64 (50 - 100) 
p <- c(0.1,	0.27,	0.59,	0.78,	0.91,	0.96,	0.99,	0.06,	0.43,	0.82,	0.95,	0.99,	0.1,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 15.8, 3, 6, 9, 12, 15, 18, 6.4, 2, 14.4) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=11, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.gamma.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt8 <- get.gamma.par(p, e, show.output=F, plot=F, tol=0.001, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 


tmp <- data.table(
  salt24h = c( rtriang(tmp2[grp == 1, .N], tt1[1], tt1[2], tt1[3]),
               rnorm  (tmp2[grp == 2, .N], tt2[1], tt2[2]),
               rlnorm (tmp2[grp == 3, .N], tt3[1], tt3[2]),
               rgamma (tmp2[grp == 4, .N], tt4[1], tt4[2]),
               rgamma (tmp2[grp == 5, .N], tt5[1], tt5[2]),
               rgamma (tmp2[grp == 6, .N], tt6[1], tt6[2]),
               rgamma (tmp2[grp == 7, .N], tt7[1], tt7[2]),
               rgamma (tmp2[grp == 8, .N], tt8[1], tt8[2])
  ),
  grp = c(rep(1, tmp2[grp == 1, .N]),
          rep(2, tmp2[grp == 2, .N]),
          rep(3, tmp2[grp == 3, .N]),
          rep(4, tmp2[grp == 4, .N]),
          rep(5, tmp2[grp == 5, .N]),
          rep(6, tmp2[grp == 6, .N]),
          rep(7, tmp2[grp == 7, .N]),
          rep(8, tmp2[grp == 8, .N])
  )
) 

tmp[, salt24h.rank := (frank(salt24h, na.last = F, ties.method="random")-1)/(.N - 1), by = grp]
setkey(tmp, grp, salt24h.rank)
tmp2 <-   tmp[tmp2, roll = "nearest"]
tmp2[, `:=` (grp = NULL, salt24h.rank = NULL)]

# Year 2008. HSE 2007 will be used. Original 24h sample from UK (not only England)
tmp3 = copy(HSE[year == -2, ])
tmp3[, year := -3]

tmp3[between(age, 19, 24) & sex == "1", grp := 1]
tmp3[between(age, 19, 24) & sex == "2", grp := 2]
tmp3[between(age, 25, 34) & sex == "1", grp := 3]
tmp3[between(age, 25, 34) & sex == "2", grp := 4]
tmp3[between(age, 35, 49) & sex == "1", grp := 5]
tmp3[between(age, 35, 49) & sex == "2", grp := 6]
tmp3[between(age, 50, 64) & sex == "1", grp := 7]
tmp3[between(age, 50, 64) & sex == "2", grp := 8]

tmp3[grp > 0, salt24h.rank := (frank(salt, na.last = F, ties.method="random")-1)/(.N - 1), by = grp]
setkey(tmp3, grp, salt24h.rank)

# define distributions from 24h urine from 2006
# Men 19-24 
p <- c(0,	0.14,	0.14,	0.25,	0.34,	0.57,	1,	0,	0.14,	0.25,	0.46,	1,	1,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 15.8, 3, 6, 9, 12, 15, 18, 12.1, 3.7, 14.4) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=8, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.triang.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt1 <- get.triang.par(p, e, show.output=F, plot=F, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 

# Women 19-24 
p <- c(0.07,	0.07,	0.39,	0.49,	0.49,	0.72,	0.91,	0,	0.21,	0.49,	0.72,	0.91,	0.91,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 15.8, 3, 6, 9, 12, 15, 18, 11.3, 3.3, 18) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=15, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.triang.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt2 <- get.triang.par(p, e, show.output=F, plot=F, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 


# Men 25-34 
p <- c(0.07,	0.07,	0.19,	0.37,	0.6,	0.72,	0.97,	0.05,	0.13,	0.43,	0.7,	0.87,	0.97,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 15.8, 3, 6, 9, 12, 15, 18, 9.1, 2.5, 22.1) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=13.1, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.gamma.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt3 <- get.gamma.par(p, e, show.output=F, plot=F, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 

# Women 25-34 
p <- c(0.01,	0.19,	0.49,	0.68,	0.79,	0.83,	0.98,	0,	0.28,	0.68,	0.82,	0.98,	0.99,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 15.8, 3, 6, 9, 12, 15, 18, 7.1, 3.9, 14.8) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=9.5, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.triang.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt4 <- get.triang.par(p, e, show.output=F, plot=F, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 

# Men 35-49 
p <- c(0.01,	0.1,	0.28,	0.43,	0.65,	0.78,	0.96,	0.01,	0.19,	0.47,	0.76,	0.95,	0.97,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 15.8, 3, 6, 9, 12, 15, 18, 9.4, 4, 19.1) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=7, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.gamma.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt5 <- get.gamma.par(p, e, show.output=F, plot=F, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 

# Women 35-49 
p <- c(0.04,	0.24,	0.52,	0.72,	0.86,	0.92,	0.99,	0.02,	0.38,	0.76,	0.92,	0.99,	0.99,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 15.8, 3, 6, 9, 12, 15, 18, 7, 3, 14.3) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=6.25, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.gamma.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt6 <- get.gamma.par(p, e, show.output=F, plot=F, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 

# Men 50-64 (50 - 100) 
p <- c(0.04,	0.12,	0.26,	0.46,	0.64,	0.8,	0.95,	0.02,	0.2,	0.48,	0.78,	0.93,	0.99,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 15.8, 3, 6, 9, 12, 15, 18, 9.5, 3.2, 16.6) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=5, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.triang.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt7 <- get.triang.par(p, e, show.output=F, plot=F, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 

# Women 50-64 (50 - 100) 
p <- c(0.1,	0.31,	0.55,	0.78,	0.88,	0.95,	0.99,	0.06,	0.39,	0.8,	0.95,	0.98,	1,	0.5,	0.025,	0.975)  # Known percentiles probabilities
e <- c(3.5, 5.3, 7.0, 8.8, 10.5, 12.3, 15.8, 3, 6, 9, 12, 15, 18, 6.7, 2.3, 13.9) # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=3.8, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.gamma.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt8 <- get.gamma.par(p, e, show.output=F, plot=F, tol=0.001, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 


tmp <- data.table(
  salt24h = c( rtriang(tmp3[grp == 1, .N], tt1[1], tt1[2], tt1[3]),
               rtriang(tmp3[grp == 2, .N], tt2[1], tt2[2], tt2[3]),
               rgamma (tmp3[grp == 3, .N], tt3[1], tt3[2]),
               rtriang(tmp3[grp == 4, .N], tt4[1], tt4[2], tt4[3]),
               rgamma (tmp3[grp == 5, .N], tt5[1], tt5[2]),
               rgamma (tmp3[grp == 6, .N], tt6[1], tt6[2]),
               rtriang(tmp3[grp == 7, .N], tt7[1], tt7[2], tt7[3]),
               rgamma (tmp3[grp == 8, .N], tt8[1], tt8[2])
  ),
  grp = c(rep(1, tmp3[grp == 1, .N]),
          rep(2, tmp3[grp == 2, .N]),
          rep(3, tmp3[grp == 3, .N]),
          rep(4, tmp3[grp == 4, .N]),
          rep(5, tmp3[grp == 5, .N]),
          rep(6, tmp3[grp == 6, .N]),
          rep(7, tmp3[grp == 7, .N]),
          rep(8, tmp3[grp == 8, .N])
  )
) 

tmp[, salt24h.rank := (frank(salt24h, na.last = F, ties.method="random")-1)/(.N - 1), by = grp]
setkey(tmp, grp, salt24h.rank)
tmp3 <- tmp[tmp3, roll = "nearest"]
tmp3[, `:=` (grp = NULL, salt24h.rank = NULL)]

# Year 2011. HSE 2010 & 12 wll be used. Note 10 has no sec gradiaent. an exception
tmp4 = copy(HSE[year == 1, ])
tmp4[, year := 0]

tmp4[between(age, 19, 34) & sex == "1", grp := 1]
tmp4[between(age, 19, 34) & sex == "2", grp := 2]
tmp4[between(age, 35, 49) & sex == "1", grp := 3]
tmp4[between(age, 35, 49) & sex == "2", grp := 4]
tmp4[between(age, 50, 64) & sex == "1", grp := 5]
tmp4[between(age, 50, 64) & sex == "2", grp := 6]

tmp4[grp > 0, salt24h.rank := (frank(salt, na.last = F, ties.method="random")-1)/(.N - 1), by = grp]
setkey(tmp4, grp, salt24h.rank)

# Men 19-34 (16-34)
p <- c(0.025, 0.15, 0.3, 0.44, 0.5, 0.61, 0.76, 0.95, 0.975, 0, 0.21,  0.72,  0.98)  # Known percentiles probabilities
e <- c(4.2  , 5.3 , 7  , 8.8 , 9.3, 10.5, 12.3, 15.8, 17.3 , 3, 6   ,  12  ,   18)  # salt from Sodium Survey England 2011 tables 9 and 10
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=4, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.triang.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt1 <- get.triang.par(p, e, show.output=F, plot=F, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 


# Women 19-34 (16-34)
p <- c(0.025, 0.12, 0.23, 0.5, 0.52, 0.77, 0.91, 0.94, 0.975, 0.05, 0.32, 0.81,  0.97, 1)  
e <- c(2.8  , 3.5 , 5.3 , 7  , 7   , 8.8 , 10.5, 12.3, 15.2 , 3   , 6   , 9   ,  15  , 18)
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=7.8, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.lnorm.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt2 <- get.lnorm.par(p, e, show.output=F, plot=F, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 

# Men 35-49
p <- c(0.025, 0.06, 0.26, 0.39, 0.5, 0.6 , 0.77, 0.94, 0.975, 0.01, 0.11, 0.42, 0.89, 0.96) 
e <- c(4.3  , 5.3 , 7   , 8.8 , 9.7, 10.5, 12.3, 15.8, 18.8 , 3   , 6   , 9   , 15  , 18) 
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=5, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.gamma.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt3 <- get.gamma.par(p, e, show.output=F, plot=F, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 


# Women 35-49
p <- c(0.025, 0.3, 0.5, 0.66, 0.78, 0.9 , 0.975, 0.98, 1, 0.02, 0.48, 0.80, 0.94,  1)
e <- c(3.4  , 5.3, 6.1, 7   , 8.8 , 10.5, 12.1 , 12.3, 15.8, 3   , 6   , 9   , 12  , 18)
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=6, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.triang.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt4 <- get.triang.par(p, e, show.output=F, plot=F, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 


# Men 50-64 (50-80)
p <- c(0.025, 0.21, 0.38, 0.5, 0.62, 0.81, 0.9 , 0.97, 0.975, 0, 0.28, 0.64, 0.88, 0.96)
e <- c(3.1  , 5.3 , 7   , 7.8, 8.8 , 10.5, 12.3, 15.8, 18   , 3   , 6   , 9   , 12  , 15)
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=4, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.lnorm.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt5 <- get.lnorm.par(p, e, show.output=F, plot=F, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 

# Women 50-64 (50-80)
p <- c(0.025, 0.09, 0.33, 0.5, 0.6, 0.82, 0.95, 0.96, 0.975, 0.05, 0.46, 0.85, 1   , 1)  
e <- c(2.6  , 3.5 , 5.3 , 6.3, 7  , 8.8 , 10.5, 12.3, 12.7 , 3   , 6   , 9   , 15  , 18)
xx <- which(p == 0 | p ==1) # index for 0, 1
p <- sort(p[p != 0 & p != 1]) # remove 0, 1
ifelse (length(xx)==0, e <- sort(e), e <- sort(e[-xx])) # remove relevant values from e
#fit.perc(p, e, show.output=F, tolPlot=3, tolConv=0.01, fit.weights=1/(abs(0.5-p)+1))
#get.gamma.par(p, e, show.output=T, plot=T, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975))
tt6 <- get.gamma.par(p, e, show.output=F, plot=F, tol=0.01, fit.weights=1/(abs(0.5-p)+1), scaleX=c(0.025,0.975)) 


tmp <- data.table(
  salt24h = c( rtriang(tmp4[grp == 1, .N], tt1[1], tt1[2], tt1[3]),
               rlnorm (tmp4[grp == 2, .N], tt2[1], tt2[2]),
               rgamma (tmp4[grp == 3, .N], tt3[1], tt3[2]),
               rtriang(tmp4[grp == 4, .N], tt4[1], tt4[2], tt4[3]),
               rlnorm (tmp4[grp == 5, .N], tt5[1], tt5[2]),
               rgamma (tmp4[grp == 6, .N], tt6[1], tt6[2]) 
  ),
  grp = c(rep(1, tmp4[grp == 1, .N]),
          rep(2, tmp4[grp == 2, .N]),
          rep(3, tmp4[grp == 3, .N]),
          rep(4, tmp4[grp == 4, .N]),
          rep(5, tmp4[grp == 5, .N]),
          rep(6, tmp4[grp == 6, .N])
  )
) # parameters from fit distr to 24h.R

tmp[, salt24h.rank := (frank(salt24h, na.last = F, ties.method="random")-1)/(.N - 1), by = grp]
setkey(tmp, grp, salt24h.rank)
tmp4 <-   tmp[tmp4, roll = "nearest"]
tmp4[, `:=` (grp = NULL, salt24h.rank = NULL)]

# combine all
HSE.salt <- setkey(rbind(tmp1, tmp2, tmp3, tmp4, use.names = T), age, sex, qimd, year)
rm(tmp1, tmp2, tmp3, tmp4, tmp, HSE, HSE.ts)
HSE.salt[salt24h <1, salt24h := 1] 
HSE.salt[salt24h > 30, salt24h := 30] 
gc()

# explore salt correlations
tt <- HSE.salt[, mean(salt24h, na.rm=T), by = year]
#tt[, V2 := (V1-6)/(max(V1)-6)] # scale from 0 t0 1 with threshold 6
#plot((V2*tt[, max(V1)-6]+6)~year, tt, xlim=c(-10, 60), ylim=c(0, 10))
plot(tt, xlim=c(-10, 25), ylim=c(0, 12))
abline(h = 6)
lines(y=(predict(glm(I(V1^(1/3))~I(log(year+14)), family=gaussian(link="identity"), data=tt), data.frame(year=-10:60), type="response"))^(3), x=-10:60, col="red")

lines(y=predict(glm(V1~I(log(year+14)^-2)+I((year+14)^-2), family=gaussian(link="identity"), data=tt), data.frame(year=-10:60), type="response"), x=-10:60, col="green")
lines(y=6 + tt[, max(V1)-6]*(predict(glm(V2~year, family=binomial(logit), data=tt), data.frame(year=-10:600), type="response")), x=-10:600, col="purple")

lines(y=predict(glm(V1~I(log(year^(1/year))), family=gaussian, data=tt), data.frame(year=-10:60), type="response"), x=-10:60, col="green")
lines(y=predict(glm(V1~poly(1/(year+30), 2), family=gaussian(), data=tt), data.frame(year=-10:60), type="response"), x=-10:60, col="black")
lines(y=predict(glm(V1~poly(year, 2), family=gaussian(), data=tt), data.frame(year=-10:60), type="response"), x=-10:60, col="red")

lines(y=-6 + predict(glm(I((V1+6)/20)~year, family=binomial(logit), data=tt), data.frame(year=-10:60), type="response")*20, x=-10:60, col="blue")

tt <- HSE.salt[, mean(salt24h, na.rm=T), by = age]
scatter.smooth(tt, xlim=c(19, 100), ylim=c(6, 10))
lines(y=predict(glm(V1~age, family=gaussian(link="identity"), data=tt), data.frame(age=19:100), type="response"), x=19:100, col="green")
lines(y=predict(glm(V1~poly(log(age), 3), family=gaussian(link="identity"), data=tt), data.frame(age=19:100), type="response"), x=19:100, col="red")

tt <- HSE.salt[, mean(salt24h, na.rm=T), by = qimd]
tt[, qimd := as.numeric(as.character(qimd))]
plot(tt, xlim=c(0, 6), ylim=c(6, 10))
lines(y=predict(glm(V1~poly(qimd, 2), family=gaussian(link="identity"), data=tt), data.frame(qimd=1:5), type="response"), x=1:5, col="green")

tt <- HSE.salt[, mean(salt24h, na.rm=T), by = round(bmival)]
tt[, round := as.integer(as.character(round))]
plot(tt, xlim=c(20, 50), ylim=c(0, 12))
lines(y=predict(glm(V1~poly(round, 2), family=gaussian(link="identity"), data=tt), data.frame(round=20:50), type="response"), x=20:50, col="green")
gc()

# Salt fitting ------------------------------------------------------------
HSE.salt <- HSE.salt[, .(age, sex, qimd, year, .id, id, salt24h, wt.urine)]
HSE.salt[, hist(salt24h^(1/3))]
q1 <- glm(I(salt24h^(1/3))~ (I(log(year+9)) + poly(log(age), 3) + sex + qimd)^3, weights = wt.urine, family = gaussian(), data = HSE.salt[.id < 20])
summary(q1)
q11 <- stepAIC(q1) #AIC: 1,634,000
summary(q11)  
rm(q1, q11)


q2 <- glm(I(salt24h^(-1/3))~ I(log(year+14)^-2)+I((year+14)^-2) + log(age) + sex + qimd + qimd:I(log(year+14)^-2), weights = wt.urine, family = gaussian(), data = HSE.salt)
AIC(q2)
rm(q2)

q3 <- glm(I(salt24h^(1/3))~ poly(year, 2) + log(age) + sex + qimd + qimd:poly(year, 2), weights = wt.urine, family = gaussian(), data = HSE.salt)
AIC(q3)
rm(q3)

gc()
summary(q12)
View(data.frame(list(a=coef(q11), b=coef(q12))))

q2 <- rq(I(salt24h^(1/3))~ log(year+14), 
         tau = .5,
         data = HSE.salt,
         weights = wt.urine,
         method = "fn")
pr <- (predict(q2, data.frame(year= -10:20)))^3
tt <- HSE.salt[, quantileWt(salt24h, wt.urine,  0.5, na.rm=T), by = year]
plot(tt, xlim=c(-10, 20), ylim=c(0, 20))
abline(v=10, h=6)
lines(y=pr, x = -10:20)

salt.rq <- rq(I(salt24h^(1/3)) ~ (I(log(year + 14)) + poly(log(age), 3) + sex + qimd)^2, 
              tau = c(0.01, 1:19/20, 0.99),
              data = HSE.salt[.id == 1, ], # Then coeff to be injected from all versions
              weights = wt.urine,
              method = "br")

# produce a model for each version of population. NOTE year+16 used. steeper decline than +14 
# but works for init.year == 2006 to avoid log(0) (-5 -10(cancer.lag))
if (Sys.info()[1] == "Linux") {
  registerDoParallel(30) # used for forking. only linux
}
if (Sys.info()[1] == "Windows") {
  cl <- makeCluster(4) # used for clustering. win compatible
  registerDoParallel(cl) 
}

salt.rq.coef <- vector("list", 1000)
salt.rq.coef <- 
  foreach(jj = 1 : 1000,
          .inorder = F,
          .verbose = T,
          .packages = c("data.table",
                        "dplyr",
                        "quantreg")
  ) %dopar% {
    md1 <- rq(I(salt24h^(1/3)) ~ (I(log(year + 14)) + poly(log(age), 3) + sex + qimd)^2, 
              tau = c(0.01, 1:19/20, 0.99),
              data = HSE.salt[.id == jj, ],
              weights = wt.urine,
              method = "br")
    coef(md1)
  }
if (exists("cl")) stopCluster(cl)

save(salt.rq, file="./Lagtimes/salt.rq.rda")
save(salt.rq.coef, file="./Lagtimes/salt.rq.coef.rda")

#save(salt.rq.coef, file="./Lagtimes/salt.rq.rda")
#save(salt.rq.coef, file="./Models/IMPACTncd/Lagtimes/salt.rq.coef.rda")
apply(simplify2array(salt.rq.coef), 1:2, mean) # calculate mean per element
apply(simplify2array(salt.rq.coef), 1:2, quantile, c(0.025, 0.5, 0.975)) # 95%CI
View(apply(simplify2array(salt.rq.coef), 1:2, quantile, c(0.025, 0.5, 0.975)))
#salt.rq$coefficients <- sample(salt.rq.coef,1)[[1]] for the MC simulation
#salt.rq$coefficients <- apply(simplify2array(salt.rq.coef), 1:2, mean) 
q2 <- salt.rq
# When I use 100 percentiles, give some wrong values. eg pct .4 gives salt 5 and 
# pct .41 gives salt 4.9 (should be above 5). The -tiles I used seems the best option
# salt.rq.old <- rq(salt24h ~ log(year+14) + log(age) + sex + qimd + log(age):sex, 
#                   tau = c(0.01, 1:19/20, 0.99),
#                   data = HSE.salt,
#                   weights = wt.urine,
#                   method = "fn")
# log(year + 14). 14 was selected so median salt for an 85 yo woman of qimd=1 is
# above 3gr in 2111. See below. Other wise linear decrease is too steep and produces 
# unrealistic results. 
# 
# Also the predictors selected seem stable for each iteration of HSE.salt. 
# interaction terms were very unstable 

# needed for plots 
q2$coefficients <- apply(simplify2array(salt.rq.coef), 1:2, mean)[, 11]
pred.s <- cmpfun(function(year, age, sex, qimd) {
  if (is.factor(sex)==F) {
    sex <-  factor(sex, 
                   levels = c(1,2), 
                   ordered = F)
  }
  if (is.ordered(qimd)==F) {
    qimd <- factor(qimd, 
                   levels = c(1,2,3,4,5), 
                   ordered = T)
  }
  cc <- predict(q2, 
                data.frame(year= year,
                           age = age, 
                           sex = sex, 
                           qimd = qimd
                )
  )
  
  return(cc^3)  
}
)
q2$coefficients <- sample(salt.rq.coef, 1)[[1]][,11]
plot (pred.s(-10:30, 30:70, 1, 1), ylim=c(0,12))
abline(h=6)
lines(pred.s(-10:30, 30:70, 1, 3), col = "blue")
lines(pred.s(-10:30, 30:70, 1, 5), col = "red")
lines(pred.s(-10:30, 30:70, 1, 1), col = "green")

salt.rq$y <- NULL
salt.rq$x <- NULL
salt.rq$residuals <- NULL
salt.rq$fitted.values <- NULL
#save(salt.rq, file="./Lagtimes/salt.rq.rda")
#save(salt.rq.coef, file="./Lagtimes/salt.rq.coef.rda")

#save(salt.rq, file="./Models/IMPACTncd/Lagtimes/salt.rq.rda")
#save(salt.rq.coef, file="./Models/IMPACTncd/Lagtimes/salt.rq.coef.rda")

# PA ----------------------------------------------------------------------
# PA (imputation is ignored, as it should for dependent variable)
# load(file="./Lagtimes/HSE.ts.RData")
# load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[bmival<16 & age>19, bmival := 16]
HSE.ts[bmival>50 & age>19, bmival := 50]
HSE.ts[age>85, age := 85]
HSE.ts[, a30to06m := ordered(a30to06m)]
HSE.ts.srv.int <- svydesign(id=~psu, strata =~cluster, weights = ~wt.int, nest=F,
                            data=HSE.ts, check.strata = T)
HSE.ts.srv.int <- subset(HSE.ts.srv.int, age>19 &  wt.int>0 & !is.na(a30to06m) &
                           !is.na(qimd))   
# repwtdesign <- as.svrepdesign(HSE.ts.srv.int)
# ologit <- 
#   withReplicates(
#     repwtdesign,
#     quote(
#       coef(
#         polr(
#           a30to06m~year + age + I(age^2) + I(age^3) + sex + qimd,
#           weights = .weights)
#       )
#     )
#   ) 
# 
# # This will not give you standard errors for the
# # intercept terms - if you want them you can do
# ologit.int <- 
#   withReplicates(
#     repwtdesign,
#     quote(
#       polr(
#         a30to06m~year + age + I(age^2) + I(age^3) + sex + qimd,
#         weights=.weights)$zeta
#     )
#   )


tt = copy(HSE.ts[age > 19 & 
                   #year > -7 &
                   wt.int > 0 & 
                   is.na(a30to06m)== F & 
                   is.na(qimd) == F, ])
tt[, a30to06m := ordered(a30to06m)]

pp <- svyby(~a30to06m, by=~age, design=HSE.ts.srv.int, svymean)
scatter.smooth(pp, family = "gaussian", ylim=c(0,1))
lines(y=predict(svyglm(as.integer(a30to06m)~age + I(age^3)+ I(age^2), family=quasipoisson(), design=HSE.ts.srv.int), data.frame(age=20:100), type="response"), x=20:100, col="red")

pp <- svyby(~a30to06m, by=~year, design=HSE.ts.srv.int, svymean)
scatter.smooth(pp, family = "gaussian", ylim=c(0,1), xlim=c(-10,1))
lines(y=predict(svyglm(a30to06m~year, family=quasipoisson(), design=HSE.ts.srv.int), data.frame(year=-10:50), type="response"), x=-10:50, col="red")
scatter.smooth(pp, family = "gaussian", ylim=c(0,6), xlim=c(-10,50))
lines(y=predict(svyglm(a30to06m~year, family=quasipoisson(), design=HSE.ts.srv.int), data.frame(year=-10:50), type="response"), x=-10:50, col="red")

# fit ordinal multinomial regression model and ignore sampling design
# I need to scale count vars for stepaic to work
# also tried with rcs and ns and got worse predictions
ttt <-data.frame(a30to06m=tt[,a30to06m],scale(tt[,.(year,age)]), 
                 tt[,.(sex,qimd,wt.int)]) 
mod1 <- polr(a30to06m~log(year + 25) * age * sex * qimd + I(age^2) + I(age^3),
             data = ttt, 
             weights = wt.int,
             method = "logistic",
             Hess = T)
summary(mod1)
mod2 <- stepAIC(mod1)

mod3 <- # best model based on aic
  polr(
    a30to06m ~ log(year + 25) + age + sex + qimd + 
      I(age^2) + I(age^3) + age:sex + log(year + 25):qimd + age:qimd + 
      sex:qimd + age:sex:qimd,
    data = tt, 
    weights = wt.int,
    method = "logistic",
    Hess = T)

pa.svylr <- #apply formula of mod2 to svy 
  svyolr(
    a30to06m ~ log(year + 25) + age + sex + qimd + 
      I(age^2) + I(age^3) + age:sex + log(year + 25):qimd + age:qimd + 
      sex:qimd + age:sex:qimd, 
    design = HSE.ts.srv.int, 
    method = "logistic")

# copy parameters of svy model to polr model so predict can work
for (k in intersect(names(mod3),  names(pa.svylr))) mod3[k] <- pa.svylr[k]

pa.svylr <- mod3

pa.svylr$data <- NULL
pa.svylr$lp <- NULL
pa.svylr$fitted.values <- NULL
#save(pa.svylr, file="./Models/IMPACTncd/Lagtimes/pa.svylr.rda")
#save(pa.svylr, file="./Lagtimes/pa.svylr.rda")

# bmi model  --------------------------------------------------------------
#load(file="./Lagtimes/HSE.ts.RData")
#load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[bmival<16 & age>19, bmival := 16]
HSE.ts[bmival>50 & age>19, bmival := 50]
HSE.ts[age>85, age:= 85]
HSE.ts.srv.nurse <- svydesign(id = ~psu, strata = ~cluster, 
                              weights = ~wt.nurse, nest = F,
                              data=HSE.ts, check.strata = T)
HSE.ts.srv.nurse <- subset(HSE.ts.srv.nurse, age > 19 & wt.nurse > 0 &
                             !is.na(qimd) & !is.na(bmival) &
                             !is.na(a30to06m.imp))
dt <- subset(HSE.ts, age > 19 & wt.nurse > 0 &
               !is.na(qimd) & !is.na(bmival) &
               !is.na(a30to06m.imp))
dt[between(age, 20, 84), hist(bmival)]
dt[between(age, 20, 84), plot(density(log(bmival), na.rm = T))]
dt[between(age, 20, 84), plot(sort(log(bmival)), pch = ".")]
dt[between(age, 20, 84), boxplot(log(bmival) ~ age)]
dt[between(age, 20, 84), boxplot(log(bmival) ~ year)]
dt[between(age, 20, 84), boxplot(log(bmival) ~ a30to06m.imp)]
dt[between(age, 20, 84), boxplot(log(bmival) ~ qimd)]

pp <- svyby(~bmival, by=~age, design=HSE.ts.srv.nurse, svymean)
scatter.smooth(pp, ylim=c(0,30), family="gaussian", span = 1/3)
lines(y=predict(svyglm(bmival~age + I(age^2), family=gaussian(link="inverse"), design=HSE.ts.srv.nurse), data.frame(age=20:100), type="response"), x=20:100, col="green")
lines(y=predict(svyglm(bmival~age + I(age^2), family=gaussian(link="identity"), design=HSE.ts.srv.nurse), data.frame(age=20:100), type="response"), x=20:100, col="red")
lines(y=predict(svyglm(bmival~age + I(age^2), family=inverse.gaussian, design=subset(HSE.ts.srv.nurse, sex == 1)), data.frame(age=20:100), type="response"), x=20:100, col="blue")

scatter.smooth(svyby(~bmival, by=~year, design=HSE.ts.srv.nurse, svymean), ylim=c(26.8,28), xlim=c(-10, 1))
lines(y=predict(svyglm(bmival~year, design=HSE.ts.srv.nurse), data.frame(year=-10:50), type="response"), x=-10:50, col="red")
lines(y=predict(svyglm(bmival~log(year+25), family=gaussian(link="identity"), design=HSE.ts.srv.nurse), data.frame(year=-10:50), type="response"), x=-10:50, col="green")
lines(y=predict(svyglm(bmival~year, family=inverse.gaussian, design=HSE.ts.srv.nurse), data.frame(year=-10:50), type="response"), x=-10:50, col="blue")
lines(y=predict(svyglm(bmival~year, family=gaussian(link="inverse"), design=HSE.ts.srv.nurse), data.frame(year=-10:50), type="response"), x=-10:50, col="purple")

scatter.smooth(svyby(~bmival, by=~a30to06m.imp, design=HSE.ts.srv.nurse, svymean), ylim=c(25,30), xlim=c(0, 7))
lines(y=predict(svyglm(bmival~a30to06m.imp , design=HSE.ts.srv.nurse), data.frame(a30to06m.imp=0:7), type="response"), x=0:7, col="red")

scatter.smooth(svyby(~bmival, by=~porftvg, design=HSE.ts.srv.nurse, svymean), ylim=c(25,30), xlim=c(0, 8))

mod1 <- svyglm(bmival~ log(year+25)*age*sex*qimd + a30to06m.imp + I(age^2),
                    method = "glm.fit2",
                    family=gaussian(link = "log"),
                    design = HSE.ts.srv.nurse) 
anova(mod1) # fv are not significant. literature support this

mod2 <- stepAIC(mod1)
anova(mod2, mod1)
AIC(mod2, mod1)

bmi.svylm <- mod2
bmi.svylm$deviance/bmi.svylm$df.null
1-bmi.svylm$deviance/bmi.svylm$null.deviance # R^2

bmi.svylm$data <- NULL
bmi.svylm$survey.design <- NULL
bmi.svylm$qr <- NULL
bmi.svylm$residuals <- NULL
bmi.svylm$y <- NULL
bmi.svylm$linear.predictors <- NULL
bmi.svylm$fitted.values <- NULL
bmi.svylm$effects <- NULL
bmi.svylm$weights <- NULL
bmi.svylm$prior.weights <- NULL
#save(bmi.svylm, file="./Lagtimes/bmi.svylm.rda")
#save(bmi.svylm, file="./Models/IMPACTncd/Lagtimes/bmi.svylm.rda")

# sbp model ---------------------------------------------------------------
#load(file="./Lagtimes/HSE.ts.RData")
#load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[bmival<16 & age>19, bmival := 16]
HSE.ts[bmival>50 & age>19, bmival := 50]
HSE.ts[age>85, age:= 85]
HSE.ts[is.na(cigst1) == F, cigst2:= ifelse((cigst1 == "4"), "1", "0")]
HSE.ts[, cigst2 := factor(cigst2)]
HSE.ts.srv.nurse <- svydesign(id=~psu, strata =~cluster, weights = ~wt.nurse, 
                              nest=F, data=HSE.ts, check.strata = T)
HSE.ts.srv.nurse <- subset(HSE.ts.srv.nurse, 
                           age>19 & wt.nurse>0 & !is.na(omsysval) & 
                             !is.na(qimd) & !is.na(bmival) &
                             !is.na(cigst2) & 
                             !is.na(a30to06m.imp))

tt <- HSE.ts[age>19 & wt.nurse>0 & !is.na(omsysval) & 
               !is.na(qimd) & !is.na(bmival) &
               !is.na(cigst2) & 
               !is.na(a30to06m.imp), ]
tt[, hist(omsysval)]
tt[, hist(log(omsysval))]

pp <- svyby(~omsysval, by=~age, design=HSE.ts.srv.nurse, svymean)
scatter.smooth(pp, ylim=c(100,150), family="gaussian", span = 1/3)
lines(y=predict(svyglm(omsysval~age + I(age^2), family=gaussian(link="log"), design=HSE.ts.srv.nurse), data.frame(age=20:100), type="response"), x=20:100, col="green")
lines(y=predict(svyglm(omsysval~age + I(age^2)+ I(age^3), family=gaussian(link="log"), design=HSE.ts.srv.nurse), data.frame(age=20:100), type="response"), x=20:100, col="red")

scatter.smooth(svyby(~omsysval, by=~year, design=HSE.ts.srv.nurse, svymean), ylim=c(120,140), xlim=c(-10, 40))
lines(y=predict(svyglm(omsysval~I((log(year+17))^-1), family=gaussian(link="log"), design=HSE.ts.srv.nurse), data.frame(year=-10:50), type="response"), x=-10:50, col="red")
lines(y=predict(svyglm(omsysval~log(log(year + 17)), family=gaussian(link="log"), design=HSE.ts.srv.nurse), data.frame(year=-10:50), type="response"), x=-10:50, col="blue")

mod1 <- svyglm(omsysval~  I((log(year+17))^-1) * age * sex * qimd + 
                          log(bmival) + cigst2 + a30to06m.imp +
                          I(log(bmival)^2) + I(age^3) + I(age^2),
            design = HSE.ts.srv.nurse,
            family=gaussian(link = "log"),
            method = "glm.fit2")
mod2 <- stepAIC(mod1)
anova(mod2)
AIC(mod2, mod1)
sbp.svylm <- mod2
sbp.svylm$deviance/sbp.svylm$df.null
1-sbp.svylm$deviance/sbp.svylm$null.deviance


bb <- svyby(~omsysval, by=~round(bmival), design=HSE.ts.srv.nurse, svymean)
scatter.smooth(bb, ylim=c(100,150))
lines(y=predict(svyglm(omsysval~ bmival + I(bmival^2), family=gaussian(link="identity"), design=HSE.ts.srv.nurse), data.frame(bmival=10:80), type="response"), x=10:80, col="green")
lines(y=predict(svyglm(omsysval~ bmival + I(bmival^2) + I(bmival^3), family=gaussian, design=HSE.ts.srv.nurse), data.frame(bmival=10:80), type="response"), x=10:80, col="red")     
lines(y=predict(svyglm(omsysval~ log(bmival) + I(log(bmival)^2), family=gaussian, design=HSE.ts.srv.nurse), data.frame(bmival=10:80), type="response"), x=10:80, col="blue")

scatter.smooth(svyby(~omsysval, by=~year, design=HSE.ts.srv.nurse, svymean), family = "gaussian", ylim=c(120,140), xlim=c(-10, 1))

scatter.smooth(svyby(~omsysval, by=~year, design=HSE.ts.srv.nurse, svymean), ylim=c(90,140), xlim=c(-30, 100))
lines(y=predict(svyglm(omsysval~year, design=HSE.ts.srv.nurse), data.frame(year=-10:100), type="response"), x=-10:100, col="red")
lines(y=predict(svyglm(omsysval~I((year+30)^-1), family=gaussian(link="identity"), design=HSE.ts.srv.nurse), data.frame(year=-10:100), type="response"), x=-10:100, col="red") # play with+50 to adjust limit
lines(y=predict(svyglm(omsysval~I(exp(-year))+year, family=gaussian(link="identity"), design=HSE.ts.srv.nurse), data.frame(year=-30:100), type="response"), x=-30:100, col="blue")

scatter.smooth(svyby(~omsysval, by=~porftvg.imp, design=HSE.ts.srv.nurse, svymean), ylim=c(90,140), xlim=c(0, 8))
lines(y=predict(svyglm(omsysval~porftvg.imp, design=HSE.ts.srv.nurse), data.frame(porftvg.imp=0:8), type="response"), x=0:8, col="red")
lines(y=predict(svyglm(omsysval~porftvg.imp + I(porftvg.imp^2), design=HSE.ts.srv.nurse), data.frame(porftvg.imp=0:8), type="response"), x=0:8, col="green")

scatter.smooth(svyby(~omsysval, by=~cigst2, design=HSE.ts.srv.nurse, svymean), ylim=c(90,140), xlim=c(0, 7))
scatter.smooth(svyby(~omsysval, by=~a30to06m.imp, design=HSE.ts.srv.nurse, svymean), ylim=c(90,140), xlim=c(0, 7))
lines(y=predict(svyglm(omsysval~a30to06m.imp, design=HSE.ts.srv.nurse), data.frame(a30to06m.imp=0:7), type="response"), x=0:7, col="red")
lines(y=predict(svyglm(omsysval~a30to06m.imp + I(a30to06m.imp^2), design=HSE.ts.srv.nurse), data.frame(a30to06m.imp=0:7), type="response"), x=0:7, col="green")

# It seems like increase PA, increases sbp. This is against current knowledge
# It could be because of misclasification of the participants or the imputation or is reverse causality
# I will manually reverse and /2 the coefficient. Have to find a better solution in the future NEEDS FIXING!!!
sbp.svylm$coefficients[grep("a30to06m.imp", names(sbp.svylm$coefficients))]
#sbp.svylm$coefficients["a30to06m.imp"] <- -0.5*sbp.svylm$coefficients["a30to06m.imp"] # TO BE PERFORMED when
# scenario involves excersize

sbp.svylm$data <- NULL
sbp.svylm$survey.design <- NULL
sbp.svylm$qr <- NULL
sbp.svylm$residuals <- NULL
sbp.svylm$y <- NULL
sbp.svylm$linear.predictors <- NULL
sbp.svylm$fitted.values <- NULL
sbp.svylm$effects <- NULL
sbp.svylm$weights <- NULL
sbp.svylm$prior.weights <- NULL
# save(sbp.svylm, file="./Models/IMPACTncd/Lagtimes/sbp.svylm.rda")
# save(sbp.svylm, file="./Lagtimes/sbp.svylm.rda")

# chol model --------------------------------------------------------------
# load(file="./Lagtimes/HSE.ts.RData")
# load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[bmival<16 & age>19, bmival := 16]
HSE.ts[bmival>40 & age>19, bmival := 40]
HSE.ts[age>85, age:= 85]
HSE.ts.srv.blood <- svydesign(id=~psu, strata =~cluster, weights = ~wt.blood, nest=F, data=HSE.ts, check.strata = T)
HSE.ts.srv.blood <- subset(HSE.ts.srv.blood, age>19 & wt.blood>0 & !is.na(cholval1) & !is.na(qimd) & !is.na(bmival) & !is.na(a30to06m.imp) & !is.na(porftvg.imp)) # year -7 excluded due to heavy influence on regression (minorities). Year -6 blood sample only above 65
# year -8 excluded due to heavy influence on time projections

scatter.smooth(svyby(~cholval1, by=~age, design=HSE.ts.srv.blood, svymean), ylim=c(3,7))
lines(y=predict(svyglm(cholval1~age + I(age^2) + I(age^3), family=gaussian, design=HSE.ts.srv.blood), data.frame(age=20:100), type="response"), x=20:100, col="red")
lines(y=predict(svyglm(cholval1~age + I(age^2), family=gaussian, design=HSE.ts.srv.blood), data.frame(age=20:100), type="response"), x=20:100, col="green")

scatter.smooth(svyby(~cholval1, by=~round(bmival), design=HSE.ts.srv.blood, svymean), ylim=c(3,7))
lines(y=predict(svyglm(cholval1~bmival + I(1/bmival), family=gaussian, design=HSE.ts.srv.blood), data.frame(bmival=10:80), type="response"), x=10:80, col="red")
lines(y=predict(svyglm(cholval1~bmival + I(1/bmival) + I(exp(-bmival)), family=gaussian, design=HSE.ts.srv.blood), data.frame(bmival=10:80), type="response"), x=10:80, col="green")


scatter.smooth(svyby(~cholval1, by=~year, design=subset(HSE.ts.srv.blood, sex == 1), svymean), family="gaussian", ylim=c(3,7), xlim=c(-10, 1))

scatter.smooth(svyby(~cholval1, by=~year, design=HSE.ts.srv.blood, svymean), family="gaussian",ylim=c(3,7), xlim=c(-10, 40))

lines(y=predict(svyglm(cholval1~year, design=HSE.ts.srv.blood), data.frame(year=-10:100), type="response"), x=-10:100, col="red")
lines(y=predict(svyglm(cholval1~log(year + 25), design=HSE.ts.srv.blood), data.frame(year=-10:100), type="response"), x=-10:100, col="blue")

scatter.smooth(svyby(~cholval1, by=~porftvg.imp, design=subset(HSE.ts.srv.blood, sex == 1), svymean), family="gaussian", ylim=c(3,7), xlim=c(0, 8))

scatter.smooth(svyby(~cholval1, by=~a30to06m.imp, design=subset(HSE.ts.srv.blood, sex == 1), svymean), family="gaussian", ylim=c(3,7), xlim=c(0, 7))

chol.svylm <- svyglm(cholval1~log(year + 25) + age + sex + qimd + porftvg.imp + a30to06m.imp +
                       bmival + I(age^2) + I(bmival^2), 
                     family = gaussian(link="identity"),
                     method = "glm.fit2",
                     design = HSE.ts.srv.blood)
anova(chol.svylm)


mod1 <- svyglm(cholval1~
  log(year+25) * age * sex * qimd + bmival + a30to06m.imp + porftvg.imp +
    I(age^2) + I(bmival^2), 
  family = gaussian(link="identity"),
  method = "glm.fit2",
  design = HSE.ts.srv.blood)
anova(mod1)
mod2 <- stepAIC(mod1)
anova(mod2)
mod2$coefficients[grep("a30to06m.imp", names(mod2$coefficients))]

chol.svylm2 <- svyglm(cholval1~year + year:age + age + sex + qimd +
                        a30to06m.imp + bmival + porftvg.imp + I(age^2) + I(bmival^-1) +
                        age:sex + qimd:age + age:a30to06m.imp + age:bmival + sex:bmival, 
                      family = gaussian(link="identity"),
                      method = "glm.fit2",
                      design = HSE.ts.srv.blood)
anova(chol.svylm2)

AIC(mod2, mod1)
chol.svylm <- mod2

chol.svylm$deviance/chol.svylm$df.null
1-chol.svylm$deviance/chol.svylm$null.deviance
plot(pred.chol(0, 42,1,1,20,5,0:7,0)) # PA activity decreases chol up to the age of ~42
plot(pred.chol(0, 50,1,1,20,5,0:7,0)) # then it increases it. (this looks like reverse causality. older people with high chol increase PA to decrease it)
# I will zero the coeff pa:age
chol.svylm$coefficients["age:a30to06m.imp"] <- 0
plot(pred.chol(0, 45,1,1,20,5,0:7,0))
plot(pred.chol(0, 50,1,1,20,5,0:7,0))
plot(pred.chol(0:50, 30,1,1,40,0,0,0), ylim = c(3,6))

chol.svylm$data <- NULL
chol.svylm$survey.design <- NULL
chol.svylm$qr <- NULL
chol.svylm$residuals <- NULL
chol.svylm$y <- NULL
chol.svylm$linear.predictors <- NULL
chol.svylm$fitted.values <- NULL
chol.svylm$effects <- NULL
chol.svylm$weights <- NULL
chol.svylm$prior.weights <- NULL
#save(chol.svylm, file="./Models/IMPACTncd/Lagtimes/chol.svylm.rda")
#save(chol.svylm, file="./Lagtimes/chol.svylm.rda")

# HDL model --------------------------------------------------------------
# load(file="./Lagtimes/HSE.ts.RData")
# load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[bmival<16 & age>19, bmival := 16]
HSE.ts[bmival>40 & age>19, bmival := 40]
HSE.ts[age>85, age:= 85]
HSE.ts[, tc.to.hdl := cholval1/hdlval1]
HSE.ts[, cigst1 := mapvalues(cigst1,  c(4:1 ), c(1,0,0,0))]
HSE.ts.srv.blood <- svydesign(id=~psu, strata =~cluster, weights = ~wt.blood, nest=F, data=HSE.ts, check.strata = T)
HSE.ts.srv.blood <- subset(HSE.ts.srv.blood, age>19 & wt.blood>0 & !is.na(cholval1) & !is.na(qimd) & !is.na(bmival) & 
                             !is.na(hdlval1) & !is.na(a30to06m.imp) & !is.na(cigst1))

HSE.ts[tc.to.hdl<10, hist(log(tc.to.hdl))]
HSE.ts[, scatter.smooth(cholval1, tc.to.hdl)]

scatter.smooth(svyby(~tc.to.hdl, by=~age, design=HSE.ts.srv.blood, svymean), ylim=c(3,5))
lines(y=predict(svyglm(tc.to.hdl~age + I(age^2) + I(age^3), family=gaussian, design=HSE.ts.srv.blood), data.frame(age=20:100), type="response"), x=20:100, col="red")
lines(y=predict(svyglm(tc.to.hdl~age + I(age^2), family=gaussian, design=HSE.ts.srv.blood), data.frame(age=20:100), type="response"), x=20:100, col="green")

scatter.smooth(svyby(~tc.to.hdl, by=~round(bmival), design=HSE.ts.srv.blood, svymean), ylim=c(3,5))
lines(y=predict(svyglm(tc.to.hdl~bmival + I(1/bmival), family=gaussian, design=HSE.ts.srv.blood), data.frame(bmival=10:80), type="response"), x=10:80, col="red")
lines(y=predict(svyglm(tc.to.hdl~ bmival + I(bmival^2), family=gaussian, design=HSE.ts.srv.blood), data.frame(bmival=10:80), type="response"), x=10:80, col="green")

scatter.smooth(svyby(~tc.to.hdl, by=~round(cholval1), design=HSE.ts.srv.blood, svymean), ylim=c(0,20))
lines(y=predict(svyglm(tc.to.hdl~cholval1 + I(1/cholval1), family=gaussian, design=HSE.ts.srv.blood), data.frame(cholval1=0:16), type="response"), x=0:16, col="red")
lines(y=predict(svyglm(tc.to.hdl~cholval1 + I(cholval1^2), family=gaussian, design=HSE.ts.srv.blood), data.frame(cholval1=0:16), type="response"), x=0:16, col="blue")
lines(y=predict(svyglm(tc.to.hdl~ cholval1 + I(log(cholval1)), family=gaussian, design=HSE.ts.srv.blood), data.frame(cholval1=0:16), type="response"), x=0:16, col="green")

scatter.smooth(svyby(~tc.to.hdl, by=~as.integer(qimd), design=subset(HSE.ts.srv.blood, sex == 1), svymean), family="gaussian", ylim=c(3,5), xlim=c(0, 8))

scatter.smooth(svyby(~tc.to.hdl, by=~as.integer(cigst1), design=subset(HSE.ts.srv.blood, sex == 1), svymean), family="gaussian", ylim=c(3,5), xlim=c(0, 8))

scatter.smooth(svyby(~tc.to.hdl, by=~porftvg.imp, design=subset(HSE.ts.srv.blood, sex == 1), svymean), family="gaussian", ylim=c(3,5), xlim=c(0, 8))

scatter.smooth(svyby(~tc.to.hdl, by=~a30to06m.imp, design=subset(HSE.ts.srv.blood, sex == 1), svymean), family="gaussian", ylim=c(3,5), xlim=c(0, 7))

tctohdl.svylm <- svyglm(tc.to.hdl~ (cholval1 + I(cholval1^2)+ age + I(age^2) + sex + qimd +
                                      bmival + I(bmival^2) + a30to06m.imp + cigst1)^2, 
                        family = gaussian(link="log"),
                        method = "glm.fit2",
                        design = HSE.ts.srv.blood)
anova(tctohdl.svylm)

tctohdl.svylm2 <- svyglm(tc.to.hdl~ cholval1 + I(cholval1^2)+ age + I(age^2) + sex + qimd +
                           bmival + I(bmival^2) + a30to06m.imp + cigst1 + 
                           cholval1:age + cholval1:sex,
                         family = gaussian(link="log"),
                         method = "glm.fit2",
                         design = HSE.ts.srv.blood)
anova(tctohdl.svylm2)

tctohdl.svylm <- tctohdl.svylm2
tctohdl.svylm$deviance/tctohdl.svylm$df.null
1-tctohdl.svylm$deviance/tctohdl.svylm$null.deviance

tctohdl.svylm$data <- NULL
tctohdl.svylm$survey.design$variables <- NULL
#save(tctohdl.svylm, file="./Models/IMPACTncd/Lagtimes/tctohdl.svylm.rda")
#save(tctohdl.svylm, file="./Lagtimes/tctohdl.svylm.rda")

# diab model --------------------------------------------------------------
# diab model (no time trend as will consider diabetes is totally dependant to BMI and age)
# load(file="./Lagtimes/HSE.ts.RData")
# load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[diabtotr == 1, diabtotr :=0]
HSE.ts[diabtotr == 2, diabtotr :=1]
HSE.ts[bmival<16 & age>19, bmival := 16]
HSE.ts[bmival>40 & age>19, bmival := 40]
HSE.ts[age>85, age:= 85] # different than the usual 85
HSE.ts.srv.blood <- svydesign(id=~psu, strata =~cluster, weights = ~wt.blood, nest=F, data=HSE.ts, check.strata = T)
HSE.ts.srv.blood <- subset(HSE.ts.srv.blood, age>19 & wt.blood>0 & is.na(diabtotr)== F & is.na(qimd)== F & 
                             year!=-6 & year!=-7 & is.na(bmival)== F & is.na(a30to06m.imp)== F) # years -6 and -7 appear problematic (focus on minorities and elder). see scatter.smooth

scatter.smooth(svyby(~diabtotr, by=~age, design=HSE.ts.srv.blood, svymean), ylim=c(0,0.25), xlim=c(20,100))
lines(y=predict(svyglm(diabtotr~age + I(age^2), family=quasibinomial(link="logit"), design=HSE.ts.srv.blood), data.frame(age=20:100), type="response"), x=20:100, col="purple")
lines(y=predict(svyglm(diabtotr~age + I(age^2) + I(age^3), family=quasibinomial(link="identity"), design=HSE.ts.srv.blood), data.frame(age=20:100), type="response"), x=20:100, col="green")

scatter.smooth(svyby(~diabtotr, by=~round(bmival), design=HSE.ts.srv.blood, svymean), ylim=c(0,0.25))
lines(y=predict(svyglm(diabtotr~bmival + I(bmival^2), family=quasibinomial(link="logit"), design=HSE.ts.srv.blood), data.frame(bmival=15:60), type="response"), x=15:60, col="blue")
lines(y=predict(svyglm(diabtotr~bmival , family=quasibinomial(link="probit"), design=HSE.ts.srv.blood), data.frame(bmival=15:60), type="response"), x=15:60, col="green")

scatter.smooth(svyby(~diabtotr, by=~year, design=HSE.ts.srv.blood, svymean), ylim=c(0,0.25), xlim=c(-9, 1))

scatter.smooth(svyby(~diabtotr, by=~year, design=HSE.ts.srv.blood, svymean), ylim=c(0,0.75), xlim=c(-11, 60))
lines(y=predict(svyglm(diabtotr~year, family=gaussian(link="identity"), design=HSE.ts.srv.blood), data.frame(year=-11:60), type="response"), x=-11:60, col="red")
lines(y=predict(svyglm(diabtotr~exp(-year+52), family=quasibinomial(link="logit"), design=HSE.ts.srv.blood), data.frame(year=-11:60), type="response"), x=-11:60, col="blue")
lines(y=predict(svyglm(diabtotr~I(log(year+11)), family=quasibinomial(link="probit"), design=HSE.ts.srv.blood), data.frame(year=-11:60), type="response"), x=-11:60, col="green")

scatter.smooth(svyby(~diabtotr, by=~a30to06m.imp, design=HSE.ts.srv.blood, svymean), ylim=c(0,0.25), xlim=c(0, 7))
lines(y=predict(svyglm(diabtotr~a30to06m.imp, family=quasibinomial, design=HSE.ts.srv.blood), data.frame(a30to06m.imp=0:7), type="response"), x=0:7, col="red")
lines(y=predict(svyglm(diabtotr~a30to06m.imp + I(a30to06m.imp^2), family=quasibinomial, design=HSE.ts.srv.blood), data.frame(a30to06m.imp=0:7), type="response"), x=0:7, col="blue")

diab.svylr <- svyglm(diabtotr~(age + sex + qimd + bmival + a30to06m.imp + I(age^2) + I(bmival^2)+ I(a30to06m.imp^2))^2,
                     design = HSE.ts.srv.blood, 
                     family=quasibinomial,
                     method = "glm.fit")
anova(diab.svylr)

diab.svylr1 <- svyglm(diabtotr~(age + sex + qimd + bmival + a30to06m.imp)^2 + I(age^2),
                      design = HSE.ts.srv.blood, 
                      family=quasibinomial,
                      method = "glm.fit")
anova(diab.svylr1)

diab.svylr2 <- svyglm(diabtotr~age + sex + qimd + bmival + a30to06m.imp + I(age^2) +
                        age:bmival + age:sex + bmival:I(age^2)  ,
                      design = HSE.ts.srv.blood, 
                      family=quasibinomial,
                      method = "glm.fit")

anova(diab.svylr2)
anova(diab.svylr,diab.svylr2)

diab.svylr <- diab.svylr2
diab.svylr$deviance/diab.svylr$df.null
1-diab.svylr$deviance/diab.svylr$null.deviance

plot(pred.diab(20:85, 1, 5, 28, 7), xlim=c(0, 65), ylim=c(0,0.5))
pred.diab(80:81,1,5, 30,7)

diab.svylr$data <- NULL
diab.svylr$survey.design <- NULL
diab.svylr$qr <- NULL
diab.svylr$residuals <- NULL
diab.svylr$y <- NULL
diab.svylr$linear.predictors <- NULL
diab.svylr$fitted.values <- NULL
diab.svylr$effects <- NULL
diab.svylr$weights <- NULL
diab.svylr$prior.weights <- NULL
#save(diab.svylr, file="./Models/IMPACTncd/Lagtimes/diab.svylr.rda")
#save(diab.svylr, file="./Lagtimes/diab.svylr.rda")

# smoking initiation model -----------------------------------------------------
#load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
#load(file="./Lagtimes/HSE.ts.RData")
HSE.ts[startsmk == 97, startsmk:=NA]
HSE.ts[age>85, age := 85]
# HSE.ts[cigst1 == 3 & between(endsmoke + smokyrs, 0, 1),
#        `:=`(#age        = age - endsmoke - smokyrs,
#             #year       = year - endsmoke - smokyrs,
#             smok.incid = 1L)]
HSE.ts[!is.na(cigst1), smok.incid:= 1L]
HSE.ts[cigst1 == 1, smok.incid:= 0L]
HSE.ts[, smok.incid:= factor(smok.incid)]

HSE.ts.srv.int <- 
  svydesign(id=~psu, strata =~cluster, weights = ~wt.int, nest=F, 
            data=HSE.ts, check.strata = T)
HSE.ts.srv.int <- 
  subset(HSE.ts.srv.int, age > 15 &  wt.int > 0 & !is.na(qimd) &
           !is.na(smok.incid))
#HSE.ts.srv.int <- subset(HSE.ts.srv.int,qimd=="5")

pp <- svyby(~smok.incid, by=~age, design=HSE.ts.srv.int, svymean)

scatter.smooth(y=pp[[3]], x=as.numeric(pp[[1]]), ylim=c(0, 1), family = "gaussian")

lines(y=predict(svyglm(smok.incid~ I(age^-1)+I(age^-2), family=quasibinomial(link="logit"), design=HSE.ts.srv.int), data.frame(age=16:90), type="response"), x=16:90, col="red")
lines(y=predict(svyglm(smok.incid~ I(age^-1)+I(age^-2)+I(age^-3), family=quasibinomial(link="logit"), design=HSE.ts.srv.int), data.frame(age=16:90), type="response"), x=16:90, col="blue")
lines(y=predict(svyglm(smok.incid~age + I(age^2) + I(age^3) + I(age^4) + I(age^5), family=quasibinomial(link="logit"), design=HSE.ts.srv.int), data.frame(age=15:90), type="response"), x=15:90, col="green")

pp <- svyby(~smok.incid, by=~year, design=HSE.ts.srv.int, svymean)
scatter.smooth(pp[[1]], pp[[3]], ylim=c(0, 1), family = "gaussian", xlim=c(-10, 10))
lines(y=predict(svyglm(smok.incid~year, family=quasibinomial(link="logit"),
                       design=HSE.ts.srv.int), data.frame(year=-10:10), type="response"), x=-10:10, col="red")
lines(y=predict(svyglm(smok.incid~I((year+25)^-2)+I((year+25)^-1), family=quasibinomial(link="logit"), design=HSE.ts.srv.int), data.frame(year=-10:10), type="response"), x=-10:10, col="green")


smok.start.svylr <- svyglm(smok.incid~ year + age + I(age^2) + I(age^3) + I(age^4) + I(age^5) + sex + qimd,
                           design = HSE.ts.srv.int, 
                           family=quasibinomial(link="logit"), 
                           method = "glm.fit2")
anova(smok.start.svylr)

smok.start.svylr <- svyglm(smok.incid~ year * I(age^-1) * sex  * qimd +I(age^-2)+I(age^-3),
                           design = HSE.ts.srv.int, 
                           family=quasibinomial(link="logit"), 
                           method = "glm.fit2")

smok.start.svylr$deviance/smok.start.svylr$df.null
1-smok.start.svylr$deviance/smok.start.svylr$null.deviance

smok.start.svylr$data <- NULL
smok.start.svylr$survey.design <- NULL
smok.start.svylr$qr <- NULL
smok.start.svylr$residuals <- NULL
smok.start.svylr$y <- NULL
smok.start.svylr$linear.predictors <- NULL
smok.start.svylr$fitted.values <- NULL
smok.start.svylr$effects <- NULL
smok.start.svylr$weights <- NULL
smok.start.svylr$prior.weights <- NULL
#save(smok.start.svylr, file="./Models/IMPACTncd/Lagtimes/smok.start.svylr.rda")
#save(smok.start.svylr, file="./Lagtimes/smok.start.svylr.rda")


# smoking cessation -----------------------------------------------------------
# load(file="./Lagtimes/HSE.ts.RData")
# load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[startsmk == 97, startsmk := NA]
HSE.ts <- HSE.ts[cigst1 != 1]
HSE.ts[!is.na(cigst1), smok.cess:= 1L]
HSE.ts[cigst1 == 4, smok.cess:= 0L]
HSE.ts[age>85, age:=85]
HSE.ts[, smok.cess:= factor(smok.cess)]
HSE.ts.srv.int <- 
  svydesign(id=~psu, strata =~cluster, weights = ~wt.int, nest=F, 
            data=HSE.ts, check.strata = T)
HSE.ts.srv.int <- 
  subset(HSE.ts.srv.int, age > 15 & wt.int > 0 & !is.na(qimd) &
           !is.na(smok.cess))

pp <- svyby(~smok.cess, by=~year, design=HSE.ts.srv.int, svymean, na.rm=T)
scatter.smooth(pp[[1]], pp[[3]], ylim=c(0, 1), xlim=c(-10,10), family = "gaussian")

lines(y=predict(svyglm(smok.cess~ year, family=quasibinomial(link="logit"),
                       design=HSE.ts.srv.int), data.frame(year=-10:50),
                type="response"), x=-10:50, col="green")
lines(y=predict(svyglm(smok.cess~I((year+25)^-2),
                       family=quasibinomial(link="logit"),
                       design=HSE.ts.srv.int), data.frame(year=-10:50),
                type="response"), x=-10:50, col="red")
lines(y=predict(svyglm(smok.cess~log(year+16) + I(log(year+16)^2), 
                       family=quasibinomial(link="logit"), design=HSE.ts.srv.int), 
                data.frame(year=-10:50), type="response"), x=-10:50, col="blue")

aa<- svyby(~smok.cess, by=~age, design=HSE.ts.srv.int, svymean, na.rm=T)

scatter.smooth(aa[[1]], aa[[3]], ylim=c(0, 1), xlim=c(15,90), family = "gaussian")
lines(y=predict(svyglm(smok.cess~age + I(age^2) + I(age^3), 
                       family=quasibinomial(link="logit"),
                       design=HSE.ts.srv.int), 
                data.frame(age=15:85), type="response"), x=15:85, col="red")
lines(y=predict(svyglm(smok.cess~I(exp(age^-0.0003)) , 
                       family=quasibinomial(link="logit"),
                       design=HSE.ts.srv.int), 
                data.frame(age=15:85), type="response"), x=15:85, col="blue")

smok.cess.svylr <- svyglm(factor(smok.cess)~ year + age + sex +
                            qimd + I(age^2) + I(age^3) + I(age^4),
                          design = HSE.ts.srv.int, 
                          family=quasibinomial(link="logit"), 
                          method = "glm.fit2")
anova(smok.cess.svylr)

smok.cess.svylr2 <- svyglm(factor(smok.cess)~ year * age * sex * qimd + 
                             I(age^2) + I(age^3),
                           design = HSE.ts.srv.int, 
                           family=quasibinomial(link="logit"), 
                           method = "glm.fit2")
anova(smok.cess.svylr2)

anova(smok.cess.svylr2, smok.cess.svylr)
smok.cess.svylr <- smok.cess.svylr2
smok.cess.svylr$deviance/smok.cess.svylr$df.null

smok.cess.svylr$data <- NULL
smok.cess.svylr$survey.design <- NULL
smok.cess.svylr$qr <- NULL
smok.cess.svylr$residuals <- NULL
smok.cess.svylr$y <- NULL
smok.cess.svylr$linear.predictors <- NULL
smok.cess.svylr$fitted.values <- NULL
smok.cess.svylr$effects <- NULL
smok.cess.svylr$weights <- NULL
smok.cess.svylr$prior.weights <- NULL
#save(smok.cess.svylr, file="./Models/IMPACTncd/Lagtimes/smok.cess.svylr.rda")
#save(smok.cess.svylr, file="./Lagtimes/smok.cess.svylr.rda")


# smoking relapse ---------------------------------------------------------
# Assuming perfecr representation of hse with no sampling error,
# no deaths
# load(file="./Lagtimes/HSE.ts.RData")
# load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[, sum(wt.int), by=year]
HSE.ts[, wt.int := wt.int*10000/sum(wt.int), by=year] # make pop of each hse =10000
HSE.ts[, sum(wt.int), by=year]
#HSE.ts[endsmoke>10, endsmoke := 11]
HSE.ts.srv.int <- svydesign(id=~psu, strata =~cluster, weights = ~wt.int, 
                            nest=F, data=HSE.ts, check.strata = T)
HSE.ts.srv.int <- subset(HSE.ts.srv.int, age > 15 & wt.int > 0 & 
                           !is.na(qimd) & endsmoke >= 0 & cigst1 == 3)
#xx<- as.data.table(prop.table(svytable(~endsmoke+year+sex+qimd, HSE.ts.srv.int),2)) 
# Above was tested and year is not significant in the regression (and also too complicated)
pp <- as.data.table(svytable(~sex+qimd, HSE.ts.srv.int))
pp[, sex := as.factor(sex)]
pp[, qimd := as.ordered(qimd)]
xx <- as.data.table(svytable(~endsmoke+sex+qimd, HSE.ts.srv.int))
xx[, endsmoke:=as.numeric(endsmoke)]
xx[, sex := as.factor(sex)]
xx[, qimd := as.ordered(qimd)]
xx <- merge(xx, pp, by = c("sex", "qimd"),  all.x = T)
setkey(xx, sex, qimd, endsmoke)
xx[, pct:= N.x/N.y]
# smoothing to adjust for round number bias
xx[sex==1 & qimd == 1, plot(N.x)]
xx[sex==1 & qimd == 1 & endsmoke<20, lines(shift(predict(glm(N.x ~ log(endsmoke+1))), type = "lead"))]

xx[endsmoke<10, sm := predict(glm(N.x ~ log(endsmoke + 1))), by = .(sex, qimd)]
xx[endsmoke == 0 & sm < N.x, sm := N.x]

xx[, failure := shift(sm) - sm, by=.(sex,qimd)]

xx[,pct:=sm/(sm+failure)]
xx[sex==1 & qimd == 5, plot(failure)]
xx[sex==1 & qimd == 5,
   lines(predict(glm(failure ~ poly(log(endsmoke+1), 2) + endsmoke)))]
xx <- xx[endsmoke<10, ]
smok.cess.success <- glm(cbind(round(sm), round(failure))~ (poly(log(endsmoke), 2) + endsmoke +
                                                              sex + qimd)^2, xx[endsmoke > 0], family="binomial"(link="logit"))
require(MASS)
mod2 <- stepAIC(smok.cess.success)
predict(smok.cess.success, data.frame(endsmoke = 1:9, sex = factor(1, levels = c(1:2)), qimd = ordered(1, levels = c(1:5))), type="response", se.fit=F)
predict(mod2, data.frame(endsmoke = 1:9, sex = factor(1, levels = c(1:2)), qimd = ordered(1, levels = c(1:5))), type="response", se.fit=F)

xx[sex==2 & qimd==5, scatter.smooth(endsmoke,pct, ylim=c(0,1))]
lines(x=c(0:20), predict(smok.cess.success, data.frame(endsmoke = 0:20, sex = factor(2, levels = c(1:2)), qimd = ordered(5, levels = c(1:5))), type="response"), col="red")
smok.cess.success.log <- mod2
smok.cess.success.log$data <- NULL
#save(smok.cess.success.log, file="./Models/IMPACTncd/Lagtimes/smok.cess.success.log.rda")
#save(smok.cess.success.log, file="./Lagtimes/smok.cess.success.log.rda")

# load(file="./Lagtimes/HSE.ts.RData")
# load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[,sum(wt.int), by=year]
HSE.ts[,wt.int := wt.int*10000/sum(wt.int), by=year] # make pop of each hse =10000
HSE.ts[,sum(wt.int), by=year]
HSE.ts.srv.int <- svydesign(id=~psu, strata =~cluster, weights = ~wt.int, 
                            nest=F, data=HSE.ts, check.strata = T)
HSE.ts.srv.int <- subset(HSE.ts.srv.int, age > 15 & wt.int > 0 & 
                           !is.na(qimd) & endsmoke >= 0 & cigst1 == 3)
pp <- as.data.table(svytable(~sex+qimd, HSE.ts.srv.int))
pp[, sex := as.factor(sex)]
pp[, qimd := as.ordered(qimd)]
xx <- as.data.table(svytable(~endsmoke+sex+qimd, HSE.ts.srv.int))
xx[, endsmoke:=as.numeric(endsmoke)]
xx[, sex := as.factor(sex)]
xx[, qimd := as.ordered(qimd)]
xx <- merge(xx, pp, by = c("sex", "qimd"),  all.x = T)
setkey(xx, sex, qimd, endsmoke)
xx[, pct:= N.x/N.y]
# smoothing to edjust for round number bias
xx[sex==1 & qimd == 5, plot(N.x)]
xx[endsmoke > 0 & sex==1 & qimd == 5, lines(predict(glm(N.x ~ poly(endsmoke^-1, 1))))]
xx[endsmoke > 0 & sex==1 & qimd == 5, lines(predict(glm(N.x ~ log(endsmoke))))]

xx[, sm := N.x]
xx[endsmoke > 0, sm := shift(predict(glm(N.x ~ I(endsmoke^-1))), type = "lead"), by = .(sex, qimd)]

xx[, failure := shift(sm) - sm, by=.(sex,qimd)]

xx[,pct:=sm/(sm+failure)]
xx <- xx[endsmoke < 10, ]

smok.cess.success.parabola <- glm(cbind(sm, failure)~ I(endsmoke^-1) +
                                    endsmoke + sex + qimd, xx[endsmoke > 0], family="quasibinomial"(link="logit"))

predict(smok.cess.success.parabola, data.frame(endsmoke = 0:9, sex = factor(1, levels = c(1:2)), qimd = ordered(1, levels = c(1:5))), type="response", se.fit=F)

xx[sex==2 & qimd==1, scatter.smooth(endsmoke,pct, ylim=c(0,1))]
lines(x=c(0:20), predict(smok.cess.success.parabola, data.frame(endsmoke = 0:20, sex = factor(2, levels = c(1:2)), qimd = ordered(1, levels = c(1:5))), type="response"), col="red")

smok.cess.success.parabola$data <- NULL
#save(smok.cess.success.parabola, file="./Models/IMPACTncd/Lagtimes/smok.cess.success.parabola.rda")
#save(smok.cess.success.parabola, file="./Lagtimes/smok.cess.success.parabola.rda")

# smoking prevalence ------------------------------------------------------
# load(file="./Lagtimes/HSE.ts.RData")
# load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[cigst1 == 4, smok.active:=1]
HSE.ts[cigst1 < 4, smok.active:=0]
HSE.ts[age>85, age := 85]
HSE.ts.srv.int <- svydesign(id=~psu, strata =~cluster, weights = ~wt.int, nest=F, data=HSE.ts, check.strata = T)
HSE.ts.srv.int <- subset(HSE.ts.srv.int, age>19 & age <21 & wt.int>0 & !is.na(qimd) & !is.na(smok.active))

pp<- svyby(~smok.active, by=~year, design=HSE.ts.srv.int, svymean, na.rm=T)
scatter.smooth(pp, ylim=c(0, 0.4), xlim=c(-10,30), family = "gaussian")
lines(y=predict(svyglm(smok.active~year , family=quasibinomial(link="logit"), design=HSE.ts.srv.int), 
                data.frame(year=-10:50), type="response"), x=-10:50, col="blue")
lines(y=predict(svyglm(smok.active~year+I(year^2) , family=quasibinomial(link="logit"), design=HSE.ts.srv.int), 
                data.frame(year=-10:50), type="response"), x=-10:50, col="red")
lines(y=predict(svyglm(smok.active~log(year+25) , family=quasibinomial(link="logit"), design=HSE.ts.srv.int), 
                data.frame(year=-10:50), type="response"), x=-10:50, col="green")

aa<- svyby(~smok.active, by=~age, design=HSE.ts.srv.int, svymean, na.rm=T)
scatter.smooth(aa, ylim=c(0, 0.4), xlim=c(15,30), family = "gaussian")
lines(y=predict(svyglm(smok.active~ log(age) , family=quasibinomial(link="logit"), design=HSE.ts.srv.int), 
                data.frame(age=10:100), type="response"), x=10:100, col="red")
lines(y=predict(svyglm(smok.active~log(age) + I(log(age)^2) + age, family=quasibinomial(link="logit"), 
                       design=HSE.ts.srv.int), 
                data.frame(age=10:100), type="response"), x=10:100, col="blue")
lines(y=predict(svyglm(smok.active~I(age^-1) + I(age^2), family=quasibinomial(link="logit"),
                       design=HSE.ts.srv.int), 
                data.frame(age=10:100), type="response"), x=10:100, col="green")

smok.active.svylr <- svyglm(smok.active~ log(year+25) + log(age) + I(log(age)^2) + age + sex + qimd, 
                            design = HSE.ts.srv.int, 
                            family=quasibinomial(link="logit"), 
                            method = "glm.fit2")
anova(smok.active.svylr)

smok.active.svylr2 <- svyglm(smok.active~ (log(year+25) + sex + qimd)^2,
                             design = HSE.ts.srv.int, 
                             family=quasibinomial(link="logit"), 
                             method = "glm.fit2")
require(MASS)
mod2 <- stepAIC(smok.active.svylr2)

anova(mod2)
anova(mod2, smok.active.svylr2)
smok.active.svylr <- mod2
smok.active.svylr$deviance/smok.active.svylr$df.null

smok.active.svylr$data <- NULL
smok.active.svylr$survey.design <- NULL
smok.active.svylr$qr <- NULL
smok.active.svylr$residuals <- NULL
smok.active.svylr$y <- NULL
smok.active.svylr$linear.predictors <- NULL
smok.active.svylr$fitted.values <- NULL
smok.active.svylr$effects <- NULL
smok.active.svylr$weights <- NULL
smok.active.svylr$prior.weights <- NULL
#save(smok.active.svylr, file="./Models/IMPACTncd/Lagtimes/smok.active.svylr.rda")
#save(smok.active.svylr, file="./Lagtimes/smok.active.svylr.rda")

# ex-smoking prevalence ------------------------------------------------------
# load(file="./Lagtimes/HSE.ts.RData")
# load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[, smok.active := 0]
HSE.ts[between(cigst1, 2, 3), smok.active := 1]
HSE.ts[age>85, age := 85]
HSE.ts.srv.int <- svydesign(id=~psu, strata =~cluster, weights = ~wt.int, nest=F, data=HSE.ts, check.strata = T)
HSE.ts.srv.int <- subset(HSE.ts.srv.int, age>19 & age <21 & wt.int>0 & !is.na(qimd) & !is.na(smok.active))

pp <- svyby(~smok.active, by=~year, design=HSE.ts.srv.int, svymean, na.rm=T)
scatter.smooth(pp, ylim=c(0, 0.25), xlim=c(-10,30), family = "gaussian")
lines(y=predict(svyglm(smok.active~year , family=quasibinomial(link="logit"), 
                       design=HSE.ts.srv.int), 
                data.frame(year=-10:50), type="response"), x=-10:50, col="blue")
lines(y=predict(svyglm(smok.active~year+I(year^2) , family=quasibinomial(link="logit"),
                       design=HSE.ts.srv.int), 
                data.frame(year=-10:50), type="response"), x=-10:50, col="red")
lines(y=predict(svyglm(smok.active~log(year+25) , family=quasibinomial(link="logit"), 
                       design=HSE.ts.srv.int), 
                data.frame(year=-10:50), type="response"), x=-10:50, col="green")

aa<- svyby(~smok.active, by=~qimd, design=HSE.ts.srv.int, svymean, na.rm=T)
scatter.smooth(aa, ylim=c(0, 0.4), xlim=c(1,5), family = "gaussian")
lines(y=predict(svyglm(smok.active~ log(age) , family=quasibinomial(link="logit"), design=HSE.ts.srv.int), 
                data.frame(age=10:100), type="response"), x=10:100, col="red")
lines(y=predict(svyglm(smok.active~log(age) + I(log(age)^2) + age, family=quasibinomial(link="logit"), 
                       design=HSE.ts.srv.int), 
                data.frame(age=10:100), type="response"), x=10:100, col="blue")
lines(y=predict(svyglm(smok.active~I(age^-1) + I(age^2), family=quasibinomial(link="logit"),
                       design=HSE.ts.srv.int), 
                data.frame(age=10:100), type="response"), x=10:100, col="green")

smok.exactive.svylr <- svyglm(smok.active~ log(year+25) + log(age) + I(log(age)^2) + age + sex + qimd, 
                              design = HSE.ts.srv.int, 
                              family=quasibinomial(link="logit"), 
                              method = "glm.fit2")
anova(smok.active.svylr)

smok.exactive.svylr2 <- svyglm(smok.active~ (log(year+25) + sex + qimd)^2,
                               design = HSE.ts.srv.int, 
                               family=quasibinomial(link="logit"), 
                               method = "glm.fit2")
require(MASS)
mod2 <- stepAIC(smok.exactive.svylr2)

anova(mod2)
anova(mod2, smok.exactive.svylr2)
smok.exactive.svylr <- mod2
smok.exactive.svylr$deviance/smok.exactive.svylr$df.null

smok.exactive.svylr$data <- NULL
smok.exactive.svylr$survey.design <- NULL
smok.exactive.svylr$qr <- NULL
smok.exactive.svylr$residuals <- NULL
smok.exactive.svylr$y <- NULL
smok.exactive.svylr$linear.predictors <- NULL
smok.exactive.svylr$fitted.values <- NULL
smok.exactive.svylr$effects <- NULL
smok.exactive.svylr$weights <- NULL
smok.exactive.svylr$prior.weights <- NULL
#save(smok.exactive.svylr, file="./Models/IMPACTncd/Lagtimes/smok.exactive.svylr.rda")
#save(smok.exactive.svylr, file="./Lagtimes/smok.exactive.svylr.rda")


# new smoking prevalence ------------------------------------------------------
# load(file="./Lagtimes/HSE.ts.RData")
# load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[cigst1 == 2, cigst1 := 3] #3 = exsmooker
HSE.ts[cigst1 == 4, cigst1 := 2] #2 = smoker
HSE.ts[age>85, age := 85]
HSE.ts[, cigst1 := ordered(cigst1)]
HSE.ts.srv.int <- svydesign(id=~psu, strata =~cluster, weights = ~wt.int, nest=F, data=HSE.ts, check.strata = T)
HSE.ts.srv.int <- subset(HSE.ts.srv.int, age>15 & wt.int>0 & !is.na(qimd) & !is.na(cigst1))
tt = copy(HSE.ts[age>15 & wt.int>0 & !is.na(qimd) & !is.na(cigst1)])
tt[, cigst1 := ordered(cigst1)]

pp <- svyby(~cigst1 == "1", by=~year, design=HSE.ts.srv.int, svymean, na.rm=T)
scatter.smooth(pp[[1]], pp[[2]], ylim=c(0, 0.70), xlim=c(-10,60), family = "gaussian")
lines(y=predict(svyglm(cigst1~year, family=quasibinomial(link="logit"), 
                       design=HSE.ts.srv.int), 
                data.frame(year=-10:60), type="response"), x=-10:60, col="blue")
pp <- svyby(~cigst1 == "2", by=~year, design=HSE.ts.srv.int, svymean, na.rm=T)
scatter.smooth(pp[[1]], pp[[2]], ylim=c(0, 0.70), xlim=c(-10,60), family = "gaussian")
lines(y=predict(svyglm(cigst1~year, family=quasibinomial(link="logit"), 
                       design=HSE.ts.srv.int), 
                data.frame(year=-10:60), type="response"), x=-10:60, col="blue")

aa<- svyby(~cigst1 == "1", by=~age, design=HSE.ts.srv.int, svymean, na.rm=T)
scatter.smooth(aa[[1]], aa[[2]], ylim=c(0, 0.7), xlim=c(20,80), family = "gaussian")
lines(y=predict(svyglm(cigst1~ log(age) , family=quasibinomial(link="logit"), design=HSE.ts.srv.int), 
                data.frame(age=10:100), type="response"), x=10:100, col="red")
lines(y=predict(svyglm(cigst1~log(age) + I(log(age)^2) + age + I(log(age)^3), family=quasibinomial(link="logit"), 
                       design=HSE.ts.srv.int), 
                data.frame(age=10:100), type="response"), x=10:100, col="blue")
lines(y=predict(svyglm(cigst1~age + I(age^2) + I(age^3)+ I(age^4), family=quasibinomial(link="logit"), 
                       design=HSE.ts.srv.int), 
                data.frame(age=10:100), type="response"), x=10:100, col="green")

aa <- svyby(~cigst1 == "2", by=~age, design=HSE.ts.srv.int, svymean, na.rm=T)
scatter.smooth(aa[[1]], aa[[3]], ylim=c(0, 0.7), xlim=c(20,80), family = "gaussian")

require(MASS)
ttt <- data.frame(cigst1=tt[,cigst1],scale(tt[,.(year)]), tt[,.(sex,qimd,wt.int, age)]) 
mod1 <- polr(cigst1~ year * sex + year * qimd + year * age +
               log(age) + I(log(age)^2) + I(log(age)^3),
             data = ttt, 
             weights = wt.int,
             method = "logistic",
             Hess = T)
summary(mod1)
mod2 <- stepAIC(mod1)

cigst1.svylr <- #apply formula of mod2 to svy 
  svyolr(
    cigst1 ~ year + sex + qimd + age + log(age) + 
      I(log(age)^2) + I(log(age)^3) + year:qimd, 
    design = HSE.ts.srv.int, 
    method = "logistic",
    start = c(rep(1, 16)))

# copy parameters of svy model to polr model so predict can work
for (k in intersect(names(mod2),  names(cigst1.svylr))) mod2[k] <- cigst1.svylr[k]

cigst1.svylr <- mod2

cigst1.svylr$data <- NULL
cigst1.svylr$lp <- NULL
cigst1.svylr$fitted.values <- NULL
#save(cigst1.svylr, file="./Models/IMPACTncd/Lagtimes/cigst1.svylr.rda")
#save(cigst1.svylr, file="./Lagtimes/cigst1.svylr.rda")


# smoking cigdyal ---------------------------------------------------------
# load(file="./Lagtimes/HSE.ts.RData")
# load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[startsmk == 97, startsmk := NA]
HSE.ts[cigdyal == 97, cigdyal := NA]
HSE.ts[cigst1 == 4, smokyrs:= age - startsmk]
HSE.ts[smokyrs <0, smokyrs := 0L]
HSE.ts[age > 85, age := 85L]
HSE.ts[, year := as.integer(year)]
# HSE.ts[cigst1 == 3, `:=` (year = year - endsmoke,
#                           cigdyal = as.numeric(numsmok),
#                           cigst1 = 4L)]
HSE.ts[cigdyal>30, cigdyal := 30]
HSE.ts[cigst1 == 4 & cigdyal<1, cigdyal := 1]
HSE.ts[, cigdyal := ordered(as.integer(cigdyal/4))]
HSE.ts <- HSE.ts[between(age, 16, 84)]
HSE.ts[, agegroup := ordered(agegroup)]
HSE.ts.srv.int <- svydesign(id=~psu, strata =~cluster, weights = ~wt.int, 
                            nest=F, data=HSE.ts, check.strata = T)
HSE.ts.srv.int <- subset(HSE.ts.srv.int, age > 15 & wt.int > 0 &
                           !is.na(smokyrs) & year > -30 &
                           !is.na(qimd) & !is.na(cigdyal) & cigst1 == 4)
tt = copy(HSE.ts[age > 15 & wt.int > 0 &
                   !is.na(smokyrs) & year > -30 &
                   !is.na(qimd) & !is.na(cigdyal) & cigst1 == 4])
tt[, cigdyal := ordered(cigdyal)]

pp<- svyby(~cigdyal, by=~year, design=HSE.ts.srv.int, svymean, na.rm=T)
scatter.smooth(pp, ylim=c(0, 30), xlim=c(-40,50), family = "gaussian")
lines(y=predict(svyglm(cigdyal~I(year), design=HSE.ts.srv.int, family = "quasipoisson"), 
                data.frame(year=-80:50), type="response"), x=-80:50, col="blue")
lines(y=predict(svyglm(cigdyal~year + I(year^2), design=HSE.ts.srv.int, family = "quasipoisson"), 
                data.frame(year=-80:50), type="response"), x=-80:50, col="red")
lines(y=predict(svyglm(cigdyal~I((year+40)^-1)+I((year+40)^-2) , design=HSE.ts.srv.int, family = "quasipoisson"), 
                data.frame(year=-30:50), type="response"), x=-30:50, col="green")
lines(y=predict(svyglm(cigdyal~I(log(year+40)^1)+I(log(year+40)^2) , design=HSE.ts.srv.int, family = "quasipoisson"), 
                data.frame(year=-30:50), type="response"), x=-30:50, col="blue")

pp<- svyby(~cigdyal, by=~age, design=HSE.ts.srv.int, svymean, na.rm=T)
scatter.smooth(pp, ylim=c(0, 30), xlim=c(20,84), family = "gaussian")
lines(y=predict(svyglm(cigdyal~age, design=HSE.ts.srv.int, family = "quasipoisson"), 
                data.frame(age=15:84), type="response"), x=15:84, col="blue")
lines(y=predict(svyglm(cigdyal~age+I(age^2), design=HSE.ts.srv.int, family = "quasipoisson"), 
                data.frame(age=15:84), type="response"), x=15:84, col="red")
lines(y=predict(svyglm(cigdyal~log(age) + I(log(age)^2), design=HSE.ts.srv.int, family = "quasipoisson"), 
                data.frame(age=15:84), type="response"), x=15:84, col="green")

pp<- svyby(~cigdyal, by=~smokyrs, design=HSE.ts.srv.int, svymean, na.rm=T)
scatter.smooth(pp, ylim=c(0, 20), xlim=c(0,84), family = "gaussian")
lines(y=predict(svyglm(cigdyal~smokyrs, design=HSE.ts.srv.int, family = "quasipoisson"), 
                data.frame(smokyrs=0:84), type="response"), x=0:84, col="blue")
lines(y=predict(svyglm(cigdyal~smokyrs+I(smokyrs^2), design=HSE.ts.srv.int, family = "quasipoisson"), 
                data.frame(smokyrs=0:84), type="response"), x=0:84, col="red")
lines(y=predict(svyglm(cigdyal~log(smokyrs+1) + I(log(smokyrs+1)^2), design=HSE.ts.srv.int, family = "quasipoisson"), 
                data.frame(smokyrs=0:84), type="response"), x=0:84, col="green")

smok.cigdyal.svylm <- svyglm(cigdyal~ log(year+40) + sex + qimd + smokyrs + I(smokyrs^2) + I(log(year+40)^2),# age excluded due to colinearity
                             design = HSE.ts.srv.int,
                             family = "quasipoisson",
                             method = "glm.fit2")
anova(smok.cigdyal.svylm)

smok.cigdyal.svylm2 <- svyglm(cigdyal~ (log(year+40) + sex + qimd + smokyrs + I(smokyrs^2))^2 + I(log(year+40)^2),
                              design = HSE.ts.srv.int, 
                              family = "quasipoisson",
                              method = "glm.fit2")

require(MASS)
mod3 <- stepAIC(smok.cigdyal.svylm2)
smok.cigdyal.svylm <- mod3
smok.cigdyal.svylm$deviance/smok.cigdyal.svylm$df.null

smok.cigdyal.svylm$data <- NULL
smok.cigdyal.svylm$survey.design <- NULL
smok.cigdyal.svylm$qr <- NULL
smok.cigdyal.svylm$residuals <- NULL
smok.cigdyal.svylm$y <- NULL
smok.cigdyal.svylm$linear.predictors <- NULL
smok.cigdyal.svylm$fitted.values <- NULL
smok.cigdyal.svylm$effects <- NULL
smok.cigdyal.svylm$weights <- NULL
smok.cigdyal.svylm$prior.weights <- NULL
#save(smok.cigdyal.svylm, file="./Models/IMPACTncd/Lagtimes/smok.cigdyal.svylm.rda")
#save(smok.cigdyal.svylm, file="./Lagtimes/smok.cigdyal.svylm.rda")

ttt <- data.frame(cigdyal=tt[,cigdyal],scale(tt[,.(year, age)]), tt[,.(sex,qimd,wt.int)]) 
mod1 <- polr(cigdyal~ year * sex * qimd * age +I(age^2),
             data = ttt, 
             weights = wt.int,
             method = "logistic",
             Hess = T)
summary(mod1)
mod2 <- stepAIC(mod1)

cigdyal.svylr <- #apply formula of mod2 to svy 
  svyolr(
    cigdyal ~ year + sex + qimd + age + I(age^2) + 
      year:sex + year:qimd + sex:qimd + year:age + sex:age + qimd:age + 
      year:sex:age + year:qimd:age + sex:qimd:age, 
    design = HSE.ts.srv.int, 
    method = "logistic")

# copy parameters of svy model to polr model so predict can work
for (k in intersect(names(mod2),  names(cigdyal.svylr))) mod2[k] <- cigdyal.svylr[k]

cigdyal.svylr <- mod2

cigdyal.svylr$data <- NULL
cigdyal.svylr$lp <- NULL
cigdyal.svylr$fitted.values <- NULL
#save(cigdyal.svylr, file="./Models/IMPACTncd/Lagtimes/cigdyal.svylr.rda")
#save(cigdyal.svylr, file="./Lagtimes/cigdyal.svylr.rda")

HSE.ts[cigst1 == 4, meansd(cigdyal)]
HSE.ts[cigst1 == 4, plot(density(cigdyal, na.rm = T))]
meansd(rnbinom(1e5, mu = 13.3, size = 2.9))
lines(density(rnbinom(1e5, mu = 13.3, size = 2.9)))
meansd(c(rnbinom(1e5, mu = 11.3, size = 2.9), rnbinom(1e5, mu = 15.3, size = 2.9)))
HSE.ts[cigst1 == 4 & qimd == "5", meansd(cigdyal)]
meansd(rnbinom(1e5, mu = 14.26, size = 2.9))

y1 <- HSE.ts[cigst1 == 4, density(cigdyal, na.rm = T)]
y2 <- density(rnbinom(1e5, mu = 13.3, size = 2.9))
auc(y1$x, y1$y, type = "spline", to = 30)
auc(y2$x, y2$y, type = "spline", to = 30)
# F&V ---------------------------------------------------------------------
# load(file="./Lagtimes/HSE.ts.RData")
# load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[bmival<16 & age>19, bmival := 16]
HSE.ts[bmival>50 & age>19, bmival := 50]
HSE.ts[age>85, age := 85]
HSE.ts[porftvg>8, porftvg := 8L]
HSE.ts[, porftvg := ordered(porftvg)]

HSE.ts.srv.int <- svydesign(id=~psu, strata =~cluster, weights = ~wt.int, nest=F, data=HSE.ts, check.strata = T)
HSE.ts.srv.int <- subset(HSE.ts.srv.int, age>19 & wt.int>0 & is.na(porftvg)== F & is.na(qimd)==F & year != -8)   

tt = copy(HSE.ts[age>19 & wt.int>0 & is.na(porftvg)== F & is.na(qimd)==F & year != -8, ])

tt[, porftvg := ordered(porftvg)]

require(MASS)
# I need to scale count vars for stepaic to work
ttt <-data.frame(porftvg=tt[,porftvg],scale(tt[,.(year,age)]), tt[,.(sex,qimd,wt.int)]) 
mod1 <- polr(porftvg~log(year + 25) * age * sex * qimd + I(age^2),
             data = ttt, 
             weights = wt.int,
             method = "logistic",
             Hess = T)
summary(mod1)
mod2 <- stepAIC(mod1)

mod2 <- # best model based on aic
  polr(
    porftvg~log(year + 25) + age + sex + qimd + 
      I(age^2) + log(year + 25):age + age:sex + log(year + 25):qimd + 
      age:qimd + sex:qimd + age:sex:qimd,
    data = tt, 
    weights = wt.int,
    method = "logistic",
    Hess = T)

fv.svylr <- #apply formula of mod2 to svy 
  svyolr(
    porftvg~log(year + 25) + age + sex + qimd + 
      I(age^2) + log(year + 25):age + age:sex + log(year + 25):qimd + 
      age:qimd + sex:qimd + age:sex:qimd, 
    design = HSE.ts.srv.int, 
    method = "logistic")

# copy parameters of svy model to polr model so predict can work
for (k in intersect(names(mod2),  names(fv.svylr))) mod2[k] <- fv.svylr[k]

fv.svylr <- mod2

pp <- svyby(~porftvg, by=~age, design=HSE.ts.srv.int, svymean)
scatter.smooth(pp, family = "gaussian", ylim=c(0,6))
lines(y=predict(svyglm(porftvg~age + I(age^2), family=quasipoisson(), design=HSE.ts.srv.int), data.frame(age=20:100), type="response"), x=20:100, col="red")

pp <- svyby(~porftvg, by=~year, design=HSE.ts.srv.int, svymean)
scatter.smooth(pp, family = "gaussian", ylim=c(3,4.5), xlim=c(-10,1))
scatter.smooth(pp, family = "gaussian", ylim=c(3,4.5), xlim=c(-10,50))
lines(y=predict(svyglm(porftvg~year, family=quasipoisson(), design=HSE.ts.srv.int), data.frame(year=-10:50), type="response"), x=-10:50, col="red")

pp<- svyby(~porftvg, by=~round(bmival), design=subset(HSE.ts.srv.int, sex == 1), svymean)
scatter.smooth(pp, family = "gaussian", ylim=c(0,6))
lines(y=predict(svyglm(porftvg~bmival + I(bmival^2) + I(bmival^3), family=quasipoisson(), design=subset(HSE.ts.srv.int, sex == 1)), data.frame(bmival=10:60), type="response"), x=10:60, col="red")
lines(y=predict(svyglm(porftvg~bmival + I(bmival^2) , family=quasipoisson(), design=subset(HSE.ts.srv.int, sex == 1)), data.frame(bmival=10:60), type="response"), x=10:60, col="blue")



fv.svylr$deviance/fv.svylr$df.null
1-fv.svylr$deviance/fv.svylr$null.deviance

fv.svylr$data <- NULL
fv.svylr$lp <- NULL
fv.svylr$fitted.values <- NULL
#save(fv.svylr, file="./Models/IMPACTncd/Lagtimes/fv.svylr.rda")
#save(fv.svylr, file="./Lagtimes/fv.svylr.rda")

# F/V ratio ----------------------------------------------------------------
# load(file="./Lagtimes/HSE.ts.RData")
# load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[bmival<16 & age>19, bmival := 16]
HSE.ts[bmival<16 & age>19, bmival := 16]
HSE.ts[bmival>50 & age>19, bmival := 50]
HSE.ts[age>85, age := 85]
HSE.ts[frtpor > porftvg, frtpor:=porftvg] # Sanity check
HSE.ts[, fvrate := frtpor/porftvg]
HSE.ts.srv.int <- svydesign(id=~psu, strata =~cluster, weights = ~wt.int, nest=F, data=HSE.ts, check.strata = T)
HSE.ts.srv.int <- subset(HSE.ts.srv.int, age>19 & wt.int>0 & year < 1 & is.na(fvrate)== F & is.na(qimd)==F & is.na(sex)==F) 

pp<- svyby(~porftvg, by=~frtpor, design=subset(HSE.ts.srv.int, sex == 2), svymean)
scatter.smooth(pp, family = "gaussian")
lines(y=predict(svyglm(porftvg~age + I(age^2), family=quasibinomial(), design=subset(HSE.ts.srv.int, sex == 1)), data.frame(age=20:100), type="response"), x=20:100, col="red")

pp<- svyby(~fvrate, by=~porftvg, design=HSE.ts.srv.int, svymean)
scatter.smooth(pp, family = "gaussian")
lines(y=predict(svyglm(fvrate~porftvg, family=quasibinomial(), design=HSE.ts.srv.int), data.frame(porftvg=0:8), type="response"), x=0:8, col="red")
lines(y=predict(svyglm(fvrate~porftvg + I(porftvg^2), family=quasibinomial(), design=HSE.ts.srv.int), data.frame(porftvg=0:8), type="response"), x=0:8, col="green")
lines(y=predict(svyglm(fvrate~porftvg + I(porftvg^2) + I(porftvg^3), family=quasibinomial(), design=HSE.ts.srv.int), data.frame(porftvg=0:8), type="response"), x=0:8, col="blue")

pp<- svyby(~fvrate, by=~age, design=HSE.ts.srv.int, svymean)
scatter.smooth(pp, family = "gaussian")
lines(y=predict(svyglm(fvrate~age, family=quasibinomial(), design=HSE.ts.srv.int), data.frame(age=20:100), type="response"), x=20:100, col="red")
lines(y=predict(svyglm(fvrate~age + I(age^2), family=quasibinomial(), design=HSE.ts.srv.int), data.frame(age=20:100), type="response"), x=20:100, col="blue")

pp<- svyby(~fvrate, by=~year, design=HSE.ts.srv.int, svymean)
scatter.smooth(pp, family = "gaussian")
scatter.smooth(pp, family = "gaussian", ylim = c(0:1))

fvrate.svylr <- svyglm(fvrate~porftvg + I(porftvg^2) + I(porftvg^3)+ age + I(age^2) + sex + qimd , family=quasibinomial(),  method = "glm.fit2", design=HSE.ts.srv.int)
anova(fvrate.svylr)

fvrate.svylr$deviance/fvrate.svylr$df.null
1-fvrate.svylr$deviance/fvrate.svylr$null.deviance

fvrate.svylr$data <- NULL
fvrate.svylr$survey.design$variables <- NULL
#save(fvrate.svylr, file="./Models/IMPACTncd/Lagtimes/fvrate.svylr.rda")
#save(fvrate.svylr, file="./Lagtimes/fvrate.svylr.rda")

# Ethnicity -------------------------------------------------------------
# load(file="./Lagtimes/HSE.ts.RData")
# load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[, origin := factor(origin)]
HSE.ts[, agegroup := agegroup.fn(age)]
HSE.ts.srv.int <- svydesign(id=~psu, strata =~cluster, weights = ~wt.int, nest=F, data=HSE.ts, check.strata = T)
HSE.ts.srv.int <- subset(HSE.ts.srv.int, wt.int>0 & year > -2 & !is.na(origin) & !is.na(qimd) & !is.na(sex)) 

ttt <- svyby(~origin, ~agegroup+sex+qimd, HSE.ts.srv.int, svymean)
require(nnet)

ttt <-data.table(
  origin = HSE.ts[year > -2, factor(origin)],
  HSE.ts[year > -2,.(age = age/100)],
  HSE.ts[year > -2,.(sex, qimd = as.character(qimd), wt.int)]
) 

mod1 <- multinom(origin ~ age + sex + qimd,
                 data = ttt, 
                 weights = wt.int,
                 Hess = T)
summary(mod1)

origin.multinom <- mod1
#save(origin.multinom, file="./Models/IMPACTncd/Lagtimes/origin.multinom.rda")
#save(origin.multinom, file="./Lagtimes/origin.multinom.rda")

# FamCVD ------------------------------------------------------------------
# load(file="./Lagtimes/HSE.ts.RData")
# load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[famcvd == 2,famcvd := 0]
HSE.ts[, famcvd := factor(famcvd)]
HSE.ts[, agegroup := agegroup.fn(age)]
HSE.ts.srv.int <- svydesign(id=~psu, strata =~cluster, weights = ~wt.int, nest=F, data=HSE.ts, check.strata = T)
HSE.ts.srv.int <- subset(HSE.ts.srv.int, wt.int>0 & !is.na(famcvd) & !is.na(qimd)) 

famcvd.svylr <- svyglm(famcvd~age + I(age^2) + qimd,
                       design = HSE.ts.srv.int, 
                       family=quasibinomial,
                       method = "glm.fit2")
anova(famcvd.svylr)
1-famcvd.svylr$deviance/famcvd.svylr$null.deviance

famcvd.svylr$data <- NULL
famcvd.svylr$survey.design$variables <- NULL
#save(famcvd.svylr, file="./Models/IMPACTncd/Lagtimes/famcvd.svylr.rda")
#save(famcvd.svylr, file="./Lagtimes/famcvd.svylr.rda")

# AF prevalence  ------------------------------------------------------------------
# load(file="./Lagtimes/HSE.ts.RData")
# load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[iregdef == 2,iregdef := 0]
HSE.ts[, iregdef := factor(iregdef)]
HSE.ts[, agegroup := agegroup.fn(age)]
#HSE.ts[, cigst1 := mapvalues(cigst1,  c(4:1 ), c(1,0,0,0))]
HSE.ts.srv.int <- svydesign(id=~psu, strata =~cluster, weights = ~wt.int, nest=F, data=HSE.ts, check.strata = T)
HSE.ts.srv.int <- subset(HSE.ts.srv.int, wt.int>0 & !is.na(iregdef) & !is.na(qimd) & !is.na(cigst1)) 

af.svylr <- svyglm(iregdef~ age + qimd + ordered(cigst1),
                   design = HSE.ts.srv.int, 
                   family=quasibinomial,
                   method = "glm.fit2")
anova(af.svylr) #omsysval non significant. ?? missing values ?? small sample

af.svylr$data <- NULL
af.svylr$survey.design$variables <- NULL
#save(af.svylr, file="./Models/IMPACTncd/Lagtimes/af.svylr.rda")
#save(af.svylr, file="./Lagtimes/af.svylr.rda")

# Kidney disease prevalence  ------------------------------------------------------------------
# load(file="./Lagtimes/HSE.ts.RData")
# load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[kiddiag == 2, kiddiag := 0]
HSE.ts[, kiddiag := factor(kiddiag)]
HSE.ts[, agegroup := agegroup.fn(age)]
HSE.ts.srv.int <- svydesign(id=~psu, strata =~cluster, weights = ~wt.int, nest=F, data=HSE.ts, check.strata = T)
HSE.ts.srv.int <- subset(HSE.ts.srv.int, wt.int>0 & !is.na(kiddiag) & !is.na(qimd) & !is.na(sex)) 

kiddiag.svylr <- svyglm(kiddiag~ age + sex + qimd,
                        design = HSE.ts.srv.int, 
                        family=quasibinomial,
                        method = "glm.fit2")
anova(kiddiag.svylr) #omsysval non significant. ?? missing values ?? small sample

kiddiag.svylr$data <- NULL
kiddiag.svylr$survey.design$variables <- NULL
#save(kiddiag.svylr, file="./Models/IMPACTncd/Lagtimes/kiddiag.svylr.rda")
#save(kiddiag.svylr, file="./Lagtimes/kiddiag.svylr.rda")

# BP medication  ------------------------------------------------------------------
# load(file="./Lagtimes/HSE.ts.RData")
# load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[, bpmedd := factor(bpmedd)]
HSE.ts[, agegroup := agegroup.fn(age)]
HSE.ts.srv.nurse <- svydesign(id=~psu, strata =~cluster, weights = ~wt.nurse, nest=F, data=HSE.ts, check.strata = T)
HSE.ts.srv.nurse <- subset(HSE.ts.srv.nurse, wt.nurse>0 & !is.na(bpmedd) & !is.na(qimd) & !is.na(sex) & !is.na(omsysval)) 
svytable(~bpmedd, subset(HSE.ts.srv.nurse, age > 30 & age < 84))

bpmed.svylr <- svyglm(bpmedd~ age + I(age^2) + sex + qimd + omsysval +
                        age:sex + age:omsysval,
                      design = HSE.ts.srv.nurse, 
                      family=quasibinomial,
                      method = "glm.fit2")
anova(bpmed.svylr) # year non significant

bpmed.svylr <- svyglm(bpmedd~ (age + I(age^2) + sex + qimd + omsysval)^2,
                      design = HSE.ts.srv.nurse, 
                      family=quasibinomial,
                      method = "glm.fit2")
bpmed.svylr2 <- stepAIC(bpmed.svylr)
anova(bpmed.svylr2) # year non significant

bpmed.svylr <- bpmed.svylr2
1-bpmed.svylr$deviance/bpmed.svylr$null.deviance

bpmed.svylr$data <- NULL
bpmed.svylr$survey.design <- NULL
bpmed.svylr$qr <- NULL
bpmed.svylr$residuals <- NULL
bpmed.svylr$y <- NULL
bpmed.svylr$linear.predictors <- NULL
bpmed.svylr$fitted.values <- NULL
bpmed.svylr$effects <- NULL
bpmed.svylr$weights <- NULL
bpmed.svylr$prior.weights <- NULL
#save(bpmed.svylr, file="./Models/IMPACTncd/Lagtimes/bpmed.svylr.rda")
#save(bpmed.svylr, file="./Lagtimes/bpmed.svylr.rda")

# Undiagnosed diab --------------------------------------------------------
# load(file="./Lagtimes/HSE.ts.RData")
# load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[, diabtotr := factor(diabtotr - 1)] #everybody
HSE.ts[diabete2 == 2, diabete2 := 0] #only diagnosed
HSE.ts[diabtotr == "1" & diabete2 == "0", undiag.diab := 1L]
HSE.ts[diabtotr == "1" & diabete2 == "1", undiag.diab := 0L]
HSE.ts[, undiag.diab :=  factor(undiag.diab)]
HSE.ts[, diabete2 := factor(diabete2)]
HSE.ts[, agegroup := agegroup.fn(age)]
HSE.ts.srv.blood <- svydesign(id=~psu, strata =~cluster, weights = ~wt.blood, nest=F, data=HSE.ts, check.strata = T)
HSE.ts.srv.blood <- subset(HSE.ts.srv.blood, wt.blood > 0 & !is.na(qimd) & !is.na(sex) &
                             !is.na(undiag.diab) & year!= -7) #-7 is influential outliar
svytable(~undiag.diab, subset(HSE.ts.srv.blood, age > 30 & age < 84))
pp<- svyby(~undiag.diab, by=~year, design=HSE.ts.srv.blood, svymean)
scatter.smooth(x=pp$year, y=pp$undiag.diab1, family = "gaussian")

undiag.diab.svylr <- svyglm(undiag.diab ~ qimd, #no other factors were signif. 
                            # nor interactions. Year is signif but data not suitable for 
                            # longitudianal analysis
                            design = HSE.ts.srv.blood, 
                            family=quasibinomial,
                            method = "glm.fit2")
anova(undiag.diab.svylr) # year non significant

undiag.diab.svylr$data <- NULL
undiag.diab.svylr$survey.design$variables <- NULL
#save(undiag.diab.svylr, file="./Models/IMPACTncd/Lagtimes/undiag.diab.svylr.rda")
#save(undiag.diab.svylr, file="./Lagtimes/undiag.diab.svylr.rda")

# COPD Prevalence model ---------------------------------------------------
setwd("E:/Dropbox/PhD/")
load(file="./Datasets/Health Survey for England/2010/hse10ai.RData")
HSE2010original <- setDT(clear.labels(HSE2010original))
HSE2010original <- filter(HSE2010original, samptype==1)
setnames(HSE2010original, "imd2007", "qimd")
HSE2010original[cholflag == 1, `:=` (cholval1 = cholval1 + 0.1, hdlval1 = hdlval1 - 0.1)]
HSE2010original[is.na(wt.nurse), wt.nurse := 0]
HSE2010original[is.na(wt.blood), wt.blood := 0]
agegroup.fn(HSE2010original)
HSE2010original[diabete2 == 2, diabtotr := 1]
HSE2010original[diabete2 == 1 | glyhbval > 6.5, diabtotr := 2]
HSE2010original[copd == 2, copd := 0L]
HSE2010original[, sex := factor(sex)]
HSE2010original[, qimd := ordered(qimd)]
HSE2010original[, cigst1 := factor(cigst1)]

HSE.ts.srv.int <- svydesign(id=~psu, strata =~cluster, weights = ~wt.int, nest=F,
                            data=HSE2010original, check.strata = T)
HSE.ts.srv.int <- subset(HSE.ts.srv.int, wt.int>0 & !is.na(copd) & !is.na(qimd) &
                           !is.na(sex) & age >= 30 & age <= 84 & !is.na(cigst1))

copd.svylr <- svyglm(copd~ age + qimd + cigst1,
                     design = HSE.ts.srv.int, 
                     family=quasibinomial)

anova(copd.svylr)
copd.svylr$data <- NULL
copd.svylr$survey.design$variables <- NULL
#save(copd.svylr, file="./Models/IMPACTncd/copd.svylr.rda")
#save(copd.svylr, file="./Lagtimes/copd.svylr.rda")
