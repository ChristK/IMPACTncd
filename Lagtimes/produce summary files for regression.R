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
               "compiler",
               "survey",
               "MASS",
               "glm2"))



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

# Import datasets (house hold and individual files)
load(file="./Datasets/Health Survey for England/2012/hse2012ai.RData")
HSE2012original <- clear.labels(HSE2012original)
HSE2012original <- data.table(HSE2012original, key="age")
agegroup.fn(HSE2012original)
#HSE.1.srv.blood <- svydesign(id=~psu, strata=~cluster, weights=~wt.blood, nest=F, data=HSE2012original[wt.blood>0], check.strata = T)
#HSE.1.srv.nurse <- svydesign(id=~psu, strata=~cluster, weights=~wt.nurse, nest=F, data=HSE2012original[wt.nurse>0], check.strata = T)

HSE2012 =copy(HSE2012original[, list(wt.int, wt.nurse, wt.blood, psu, cluster, age, agegroup, sex, group, qimd, bmival, cholval12, omsysval, diabtotr, cigst1, startsmk, endsmoke, numsmok, smokyrs, cigdyal)])
HSE2012[, `:=`(year=1, porftvg = NA, frtpor = NA)]
HSE2012[, psu := paste0(psu, "2012")]
HSE2012[, cluster := paste0(cluster, "2012")]
setnames(HSE2012, "cholval12", "cholval1")

load(file="./Datasets/Health Survey for England/2011/hse2011ai.RData")
HSE2011original <- clear.labels(HSE.2011)
rm(HSE.2011)
HSE2011original <- data.table(HSE2011original, key="age")
agegroup.fn(HSE2011original)
#HSE0.srv.blood <- svydesign(id=~psu, strata=~cluster, weights=~wt.blood, nest=F, data=HSE2011original[wt.blood>0], check.strata = T)
#HSE0.srv.nurse <- svydesign(id=~psu, strata=~cluster, weights=~wt.nurse, nest=F, data=HSE2011original[wt.nurse>0], check.strata = T)
#HSE0.srv.int <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=HSE2011original[wt.nurse>0], check.strata = T)

HSE2011 =copy(HSE2011original[, list(wt.int, wt.nurse, wt.blood, psu, cluster, age, agegroup, sex, group, qimd, bmival, cholval1, omsysval, diabtotr, cigst1, startsmk, endsmoke, porftvg, frtpor, numsmok, smokyrs, cigdyal)])
HSE2011[, `:=`(year=0)]
HSE2011[, psu := paste0(psu, "2011")]
HSE2011[, cluster := paste0(cluster, "2011")]

load(file="./Datasets/Health Survey for England/2010/hse10ai.RData")
HSE2010original <- clear.labels(HSE2010original)
HSE2010original <- data.table(filter(HSE2010original, samptype==1), key="age")
setnames(HSE2010original, "imd2007", "qimd")
HSE2010original[, cholval1 := cholval1 + 0.1]
HSE2010original[is.na(wt.nurse), wt.nurse := 0]
HSE2010original[is.na(wt.blood), wt.blood := 0]
agegroup.fn(HSE2010original)
HSE2010original[diabete2 == 2, diabtotr := 1]
HSE2010original[diabete2 == 1 | glyhbval > 6.5, diabtotr := 2]
#HSE1.srv.blood <- svydesign(id=~psu, strata=~cluster, weights=~wt.blood, nest=F, data=HSE2010original[wt.blood>0], check.strata = T)
#HSE1.srv.nurse <- svydesign(id=~psu, strata=~cluster, weights=~wt.nurse, nest=F, data=HSE2010original[wt.nurse>0], check.strata = T)
#HSE1.srv.int <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=HSE2010original[wt.int>0], check.strata = T)

HSE2010 =copy(HSE2010original[, list(wt.int, wt.nurse, wt.blood, psu, cluster, age, agegroup, sex, group, qimd, bmival, cholval1, omsysval, diabtotr, cigst1, startsmk, endsmoke, porftvg, frtpor, numsmok, smokyrs, cigdyal)])
HSE2010[, `:=`(year=-1)]
HSE2010[, psu := paste0(psu, "2010")]
HSE2010[, cluster := paste0(cluster, "2010")]

load(file="./Datasets/Health Survey for England/2009/hse09ai.RData")
HSE2009original <- clear.labels(HSE2009original)
HSE2009original <- data.table(filter(HSE2009original, samptype==1), key="age") 
setnames(HSE2009original, "imd2007", "qimd")
HSE2009original[, cholval1 := cholval1 + 0.1]
agegroup.fn(HSE2009original)
HSE2009original[diabete2 == 2, diabtotr := 1]
HSE2009original[diabete2 == 1 | glyhbval > 6.5, diabtotr := 2]
#HSE2.srv.blood <- svydesign(id=~psu, strata=~cluster, weights=~wt.blood, nest=F, data=HSE2009original[wt.blood>0], check.strata = T)
#HSE2.srv.nurse <- svydesign(id=~psu, strata=~cluster, weights=~wt.nurse, nest=F, data=HSE2009original[wt.nurse>0], check.strata = T)
#HSE2.srv.int <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=HSE2009original[wt.int>0], check.strata = T)

HSE2009 =copy(HSE2009original[, list(wt.int, wt.nurse, wt.blood, psu, cluster, age, agegroup, sex, group, qimd, bmival, cholval1, omsysval, diabtotr, cigst1, startsmk, endsmoke, porftvg, frtpor, numsmok, smokyrs, cigdyal)])
HSE2009[, `:=`(year=-2)]
HSE2009[, psu := paste0(psu, "2009")]
HSE2009[, cluster := paste0(cluster, "2009")]

load(file="./Datasets/Health Survey for England/2008/hse08ai.RData")
HSE2008original <- clear.labels(HSE2008original)
HSE2008original <- data.table(filter(HSE2008original, samptype==1), key="age") 
HSE2008original[, cholval1 := cholval1 + 0.1]
agegroup.fn(HSE2008original)
#HSE3.srv.blood <- svydesign(id=~psu, strata=~cluster, weights=~wt.blood, nest=F, data=HSE2008original[wt.blood>0], check.strata = T)
#HSE3.srv.nurse <- svydesign(id=~psu, strata=~cluster, weights=~wt.nurse, nest=F, data=HSE2008original[wt.nurse>0], check.strata = T)
#HSE3.srv.int <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=HSE2008original[wt.int>0], check.strata = T)

HSE2008 =copy(HSE2008original[, list(wt.int, wt.nurse, wt.blood, psu, cluster, age, agegroup, sex, group, qimd, bmival, cholval1, omsysval,  cigst1, startsmk, endsmoke, porftvg, frtpor, numsmok, smokyrs, cigdyal)])
HSE2008[, `:=`(year=-3, diabtotr = NA)]
HSE2008[, psu := paste0(psu, "2008")]
HSE2008[, cluster := paste0(cluster, "2008")]


load(file="./Datasets/Health Survey for England/2007/hse07ai.RData")
HSE2007original <- clear.labels(HSE2007original)
HSE2007original <- data.table(filter(HSE2007original, samptype==1), key="age")
setnames(HSE2007original, c("imd2007", "area"), c("qimd", "psu"))
agegroup.fn(HSE2007original)
#HSE4.srv.nurse <- svydesign(id=~psu, strata=~cluster, weights=~wt.nurse, nest=F, data=HSE2007original[wt.nurse>0], check.strata = T)
#HSE4.srv.int <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=HSE2007original[wt.int>0], check.strata = T)

HSE2007 =copy(HSE2007original[, list(wt.int, wt.nurse,  psu, cluster, age, agegroup, sex, group, qimd, bmival, omsysval, cigst1, startsmk, endsmoke, porftvg, frtpor, numsmok, smokyrs, cigdyal)])
HSE2007[, `:=`(year=-4, diabtotr = NA, wt.blood = 1, cholval1=NA)]
HSE2007[, psu := paste0(psu, "2007")]
HSE2007[, cluster := paste0(cluster, "2007")]

load(file="./Datasets/Health Survey for England/2006/hse06ai.RData")
HSE <- clear.labels(HSE)
HSE2006original <- data.table(filter(HSE, samptype!=3), key="age") 
rm(HSE)
setnames(HSE2006original, "imd2004", "qimd")
HSE2006original[, cholval1 := cholval1 + 0.1]
agegroup.fn(HSE2006original)
HSE2006original[diabete2 == 2, diabtotr := 1]
HSE2006original[diabete2 == 1 | glyhbval > 6.5, diabtotr := 2]
#HSE5.srv.blood <- svydesign(id=~psu, strata=~cluster, weights=~wt.blood, nest=F, data=HSE2006original[wt.blood>0], check.strata = T)
#HSE5.srv.nurse <- svydesign(id=~psu, strata=~cluster, weights=~wt.nurse, nest=F, data=HSE2006original[wt.nurse>0], check.strata = T)
#HSE5.srv.int <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=HSE2006original[wt.int>0], check.strata = T)

HSE2006 =copy(HSE2006original[, list(wt.int, wt.nurse, wt.blood, psu, cluster, age, agegroup, sex, group, qimd, bmival, cholval1, omsysval, diabtotr, cigst1, startsmk, endsmoke, porftvg, frtpor, numsmok, smokyrs, cigdyal)])
HSE2006[, `:=`(year=-5)]
HSE2006[, psu := paste0(psu, "2006")]
HSE2006[, cluster := paste0(cluster, "2006")]

load(file="./Datasets/Health Survey for England/2005/hse05ai.RData") # Only individuals aged 65 and over were asked for a blood sample
HSE2005original <- clear.labels(HSE2005original)
HSE2005original <- data.table(filter(HSE2005original, samptype==1), key="age") 
setnames(HSE2005original, c("imd2004", "area", "wt.bldel"), c("qimd", "psu", "wt.blood"))
HSE2005original[, cholval1 := cholval1 + 0.1]
agegroup.fn(HSE2005original)
HSE2005original[diabete2 == 2, diabtotr := 1]
HSE2005original[diabete2 == 1 | glyhbval > 6.5, diabtotr := 2]
#HSE6.srv.blood <- svydesign(id=~psu, strata=~cluster, weights=~wt.blood, nest=F, data=HSE2005original[wt.blood>0], check.strata = T)
#HSE6.srv.nurse <- svydesign(id=~psu, strata=~cluster, weights=~wt.nurse, nest=F, data=HSE2005original[wt.nurse>0], check.strata = T)
#HSE6.srv.int <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=HSE2005original[wt.int>0], check.strata = T)

HSE2005 =copy(HSE2005original[, list(wt.int, wt.nurse, wt.blood, psu, cluster, age, agegroup, sex, group, qimd, bmival, cholval1, omsysval, diabtotr, cigst1, startsmk, endsmoke, porftvg, frtpor, numsmok, smokyrs, cigdyal)])
HSE2005[, `:=`(year=-6)]
HSE2005[, psu := paste0(psu, "2005")]
HSE2005[, cluster := paste0(cluster, "2005")]

load(file="./Datasets/Health Survey for England/2004/hse04gpa.RData") # It seems to be a mess with the weighting for the core and boost sample
HSE2004original <- clear.labels(HSE2004original)
HSE2004original <- data.table(filter(HSE2004original, samptype==7), key="age") 
setnames(HSE2004original, c("imd2004", "area"), c("qimd", "psu"))
HSE2004original[, cholval1 := cholval1 + 0.1]
agegroup.fn(HSE2004original)
HSE2004original[diabete2 == 2, diabtotr := 1]
HSE2004original[diabete2 == 1 | glyhbval > 6.5, diabtotr := 2]
HSE2004original[, `:=` (wt.blood = wt.int, wt.nurse = wt.int)]
#HSE7.srv.blood <- svydesign(id=~psu, strata=~cluster, weights=~wt.blood, nest=F, data=HSE2004original[wt.blood>0], check.strata = T)
#HSE7.srv.nurse <- svydesign(id=~psu, strata=~cluster, weights=~wt.nurse, nest=F, data=HSE2004original[wt.nurse>0], check.strata = T)
#HSE7.srv.int <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=HSE2004original[wt.int>0], check.strata = T)

HSE2004 =copy(HSE2004original[, list(wt.int, wt.nurse, wt.blood, psu, cluster, age, agegroup, sex, group, qimd, bmival, cholval1, omsysval, diabtotr, cigst1, startsmk, endsmoke, porftvg, frtpor, numsmok, smokyrs, cigdyal)])
HSE2004[, `:=`(year=-7)]
HSE2004[, psu := paste0(psu, "2004")]
HSE2004[, cluster := paste0(cluster, "2004")]

load(file="./Datasets/Health Survey for England/2003/hse03ai.RData")
HSE2003original <- clear.labels(HSE2003original)
HSE2003original <- data.table(filter(HSE2003original, samptype==1), key="age") 
setnames(HSE2003original, c("imd2004", "area", "int.wt", "blood.wt", "nurse.wt"), c("qimd", "psu", "wt.int", "wt.blood", "wt.nurse"))
HSE2003original[, cholval1 := cholval1 + 0.1]
agegroup.fn(HSE2003original)
HSE2003original[diabete2 == 2, diabtotr := 1]
HSE2003original[diabete2 == 1 | glyhbval > 6.5, diabtotr := 2]
#HSE8.srv.blood <- svydesign(id=~psu, strata=~cluster, weights=~wt.blood, nest=F, data=HSE2003original[wt.blood>0], check.strata = T)
#HSE8.srv.nurse <- svydesign(id=~psu, strata=~cluster, weights=~wt.nurse, nest=F, data=HSE2003original[wt.nurse>0], check.strata = T)
#HSE8.srv.int <- svydesign(id=~psu, strata=~cluster, weights=~wt.int, nest=F, data=HSE2003original[wt.int>0], check.strata = T)

HSE2003 =copy(HSE2003original[, list(wt.int, wt.nurse, wt.blood, psu, cluster, age, agegroup, sex, group, qimd, bmival, cholval1, omsysval, diabtotr, cigst1, startsmk, endsmoke, porftvg, frtpor, numsmok, smokyrs, cigdyal)])
HSE2003[, `:=`(year=-8)]
HSE2003[, psu := paste0(psu, "2003")]
HSE2003[, cluster := paste0(cluster, "2003")]

load(file="./Datasets/Health Survey for England/2002/hse02ai.RData")
HSE2002original <- clear.labels(HSE2002original)
HSE2002original <- data.table(HSE2002original, key="age")
setnames(HSE2002original, c("nimd", "area", "sysval"), c("qimd", "psu", "omsysval"))
HSE2002original[, `:=`(cluster = 2002, wt.blood = tablewt, wt.nurse = tablewt, wt.int = tablewt)]
agegroup.fn(HSE2002original)
#HSE9.srv <- svydesign(id=~psu, strata=~cluster, weights = ~wt.int, nest=F, data=HSE2002original)

HSE2002 = copy(HSE2002original[, list(wt.int, wt.nurse, wt.blood, psu, cluster, age, agegroup, sex, group, qimd, bmival, omsysval, cigst1, startsmk, endsmoke, porftvg, frtpor, numsmok, smokyrs, cigdyal)])
HSE2002[, `:=`(year=-9, cholval1 =NA, diabtotr = NA)]
HSE2002[, psu := paste0(psu, "2002")]
HSE2002[, cluster := paste0(cluster, "2002")]

load(file="./Datasets/Health Survey for England/2001/hse01ai.RData")
HSE2001original <- clear.labels(HSE2001original)
HSE2001original <- data.table(HSE2001original, key="age") 
setnames(HSE2001original, c("nimd", "area", "sysval"), c("qimd", "psu", "omsysval"))
HSE2001original[, `:=`(cluster = 2001, wt.blood =1, wt.nurse = 1, wt.int = 1)]
agegroup.fn(HSE2001original)
#HSE10.srv <- svydesign(id=~psu, strata =~cluster, weights = ~wt.int, nest=F, data=HSE2001original, check.strata = T)

HSE2001 =copy(HSE2001original[, list(wt.int, wt.nurse, wt.blood, psu, cluster, age, agegroup, sex, group, qimd, bmival, omsysval, cigst1, startsmk, endsmoke, porftvg, frtpor, numsmok, smokyrs, cigdyal)])
HSE2001[, `:=`(year=-10, cholval1 =NA, diabtotr = NA)]
HSE2001[, psu := paste0(psu, "2001")]
HSE2001[, cluster := paste0(cluster, "2001")]

HSE.ts <-rbind(HSE2012, HSE2011, HSE2010, HSE2009, HSE2008, HSE2007, HSE2006, HSE2005, HSE2004, HSE2003, HSE2002, HSE2001)
HSE.ts[, sex := factor(sex)]
HSE.ts[, qimd := ordered(qimd)]
HSE.ts[, frtpor := cut(frtpor, breaks = c(0, 0.1:8.1, Inf), 
                                labels = c(0:9), include.lowest = T, 
                                right = F, 
                                ordered_result = T)]
HSE.ts[, frtpor := as.numeric(as.character(frtpor))]
#HSE.ts.srv.int <- svydesign(id=~psu, strata =~cluster, weights = ~wt.int, nest=F, data=HSE.ts[wt.int>0,], check.strata = T)
#HSE.ts.srv.nurse <- svydesign(id=~psu, strata =~cluster, weights = ~wt.nurse, nest=F, data=HSE.ts[wt.nurse>0,], check.strata = T)
#HSE.ts.srv.blood <- svydesign(id=~psu, strata =~cluster, weights = ~wt.blood, nest=F, data=HSE.ts[wt.blood>0,], check.strata = T)
#save(HSE.ts, file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
#load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
rm(list=ls(pattern="HSE20"))

# Build Models
# bmi model (needs to be first)
#load(file="./Lagtimes/HSE.ts.RData")
#load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[bmival<16 & age>19, bmival := 16]
HSE.ts[bmival>50 & age>19, bmival := 50]
HSE.ts[age>85, age:= 85]
HSE.ts.srv.nurse <- svydesign(id=~psu, strata =~cluster, weights = ~wt.nurse, nest = F, data=HSE.ts, check.strata = T)
HSE.ts.srv.nurse <- subset(HSE.ts.srv.nurse, age>19 & wt.nurse>0 & is.na(qimd)== F & is.na(bmival)== F)

pp <- svyby(~bmival, by=~age, design=HSE.ts.srv.nurse, svymean)
scatter.smooth(pp, ylim=c(0,30), family="gaussian", span = 1/3)
lines(y=predict(svyglm(bmival~age + I(age^2), family=gaussian(link="inverse"), design=HSE.ts.srv.nurse), data.frame(age=20:100), type="response"), x=20:100, col="green")
lines(y=predict(svyglm(bmival~age + I(age^2), family=gaussian(link="identity"), design=HSE.ts.srv.nurse), data.frame(age=20:100), type="response"), x=20:100, col="red")
lines(y=predict(svyglm(bmival~age + I(age^2), family=inverse.gaussian, design=subset(HSE.ts.srv.nurse, sex == 1)), data.frame(age=20:100), type="response"), x=20:100, col="blue")

scatter.smooth(svyby(~bmival, by=~year, design=HSE.ts.srv.nurse, svymean), ylim=c(25,30), xlim=c(-10, 5))
lines(y=predict(svyglm(bmival~year, design=HSE.ts.srv.nurse), data.frame(year=-10:50), type="response"), x=-10:50, col="red")
lines(y=predict(svyglm(bmival~I(exp(-year))+year, family=gaussian(link="identity"), design=HSE.ts.srv.nurse), data.frame(year=-10:50), type="response"), x=-10:50, col="green")
lines(y=predict(svyglm(bmival~year, family=inverse.gaussian, design=HSE.ts.srv.nurse), data.frame(year=-10:50), type="response"), x=-10:50, col="blue")
lines(y=predict(svyglm(bmival~year, family=gaussian(link="inverse"), design=HSE.ts.srv.nurse), data.frame(year=-10:50), type="response"), x=-10:50, col="purple")

bmi.svylm <- svyglm(bmival~(year + I(age^2) + age + sex + qimd + porftvg)^2- I(age^2) : age,
                    method = "glm.fit2",
                    family=gaussian,
                    design = HSE.ts.srv.nurse) 
anova(bmi.svylm)

bmi.svylm2 <- svyglm(bmival~year + I(age^2) + age + sex + qimd + porftvg +
                       I(age^2):sex  + I(age^2):porftvg  +
                       year:qimd + age:sex +  sex:qimd +
                       sex:porftvg + qimd:porftvg,
                    method = "glm.fit2",
                    family=gaussian,
                    design = HSE.ts.srv.nurse) 
anova(bmi.svylm2)
anova(bmi.svylm, bmi.svylm2)

bmi.svylm <- bmi.svylm2 # year:porftvg significant but removed as no meaningful
bmi.svylm$deviance/bmi.svylm$df.null
1-bmi.svylm$deviance/bmi.svylm$null.deviance # R^2
#save(bmi.svylm, file="./Lagtimes/bmi.svylm.rda")
#save(bmi.svylm, file="./Models/IMPACTncd/Lagtimes/bmi.svylm.rda")

# sbp model
#load(file="./Lagtimes/HSE.ts.RData")
#load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[bmival<16 & age>19, bmival := 16]
HSE.ts[bmival>50 & age>19, bmival := 50]
HSE.ts[age>85, age:= 85]
HSE.ts[, cigst1 := factor(cigst1)]
HSE.ts.srv.nurse <- svydesign(id=~psu, strata =~cluster, weights = ~wt.nurse, nest=F, data=HSE.ts, check.strata = T)
HSE.ts.srv.nurse <- subset(HSE.ts.srv.nurse, age>19 & wt.nurse>0 & is.na(omsysval)== F & is.na(qimd)== F
                           & is.na(bmival)== F & is.na(porftvg)== F & is.na(cigst1)== F)

scatter.smooth(svyby(~omsysval, by=~age, design=HSE.ts.srv.nurse, svymean), ylim=c(100,150))
lines(y=predict(svyglm(omsysval~age + I(age^2) + I(age^3), family=gaussian(link="inverse"), design=HSE.ts.srv.nurse), data.frame(age=20:100), type="response"), x=20:100, col="red")
lines(y=predict(svyglm(omsysval~age + I(age^2) + I(age^3), family=gaussian(link="identity"), design=HSE.ts.srv.nurse), data.frame(age=20:100), type="response"), x=20:100, col="green")
lines(y=predict(svyglm(omsysval~age + I(age^2) + I(age^3), family=inverse.gaussian, design=HSE.ts.srv.nurse), data.frame(age=20:100), type="response"), x=20:100, col="blue")

scatter.smooth(svyby(~omsysval, by=~round(bmival), design=HSE.ts.srv.nurse, svymean), ylim=c(100,150))
lines(y=predict(svyglm(omsysval~ bmival + I((bmival^-1)), family=gaussian(link="log"), design=HSE.ts.srv.nurse), data.frame(bmival=10:80), type="response"), x=10:80, col="green")
lines(y=predict(svyglm(omsysval~bmival + I(bmival^2), family=gaussian, design=HSE.ts.srv.nurse), data.frame(bmival=10:80), type="response"), x=10:80, col="red")     
lines(y=predict(svyglm(omsysval~bmival + I(bmival^2), family=inverse.gaussian, design=HSE.ts.srv.nurse), data.frame(bmival=10:80), type="response"), x=10:80, col="blue")

scatter.smooth(svyby(~omsysval, by=~year, design=HSE.ts.srv.nurse, svymean), ylim=c(120,140), xlim=c(-10, 1))

scatter.smooth(svyby(~omsysval, by=~year, design=HSE.ts.srv.nurse, svymean), ylim=c(90,140), xlim=c(-30, 100))
lines(y=predict(svyglm(omsysval~year, design=HSE.ts.srv.nurse), data.frame(year=-10:100), type="response"), x=-10:100, col="red")
lines(y=predict(svyglm(omsysval~I((year+50)^-1), family=gaussian(link="identity"), design=HSE.ts.srv.nurse), data.frame(year=-10:100), type="response"), x=-10:100, col="green") # play with+50 to adjust limit
lines(y=predict(svyglm(omsysval~I(exp(-year))+year, family=gaussian(link="identity"), design=HSE.ts.srv.nurse), data.frame(year=-30:100), type="response"), x=-30:100, col="blue")


sbp.svylm <- svyglm(omsysval~(I(exp(-year))+ year + sex + qimd +
                                age + I(age^2) + I(age^3) + 
                                bmival + I((bmival^-1)) + cigst1 + porftvg)^2 - 
                      bmival : I((bmival^-1)) - I(exp(-year)):year -
                      age : I(age^2) - age : I(age^3), 
                    family = gaussian(link="identity"),
                    method = "glm.fit2",
                    design = HSE.ts.srv.nurse)
anova(sbp.svylm)

sbp.svylm2 <- svyglm(omsysval~I(exp(-year))+ year + sex + qimd +
                                 age + I(age^2) + I(age^3) + 
                                 bmival + I((bmival^-1)) + cigst1 + porftvg + 
                                 I(exp(-year)):sex + age:cigst1 +
                                 I(exp(-year)):age + I(exp(-year)):I(age^2) + 
                                 I(exp(-year)):bmival + year:qimd + year:age +
                                 year:I(age^2) + sex:age + 
                                 qimd:age  + qimd:I(age^2) + age:bmival, 
                     family = gaussian(link="identity"),
                     method = "glm.fit2",
                     design = HSE.ts.srv.nurse)
anova(sbp.svylm2)
anova(sbp.svylm, sbp.svylm2)
sbp.svylm <- sbp.svylm2
sbp.svylm$deviance/sbp.svylm$df.null
1-sbp.svylm$deviance/sbp.svylm$null.deviance # R^2
# save(sbp.svylm, file="./Models/IMPACTncd/Lagtimes/sbp.svylm.rda")
# save(sbp.svylm, file="./Lagtimes/sbp.svylm.rda")

# chol model
# load(file="./Lagtimes/HSE.ts.RData")
# load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[bmival<16 & age>19, bmival := 16]
HSE.ts[bmival>50 & age>19, bmival := 50]
HSE.ts[age>85, age:= 85]
HSE.ts.srv.blood <- svydesign(id=~psu, strata =~cluster, weights = ~wt.blood, nest=F, data=HSE.ts, check.strata = T)
HSE.ts.srv.blood <- subset(HSE.ts.srv.blood, age>19 & wt.blood>0 & is.na(cholval1)== F & is.na(qimd)== F & is.na(bmival)== F & year!=-7 & year!=-6) # year -7 excluded due to heavy influence on regression. Year -6 blood sample only above 65

scatter.smooth(svyby(~cholval1, by=~age, design=HSE.ts.srv.blood, svymean), ylim=c(3,7))
lines(y=predict(svyglm(cholval1~age + I(age^2)*I(age^3), family=gaussian, design=HSE.ts.srv.blood), data.frame(age=20:100), type="response"), x=20:100, col="red")
lines(y=predict(svyglm(cholval1~age + I(age^2), family=gaussian(link="inverse"), design=HSE.ts.srv.blood), data.frame(age=20:100), type="response"), x=20:100, col="green")
lines(y=predict(svyglm(cholval1~age + I(age^2), family=inverse.gaussian, design=HSE.ts.srv.blood), data.frame(age=20:100), type="response"), x=20:100, col="purple")
lines(y=predict(svyglm(cholval1~age + I(age^2) + I(age^3), family=inverse.gaussian, design=HSE.ts.srv.blood), data.frame(age=20:100), type="response"), x=20:100, col="blue")

scatter.smooth(svyby(~cholval1, by=~round(bmival), design=HSE.ts.srv.blood, svymean), ylim=c(3,7))
lines(y=predict(svyglm(cholval1~bmival + I(bmival^2) * I(bmival^3), family=gaussian, design=HSE.ts.srv.blood), data.frame(bmival=10:80), type="response"), x=10:80, col="red")
lines(y=predict(svyglm(cholval1~bmival + I(bmival^2) + I(bmival^3), family=gaussian(link="inverse"), design=HSE.ts.srv.blood), data.frame(bmival=10:80), type="response"), x=10:80, col="green")
lines(y=predict(svyglm(cholval1~bmival + I(bmival^2) + I(bmival^3), family=inverse.gaussian, design=HSE.ts.srv.blood), data.frame(bmival=10:80), type="response"), x=10:80, col="blue")

scatter.smooth(svyby(~cholval1, by=~year, design=subset(HSE.ts.srv.blood, sex == 1), svymean), family="gaussian", ylim=c(3,7), xlim=c(-10, 1))

scatter.smooth(svyby(~cholval1, by=~year, design=HSE.ts.srv.blood, svymean), family="gaussian",ylim=c(3,7), xlim=c(-10, 50))

lines(y=predict(svyglm(cholval1~year, design=HSE.ts.srv.blood), data.frame(year=-10:100), type="response"), x=-10:100, col="red")
lines(y=predict(svyglm(cholval1~year, family=gaussian(link="inverse"), design=HSE.ts.srv.blood), data.frame(year=-10:100), type="response"), x=-10:100, col="green")
lines(y=predict(svyglm(cholval1~exp(-year)+year, design=HSE.ts.srv.blood), data.frame(year=-10:100), type="response"), x=-10:100, col="blue")

chol.svylm <- svyglm(cholval1~(exp(-year) + year + sex + qimd +
                               age + I(age^2) + I(age^3) + 
                               bmival + I(bmival^2) + I(bmival^3) + porftvg)^2 - 
                               bmival : I(bmival^2) - bmival : I(bmival^3) - 
                               age : I(age^2) -  age : I(age^3), 
                     family = gaussian(link="identity"),
                     method = "glm.fit2",
                     design = HSE.ts.srv.blood)
anova(chol.svylm)

chol.svylm2 <- svyglm(cholval1~exp(-year) + year + sex + qimd +
                                 age + I(age^2) +  
                                 bmival + I(bmival^2) + I(bmival^3) + porftvg + 
                        exp(-year):age + exp(-year):I(age^2) +
                        exp(-year):bmival + year:age + 
                        sex:age + sex:I(age^2) + sex:I(age^3) +
                        sex:bmival + sex:I(bmival^2) +
                        qimd:age + year:qimd +
                        age:bmival + 
                        age:porftvg +   
                        I(age^2):I(age^3) +
                        I(age^2):bmival + 
                        I(age^3):bmival + I(age^3):I(bmival^3), 
                     family = gaussian(link="identity"),
                     method = "glm.fit2",
                     design = HSE.ts.srv.blood)
anova(chol.svylm2)
anova(chol.svylm, chol.svylm2)
chol.svylm <- chol.svylm2

chol.svylm$deviance/chol.svylm$df.null
1-chol.svylm$deviance/chol.svylm$null.deviance
#save(chol.svylm, file="./Models/IMPACTncd/Lagtimes/chol.svylm.rda")
#save(chol.svylm, file="./Lagtimes/chol.svylm.rda")

# diab model
# load(file="./Lagtimes/HSE.ts.RData")
# load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[diabtotr == 1, diabtotr :=0]
HSE.ts[diabtotr == 2, diabtotr :=1]
HSE.ts[bmival<16 & age>19, bmival := 16]
HSE.ts[bmival>40 & age>19, bmival := 40]
HSE.ts[age>85, age:= 85]
HSE.ts.srv.blood <- svydesign(id=~psu, strata =~cluster, weights = ~wt.blood, nest=F, data=HSE.ts, check.strata = T)
HSE.ts.srv.blood <- subset(HSE.ts.srv.blood, age>19 & wt.blood>0 & is.na(diabtotr)== F & is.na(qimd)== F & year!=-6 & year!=-7) # years -6 and -7 appear problematic. see scatter.smooth

scatter.smooth(svyby(~diabtotr, by=~age, design=HSE.ts.srv.blood, svymean), ylim=c(0,0.25))
lines(y=predict(svyglm(diabtotr~age + I(age^2), family=gaussian(link="identity"), design=HSE.ts.srv.blood), data.frame(age=20:100), type="response"), x=20:100, col="red")
lines(y=predict(svyglm(diabtotr~age * I(age^2), family=quasibinomial(link="logit"), design=HSE.ts.srv.blood), data.frame(age=20:100), type="response"), x=20:100, col="blue")
lines(y=predict(svyglm(diabtotr~age + I(age^2) + I(age^3), family=quasibinomial(link="identity"), design=HSE.ts.srv.blood), data.frame(age=20:100), type="response"), x=20:100, col="green")

scatter.smooth(svyby(~diabtotr, by=~round(bmival), design=HSE.ts.srv.blood, svymean), ylim=c(0,0.25))
lines(y=predict(svyglm(diabtotr~bmival * I(bmival^2), family=gaussian(link="identity"), design=HSE.ts.srv.blood), data.frame(bmival=15:60), type="response"), x=15:60, col="red")
lines(y=predict(svyglm(diabtotr~bmival + I(bmival^2), family=quasibinomial(link="logit"), design=HSE.ts.srv.blood), data.frame(bmival=15:60), type="response"), x=15:60, col="blue")
lines(y=predict(svyglm(diabtotr~bmival , family=quasibinomial(link="probit"), design=HSE.ts.srv.blood), data.frame(bmival=15:60), type="response"), x=15:60, col="green")

scatter.smooth(svyby(~diabtotr, by=~year, design=HSE.ts.srv.blood, svymean), ylim=c(0,0.25), xlim=c(-9, 1))

scatter.smooth(svyby(~diabtotr, by=~year, design=HSE.ts.srv.blood, svymean), ylim=c(0,0.75), xlim=c(-11, 60))
lines(y=predict(svyglm(diabtotr~year, family=gaussian(link="identity"), design=HSE.ts.srv.blood), data.frame(year=-11:60), type="response"), x=-11:60, col="red")
lines(y=predict(svyglm(diabtotr~exp(-year-102), family=quasibinomial(link="logit"), design=HSE.ts.srv.blood), data.frame(year=-11:60), type="response"), x=-11:60, col="blue")
lines(y=predict(svyglm(diabtotr~year, family=quasibinomial(link="probit"), design=HSE.ts.srv.blood), data.frame(year=-11:60), type="response"), x=-11:60, col="green")

diab.svylr <- svyglm(diabtotr~(year + age + I(age^2) + sex + qimd + bmival)^2,
                     design = HSE.ts.srv.blood, 
                     family=gaussian(link="identity"),
                     method = "glm.fit")
anova(diab.svylr)

diab.svylr2 <- svyglm(diabtotr~(year + age + I(age^2) + sex + qimd + bmival)^2 -
                        I(age^2):qimd - sex:qimd,
                      design = HSE.ts.srv.blood, 
                      family=gaussian(link="identity"),
                      method = "glm.fit")
anova(diab.svylr2)
anova(diab.svylr,diab.svylr2)

diab.svylr <- diab.svylr2
diab.svylr$deviance/diab.svylr$df.null
1-diab.svylr$deviance/diab.svylr$null.deviance
#save(diab.svylr, file="./Models/IMPACTncd/Lagtimes/diab.svylr.rda")
#save(diab.svylr, file="./Lagtimes/diab.svylr.rda")

# smoking start model
#load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
#load(file="./Lagtimes/HSE.ts.RData")
HSE.ts[,wt.int := wt.int*10000/sum(wt.int), by=year] # make pop of each hse =10000
HSE.ts[,sum(wt.int), by=year]
HSE.ts[cigst1==4 & between(age-startsmk,0,2), `:=`(year = year - (age-startsmk))]
HSE.ts[cigst1==4 & between(age-startsmk,0,2), `:=`(age = startsmk, smok.incid = 1)]
HSE.ts[between(cigst1, 2, 3) & between(endsmoke + smokyrs,0,2), `:=`(age = age - endsmoke - smokyrs, year = year - endsmoke - smokyrs, smok.incid = 1)]
HSE.ts[cigst1==1, smok.incid:= 0]
HSE.ts[age>85, age:= 85]
HSE.ts[qimd %in% c("3", "4", "5"), qimd:="2"]
HSE.ts[, qimd:= ordered(qimd, levels=c(1,2))]
HSE.ts[, smok.incid:= factor(smok.incid)]
HSE.ts.srv.int <- svydesign(id=~psu, strata =~cluster, weights = ~wt.int, nest=F, 
                            data=HSE.ts, check.strata = T)
HSE.ts.srv.int <- subset(HSE.ts.srv.int, age>15 & wt.int>0 & is.na(qimd)== F &
                        is.na(smok.incid) == F & year>-7 & year<0 & age<60)
#HSE.ts.srv.int <- subset(HSE.ts.srv.int,qimd=="5")
# year -7 was removed as highly influential 'outlier'
# this gives the probability of a never smoker to become a smoker
# I had to do it this way because of the way the question was asked in HSE
pp <- svyby(~smok.incid, by=~age, design=HSE.ts.srv.int, svymean)

scatter.smooth(y=pp[[3]], x=as.numeric(pp[[1]]), ylim=c(0, 0.2), family = "gaussian")

lines(y=predict(svyglm(smok.incid~age , family=quasibinomial(link="probit"), design=HSE.ts.srv.int), data.frame(age=18:100), type="response"), x=18:100, col="red")
lines(y=predict(svyglm(smok.incid~age+ I(age^4)+ I(age^3) + + I(age^2) , family=quasibinomial(link="logit"), design=HSE.ts.srv.int), data.frame(age=16:100), type="response"), x=16:100, col="blue")

pp <- svyby(~smok.incid, by=~qimd, design=HSE.ts.srv.int, svymean)
scatter.smooth(pp[3], ylim=c(0, 0.1), family = "gaussian")
lines(y=predict(svyglm(smok.incid~qimd , family=quasibinomial(link="probit"), design=HSE.ts.srv.int), data.frame(qimd=ordered(1:2)), type="response"), x= 1:2, col="red")
lines(y=predict(svyglm(smok.incid~qimd , family=quasibinomial(link="logit"), design=HSE.ts.srv.int), data.frame(qimd=ordered(1:2)), type="response"), x= 1:2, col="blue")

pp <- svyby(~smok.incid, by=~year, design=HSE.ts.srv.int, svymean)
scatter.smooth(pp[[1]], pp[[3]], ylim=c(0, 0.1), family = "gaussian", xlim=c(-10, 50))
lines(y=predict(svyglm(smok.incid~year , family=quasibinomial(link="logit"), design=HSE.ts.srv.int), data.frame(year=-10:50), type="response"), x=-10:50, col="red")
lines(y=predict(svyglm(smok.incid~year , family=quasibinomial(link="probit"), design=subset(HSE.ts.srv.int, qimd==1)), data.frame(year=-10:50), type="response"), x=-10:50, col="blue")
lines(y=predict(svyglm(smok.incid~year , family=quasibinomial(link="probit"), design=subset(HSE.ts.srv.int, qimd==2)), data.frame(year=-10:50), type="response"), x=-10:50, col="green")
lines(y=predict(svyglm(smok.incid~year , family=quasibinomial(link="probit"), design=subset(HSE.ts.srv.int, qimd==3)), data.frame(year=-10:50), type="response"), x=-10:50, col="yellow")
lines(y=predict(svyglm(smok.incid~year , family=quasibinomial(link="probit"), design=subset(HSE.ts.srv.int, qimd==4)), data.frame(year=-10:50), type="response"), x=-10:50, col="gray")
lines(y=predict(svyglm(smok.incid~year , family=quasibinomial(link="probit"), design=subset(HSE.ts.srv.int, qimd==5)), data.frame(year=-10:50), type="response"), x=-10:50, col="pink")
smok.start.svylr <- svyglm(smok.incid~ age+ (I(age^2) + I(age^3) + I(age^4))^2 + qimd, design = HSE.ts.srv.int, 
                           family=quasibinomial(link="logit"), 
                           method = "glm.fit2")
anova(smok.start.svylr)

smok.start.svylr2 <- svyglm(smok.incid~ year + age+ (I(age^2) + I(age^3) + I(age^4))^2 + qimd, design = HSE.ts.srv.int, # year not signifficant but included for scenarios
                           family=quasibinomial(link="logit"), 
                           method = "glm.fit2")
anova(smok.start.svylr2)


anova(smok.start.svylr, smok.start.svylr2)

smok.start.svylr <- smok.start.svylr2
smok.start.svylr$deviance/smok.start.svylr$df.null
1-smok.start.svylr$deviance/smok.start.svylr$null.deviance
#save(smok.start.svylr, file="./Models/IMPACTncd/Lagtimes/smok.start.svylr.rda")
#save(smok.start.svylr, file="./Lagtimes/smok.start.svylr.rda")

# load(file="./Lagtimes/HSE.ts.RData")
# load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[endsmoke<1, smok.cess:=1]
HSE.ts[cigst1==4, smok.cess:=0]
HSE.ts[age>85, age := 85]
HSE.ts.srv.int <- svydesign(id=~psu, strata =~cluster, weights = ~wt.int, nest=F, data=HSE.ts, check.strata = T)
HSE.ts.srv.int <- subset(HSE.ts.srv.int, age>15 & wt.int>0 & is.na(qimd)==F & is.na(smok.cess)==F & year!=-7)

pp<- svyby(~smok.cess, by=~year, design=HSE.ts.srv.int, svymean, na.rm=T)
scatter.smooth(pp, ylim=c(0, 0.4), xlim=c(-10,50), family = "gaussian")
lines(y=predict(svyglm(smok.cess~year , family=quasibinomial(link="logit"), design=HSE.ts.srv.int), 
                data.frame(year=-10:50), type="response"), x=-10:50, col="red")
lines(y=predict(svyglm(smok.cess~year+I(year^2) , family=quasibinomial(link="logit"), design=HSE.ts.srv.int), 
                data.frame(year=-10:50), type="response"), x=-10:50, col="blue")

aa<- svyby(~smok.cess, by=~age, design=HSE.ts.srv.int, svymean, na.rm=T)
scatter.smooth(aa, ylim=c(0, 0.2), xlim=c(10,100), family = "gaussian")
lines(y=predict(svyglm(smok.cess~age , family=quasibinomial(link="logit"), design=HSE.ts.srv.int), 
                data.frame(age=10:100), type="response"), x=10:100, col="red")
lines(y=predict(svyglm(smok.cess~age + I(age^2), family=quasibinomial(link="logit"), design=HSE.ts.srv.int), 
                data.frame(age=10:100), type="response"), x=10:100, col="blue")
lines(y=predict(svyglm(smok.cess~age + (I(age^2) + I(age^3) + I(age^4))^2, family=quasibinomial(link="logit"), design=HSE.ts.srv.int), 
                data.frame(age=10:84), type="response"), x=10:84, col="grean")

smok.cess.svylr <- svyglm(factor(smok.cess)~ (year + age + I(age^2) + I(age^3) + I(age^4) + sex + qimd)^2 - 
                            age:I(age^2) - age:I(age^3)- age:I(age^4),
                          design = HSE.ts.srv.int, 
                          family=quasibinomial(link="logit"), 
                          method = "glm.fit2")
anova(smok.cess.svylr)

smok.cess.svylr2 <- svyglm(smok.cess ~ year + age  + sex + qimd + I(age^2) + I(age^3) + I(age^4) + 
                             year:qimd + age:sex +I(age^2):I(age^3)+ I(age^2):sex,
                           design = HSE.ts.srv.int, 
                           family=quasibinomial(link="logit"), 
                           method = "glm.fit2")
anova(smok.cess.svylr2)
anova(smok.cess.svylr, smok.cess.svylr2)
smok.cess.svylr <- smok.cess.svylr2
smok.cess.svylr$deviance/smok.cess.svylr$df.null
#save(smok.cess.svylr, file="./Models/IMPACTncd/Lagtimes/smok.cess.svylr.rda")
#save(smok.cess.svylr, file="./Lagtimes/smok.cess.svylr.rda")

# load(file="./Lagtimes/HSE.ts.RData")
# load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[,sum(wt.int), by=year]
HSE.ts[,wt.int := wt.int*10000/sum(wt.int), by=year] # make pop of each hse =10000
HSE.ts[,sum(wt.int), by=year]
HSE.ts[endsmoke>10, endsmoke := 11]
HSE.ts.srv.int <- svydesign(id=~psu, strata =~cluster, weights = ~wt.int, nest=F, data=HSE.ts, check.strata = T)
HSE.ts.srv.int <- subset(HSE.ts.srv.int, age>15 & wt.int>0 & is.na(qimd)==F & endsmoke>=0 & year>-7)
#xx<- as.data.table(prop.table(svytable(~endsmoke+year+sex+qimd, HSE.ts.srv.int),2)) 
# Above was tested and year is not significant in the regression (and also too complicated)

xx<- as.data.table(svytable(~endsmoke+sex+qimd, HSE.ts.srv.int))
xx[, endsmoke:=as.numeric(endsmoke)]
xx[, sex := as.factor(sex)]
xx[, qimd := as.ordered(qimd)]
xx = copy(xx[endsmoke<7])
xx[, failure := .SD[endsmoke==0, N] - N, by=.(sex,qimd)]
xx[failure<0, failure := NA]
xx=copy(xx[endsmoke %!in% c(2,5)])
xx[,pct:=N/(N+failure)]
smok.cess.success <- glm(cbind(N, failure)~ I(1/(endsmoke+1)^2) + sex + qimd, xx, family="quasibinomial"(link="logit"))
predict(smok.cess.success, data.frame(endsmoke = 1:9, sex = factor(1, levels = c(1:2)), qimd = ordered(1, levels = c(1:5))), type="response", se.fit=F)
xx[sex==1 & qimd==1, scatter.smooth(endsmoke,pct, ylim=c(0,1))]
lines(x=c(0:9), predict(smok.cess.success, data.frame(endsmoke = 0:9, sex = factor(1, levels = c(1:2)), qimd = ordered(1, levels = c(1:5))), type="response"), col="red")
#save(smok.cess.success, file="./Models/IMPACTncd/Lagtimes/smok.cess.success.rda")
#save(smok.cess.success, file="./Lagtimes/smok.cess.success.rda")

# load(file="./Lagtimes/HSE.ts.RData")
# load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[cigst1==4, smok.active:=1]
HSE.ts[cigst1<4, smok.active:=0]
HSE.ts[age>85, age := 85]
HSE.ts.srv.int <- svydesign(id=~psu, strata =~cluster, weights = ~wt.int, nest=F, data=HSE.ts, check.strata = T)
HSE.ts.srv.int <- subset(HSE.ts.srv.int, age>15 & wt.int>0 & is.na(qimd)==F & is.na(smok.active)==F)

pp<- svyby(~smok.active, by=~year, design=HSE.ts.srv.int, svymean, na.rm=T)
scatter.smooth(pp, ylim=c(0, 0.3), xlim=c(-10,50), family = "gaussian")
lines(y=predict(svyglm(smok.active~year , family=quasibinomial(link="logit"), design=HSE.ts.srv.int), 
                data.frame(year=-10:50), type="response"), x=-10:50, col="blue")
lines(y=predict(svyglm(smok.active~year+I(year^2) , family=quasibinomial(link="logit"), design=HSE.ts.srv.int), 
                data.frame(year=-10:50), type="response"), x=-10:50, col="blue")

aa<- svyby(~smok.active, by=~age, design=HSE.ts.srv.int, svymean, na.rm=T)
scatter.smooth(aa, ylim=c(0, 0.8), xlim=c(10,100), family = "gaussian")
lines(y=predict(svyglm(smok.active~age , family=quasibinomial(link="logit"), design=HSE.ts.srv.int), 
                data.frame(age=10:100), type="response"), x=10:100, col="red")
lines(y=predict(svyglm(smok.active~age + I(age^2), family=quasibinomial(link="logit"), design=HSE.ts.srv.int), 
                data.frame(age=10:100), type="response"), x=10:100, col="blue")
lines(y=predict(svyglm(smok.active~age + I(age^2)+ I(age^3), family=quasibinomial(link="logit"), design=HSE.ts.srv.int), 
                data.frame(age=10:100), type="response"), x=10:100, col="green")

smok.active.svylr <- svyglm(smok.active~ year + age + I(age^2) + sex + qimd + age:sex + I(age^2):sex + I(age^2):qimd, design = HSE.ts.srv.int, 
                            family=quasibinomial(link="logit"), 
                            method = "glm.fit2")
anova(smok.active.svylr)

smok.active.svylr2 <- svyglm(smok.active~ (year + age + I(age^2) + sex + qimd)^2 - 
                                 year:I(age^2) - sex:qimd - age:I(age^2),
                             design = HSE.ts.srv.int, 
                             family=quasibinomial(link="logit"), 
                             method = "glm.fit2")
anova(smok.active.svylr2)
anova(smok.active.svylr, smok.active.svylr2)
smok.active.svylr <- smok.active.svylr2
smok.active.svylr$deviance/smok.active.svylr$df.null
#save(smok.active.svylr, file="./Models/IMPACTncd/Lagtimes/smok.active.svylr.rda")
#save(smok.active.svylr, file="./Lagtimes/smok.active.svylr.rda")


# F&V
# load(file="./Lagtimes/HSE.ts.RData")
# load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[bmival<16 & age>19, bmival := 16]
HSE.ts[bmival>50 & age>19, bmival := 50]
HSE.ts[age>85, age := 85]
HSE.ts.srv.int <- svydesign(id=~psu, strata =~cluster, weights = ~wt.int, nest=F, data=HSE.ts, check.strata = T)
HSE.ts.srv.int <- subset(HSE.ts.srv.int, age>19 & wt.int>0 & year < 1 & is.na(porftvg)== F & is.na(qimd)==F)   

pp<- svyby(~porftvg, by=~age, design=HSE.ts.srv.int, svymean)
scatter.smooth(pp, family = "gaussian", ylim=c(0,6))
lines(y=predict(svyglm(porftvg~age + I(age^2), family=quasipoisson(), design=HSE.ts.srv.int), data.frame(age=20:100), type="response"), x=20:100, col="red")

pp<- svyby(~porftvg, by=~year, design=HSE.ts.srv.int, svymean)
scatter.smooth(pp, family = "gaussian", ylim=c(3,6), xlim=c(-10,50))
lines(y=predict(svyglm(porftvg~year, family=quasipoisson(), design=HSE.ts.srv.int), data.frame(year=-10:50), type="response"), x=-10:50, col="red")

pp<- svyby(~porftvg, by=~round(bmival), design=subset(HSE.ts.srv.int, sex == 1), svymean)
scatter.smooth(pp, family = "gaussian", ylim=c(0,6))
lines(y=predict(svyglm(porftvg~bmival + I(bmival^2) + I(bmival^3), family=quasipoisson(), design=subset(HSE.ts.srv.int, sex == 1)), data.frame(bmival=10:60), type="response"), x=10:60, col="red")
lines(y=predict(svyglm(porftvg~bmival + I(bmival^2) , family=quasipoisson(), design=subset(HSE.ts.srv.int, sex == 1)), data.frame(bmival=10:60), type="response"), x=10:60, col="blue")

fv.svylr <- svyglm(porftvg~(year + age + I(age^2) + sex + qimd)^2, 
                   design = HSE.ts.srv.int, 
                   family=quasipoisson(), 
                   method = "glm.fit2")
anova(fv.svylr)

fv.svylr2 <- svyglm(porftvg~year + age + I(age^2) + sex + qimd +
                     year:qimd + age:I(age^2) + age:qimd  +
                     I(age^2):sex, 
                   design = HSE.ts.srv.int, 
                   family=quasipoisson(), 
                   method = "glm.fit2")
anova(fv.svylr2)
anova(fv.svylr, fv.svylr2)
fv.svylr <- fv.svylr2

1-fv.svylr$deviance/fv.svylr$null.deviance
#save(fv.svylr, file="./Models/IMPACTncd/Lagtimes/fv.svylr.rda")
#save(fv.svylr, file="./Lagtimes/fv.svylr.rda")


# FV rate
# load(file="./Lagtimes/HSE.ts.RData")
# load(file="./Models/IMPACTncd/Lagtimes/HSE.ts.RData")
HSE.ts[bmival<16 & age>19, bmival := 16]
HSE.ts[bmival<16 & age>19, bmival := 16]
HSE.ts[bmival>50 & age>19, bmival := 50]
HSE.ts[age>85, age := 85]
HSE.ts[frtpor == 0, frtporCat := 0L ] # code frtpor as porftvg
HSE.ts[frtpor > 0,  frtporCat := as.integer(frtpor) + 1L]
HSE.ts[frtporCat > 9,  frtporCat := 9L]
HSE.ts[frtporCat > porftvg, frtporCat:=porftvg] # For some fruit consumption is larger than total FV consumption. FIX
HSE.ts[, fvrate := frtporCat/porftvg]
HSE.ts.srv.int <- svydesign(id=~psu, strata =~cluster, weights = ~wt.int, nest=F, data=HSE.ts, check.strata = T)
HSE.ts.srv.int <- subset(HSE.ts.srv.int, age>19 & wt.int>0 & year < 1 & is.na(porftvg)== F & is.na(frtpor)== "F") 

pp<- svyby(~porftvg, by=~frtporCat, design=subset(HSE.ts.srv.int, sex == 2), svymean)
scatter.smooth(pp, family = "gaussian")
lines(y=predict(svyglm(porftvg~age + I(age^2), family=quasipoisson(), design=subset(HSE.ts.srv.int, sex == 1)), data.frame(age=20:100), type="response"), x=20:100, col="red")

fvrate.svylr <- svyglm(fvrate~(porftvg+year+age+sex+qimd)^2, family=quasibinomial(),  method = "glm.fit2", design=HSE.ts.srv.int)
#save(fvrate.svylr, file="./Models/IMPACTncd/Lagtimes/fvrate.svylr.rda")
#save(fvrate.svylr, file="./Lagtimes/fvrate.svylr.rda")



