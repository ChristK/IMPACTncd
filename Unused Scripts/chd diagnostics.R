#****************** TESTING FOR CHD MODEL ***************************#

# Preparation
year <- 2011

ageL <- 30  # Define lower age limit to diseases-model simulation (min = 30)

ageH <- 79  # Define lower age limit to diseases-model simulation (max = 84)

cvd.lag <- 5 # Avoid 0

cancer.lag <- 10


options(warn = 1)
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
            stop("Currently, 'get.dropbox.folder' works for Windows and Linux only. Sorry.")
        db.file <- paste(Sys.getenv("APPDATA"), "\\Dropbox\\host.db", sep = "")
        base64coded <- readLines(db.file, warn = F)[2]
        base64(base64coded, encode = F)
    }
    setwd(paste(get.dropbox.folder(), "/PhD", sep = ""))
    rm(get.dropbox.folder)
}

# preample
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
               "randtoolbox", 
               "truncnorm", 
               "ggplot2", 
               "stringr", 
               "reshape2", 
               "compiler",
               "survey"))

dice <- function(n = .N) {
    rand <- SFMT(n, dim = 1, mexp = 19937, usepset = T, withtorus = F, usetime = T)
    return(rand)
}



CHDincid <- fread("./CVD Statistics/CHDincid.csv", 
                  sep = ",",
                  header = T, 
                  stringsAsFactors = T)
CHDincid[, `:=` (sex = factor(sex), agegroup = ordered(agegroup))]
setkey(CHDincid, sex, agegroup)

CHDpreval <- fread("./CVD Statistics/CHDpreval.csv", 
                   sep = ",",
                   header = T, 
                   stringsAsFactors = T)
CHDpreval[, `:=` (sex = factor(sex), agegroup = ordered(agegroup))]
setkey(CHDpreval, sex, agegroup)
#CHDpreval[agegroup %in% c("<1   ", "01-04", "05-09", "10-14", "15-19"), prevalence :=0] # Set prevalence of younger than 20 to 0

CHDsurv <- fread("./CVD Statistics/CHDsurv.csv", 
                 sep = ",", 
                 header = T, 
                 stringsAsFactors = T)  
CHDsurv[, `:=`(sex = as.factor(sex), agegroup = as.ordered(agegroup))]
setkey(CHDsurv, sex, agegroup)

all.files <- list.files(path = "./CVD Statistics", pattern = "chd", full.names = T) # Create a list of files containing chd in their filenames
readdata <- function(fn) {
    sex <- agegroup <- NULL
    nam <- gsub(".csv", "", fn)
    nam <- gsub("./CVD Statistics/", "", nam)
    assign(nam, fread(fn, sep=",", header = T, stringsAsFactors = T))
    dt <- get(nam)
    dt[, `:=` (sex = factor(sex), agegroup = ordered(agegroup))]
    keycols <- c("sex", "agegroup")
    setkeyv(dt,keycols)
    return(dt)
}

mylist <- sapply(all.files, readdata, simplify=F, USE.NAMES=T)
names(mylist) <- gsub(".csv", "", names(mylist))
names(mylist) <- gsub("./CVD Statistics/", "", names(mylist))

list2env(mylist ,.GlobalEnv) # copy each object of the list to the global environment
rm(all.files, readdata, mylist) # garbage cleaning

load(file = "./POPtest.RData")
#save(POP, file = "./POPtest.RData")

# ACTUAL TESTING

#CHDincid[chdbmipaf[chdcholpaf[chddiabpaf[chdetspaf[chdfvpaf[chdsbppaf[chdsmokepaf]]]]]], 
#         p0 := incidence * (1 - bmipaf) * (1 - cholpaf) * (1 - diabpaf) * (1 - etspaf) * (1 - fvpaf) * (1 - sbppaf) * (1 - tobaccopaf)]

POP <- merge(POP, CHDincid, by = c("agegroup", "sex"), all.x = T)
for (ll in 1:100) {
    POP[between(age, ageL, ageH) & chd.incidence == 0, v := dice(.N) <= incidence]
   # POP[v==T, hist(age, ylim = c(0,300))]
    nam <- paste0("temp", ll)
    assign(nam, POP[v==T, .N, by = c("agegroup", "sex")])
    setkey(get(nam), agegroup, sex)
}

POP[, ':='(p0 = NULL, incidence = NULL)]

x <- lapply(as.list(paste0("temp", 1:100)),get)
x <- data.table(rbind_all(x))
ideal =copy( x[, mean(N), by= c("agegroup", "sex")])
setkey(ideal, sex, agegroup)
ideal[, plot(V1, ylim = c(0,250), main = "observed incidence")]

#c("bmipaf","cholpaf","diabpaf","etspaf","fvpaf","sbppaf","tobaccopaf")

# BMI
for (m in "bmipaf") {
    CHDincid[chdbmipaf[chdcholpaf[chddiabpaf[chdetspaf[chdfvpaf[chdsbppaf[chdsmokepaf]]]]]], 
             p0 := incidence * (1 - get(m))]
    POP[, ':='(p0 = NULL, incidence = NULL)]
    POP <- merge(POP, CHDincid, by = c("agegroup", "sex"), all.x = T)
}    
   
corr.factor.bmi <- merge(POP[, mean(p0 * chd.bmi.rr), by = c("agegroup", "sex")], CHDincid, by = c("agegroup", "sex"), all.x = T)
corr.factor.bmi [,b.bmi := incidence/V1]
corr.factor.bmi [, `:=` (p0=NULL, incidence=NULL, V1=NULL)]
POP <- merge(POP, corr.factor.bmi, by = c("agegroup", "sex"), all.x = T)


for (ll in 1:100) {
    POP[between(age, ageL, ageH) & chd.incidence == 0, v := dice(.N) <= p0 * chd.bmi.rr * b.bmi]
    nam <- paste0("temp", ll)
    assign(nam, POP[v==T, .N, by = c("agegroup", "sex")])
    setkey(get(nam), agegroup, sex)
}

x <- lapply(as.list(paste0("temp", 1:100)),get)
x <- data.table(rbind_all(x))
bmi =copy( x[, mean(N), by= c("agegroup", "sex")])
setkey(bmi, sex, agegroup)
ideal[, plot(V1, ylim = c(0,250), main = "bmi alignment")]
bmi[, points(V1, col="red")]

for (ll in 1:100) {
    POP[between(age, ageL, ageH) & chd.incidence == 0, v := dice(.N) <= p0 * chd.bmi.rr]
    nam <- paste0("temp", ll)
    assign(nam, POP[v==T, .N, by = c("agegroup", "sex")])
    setkey(get(nam), agegroup, sex)
}

x <- lapply(as.list(paste0("temp", 1:100)),get)
x <- data.table(rbind_all(x))
bmi =copy( x[, mean(N), by= c("agegroup", "sex")])
setkey(bmi, sex, agegroup)
ideal[, plot(V1, ylim = c(0,250), main = "bmi no alignment")]
bmi[, points(V1, col="red")]

# CHOL
for (m in "cholpaf") {
    CHDincid[chdbmipaf[chdcholpaf[chddiabpaf[chdetspaf[chdfvpaf[chdsbppaf[chdsmokepaf]]]]]], 
             p0 := incidence * (1 - get(m))]
    POP[, ':='(p0 = NULL, incidence = NULL)]
    POP <- merge(POP, CHDincid, by = c("agegroup", "sex"), all.x = T)
}    

corr.factor.chol <- merge(POP[, mean(p0 * chd.chol.rr), by = c("agegroup", "sex")], CHDincid, by = c("agegroup", "sex"), all.x = T)
corr.factor.chol [,b.chol := incidence/V1]
corr.factor.chol [, `:=` (p0 = NULL, incidence = NULL, V1 = NULL)]
POP <- merge(POP, corr.factor.chol, by = c("agegroup", "sex"), all.x = T)


for (ll in 1:100) {
    POP[between(age, ageL, ageH) & chd.incidence == 0, v := dice(.N) <= p0 * chd.chol.rr * b.chol]
    nam <- paste0("temp", ll)
    assign(nam, POP[v==T, .N, by = c("agegroup", "sex")])
    setkey(get(nam), agegroup, sex)
}

x <- lapply(as.list(paste0("temp", 1:100)),get)
x <- data.table(rbind_all(x))
chol =copy( x[, mean(N), by= c("agegroup", "sex")])
setkey(chol, sex, agegroup)
ideal[, plot(V1, ylim = c(0,250), main = "chol alignment")]
chol[, points(V1, col="red")]

for (ll in 1:100) {
    POP[between(age, ageL, ageH) & chd.incidence == 0, v := dice(.N) <= p0 * chd.chol.rr]
    nam <- paste0("temp", ll)
    assign(nam, POP[v==T, .N, by = c("agegroup", "sex")])
    setkey(get(nam), agegroup, sex)
}

x <- lapply(as.list(paste0("temp", 1:100)),get)
x <- data.table(rbind_all(x))
chol =copy( x[, mean(N), by= c("agegroup", "sex")])
setkey(chol, sex, agegroup)
ideal[, plot(V1, ylim = c(0,250), main = "chol no alignment")]
chol[, points(V1, col="red")]

# SBP
for (m in "sbppaf") {
    CHDincid[chdbmipaf[chdcholpaf[chddiabpaf[chdetspaf[chdfvpaf[chdsbppaf[chdsmokepaf]]]]]], 
             p0 := incidence * (1 - get(m))]
    POP[, ':='(p0 = NULL, incidence = NULL)]
    POP <- merge(POP, CHDincid, by = c("agegroup", "sex"), all.x = T)
}    

corr.factor.sbp <- merge(POP[, mean(p0 * chd.sbp.rr), by = c("agegroup", "sex")], CHDincid, by = c("agegroup", "sex"), all.x = T)
corr.factor.sbp [,b.sbp := incidence/V1]
corr.factor.sbp [, `:=` (p0=NULL, incidence=NULL, V1=NULL)]
POP <- merge(POP, corr.factor.sbp, by = c("agegroup", "sex"), all.x = T)

for (ll in 1:100) {
    POP[between(age, ageL, ageH) & chd.incidence == 0, v := dice(.N) <= p0 * chd.sbp.rr * b.sbp]
    nam <- paste0("temp", ll)
    assign(nam, POP[v==T, .N, by = c("agegroup", "sex")])
    setkey(get(nam), agegroup, sex)
}

x <- lapply(as.list(paste0("temp", 1:100)),get)
x <- data.table(rbind_all(x))
sbp =copy( x[, mean(N), by= c("agegroup", "sex")])
setkey(sbp, sex, agegroup)
ideal[, plot(V1, ylim = c(0,250), main = "sbp alignment")]
sbp[, points(V1, col="red")]

for (ll in 1:100) {
    POP[between(age, ageL, ageH) & chd.incidence == 0, v := dice(.N) <= p0 * chd.sbp.rr]
    nam <- paste0("temp", ll)
    assign(nam, POP[v==T, .N, by = c("agegroup", "sex")])
    setkey(get(nam), agegroup, sex)
}

x <- lapply(as.list(paste0("temp", 1:100)),get)
x <- data.table(rbind_all(x))
sbp =copy( x[, mean(N), by= c("agegroup", "sex")])
setkey(sbp, sex, agegroup)
ideal[, plot(V1, ylim = c(0,250), main = "sbp no alignment")]
sbp[, points(V1, col="red")]


# DIAB
for (m in "diabpaf") {
    CHDincid[chdbmipaf[chdcholpaf[chddiabpaf[chdetspaf[chdfvpaf[chdsbppaf[chdsmokepaf]]]]]], 
             p0 := incidence * (1 - get(m))]
    POP[, ':='(p0 = NULL, incidence = NULL)]
    POP <- merge(POP, CHDincid, by = c("agegroup", "sex"), all.x = T)
}    

corr.factor.diab <- merge(POP[, mean(p0 * chd.diab.rr), by = c("agegroup", "sex")], CHDincid, by = c("agegroup", "sex"), all.x = T)
corr.factor.diab [,b.diab := incidence/V1]
corr.factor.diab [, `:=` (p0=NULL, incidence=NULL, V1=NULL)]
POP <- merge(POP, corr.factor.diab, by = c("agegroup", "sex"), all.x = T)

for (ll in 1:100) {
    POP[between(age, ageL, ageH) & chd.incidence == 0, v := dice(.N) <= p0 * chd.diab.rr * b.diab]
    nam <- paste0("temp", ll)
    assign(nam, POP[v==T, .N, by = c("agegroup", "sex")])
    setkey(get(nam), agegroup, sex)
}

x <- lapply(as.list(paste0("temp", 1:100)),get)
x <- data.table(rbind_all(x))
diab =copy( x[, mean(N), by= c("agegroup", "sex")])
setkey(diab, sex, agegroup)
ideal[, plot(V1, ylim = c(0,250), main = "diab alignment")]
diab[, points(V1, col="red")]

for (ll in 1:100) {
    POP[between(age, ageL, ageH) & chd.incidence == 0, v := dice(.N) <= p0 * chd.diab.rr]
    nam <- paste0("temp", ll)
    assign(nam, POP[v==T, .N, by = c("agegroup", "sex")])
    setkey(get(nam), agegroup, sex)
}

x <- lapply(as.list(paste0("temp", 1:100)),get)
x <- data.table(rbind_all(x))
diab =copy( x[, mean(N), by= c("agegroup", "sex")])
setkey(diab, sex, agegroup)
ideal[, plot(V1, ylim = c(0,250), main = "diab no alignment")]
diab[, points(V1, col="red")]

# ETS
for (m in "etspaf") {
    CHDincid[chdbmipaf[chdcholpaf[chddiabpaf[chdetspaf[chdfvpaf[chdsbppaf[chdsmokepaf]]]]]], 
             p0 := incidence * (1 - get(m))]
    POP[, ':='(p0 = NULL, incidence = NULL)]
    POP <- merge(POP, CHDincid, by = c("agegroup", "sex"), all.x = T)
}    

corr.factor.ets <- merge(POP[, mean(p0 * chd.ets.rr), by = c("agegroup", "sex")], CHDincid, by = c("agegroup", "sex"), all.x = T)
corr.factor.ets [,b.ets := incidence/V1]
corr.factor.ets [, `:=` (p0=NULL, incidence=NULL, V1=NULL)]
POP <- merge(POP, corr.factor.ets, by = c("agegroup", "sex"), all.x = T)

for (ll in 1:100) {
    POP[between(age, ageL, ageH) & chd.incidence == 0, v := dice(.N) <= p0 * chd.ets.rr * b.ets]
    nam <- paste0("temp", ll)
    assign(nam, POP[v==T, .N, by = c("agegroup", "sex")])
    setkey(get(nam), agegroup, sex)
}

x <- lapply(as.list(paste0("temp", 1:100)),get)
x <- data.table(rbind_all(x))
ets =copy( x[, mean(N), by= c("agegroup", "sex")])
setkey(ets, sex, agegroup)
ideal[, plot(V1, ylim = c(0,250), main = "ets alignment")]
ets[, points(V1, col="red")]

for (ll in 1:100) {
    POP[between(age, ageL, ageH) & chd.incidence == 0, v := dice(.N) <= p0 * chd.ets.rr]
    nam <- paste0("temp", ll)
    assign(nam, POP[v==T, .N, by = c("agegroup", "sex")])
    setkey(get(nam), agegroup, sex)
}

x <- lapply(as.list(paste0("temp", 1:100)),get)
x <- data.table(rbind_all(x))
ets =copy( x[, mean(N), by= c("agegroup", "sex")])
setkey(ets, sex, agegroup)
ideal[, plot(V1, ylim = c(0,250), main = "ets no alignment")]
ets[, points(V1, col="red")]

# FV
for (m in "fvpaf") {
    CHDincid[chdbmipaf[chdcholpaf[chddiabpaf[chdetspaf[chdfvpaf[chdsbppaf[chdsmokepaf]]]]]], 
             p0 := incidence * (1 - get(m))]
    POP[, ':='(p0 = NULL, incidence = NULL)]
    POP <- merge(POP, CHDincid, by = c("agegroup", "sex"), all.x = T)
}    

corr.factor.fv <- merge(POP[, mean(p0 * chd.fv.rr), by = c("agegroup", "sex")], CHDincid, by = c("agegroup", "sex"), all.x = T)
corr.factor.fv [,b.fv := incidence/V1]
corr.factor.fv [, `:=` (p0=NULL, incidence=NULL, V1=NULL)]
POP <- merge(POP, corr.factor.fv, by = c("agegroup", "sex"), all.x = T)

for (ll in 1:100) {
    POP[between(age, ageL, ageH) & chd.incidence == 0, v := dice(.N) <= p0 * chd.fv.rr * b.fv]
    nam <- paste0("temp", ll)
    assign(nam, POP[v==T, .N, by = c("agegroup", "sex")])
    setkey(get(nam), agegroup, sex)
}

x <- lapply(as.list(paste0("temp", 1:100)),get)
x <- data.table(rbind_all(x))
fv =copy( x[, mean(N), by= c("agegroup", "sex")])
setkey(fv, sex, agegroup)
ideal[, plot(V1, ylim = c(0,250), main = "fv alignment")]
fv[, points(V1, col="red")]

for (ll in 1:100) {
    POP[between(age, ageL, ageH) & chd.incidence == 0, v := dice(.N) <= p0 * chd.fv.rr]
    nam <- paste0("temp", ll)
    assign(nam, POP[v==T, .N, by = c("agegroup", "sex")])
    setkey(get(nam), agegroup, sex)
}

x <- lapply(as.list(paste0("temp", 1:100)),get)
x <- data.table(rbind_all(x))
fv =copy( x[, mean(N), by= c("agegroup", "sex")])
setkey(fv, sex, agegroup)
ideal[, plot(V1, ylim = c(0,250), main = "fv no alignment")]
fv[, points(V1, col="red")]


# TOBACCO
for (m in "tobaccopaf") {
    CHDincid[chdbmipaf[chdcholpaf[chddiabpaf[chdetspaf[chdfvpaf[chdsbppaf[chdsmokepaf]]]]]], 
             p0 := incidence * (1 - get(m))]
    POP[, ':='(p0 = NULL, incidence = NULL)]
    POP <- merge(POP, CHDincid, by = c("agegroup", "sex"), all.x = T)
}    

corr.factor.tob <- merge(POP[, mean(p0 * chd.tob.rr), by = c("agegroup", "sex")], CHDincid, by = c("agegroup", "sex"), all.x = T)
corr.factor.tob [,b.tob := incidence/V1]
corr.factor.tob [, `:=` (p0=NULL, incidence=NULL, V1=NULL)]
POP <- merge(POP, corr.factor.tob, by = c("agegroup", "sex"), all.x = T)

for (ll in 1:100) {
    POP[between(age, ageL, ageH) & chd.incidence == 0, v := dice(.N) <= p0 * chd.tob.rr * b.tob]
    nam <- paste0("temp", ll)
    assign(nam, POP[v==T, .N, by = c("agegroup", "sex")])
    setkey(get(nam), agegroup, sex)
}


x <- lapply(as.list(paste0("temp", 1:100)),get)
x <- data.table(rbind_all(x))
tob =copy( x[, mean(N), by= c("agegroup", "sex")])
setkey(tob, sex, agegroup)
ideal[, plot(V1, ylim = c(0,250), main = "tobacco alignment")]
tob[, points(V1, col="red")]

for (ll in 1:100) {
    POP[between(age, ageL, ageH) & chd.incidence == 0, v := dice(.N) <= p0 * chd.tob.rr]
    nam <- paste0("temp", ll)
    assign(nam, POP[v==T, .N, by = c("agegroup", "sex")])
    setkey(get(nam), agegroup, sex)
}


x <- lapply(as.list(paste0("temp", 1:100)),get)
x <- data.table(rbind_all(x))
tob =copy( x[, mean(N), by= c("agegroup", "sex")])
setkey(tob, sex, agegroup)
ideal[, plot(V1, ylim = c(0,250), main = "tobacco no alignment")]
tob[, points(V1, col="red")]

# ALL TOGETHER
CHDincid[chdbmipaf[chdcholpaf[chddiabpaf[chdetspaf[chdfvpaf[chdsbppaf[chdsmokepaf]]]]]], 
         p0 := incidence * (1 - bmipaf) * (1 - cholpaf) * (1 - diabpaf) * (1 - etspaf) * (1 - fvpaf) * (1 - sbppaf) * (1 - tobaccopaf)]
POP[, ':='(p0 = NULL, incidence = NULL)]
POP <- merge(POP, CHDincid, by = c("agegroup", "sex"), all.x = T)

# correction factor
corr.factor <- merge(POP[, mean(p0* chd.tob.rr * chd.ets.rr * 
                             chd.sbp.rr * chd.chol.rr * 
                             chd.bmi.rr * chd.diab.rr * chd.fv.rr), by = c("agegroup", "sex")], CHDincid, by = c("agegroup", "sex"), all.x = T)
corr.factor [,b := incidence/V1]
corr.factor[, `:=` (p0=NULL, incidence=NULL, V1=NULL)]
POP <- merge(POP, corr.factor, by = c("agegroup", "sex"), all.x = T)


for (ll in 1:100) {
    POP[between(age, ageL, ageH) & chd.incidence == 0, v := dice(.N) <= p0 * chd.tob.rr * chd.ets.rr * 
            chd.sbp.rr * chd.chol.rr * 
            chd.bmi.rr * chd.diab.rr * chd.fv.rr]
    nam <- paste0("temp", ll)
    assign(nam, POP[v==T, .N, by = c("agegroup", "sex")])
    setkey(get(nam), agegroup, sex)
}
#POP[, ':='(p0 = NULL, incidence = NULL, b = NULL)]

x <- lapply(as.list(paste0("temp", 1:100)),get)
x <- data.table(rbind_all(x))
all =copy( x[, mean(N), by= c("agegroup", "sex")])
m =copy( x[, mean(N), by= c("agegroup", "sex")])
setnames(m, "V1", "mean")
u =copy( x[, quantile(N, probs=0.975), by= c("agegroup", "sex")])
setnames(u, "V1", "ci_u")
l =copy( x[, quantile(N, probs=0.025), by= c("agegroup", "sex")])
setnames(l, "V1", "ci_l")
pred <- merge(m,l, by = c("agegroup", "sex"))
pred <- merge(pred,u, by = c("agegroup", "sex"))
rm(m,u,l)
setkey(all, sex, agegroup)
ideal[, plot(V1, ylim = c(0,250), main = "ALL TOGETHER NO ALIGNMENT")]
all[, points(V1, col="red")]

# EXPOSURE ALIGNMENT
for (ll in 1:100) {
    POP[between(age, ageL, ageH) & chd.incidence == 0, v := dice(.N) <= p0 * chd.tob.rr * chd.ets.rr * 
            chd.sbp.rr * chd.chol.rr * 
            chd.bmi.rr * chd.diab.rr * chd.fv.rr * b.tob * b.ets * b.sbp * b.chol * b.bmi * b.diab *b.fv]
    nam <- paste0("temp", ll)
    assign(nam, POP[v==T, .N, by = c("agegroup", "sex")])
    setkey(get(nam), agegroup, sex)
}
#POP[, ':='(p0 = NULL, incidence = NULL, b = NULL)]

x <- lapply(as.list(paste0("temp", 1:100)),get)
x <- data.table(rbind_all(x))
all =copy( x[, mean(N), by= c("agegroup", "sex")])
setkey(all, sex, agegroup)
ideal[, plot(V1, ylim = c(0,250), main = "ALL TOGETHER EXPOSURE ALIGNMENT")]
all[, points(V1, col="red")]

# FULL ALIGNMENT
for (ll in 1:100) {
    POP[between(age, ageL, ageH) & chd.incidence == 0, v := dice(.N) <= p0 * chd.tob.rr * chd.ets.rr * 
            chd.sbp.rr * chd.chol.rr * 
            chd.bmi.rr * chd.diab.rr * chd.fv.rr * b]
    nam <- paste0("temp", ll)
    assign(nam, POP[v==T, .N, by = c("agegroup", "sex")])
    setkey(get(nam), agegroup, sex)
}
#POP[, ':='(p0 = NULL, incidence = NULL, b = NULL)]

x <- lapply(as.list(paste0("temp", 1:100)),get)
x <- data.table(rbind_all(x))
all =copy( x[, mean(N), by= c("agegroup", "sex")])
setkey(all, sex, agegroup)
ideal[, plot(V1, ylim = c(0,250), main = "ALL TOGETHER FULL ALIGNMENT")]
all[, points(V1, col="red")]

# Plot for Martin
age.structure <- table(POP[, sex, by = agegroup])
age.structure <- melt(age.structure)
obs <- merge(ideal, age.structure, by = c("agegroup", "sex"))
obs[, incidence := V1*10000/value]
obs[, group := "observed 2011"]
setnames(obs, "V1", "mean")
obs[, `:=` (ci_u = mean, ci_l = mean)]
pred <- merge(pred, age.structure, by = c("agegroup", "sex"))
pred[, incidence := mean*10000/value]
pred[, group := "modelled 2011"]
mar <- rbind(obs, pred, fill =T)

pd <- position_dodge(.05) # move them .05 to the left and right

p <- ggplot(mar[sex==1], aes(x = agegroup, 
                        y = mean, 
                        ymin = ci_l, 
                        ymax = ci_u, 
                        col = group)) + 
    geom_errorbar(size = 0.6, width = 0.2, position=pd, alpha = 0.6) +
    geom_point(size = 4, position=pd, alpha = 0.8) +
    ylab("Incidence per 10 000 population") +
    xlab("Agegroups") +
    #scale_colour_hue(l=40) +                  # Use darker colors, lightness=40
    ggtitle("CHD incidence\nEngland 2011") +
    theme_grey() + 
    theme(legend.justification=c(1,0), legend.position=c(1,0))

#ggsave("./Graphs for Martin/CHD incidence.jpg", p, units = "cm", antialias = "subpixel", dpi=600, width = 20, height = 20)


# ADDITIVE
CHDincid[chdbmipaf[chdcholpaf[chddiabpaf[chdetspaf[chdfvpaf[chdsbppaf[chdsmokepaf]]]]]], 
         p0 := incidence * (bmipaf - cholpaf - diabpaf - etspaf + fvpaf - sbppaf - tobaccopaf)]
POP[, ':='(p0 = NULL, incidence = NULL)]
POP <- merge(POP, CHDincid, by = c("agegroup", "sex"), all.x = T)

for (ll in 1:100) {
    POP[between(age, ageL, ageH) & chd.incidence == 0, v := dice(.N) <= p0 * (chd.tob.rr + chd.ets.rr + chd.bmi.rr + chd.diab.rr + chd.chol.rr + chd.sbp.rr - chd.fv.rr - 5)]
    nam <- paste0("temp", ll)
    assign(nam, POP[v==T, .N, by = c("agegroup", "sex")])
    setkey(get(nam), agegroup, sex)
}
#POP[, ':='(p0 = NULL, incidence = NULL, b = NULL)]

x <- lapply(as.list(paste0("temp", 1:100)),get)
x <- data.table(rbind_all(x))
all =copy( x[, mean(N), by= c("agegroup", "sex")])
setkey(all, sex, agegroup)
ideal[, plot(V1, ylim = c(0,250), main = "ALL TOGETHER ADDITIVE")]
all[, points(V1, col="red")]










#*******************************************************************************************************************

load(file="./Datasets/Health Survey for England/2006/hse06ai.RData") # load HSE2006
HSE <- clear.labels(HSE)
HSE <- data.table(filter(HSE, samptype!=3), key = "age") # remove boost sample of children and convert to datatable
HSE[, qimd := imd2004]
#HSE[, age := age+5]
agegroup.fn(HSE)
HSE[, group := paste0(sex, agegroup)]
HSE[, cholval.cvdlag := cholval1 + 0.1]
HSE[, omsysval.cvdlag := omsysval]
HSE[, bmival.cvdlag := bmival]
POP[, group := paste0(sex, agegroup)]

require(simPopulation)
spCdfplot(x ='cholval.cvdlag', weights = 'wt.blood', cond = c("group"), 
          dataS = HSE[between(as.numeric(as.character(age)), 30, 79)], dataP = POP[between(age, 30, 79)])
spCdfplot(x ='omsysval.cvdlag', weights = 'wt.nurse', cond = c("group"), 
          dataS = HSE[between(as.numeric(as.character(age)), 30, 79)], dataP = POP[between(age, 30, 79)])


HSE[, age := age+5]
agegroup.fn(HSE)

spCdfplot(x ='cholval.cvdlag', weights = 'wt.blood', cond = c("group"), 
          dataS = HSE[between(as.numeric(as.character(age)), 30, 79)], dataP = POP[between(as.numeric(as.character(age)), 30, 79)])
