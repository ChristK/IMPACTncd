



# HSE2010
load(file="./Datasets/Health Survey for England/2010/hse10ai.RData")
HSE2010original <- clear.labels(HSE2010original)
#summary(as.factor(HSE2010original$samptype))
HSE1 <- data.table(filter(HSE2010original, samptype==1), key="age") #1 denotes the lag years
rm(HSE2010original)
setnames(HSE1, "imd2007", "qimd")
HSE1[, cholval1 := cholval1 + 0.1]
agegroup.fn("HSE1")
setkey(HSE1, group, cholval1) # short by group and then by cholval1
#svyquantile(~cholval1, subset(HSE1.srv.blood, group == "1220-24"), c(seq(0, 0.1, 0.01)), na.rm = T, ties = "rounded")

# HSE2009
load(file="./Datasets/Health Survey for England/2009/hse09ai.RData")
HSE2009original <- clear.labels(HSE2009original)
HSE2 <- data.table(filter(HSE2009original, samptype==1), key="age") 
rm(HSE2009original)
setnames(HSE2, "imd2007", "qimd")
HSE2[, cholval1 := cholval1 + 0.1]
agegroup.fn("HSE2")
setkey(HSE2, group, cholval1) # short by group and then by cholval1

# HSE2008
load(file="./Datasets/Health Survey for England/2008/hse08ai.RData")
HSE2008original <- clear.labels(HSE2008original)
HSE3 <- data.table(filter(HSE2008original, samptype==1), key="age") 
rm(HSE2008original)
HSE3[, cholval1 := cholval1 + 0.1]
agegroup.fn("HSE3")
setkey(HSE3, group, cholval1)

# HSE2007 (no chol)
load(file="./Datasets/Health Survey for England/2007/hse07ai.RData")
HSE2007original <- clear.labels(HSE2007original)
HSE4 <- data.table(filter(HSE2007original, samptype==1), key="age") 
rm(HSE2007original)
setnames(HSE4, "imd2007", "qimd")
agegroup.fn("HSE4")
setkey(HSE4, group)

# HSE2006
load(file="./Datasets/Health Survey for England/2006/hse06ai.RData")
HSE <- clear.labels(HSE)
HSE5 <- data.table(filter(HSE, samptype!=3), key="age") 
rm(HSE)
setnames(HSE5, "imd2004", "qimd")
HSE5[, cholval1 := cholval1 + 0.1]
agegroup.fn("HSE5")
setkey(HSE5, group, cholval1)

# lagtime
# get percentiles for risk factors
agegroup.fn("POP", 0)
POP[, cholval.pct := ecdf(.SD[,cholval])(cholval), by = "group"] # get percentile of 2011 distribution by group
POP[, omsysval.pct := ecdf(.SD[,omsysval])(omsysval), by = "group"] # get percentile of 2011 distribution by group
POP[, bmival.pct := ecdf(.SD[,bmival])(bmival), by = "group"] # get percentile of 2011 distribution by group
POP[, cigst1.pct := ecdf(.SD[,cigst1])(cigst1), by = "group"]


agegroup.fn("POP", -1)
# for (k in HSE1[between(age, ageL, ageH),unique(group)]) { # expand min & max values in HSE to match those in POP by agegroup, sex and qimd
#     # cat(k)
#     set(HSE1, i = HSE1[cholval1 > 0 & group == k, .I[which.min(cholval1)], by = "group"][["V1"]], j = "cholval1", value = POP[group == k, min(cholval)])
#     set(HSE1, i = HSE1[cholval1 > 0 & group == k, .I[which.max(cholval1)], by = "group"][["V1"]], j = "cholval1", value = POP[group == k, max(cholval)])
# }
#View(HSE1[between(age, ageL, ageH), list(group, cholval1)])

HSE1.srv.blood <- svydesign(id=~psu, strata=~cluster, weights=~wt.blood, nest=F, data=HSE1[wt.blood>0])
POP[between(age, ageL, ageH), cholval1 := t(svyquantile(~cholval1, subset(HSE1.srv.blood, group == .BY[[1L]]), na.rm = T, ties = "rounded",  cholval.pct)), by = "group"]

HSE1.srv.nurse <- svydesign(id=~psu, strata=~cluster, weights=~wt.nurse, nest=F, data=HSE1[wt.nurse>0])

POP[between(age, ageL, ageH), summary(cholval)]
POP[between(age, ageL, ageH), summary(cholval1)]
spCdfplot(x ='cholval1', weights = 'wt.blood', cond = c("sex"), 
          dataS = HSE1[between(age, ageL, ageH)], dataP = POP[between(age, ageL, ageH)])

SPOP2011[between(age, ageL, ageH), hist(cholval)]
SPOP2011[between(age, ageL, ageH), hist(cholval1)]
SPOP2011[between(age, ageL, ageH), hist(cholval2)]

svyhist(~cholval1, subset(HSE1.srv.blood, age>ageL & age<ageH))

SPOP2011[group == "3130-34", hist((cholval1))]




agegroup.fn("POP", -2)
HSE2.srv.blood <- svydesign(id=~psu, strata=~cluster, weights=~wt.blood, nest=F, data=HSE2[wt.blood>0])
POP[between(age, ageL, ageH), cholval2 := t(svyquantile(~cholval1, subset(HSE2.srv.blood, group == .BY[[1L]]), na.rm = T, ties = "rounded",  cholval.pct)), by = "group"]

agegroup.fn("POP", -3)
HSE3.srv.blood <- svydesign(id=~psu, strata=~cluster, weights=~wt.blood, nest=F, data=HSE3[wt.blood>0])
POP[between(age, ageL, ageH), cholval3 := t(svyquantile(~cholval1, subset(HSE3.srv.blood, group == .BY[[1L]]), na.rm = T, ties = "rounded",  cholval.pct)), by = "group"]

agegroup.fn("POP", -5) # no cholesterol for (2007 HSE4)
HSE5.srv.blood <- svydesign(id=~psu, strata=~cluster, weights=~wt.blood, nest=F, data=HSE5[wt.blood>0])
POP[between(age, ageL, ageH), cholval5 := t(svyquantile(~cholval1, subset(HSE5.srv.blood, group == .BY[[1L]]), na.rm = T, ties = "rounded",  cholval.pct)), by = "group"]

agegroup.fn("POP", -4) # Estimate cholesterol for (2007 HSE4) between HSE3 and HSE5
POP[between(age, ageL, ageH), cholval4 := (cholval5 + cholval3)/2]

POP[between(age, ageL, ageH), summary(cholval)]
POP[between(age, ageL, ageH), summary(cholval1)]
POP[between(age, ageL, ageH), summary(cholval2)]
POP[between(age, ageL, ageH), summary(cholval3)]
POP[between(age, ageL, ageH), summary(cholval4)]
POP[between(age, ageL, ageH), summary(cholval5)]

setcolorder(POP, POP[,order(names(POP))])
View(SPOP2011[group == "3240-44"])

tt <- ecdf(POP$cigst1)
plot(tt)
tt(c(1,2,3,4))
quantile(tt, 0.57, names = T)
