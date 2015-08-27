load("C:/Users/ckyprid/Dropbox/PhD/Datasets/Health Survey for England/2006/hse06ai.RData")
load("C:/Users/ckyprid/Dropbox/PhD/Datasets/Health Survey for England/2010/hse10ai.RData")
load("C:/Users/ckyprid/Dropbox/PhD/Models/IMPACTncd/SynthPop/spop2011-1.RData")
load("C:/Users/ckyprid/Dropbox/PhD/Models/IMPACTncd/Lagtime Populations/SPOP2006.RData")
HSE2006 <- data.table(HSE)
HSE2010 <- data.table(HSE2010original)
HSE2010 <- HSE2010[samptype == 1 & wt.blood>0,]
HSE2010.srv.blood <- svydesign(id=~psu, strata=~cluster, weights=~wt.blood, nest=F, data=HSE2010)

tt2011 <- ecdf(SPOP2011$cholval1)
tt2006 <- ecdf(SPOP$cholval1)
tt2006w <- ewcdf(HSE2006[cholval1>0 & wt.blood>0,cholval1], HSE2006[cholval1>0 & wt.blood>0,wt.blood])
tt2006w(5.23)/HSE2006[cholval1>0 & wt.blood>0,sum(wt.blood)]
tt2010 <- svycdf(~cholval1, HSE2010.srv.blood)
tt2010[[1]](5.1)

svyquantile(~cholval1, HSE2010.srv.blood, na.rm = T,ties = "rounded",  c(0.467, 0.6, 0.4657196))[]
svyby(~cholval1, ~sex, HSE2010.srv.blood, svyquantile, na.rm = T, ties = "rounded",  quantiles = c(0.467))

SPOP2011[, percentil := tt2011(cholval1)]
SPOP2011[, summary(percentil)]
SPOP2011[, cholval2006 := quantile(tt2006, percentil, names = F)]
SPOP2011[, cholval2010 := t(svyquantile(~cholval1, HSE2010.srv.blood, na.rm = T, ties = "rounded",  percentil))]

SPOP2011[, summary(cholval1)]
SPOP2011[, summary(cholval2006)]
SPOP2011[, summary(cholval2010)]
SPOP2011[, plot(density(cholval1), xlim=c(0,14))]
SPOP2011[, hist(cholval2006, col = "red", xlim=c(0,14), add = T)]
