# Diagnostic graphs ofor synth pop

require(simPopulation)
# load  HSE2011
load(file="./Datasets/Health Survey for England/2011/hse2011ai.RData")
HSE.2011 <- clear.labels(HSE.2011)
HSE.2011 <- data.table(filter(HSE.2011, samptype==1), key="age") #1 denotes the lag years
agegroup.fn("HSE.2011")
HSE.2011[, cholval := cholval1]
setkey(HSE.2011, group, cholval) # short by group and then by cholval1
POP[between(age, ageL, ageH), summary(cholval)]
svyquantile(~cholval, subset(HSE.2011.srv.blood, age>=ageL & age<=ageH), c(seq(0,1, 0.25)), na.rm=T)
svymean(~cholval, subset(HSE.2011.srv.blood, age>=ageL & age<=ageH), na.rm = T)

# Graphs
spCdfplot(x ='cholval', weights = 'wt.blood', cond = c("group"), 
          dataS = HSE.2011[between(age, ageL, ageH)], dataP = SPOP2011[between(age, ageL, ageH)], aprox = c(T,T))
spCdfplot(x ='cholval', weights = 'wt.blood', cond = c("sex"), 
          dataS = HSE.2011[between(age, ageL, ageH)], dataP = SPOP2011[between(age, ageL, ageH)])
spCdfplot(x ='cholval', weights = 'wt.blood', cond = c("sex"), 
          dataS = HSE.2011, dataP = SPOP2011)
spCdfplot(x ='cholval', weights = 'wt.blood', cond = c("age"), 
          dataS = HSE.2011[between(age, ageL, ageH)], dataP = SPOP2011[between(age, ageL, ageH)], aprox = c(T,T))

spCdfplot(x ='omsysval', weights = 'wt.nurse', cond = c("group"), 
          dataS = HSE.2011[between(age, ageL, ageH)], dataP = SPOP2011[between(age, ageL, ageH)])
spCdfplot(x ='omsysval', weights = 'wt.nurse', cond = c("sex"), 
          dataS = HSE.2011[between(age, ageL, ageH)], dataP = SPOP2011[between(age, ageL, ageH)])
spCdfplot(x ='omsysval', weights = 'wt.nurse', cond = c("age"), 
          dataS = HSE.2011[between(age, ageL, ageH)], dataP = SPOP2011[between(age, ageL, ageH)])
