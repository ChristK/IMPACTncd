# Lifetable engine

# q is the probability of dying by ANY CAUSE between ages x to x+1 Ndiseases is the number of deaths
# by the disease defined in diseasestoexclude variable qd is the probability of dying by a CAUSE
# OTHER THAN THE DISEASESTOEXCLUDE ones mqd is the mean probability of dying by a CAUSE OTHER THAN
# THE DISEASESTOEXCLUDE ones, for years 2010 to 2012


# Create diseasestoexclude.ICD from diseasestoexclude user input
diseasestoexclude.ICD <- diseasestoexclude
if ("CHD" %in% diseasestoexclude) {
    remove <- "CHD"
    diseasestoexclude.ICD <- outersect(diseasestoexclude.ICD, remove)
    diseasestoexclude.ICD <- c(diseasestoexclude.ICD, paste0("I", 20:25))
}

if ("stroke" %in% diseasestoexclude) {
    remove <- "stroke"
    diseasestoexclude.ICD <- outersect(diseasestoexclude.ICD, remove)
    diseasestoexclude.ICD <- c(diseasestoexclude.ICD, paste0("I", 60:69))
}

# import England and Wales death rates by COD and mid-year population estimates 2001-2010
load(file="./LifeTables/deaths.by.cause.RData")

# Remove deaths from the modelled diseases for the modelled agegroups and recalculate Mx
agegrouptoexclude <- levels(factor(agegroup.fn(ageL:ageH)))
deaths.by.cause[agegroup %in% agegrouptoexclude & icd %in% diseasestoexclude.ICD, ndths := 0]
deaths.by.cause[, Mx:= sum(ndths)/pop, by=.(year, agegroup, sex)]
deaths.by.cause =copy(unique(deaths.by.cause, by=c("year", "agegroup", "sex"))[, c("icd", "ndths"):= NULL][])
deaths.by.cause[, agegroup:= ordered(agegroup, levels = c("<1   ", "01-04", "05-09",
                                                          "10-14", "15-19", "20-24", 
                                                          "25-29", "30-34", "35-39", 
                                                          "40-44", "45-49", "50-54",
                                                          "55-59", "60-64", "65-69",
                                                          "70-74", "75-79", "80-84", 
                                                          "85+"))]
setkey(deaths.by.cause, year, sex, agegroup)

# Smooth, project, and create lifetable
mortal.Men <- demogdata(acast(deaths.by.cause[sex=="1"], agegroup~year, value.var="Mx"),
                        acast(deaths.by.cause[sex=="1"], agegroup~year, value.var="pop"),
                        c(0 , 3, seq(7.5, 82.5, 5), 90),
                        deaths.by.cause[sex=="1", unique(year)],
                        "mortality", "EnglandandWalesMen", 0)
mortal.Women <- demogdata(acast(deaths.by.cause[sex=="2"], agegroup~year, value.var="Mx"),
                        acast(deaths.by.cause[sex=="2"], agegroup~year, value.var="pop"),
                        c(0 , 3, seq(7.5, 82.5, 5), 90),
                        deaths.by.cause[sex=="2", unique(year)],
                        "mortality", "EnglandandWalesWomen", 0)

mortal.Men.sm <- smooth.demogdata(mortal.Men, age.grid=0:100)
mortal.Women.sm <- smooth.demogdata(mortal.Women, age.grid=0:100)

mortal.Men.sm.fdm <- forecast.fdm(fdm(mortal.Men.sm,  method = "classical", max.age=100), yearstoproject)
mortal.Women.sm.fdm <- forecast.fdm(fdm(mortal.Women.sm,  method = "classical", max.age=100), yearstoproject)

mortal.Men.sm <- combine.demogdata(mortal.Men.sm, mortal.Men.sm.fdm)
mortal.Women.sm <- combine.demogdata(mortal.Women.sm, mortal.Women.sm.fdm)

life.table.Men <- lifetable(mortal.Men.sm, type = "period")
life.table.Women <- lifetable(mortal.Men.sm, type = "period")

# Create DT with qx (The probability that an individual of exact age x will die before exact age x+1) by year, age and sex
LifetableM <- as.data.table(life.table.Men$qx, keep.rownames = T)[, sex := 1]
LifetableW <- as.data.table(life.table.Women$qx, keep.rownames = T)[, sex := 2]
Lifetable <- rbind(LifetableM, LifetableW)
setnames(Lifetable, "rn", "age")
Lifetable[, `:=` (sex = factor(sex), age = as.numeric(age))]
# Garbage cleaning
rm(agegrouptoexclude, diseasestoexclude.ICD, 
   mortal.Men, mortal.Women, mortal.Men.sm, mortal.Women.sm, mortal.Men.sm.fdm, mortal.Women.sm.fdm,
   LifetableM, LifetableW, life.table.Men, life.table.Women)
