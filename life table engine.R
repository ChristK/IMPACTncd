#cmpfile("./life table engine.R")

# Load files --------------------------------------------------------------


# Create diseasestoexclude.ICD from diseasestoexclude user input
diseasestoexclude.ICD <- diseasestoexclude
if ("CHD" %in% diseasestoexclude) {
  remove <- "CHD"
  diseasestoexclude.ICD <- outersect(diseasestoexclude.ICD, remove)
  diseasestoexclude.ICD <-
    c(diseasestoexclude.ICD, "Ischaemic heart diseases")
}

if ("stroke" %in% diseasestoexclude) {
  remove <- "stroke"
  diseasestoexclude.ICD <- outersect(diseasestoexclude.ICD, remove)
  diseasestoexclude.ICD <-
    c(diseasestoexclude.ICD,"Cerebrovascular diseases")
}

if ("C16" %in% diseasestoexclude) {
  remove <- "C16"
  diseasestoexclude.ICD <- outersect(diseasestoexclude.ICD, remove)
  diseasestoexclude.ICD <-
    c(diseasestoexclude.ICD,"Malignant neoplasm of stomach")
}

population <-
  fread(
    "./Population/Population by adjusted IMD 2010 quintile_final.csv",
    header = T, 
    skip = 0
  )
setnames(population, names(population), str_trim(names(population)))
population <-
  melt(
    population, id.vars = c("year", "sex", "qimd"), 
    variable.name = "agegroup",
    value.name = "pop"
  )

deaths.qimd <-
  fread(
    "./LifeTables/Deaths by adjusted IMD 2010 quintile_final.csv",
    header = T, 
    skip = 0
  )

setnames(deaths.qimd, names(deaths.qimd), str_trim(names(deaths.qimd)))
deaths.qimd <-
  melt(
    deaths.qimd, id.vars = c("year", "sex", "qimd"), 
    variable.name = "agegroup", 
    value.name = "deaths"
  )

deaths.causes <-
  fread(
    "./LifeTables/deaths from selected causes by quintile_final_tcm77-388639.csv",
    header = T,
    skip = 0
  )

setnames(deaths.causes, names(deaths.causes), str_trim(names(deaths.causes)))
deaths.causes <- deaths.causes[cause %in% diseasestoexclude.ICD,]
deaths.causes <-
  melt(
    deaths.causes, id.vars = c("year", "sex", "qimd", "cause"), 
    variable.name = "agegroup", 
    value.name = "disease"
  )

deaths.causes <-
  deaths.causes[, list(disease = sum(disease)), 
                by = .(year, agegroup, sex, qimd)]

tt <-
  merge(
    population, deaths.qimd, by = c("year", "sex", "qimd", "agegroup"),
    all.x = T
  )
tt <-
  merge(
    tt, deaths.causes, by = c("year", "sex", "qimd", "agegroup"), 
    all.x = T
  )
tt[, Mx.all := deaths / pop]  
#death rate all diseases tt[as.numeric(as.character(agegroup)) < ageL | 
#as.numeric(as.character(agegroup)) > ageH, disease := 0] # Exclude disease 
#mortality only for ages between ageL and ageH
tt[agegroup %!in% unique(agegroup.fn(ageL:ageH)), disease := 0] 
# Exclude disease mortality only for ages between ageL and ageH
tt[, Mx.disease := (deaths - disease) / (pop - disease)]
# death rate modelled diseases excluded

# Life tables loop --------------------------------------------------------


# Full mortality projections (no disease excluded)
if (Sys.info()[1] == "Linux")
  registerDoParallel(clusternumber)
if (Sys.info()[1] == "Windows") {
  cl <- makeCluster(clusternumber)
  registerDoParallel(cl)
}
pp <- foreach(k = 1:2, .inorder = F, .combine = 'c') %:% # sex
  foreach(
    l = 1:5,
    .inorder = F,
    .packages = c("demography", "reshape2", "data.table")
  ) %dopar% {
    output <- vector("list", 3)
    # all cause mortality
    xx <- demogdata(
      acast(tt[sex == k & qimd == l,],
            agegroup ~ year, value.var = "Mx.disease"),
      acast(tt[sex == k & qimd == l,],
            agegroup ~ year, value.var = "pop"),
      c(0, 1, 6, seq(12.5, 87.5, 5), 100), #c(0:99),
      #c(0, 1, 5, seq(10, 85, 5), 100), 
      tt[sex == k & qimd == l, unique(year)],
      "mortality",
      paste0("England.sex-", k, ".qimd-", l), 0
    )
    xx <-
      smooth.demogdata(
        xx, age.grid = 0:100, weight = F, interpolate = T
      )
    temp <-
      forecast.fdm(fdm(xx,  method = "M", max.age = 100), yearstoproject)
    xx <- combine.demogdata(xx, temp)
    xx <- lifetable(xx, type = "period")
    xx <-
      as.data.table(xx$qx, keep.rownames = T)[, `:=`(sex = k, qimd = l)]
    setnames(xx, "rn", "age")
    xx[, `:=`(sex = factor(sex), age = as.numeric(age), qimd = ordered(qimd))]
    output[[1]] <- xx
    # minus disease specific mortality
    xx <- demogdata(
      acast(tt[sex == k & qimd == l,],
            agegroup ~ year, value.var = "Mx.all"),
      acast(tt[sex == k & qimd == l,],
            agegroup ~ year, value.var = "pop"),
      c(0, 1, 6, seq(12.5, 87.5, 5), 100), #c(0:99),
      #c(0, 1, 5, seq(10, 85, 5), 100),
      tt[sex == k &
           qimd == l, unique(year)],
      "mortality",
      paste0("England.sex-", k, ".qimd-", l), 0
    )
    #plot(xx)
    xx <-
      smooth.demogdata(
        xx, age.grid = 0:100, weight = F, interpolate = T
      )
    xxx <- xx$pop
    temp <-
      forecast.fdm(fdm(xx,  method = "M", max.age = 100), yearstoproject)
    
    xx <- combine.demogdata(xx, temp)
    #plot(xx)
    xx <- lifetable(xx, type = "period")
    
    xx <-
      as.data.table(xx$qx, keep.rownames = T)[, `:=`(sex = k, qimd = l)]
    xxx <-
      as.data.table(data.frame(xxx),
                    keep.rownames = T)[, `:=`(sex = k, qimd = l)]
    setnames(xx, "rn", "age")
    setnames(xxx, "rn", "age")
    setnames(xxx, paste0("X0.", 2002:2013), paste0(2002:2013))
    xx[, `:=`(sex = factor(sex), age = as.numeric(age), qimd = ordered(qimd))]
    xxx[, `:=`(sex = factor(sex), age = as.numeric(age), qimd = ordered(qimd))]
    output[[2]] <- xx
    output[[3]] <- xxx
    output
  }

if (Sys.info()[1] == "Windows")
  stopCluster(cl)

Lifetable.diab <- rbindlist(lapply(pp, `[[`, 2))
Lifetable.diab[, age := as.integer(age)]
Lifetable <-
  rbindlist(lapply(pp, `[[`, 1)) # access first element of a nested list
Lifetable[, age := as.integer(age)]

setkey(Lifetable.diab, age, sex, qimd)
setkey(Lifetable,      age, sex, qimd)


# Population and smoking death-rate inflation tables ----------------------


# Match the sex and age structure of the initial year
population.actual <- rbindlist(lapply(pp, `[[`, 3))[, .SD, .SDcols = c("age", "sex", "qimd", paste0(init.year))]
setnames(population.actual, paste0(init.year), "pop.smooth")
population.actual[, agegroup := agegroup.fn(age)
                  ][between(age, 85, 89), agegroup := "85-89"
                    ][age > 89, agegroup := "90+"
                      ][, agegroup := ordered(agegroup)
                        ][, pop.group := sum(pop.smooth),
                          by = .(agegroup, sex, qimd)]

tt = copy(population[year == init.year]
)[, year := NULL
  ][, agegroup := mapvalues(agegroup, "0", "<1   ")
    ][, sex := factor(sex)
      ][, qimd := ordered(qimd)]
population.actual[tt, on = c("agegroup", "sex", "qimd"),
                  pop := round(pop.smooth * pop/pop.group)]

population.actual[, pct := round(as.numeric(n) * pop / sum(pop))]
population.actual[, pct2 := pct]
population.actual[age > 89, pct2 := sum(pct), by = .(sex, qimd)] # aggregate ages>90
# Calculate the exact fraction of the mid 2010 population this sample represents
pop.fraction <-
  n / population.actual[, sum(pop)]
# 53107200 is the total mid 2011 population of England (52642600 for 2010)

cat(paste0("Population fraction = ", pop.fraction, "\n"), 
    file = "./Output/simulation parameters temp.txt",
    append = T)

# Load rr of death for smokers from Peto, Mortality in relation to smoking: 50
# years observations on male British doctors. Table 1
smokriskofdeath <- fread("./LifeTables/smokriskofdeath.csv")
setkey(smokriskofdeath)
smokriskofdeath <-
  smokriskofdeath[!diseasestoexclude,] 
# Need to adjust 1st column for each new disease
smokriskofdeath <-
  smokriskofdeath[,(sum(Current) * 4680 / 35.30) / (sum(Lifelong.non.smokers) *
                                                      2917 / 19.34)] 
# to calculate number of deaths

#rm(list = c(apropos(glob2rx("mortal.sex-*"))))
#rm(list = c(apropos(glob2rx("POP.sex-*"))))
rm(tt, deaths.qimd, population, deaths.causes, diseasestoexclude.ICD, pp)
