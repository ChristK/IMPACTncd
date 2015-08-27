#cmpfile("./cancer statistics.R") 
# Gastric cancer
if ("C16" %in% diseasestoexclude) {
  xx <-  suppressWarnings(
    rbind(fread("./Cancer Statistics/C16 DISMOD Males.csv", sep = ",", header = T, stringsAsFactors = F,
                skip = 3, nrows = 100)[, sex := 1],
          fread("./Cancer Statistics/C16 DISMOD Females.csv", sep = ",", header = T, stringsAsFactors = F, 
                skip = 3, nrows = 100)[, sex := 2]
    )[, `:=` (sex = factor(sex), age = as.integer(Age))]
  )
  
  
  C16incid  <- setnames(copy(xx[, c(15, 14, 6), with = F]), "Incidence (rates)", "incidence")[, incidence := as.numeric(incidence)]
  
  C16preval <- setnames(copy(xx[, c(15, 14, 7), with = F]), "Prevalence (rates)", "prevalence")[, prevalence := as.numeric(prevalence)]
  
  C16remis  <- setnames(copy(xx[, c(15, 14, 8), with = F]), "Remission (rates)", "remission")[, remission := as.numeric(remission)]
  
  C16fatal  <- setnames(copy(xx[, c(15, 14, 9), with = F]), "Case fatality (rates)", "fatality")[, fatality := as.numeric(fatality)]
  
  C16surv <- fread("./Cancer Statistics/c16survival.csv", sep = ",", header = T, stringsAsFactors = F)  # Estimate survival
  for (j in c(4L, 5L, 6L, 8L:22L)) C16surv[, (j) := survival.fn(X1, X5, j - 2)]  # X1, X2,... X20 denote the percentage of survivors in years 1, 2,... 20
  
  C16surv[, `:=`(p1 = 1 - X1, 
                 p2 = (X1 - X2)/X1, 
                 p3 = (X2 - X3)/X2, 
                 p4 = (X3 - X4)/X3, 
                 p5 = (X4 - X5)/X4, 
                 p6 = (X5 - X6)/X5, 
                 p7 = (X6 - X7)/X6,
                 p8 = (X7 - X8)/X7, 
                 p9 = (X8 - X9)/X8, 
                 p10 = (X9 - X20)/(X9*10))]  # p1, p2, ... p9 is the probability of dying the 1st, 2nd etc year. 
  # p10 is the mean probability of death for years 10 to 20 after diagnosis
  C16surv[, paste0("X", 1:20) := NULL]
  setnames(C16surv, paste0("p", 1:10), paste0(1:10))
  C16surv <- melt(C16surv, 1:2, variable.name = "duration", value.name = "p.death")
  
  C16surv[, `:=`(sex = as.factor(as.character(sex)), 
                 agegroup = as.ordered(as.character(agegroup)),
                 duration = as.numeric(as.character(duration)))]
  
  if (init.year < 2011) {
    C16fatal[, fatality := fatality * 
               ((100 + fatality.annual.improvement.c16) / 100)^(2011 - init.year)
             ]
    
    C16remis[, remission := remission * 
               ((100 - fatality.annual.improvement.c16) / 100)^(2011 - init.year)
             ]
  }
}

# Lung cancer
# NOTE NEEDS update to use dismod output
# Estimate P0(incidence if all risks at recommended (optimal) level) and survival
if ("C34" %in% diseasestoexclude) {
  C34tobaccopaf <- fread("./Cancer Statistics/c34tobaccopaf.csv", 
                         sep = ",", 
                         header = T, 
                         stringsAsFactors = F)
  C34tobaccopaf[, `:=`(sex = as.factor(as.character(sex)), 
                       agegroup = as.ordered(as.character(agegroup)))]
  setkey(C34tobaccopaf, agegroup, sex)
  
  C34fruitpaf <- fread("./Cancer Statistics/c34fruitpaf.csv", 
                       sep = ",", 
                       header = T, 
                       stringsAsFactors = F)
  C34fruitpaf[, `:=`(sex = as.factor(as.character(sex)), 
                     agegroup = as.ordered(as.character(agegroup)))]
  setkey(C34fruitpaf, agegroup, sex)
  
  C34etspaf <- fread("./Cancer Statistics/c34etspaf.csv", 
                     sep = ",", 
                     header = T, 
                     stringsAsFactors = F)
  C34etspaf[, `:=`(sex = as.factor(as.character(sex)), 
                   agegroup = as.ordered(as.character(agegroup)))]
  setkey(C34etspaf, agegroup, sex)
  
  setkey(C34incid, agegroup, sex, incidence)
  
  C34incid[C34tobaccopaf[C34etspaf[C34fruitpaf]], p0 := incidence * (1 - tobaccopaf) * (1 - etspaf) * (1 - fruitpaf)]
  C34incid[C34tobaccopaf[C34etspaf[C34fruitpaf]], p0tobonly := incidence * (1 - tobaccopaf)] # needed for formula to convert OR to RR
  C34incid[, site := NULL]
  
  
  C34surv <- fread("./Cancer Statistics/c34survival.csv", sep = ",", header = T, stringsAsFactors = F)  # Estimate survival
  for (j in c(4L, 5L, 6L, 8L:22L)) C34surv[, (j) := survival.fn(X1, X5, j - 2)]  # X1, X2,... X20 denote the percentage of survivors in years 1, 2,... 20
  C34surv[, `:=`(p1 = 1 - X1, 
                 p2 = (X1 - X2)/X1, 
                 p3 = (X2 - X3)/X2, 
                 p4 = (X3 - X4)/X3, 
                 p5 = (X4 - X5)/X4, 
                 p6 = (X5 - X6)/X5, 
                 p7 = (X6 - X7)/X6,
                 p8 = (X7 - X8)/X7, 
                 p9 = (X8 - X9)/X8, 
                 p10 = (X9 - X20)/(X9*10))]  # p1, p2, ... p9 is the probability of dying the 1st, 2nd etc year. 
  # p10 is the mean probability of death for years 10 to 20 after diagnosis
  C34surv[, `:=`(sex = as.factor(as.character(sex)), agegroup = as.ordered(as.character(agegroup)))]
}

if (exists("nam")) rm(nam)
if (exists("xx")) rm(xx)
#rm(Incidence2011, Incidence2010, Incidence2012, Incidence, j, diseasestoexclude.ca, population)




