#cmpfile("./cancer statistics.R") # for cancer
# For year 2011
Incidence2011 <- fread("./Cancer Statistics/2011 cases.csv", 
                       sep = ",", 
                       header = T, 
                       stringsAsFactors = T)
setnames(Incidence2011, c("site", "sex", "<1   ", "01-04", "05-09", "10-14", "15-19", "20-24", "25-29", 
    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", 
    "85+"))
for (j in seq_len(ncol(Incidence2011))) set(Incidence2011, which(is.na(Incidence2011[[j]])), j, 0)  # replace NA with 0

ONSpop2011 <- fread("./Cancer Statistics/ONSpopulation2011.csv", 
                    sep = ",",
                    header = T, 
                    stringsAsFactors = T)
setnames(ONSpop2011, c("site", "sex", "<1   ", "01-04", "05-09", "10-14", "15-19", "20-24", "25-29", 
    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", 
    "85+"))



Incidence2011[, site := str_trim(site, side = "both")]

Incidence2011 <- rbind(Incidence2011, ONSpop2011)
for (j in 3L:21L) set(Incidence2011, i = NULL, j, Incidence2011[[j]]/ONSpop2011[[j]]) # Calculate incidence rates by site, agegroup, and sex



# For year 2010
Incidence2010 <- fread("./Cancer Statistics/2010 cases.csv",
                       sep = ",",
                       header = T, 
                       stringsAsFactors = T)
setnames(Incidence2010, c("site", "sex", "<1   ", "01-04", "05-09", "10-14", "15-19", "20-24", "25-29", 
    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", 
    "85+"))
for (j in seq_len(ncol(Incidence2010))) set(Incidence2010, which(is.na(Incidence2010[[j]])), j, 0)  # replace NA with 0

ONSpop2010 <- fread("./Cancer Statistics/ONSpopulation2010.csv",
                    sep = ",",
                    header = T, 
                    stringsAsFactors = T)
setnames(ONSpop2010, c("site", "sex", "<1   ", "01-04", "05-09", "10-14", "15-19", "20-24", "25-29", 
    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", 
    "85+"))



Incidence2010[, site := str_trim(site, side = "both")]

Incidence2010 <- rbind(Incidence2010, ONSpop2010)
for (j in 3L:21L) set(Incidence2010, i = NULL, j, Incidence2010[[j]]/ONSpop2010[[j]]) # Calculate incidence rates by site, agegroup, and sex




#For year 2009
Incidence2009 <- fread("./Cancer Statistics/2009 cases.csv", 
                       sep = ",",
                       header = T, 
                       stringsAsFactors = T)
setnames(Incidence2009, c("site", "sex", "<1   ", "01-04", "05-09", "10-14", "15-19", "20-24", "25-29", 
    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", 
    "85+"))
for (j in seq_len(ncol(Incidence2009))) set(Incidence2009, which(is.na(Incidence2009[[j]])), j, 0)  # replace NA with 0

ONSpop2009 <- fread("./Cancer Statistics/ONSpopulation2009.csv", 
                    sep = ",",
                    header = T, 
                    stringsAsFactors = T)
setnames(ONSpop2009, c("site", "sex", "<1   ", "01-04", "05-09", "10-14", "15-19", "20-24", "25-29", 
    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", 
    "85+"))



Incidence2009[, site := str_trim(site, side = "both")]

Incidence2009 <- rbind(Incidence2009, ONSpop2009)
for (j in 3L:21L) set(Incidence2009, i = NULL, j, Incidence2009[[j]]/ONSpop2009[[j]]) # Calculate incidence rates by site, agegroup, and sex


Incidence <- rbind(Incidence2009, Incidence2010, Incidence2011)

Incidence[, group := paste(site, sex, sep = "")]

for (j in 3L:21L) Incidence[, (j) := sum(.SD[[j]]/3), by = group] # calculate mean incidence rate from the three years (2011, 2010, 2009)

Incidence <- unique(Incidence, by = "group")
Incidence[, `:=`(group = NULL)]
Incidence[, `:=`(sex = as.factor(as.character(sex)))]
setkey(Incidence, site)

# Create datatables in the form of "site"incid based on the diseases to be modelled, from the Incidence data
for (j in diseasestoexclude) {
    nam <- paste0(j, "incid")
    assign(nam, melt(Incidence[site == j], 
                     id.vars = c("site", "sex"), 
                     variable.name = "agegroup", 
                     value.name = "incidence"))
    rm(nam)
}

# Estimate P0(incidence if all risks at recommended (optimal) level) and survival
if ("C34" %in% diseasestoexclude) {
    C34tobaccopaf <- fread("./Cancer Statistics/c34tobaccopaf.csv", 
                           sep = ",", 
                           header = T, 
                           stringsAsFactors = T)
    C34tobaccopaf[, `:=`(sex = as.factor(as.character(sex)), 
                         agegroup = as.ordered(as.character(agegroup)))]
    setkey(C34tobaccopaf, sex, agegroup)
    
    C34fruitpaf <- fread("./Cancer Statistics/c34fruitpaf.csv", 
                         sep = ",", 
                         header = T, 
                         stringsAsFactors = T)
    C34fruitpaf[, `:=`(sex = as.factor(as.character(sex)), 
                       agegroup = as.ordered(as.character(agegroup)))]
    setkey(C34fruitpaf, sex, agegroup)
    
    C34etspaf <- fread("./Cancer Statistics/c34etspaf.csv", 
                       sep = ",", 
                       header = T, 
                       stringsAsFactors = T)
    C34etspaf[, `:=`(sex = as.factor(as.character(sex)), 
                     agegroup = as.ordered(as.character(agegroup)))]
    setkey(C34etspaf, sex, agegroup)
    
    setkey(C34incid, sex, agegroup, incidence)
    
    C34incid[C34tobaccopaf[C34etspaf[C34fruitpaf]], p0 := incidence * (1 - tobaccopaf) * (1 - etspaf) * (1 - fruitpaf)]
    C34incid[C34tobaccopaf[C34etspaf[C34fruitpaf]], p0tobonly := incidence * (1 - tobaccopaf)] # needed for formula to convert OR to RR
    C34incid[, site := NULL]
    
    
    C34surv <- fread("./Cancer Statistics/c34survival.csv", sep = ",", header = T, stringsAsFactors = T)  # Estimate survival
    for (j in c(4L, 5L, 6L, 8L:22L)) C34surv[, (j) := hyperbola(X1, X5, j - 2)]  # X1, X2,... X20 denote the percentage of survivors in years 1, 2,... 20
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

rm(ONSpop2011, ONSpop2010, ONSpop2009, Incidence2011, Incidence2010, Incidence2009, Incidence, j)



 
