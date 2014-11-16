
Incidence2011 <- fread("./Models/IMPACTncd/Cancer Statistics/2011 cases.csv", sep=",", header=T, stringsAsFactors=T)
setnames(Incidence2011, c("site", "sex", "<1   ", "01-04", "05-09", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"))
for (j in seq_len(ncol(Incidence2011))) set(Incidence2011, which(is.na(Incidence2011[[j]])), j, 0) # replace NA with 0

ONSpop2011 <- fread("./Models/IMPACTncd/Cancer Statistics/ONSpopulation2011.csv", sep=",", header=T, stringsAsFactors=T)
setnames(ONSpop2011, c("site", "sex", "<1   ", "01-04", "05-09", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"))



Incidence2011[,site:= str_trim(Incidence2011[,site], side="both")]

Incidence2011 <- data.table(rbind_all(list(Incidence2011, ONSpop2011)))
for (j in 3L:21L) set(Incidence2011, i=NULL, j, Incidence2011[[j]]/ONSpop2011[[j]])




Incidence2010 <- fread("./Models/IMPACTncd/Cancer Statistics/2010 cases.csv", sep=",", header=T, stringsAsFactors=T)
setnames(Incidence2010, c("site", "sex", "<1   ", "01-04", "05-09", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"))
for (j in seq_len(ncol(Incidence2010))) set(Incidence2010, which(is.na(Incidence2010[[j]])), j, 0) # replace NA with 0

ONSpop2010 <- fread("./Models/IMPACTncd/Cancer Statistics/ONSpopulation2010.csv", sep=",", header=T, stringsAsFactors=T)
setnames(ONSpop2010, c("site", "sex", "<1   ", "01-04", "05-09", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"))



Incidence2010[,site:= str_trim(Incidence2010[,site], side="both")]

Incidence2010 <- data.table(rbind_all(list(Incidence2010, ONSpop2010)))
for (j in 3L:21L) set(Incidence2010, i=NULL, j, Incidence2010[[j]]/ONSpop2010[[j]])





Incidence2009 <- fread("./Models/IMPACTncd/Cancer Statistics/2009 cases.csv", sep=",", header=T, stringsAsFactors=T)
setnames(Incidence2009, c("site", "sex", "<1   ", "01-04", "05-09", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"))
for (j in seq_len(ncol(Incidence2009))) set(Incidence2009, which(is.na(Incidence2009[[j]])), j, 0) # replace NA with 0

ONSpop2009 <- fread("./Models/IMPACTncd/Cancer Statistics/ONSpopulation2009.csv", sep=",", header=T, stringsAsFactors=T)
setnames(ONSpop2009, c("site", "sex", "<1   ", "01-04", "05-09", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"))



Incidence2009[,site:= str_trim(Incidence2009[,site], side="both")]

Incidence2009 <- data.table(rbind_all(list(Incidence2009, ONSpop2009)))
for (j in 3L:21L) set(Incidence2009, i=NULL, j, Incidence2009[[j]]/ONSpop2009[[j]])


Incidence <- data.table(rbind_all(list(Incidence2009, Incidence2010, Incidence2011)))

Incidence[,group:=paste(site,sex,sep="")]

for (j in 3L:21L) Incidence[, (j):=sum(.SD[[j]])/3 , by=group]

Incidence= copy(unique(Incidence, by="group"))
Incidence[,group:=NULL]
Incidence[,sex:=as.factor(as.character(sex))]
setkey(Incidence, site)

for (j in diseasestoexclude) {
  nam <- paste(j,"incid", sep = "")
  assign(nam, melt(filter(Incidence, site == j), id.vars=c("site", "sex"), variable.name="agegroup", value.name="incidence"))
  rm(nam)
}

# Estimate P0(incidence if all risks at recommended (optimal) level) and survival

if ("C34" %in% diseasestoexclude) {
  C34tobaccopaf <- fread("./Models/IMPACTncd/Cancer Statistics/c34tobaccopaf.csv", sep=",", header=T, stringsAsFactors=T)
  C34tobaccopaf[, sex:= as.factor(as.character(sex))]
  C34tobaccopaf[, agegroup:= as.factor(as.character(agegroup))]
  setkey(C34tobaccopaf, agegroup, sex)
  
  C34fruitpaf <- fread("./Models/IMPACTncd/Cancer Statistics/c34fruitpaf.csv", sep=",", header=T, stringsAsFactors=T)
  C34fruitpaf[, sex:= as.factor(as.character(sex))]
  C34fruitpaf[, agegroup:= as.factor(as.character(agegroup))]
  setkey(C34fruitpaf, agegroup, sex)
  
  setkey(C34incid, agegroup, sex, incidence)
  C34incid[C34tobaccopaf[C34fruitpaf], p0:=incidence*(1-tobaccopaf)*(1-fruitpaf)]
  C34incid[,site:=NULL]
  
  
  C34surv <- fread("./Models/IMPACTncd/Cancer Statistics/c34survival.csv", sep=",", header=T, stringsAsFactors=T) # Estimate survival
  for (j in c(4L,5L,6L,8L:22L)) C34surv[, (j):= hyperbola(X1, X5, j-2)]
}





rm(ONSpop2011, ONSpop2010, ONSpop2009, Incidence2011, Incidence2010, Incidence2009, Incidence)




