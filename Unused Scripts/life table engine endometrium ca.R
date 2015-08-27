# Lifetable engine

# q is the probability of dying by ANY CAUSE between ages x to x+1 Ndiseases is the number of deaths
# by the disease defined in diseasestoexclude variable qd is the probability of dying by a CAUSE
# OTHER THAN THE DISEASESTOEXCLUDE ones mqd is the mean probability of dying by a CAUSE OTHER THAN
# THE DISEASESTOEXCLUDE ones, for years 2010 to 2012

# import England and Wales mid-year population and mortality 2010-2012

load(file = "./Models/IMPACTncd/LifeTables/DeathsONS2010.RData")
load(file = "./Models/IMPACTncd/LifeTables/DeathsONS2011.RData")
load(file = "./Models/IMPACTncd/LifeTables/DeathsONS2012.RData")
load(file = "./Models/IMPACTncd/LifeTables/Lifetable2012m.RData")
load(file = "./Models/IMPACTncd/LifeTables/Lifetable2012f.RData")
load(file = "./Models/IMPACTncd/LifeTables/Lifetable2011m.RData")
load(file = "./Models/IMPACTncd/LifeTables/Lifetable2011f.RData")
load(file = "./Models/IMPACTncd/LifeTables/Lifetable2010m.RData")
load(file = "./Models/IMPACTncd/LifeTables/Lifetable2010f.RData")


# Create ICD code only with 3 first characters. (Will cause problems with Endometrium (need special
# case))

DeathsONS2010$ICD10short <- substr(DeathsONS2010[, 1], 1, 3)
DeathsONS2011$ICD10short <- substr(DeathsONS2011[, 1], 1, 3)
DeathsONS2012$ICD10short <- substr(DeathsONS2012[, 1], 1, 3)

# Add specific cause mortality

# Males 2010

DeathsDisease2010m0 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "1" & (DeathsONS2010[, 4] == "<1" | 
    DeathsONS2010[, 4] == "neonatal") & (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 
    1] == "C541")), 5])

DeathsDisease2010m1 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "1" & DeathsONS2010[, 4] == "01-04" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010m5 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "1" & DeathsONS2010[, 4] == "05-09" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010m10 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "1" & DeathsONS2010[, 4] == "10-14" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010m15 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "1" & DeathsONS2010[, 4] == "15-19" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010m20 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "1" & DeathsONS2010[, 4] == "20-24" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010m25 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "1" & DeathsONS2010[, 4] == "25-29" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010m30 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "1" & DeathsONS2010[, 4] == "30-34" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010m35 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "1" & DeathsONS2010[, 4] == "35-39" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010m40 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "1" & DeathsONS2010[, 4] == "40-44" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010m45 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "1" & DeathsONS2010[, 4] == "45-49" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010m50 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "1" & DeathsONS2010[, 4] == "50-54" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010m55 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "1" & DeathsONS2010[, 4] == "55-59" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010m60 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "1" & DeathsONS2010[, 4] == "60-64" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010m65 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "1" & DeathsONS2010[, 4] == "65-69" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010m70 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "1" & DeathsONS2010[, 4] == "70-74" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010m75 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "1" & DeathsONS2010[, 4] == "75-79" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010m80 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "1" & DeathsONS2010[, 4] == "80-84" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010m85 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "1" & DeathsONS2010[, 4] == "85+" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

Lifetable2010m$Ndiseases <- c(DeathsDisease2010m0, DeathsDisease2010m1, DeathsDisease2010m5, DeathsDisease2010m10, 
    DeathsDisease2010m15, DeathsDisease2010m20, DeathsDisease2010m25, DeathsDisease2010m30, DeathsDisease2010m35, 
    DeathsDisease2010m40, DeathsDisease2010m45, DeathsDisease2010m50, DeathsDisease2010m55, DeathsDisease2010m60, 
    DeathsDisease2010m65, DeathsDisease2010m70, DeathsDisease2010m75, DeathsDisease2010m80, DeathsDisease2010m85)

remove(DeathsDisease2010m0, DeathsDisease2010m1, DeathsDisease2010m5, DeathsDisease2010m10, DeathsDisease2010m15, 
    DeathsDisease2010m20, DeathsDisease2010m25, DeathsDisease2010m30, DeathsDisease2010m35, DeathsDisease2010m40, 
    DeathsDisease2010m45, DeathsDisease2010m50, DeathsDisease2010m55, DeathsDisease2010m60, DeathsDisease2010m65, 
    DeathsDisease2010m70, DeathsDisease2010m75, DeathsDisease2010m80, DeathsDisease2010m85)

# Females 2010

DeathsDisease2010f0 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "2" & (DeathsONS2010[, 4] == "<1" | 
    DeathsONS2010[, 4] == "neonatal") & (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 
    1] == "C541")), 5])

DeathsDisease2010f1 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "2" & DeathsONS2010[, 4] == "01-04" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010f5 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "2" & DeathsONS2010[, 4] == "05-09" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010f10 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "2" & DeathsONS2010[, 4] == "10-14" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010f15 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "2" & DeathsONS2010[, 4] == "15-19" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010f20 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "2" & DeathsONS2010[, 4] == "20-24" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010f25 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "2" & DeathsONS2010[, 4] == "25-29" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010f30 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "2" & DeathsONS2010[, 4] == "30-34" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010f35 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "2" & DeathsONS2010[, 4] == "35-39" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010f40 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "2" & DeathsONS2010[, 4] == "40-44" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010f45 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "2" & DeathsONS2010[, 4] == "45-49" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010f50 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "2" & DeathsONS2010[, 4] == "50-54" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010f55 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "2" & DeathsONS2010[, 4] == "55-59" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010f60 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "2" & DeathsONS2010[, 4] == "60-64" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010f65 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "2" & DeathsONS2010[, 4] == "65-69" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010f70 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "2" & DeathsONS2010[, 4] == "70-74" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010f75 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "2" & DeathsONS2010[, 4] == "75-79" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010f80 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "2" & DeathsONS2010[, 4] == "80-84" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

DeathsDisease2010f85 <- sum(DeathsONS2010[which(DeathsONS2010[, 3] == "2" & DeathsONS2010[, 4] == "85+" & 
    (DeathsONS2010[, 6] %in% diseasestoexclude | DeathsONS2010[, 1] == "C541")), 5])

Lifetable2010f$Ndiseases <- c(DeathsDisease2010f0, DeathsDisease2010f1, DeathsDisease2010f5, DeathsDisease2010f10, 
    DeathsDisease2010f15, DeathsDisease2010f20, DeathsDisease2010f25, DeathsDisease2010f30, DeathsDisease2010f35, 
    DeathsDisease2010f40, DeathsDisease2010f45, DeathsDisease2010f50, DeathsDisease2010f55, DeathsDisease2010f60, 
    DeathsDisease2010f65, DeathsDisease2010f70, DeathsDisease2010f75, DeathsDisease2010f80, DeathsDisease2010f85)

remove(DeathsDisease2010f0, DeathsDisease2010f1, DeathsDisease2010f5, DeathsDisease2010f10, DeathsDisease2010f15, 
    DeathsDisease2010f20, DeathsDisease2010f25, DeathsDisease2010f30, DeathsDisease2010f35, DeathsDisease2010f40, 
    DeathsDisease2010f45, DeathsDisease2010f50, DeathsDisease2010f55, DeathsDisease2010f60, DeathsDisease2010f65, 
    DeathsDisease2010f70, DeathsDisease2010f75, DeathsDisease2010f80, DeathsDisease2010f85)


# Males 2011

DeathsDisease2011m0 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "1" & (DeathsONS2011[, 4] == "<1" | 
    DeathsONS2011[, 4] == "neonatal") & (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 
    1] == "C541")), 5])

DeathsDisease2011m1 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "1" & DeathsONS2011[, 4] == "01-04" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011m5 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "1" & DeathsONS2011[, 4] == "05-09" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011m10 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "1" & DeathsONS2011[, 4] == "10-14" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011m15 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "1" & DeathsONS2011[, 4] == "15-19" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011m20 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "1" & DeathsONS2011[, 4] == "20-24" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011m25 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "1" & DeathsONS2011[, 4] == "25-29" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011m30 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "1" & DeathsONS2011[, 4] == "30-34" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011m35 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "1" & DeathsONS2011[, 4] == "35-39" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011m40 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "1" & DeathsONS2011[, 4] == "40-44" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011m45 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "1" & DeathsONS2011[, 4] == "45-49" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011m50 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "1" & DeathsONS2011[, 4] == "50-54" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011m55 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "1" & DeathsONS2011[, 4] == "55-59" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011m60 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "1" & DeathsONS2011[, 4] == "60-64" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011m65 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "1" & DeathsONS2011[, 4] == "65-69" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011m70 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "1" & DeathsONS2011[, 4] == "70-74" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011m75 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "1" & DeathsONS2011[, 4] == "75-79" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011m80 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "1" & DeathsONS2011[, 4] == "80-84" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011m85 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "1" & DeathsONS2011[, 4] == "85+" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

Lifetable2011m$Ndiseases <- c(DeathsDisease2011m0, DeathsDisease2011m1, DeathsDisease2011m5, DeathsDisease2011m10, 
    DeathsDisease2011m15, DeathsDisease2011m20, DeathsDisease2011m25, DeathsDisease2011m30, DeathsDisease2011m35, 
    DeathsDisease2011m40, DeathsDisease2011m45, DeathsDisease2011m50, DeathsDisease2011m55, DeathsDisease2011m60, 
    DeathsDisease2011m65, DeathsDisease2011m70, DeathsDisease2011m75, DeathsDisease2011m80, DeathsDisease2011m85)

remove(DeathsDisease2011m0, DeathsDisease2011m1, DeathsDisease2011m5, DeathsDisease2011m10, DeathsDisease2011m15, 
    DeathsDisease2011m20, DeathsDisease2011m25, DeathsDisease2011m30, DeathsDisease2011m35, DeathsDisease2011m40, 
    DeathsDisease2011m45, DeathsDisease2011m50, DeathsDisease2011m55, DeathsDisease2011m60, DeathsDisease2011m65, 
    DeathsDisease2011m70, DeathsDisease2011m75, DeathsDisease2011m80, DeathsDisease2011m85)

# Females 2011

DeathsDisease2011f0 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "2" & (DeathsONS2011[, 4] == "<1" | 
    DeathsONS2011[, 4] == "neonatal") & (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 
    1] == "C541")), 5])

DeathsDisease2011f1 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "2" & DeathsONS2011[, 4] == "01-04" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011f5 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "2" & DeathsONS2011[, 4] == "05-09" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011f10 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "2" & DeathsONS2011[, 4] == "10-14" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011f15 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "2" & DeathsONS2011[, 4] == "15-19" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011f20 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "2" & DeathsONS2011[, 4] == "20-24" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011f25 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "2" & DeathsONS2011[, 4] == "25-29" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011f30 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "2" & DeathsONS2011[, 4] == "30-34" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011f35 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "2" & DeathsONS2011[, 4] == "35-39" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011f40 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "2" & DeathsONS2011[, 4] == "40-44" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011f45 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "2" & DeathsONS2011[, 4] == "45-49" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011f50 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "2" & DeathsONS2011[, 4] == "50-54" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011f55 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "2" & DeathsONS2011[, 4] == "55-59" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011f60 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "2" & DeathsONS2011[, 4] == "60-64" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011f65 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "2" & DeathsONS2011[, 4] == "65-69" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011f70 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "2" & DeathsONS2011[, 4] == "70-74" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011f75 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "2" & DeathsONS2011[, 4] == "75-79" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011f80 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "2" & DeathsONS2011[, 4] == "80-84" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

DeathsDisease2011f85 <- sum(DeathsONS2011[which(DeathsONS2011[, 3] == "2" & DeathsONS2011[, 4] == "85+" & 
    (DeathsONS2011[, 6] %in% diseasestoexclude | DeathsONS2011[, 1] == "C541")), 5])

Lifetable2011f$Ndiseases <- c(DeathsDisease2011f0, DeathsDisease2011f1, DeathsDisease2011f5, DeathsDisease2011f10, 
    DeathsDisease2011f15, DeathsDisease2011f20, DeathsDisease2011f25, DeathsDisease2011f30, DeathsDisease2011f35, 
    DeathsDisease2011f40, DeathsDisease2011f45, DeathsDisease2011f50, DeathsDisease2011f55, DeathsDisease2011f60, 
    DeathsDisease2011f65, DeathsDisease2011f70, DeathsDisease2011f75, DeathsDisease2011f80, DeathsDisease2011f85)

remove(DeathsDisease2011f0, DeathsDisease2011f1, DeathsDisease2011f5, DeathsDisease2011f10, DeathsDisease2011f15, 
    DeathsDisease2011f20, DeathsDisease2011f25, DeathsDisease2011f30, DeathsDisease2011f35, DeathsDisease2011f40, 
    DeathsDisease2011f45, DeathsDisease2011f50, DeathsDisease2011f55, DeathsDisease2011f60, DeathsDisease2011f65, 
    DeathsDisease2011f70, DeathsDisease2011f75, DeathsDisease2011f80, DeathsDisease2011f85)


# Males 2012

DeathsDisease2012m0 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "1" & (DeathsONS2012[, 4] == "<1" | 
    DeathsONS2012[, 4] == "neonatal") & (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 
    1] == "C541")), 5])

DeathsDisease2012m1 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "1" & DeathsONS2012[, 4] == "01-04" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012m5 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "1" & DeathsONS2012[, 4] == "05-09" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012m10 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "1" & DeathsONS2012[, 4] == "10-14" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012m15 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "1" & DeathsONS2012[, 4] == "15-19" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012m20 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "1" & DeathsONS2012[, 4] == "20-24" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012m25 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "1" & DeathsONS2012[, 4] == "25-29" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012m30 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "1" & DeathsONS2012[, 4] == "30-34" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012m35 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "1" & DeathsONS2012[, 4] == "35-39" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012m40 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "1" & DeathsONS2012[, 4] == "40-44" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012m45 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "1" & DeathsONS2012[, 4] == "45-49" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012m50 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "1" & DeathsONS2012[, 4] == "50-54" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012m55 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "1" & DeathsONS2012[, 4] == "55-59" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012m60 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "1" & DeathsONS2012[, 4] == "60-64" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012m65 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "1" & DeathsONS2012[, 4] == "65-69" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012m70 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "1" & DeathsONS2012[, 4] == "70-74" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012m75 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "1" & DeathsONS2012[, 4] == "75-79" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012m80 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "1" & DeathsONS2012[, 4] == "80-84" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012m85 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "1" & DeathsONS2012[, 4] == "85+" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

Lifetable2012m$Ndiseases <- c(DeathsDisease2012m0, DeathsDisease2012m1, DeathsDisease2012m5, DeathsDisease2012m10, 
    DeathsDisease2012m15, DeathsDisease2012m20, DeathsDisease2012m25, DeathsDisease2012m30, DeathsDisease2012m35, 
    DeathsDisease2012m40, DeathsDisease2012m45, DeathsDisease2012m50, DeathsDisease2012m55, DeathsDisease2012m60, 
    DeathsDisease2012m65, DeathsDisease2012m70, DeathsDisease2012m75, DeathsDisease2012m80, DeathsDisease2012m85)

remove(DeathsDisease2012m0, DeathsDisease2012m1, DeathsDisease2012m5, DeathsDisease2012m10, DeathsDisease2012m15, 
    DeathsDisease2012m20, DeathsDisease2012m25, DeathsDisease2012m30, DeathsDisease2012m35, DeathsDisease2012m40, 
    DeathsDisease2012m45, DeathsDisease2012m50, DeathsDisease2012m55, DeathsDisease2012m60, DeathsDisease2012m65, 
    DeathsDisease2012m70, DeathsDisease2012m75, DeathsDisease2012m80, DeathsDisease2012m85)

# Females 2012

DeathsDisease2012f0 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "2" & (DeathsONS2012[, 4] == "<1" | 
    DeathsONS2012[, 4] == "neonatal") & (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 
    1] == "C541")), 5])

DeathsDisease2012f1 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "2" & DeathsONS2012[, 4] == "01-04" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012f5 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "2" & DeathsONS2012[, 4] == "05-09" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012f10 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "2" & DeathsONS2012[, 4] == "10-14" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012f15 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "2" & DeathsONS2012[, 4] == "15-19" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012f20 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "2" & DeathsONS2012[, 4] == "20-24" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012f25 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "2" & DeathsONS2012[, 4] == "25-29" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012f30 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "2" & DeathsONS2012[, 4] == "30-34" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012f35 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "2" & DeathsONS2012[, 4] == "35-39" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012f40 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "2" & DeathsONS2012[, 4] == "40-44" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012f45 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "2" & DeathsONS2012[, 4] == "45-49" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012f50 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "2" & DeathsONS2012[, 4] == "50-54" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012f55 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "2" & DeathsONS2012[, 4] == "55-59" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012f60 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "2" & DeathsONS2012[, 4] == "60-64" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012f65 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "2" & DeathsONS2012[, 4] == "65-69" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012f70 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "2" & DeathsONS2012[, 4] == "70-74" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012f75 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "2" & DeathsONS2012[, 4] == "75-79" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012f80 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "2" & DeathsONS2012[, 4] == "80-84" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

DeathsDisease2012f85 <- sum(DeathsONS2012[which(DeathsONS2012[, 3] == "2" & DeathsONS2012[, 4] == "85+" & 
    (DeathsONS2012[, 6] %in% diseasestoexclude | DeathsONS2012[, 1] == "C541")), 5])

Lifetable2012f$Ndiseases <- c(DeathsDisease2012f0, DeathsDisease2012f1, DeathsDisease2012f5, DeathsDisease2012f10, 
    DeathsDisease2012f15, DeathsDisease2012f20, DeathsDisease2012f25, DeathsDisease2012f30, DeathsDisease2012f35, 
    DeathsDisease2012f40, DeathsDisease2012f45, DeathsDisease2012f50, DeathsDisease2012f55, DeathsDisease2012f60, 
    DeathsDisease2012f65, DeathsDisease2012f70, DeathsDisease2012f75, DeathsDisease2012f80, DeathsDisease2012f85)

remove(DeathsDisease2012f0, DeathsDisease2012f1, DeathsDisease2012f5, DeathsDisease2012f10, DeathsDisease2012f15, 
    DeathsDisease2012f20, DeathsDisease2012f25, DeathsDisease2012f30, DeathsDisease2012f35, DeathsDisease2012f40, 
    DeathsDisease2012f45, DeathsDisease2012f50, DeathsDisease2012f55, DeathsDisease2012f60, DeathsDisease2012f65, 
    DeathsDisease2012f70, DeathsDisease2012f75, DeathsDisease2012f80, DeathsDisease2012f85)


# Calculate q without the disease qd

Lifetable2010m$qd <- (Lifetable2010m$Ndeaths - Lifetable2010m$Ndiseases)/Lifetable2010m$pops
Lifetable2010f$qd <- (Lifetable2010f$Ndeaths - Lifetable2010f$Ndiseases)/Lifetable2010f$pops
Lifetable2011m$qd <- (Lifetable2011m$Ndeaths - Lifetable2011m$Ndiseases)/Lifetable2011m$pops
Lifetable2011f$qd <- (Lifetable2011f$Ndeaths - Lifetable2011f$Ndiseases)/Lifetable2011f$pops
Lifetable2012m$qd <- (Lifetable2012m$Ndeaths - Lifetable2012m$Ndiseases)/Lifetable2012m$pops
Lifetable2012f$qd <- (Lifetable2012f$Ndeaths - Lifetable2012f$Ndiseases)/Lifetable2012f$pops

# Calculate mean qd of the 3 years mqd (not accuarate method due to truncation (54bit) but good
# enough for the job)
Lifetable2010m$mqd <- (Lifetable2010m$qd + Lifetable2011m$qd + Lifetable2012m$qd)/3
Lifetable2010f$mqd <- (Lifetable2010f$qd + Lifetable2011f$qd + Lifetable2012f$qd)/3
Lifetable2011m$mqd <- (Lifetable2010m$qd + Lifetable2011m$qd + Lifetable2012m$qd)/3
Lifetable2011f$mqd <- (Lifetable2010f$qd + Lifetable2011f$qd + Lifetable2012f$qd)/3
Lifetable2012m$mqd <- (Lifetable2010m$qd + Lifetable2011m$qd + Lifetable2012m$qd)/3
Lifetable2012f$mqd <- (Lifetable2010f$qd + Lifetable2011f$qd + Lifetable2012f$qd)/3

# export Lifetable

Lifetable <- rbind(Lifetable2010m, Lifetable2010f, Lifetable2011m, Lifetable2011f, Lifetable2012m, Lifetable2012f)
row.names(Lifetable) <- NULL

# Garbage cleaning

rm(Lifetable2010m, Lifetable2010f, Lifetable2011m, Lifetable2011f, Lifetable2012m, Lifetable2012f, DeathsONS2010, 
    DeathsONS2011, DeathsONS2012)
 
