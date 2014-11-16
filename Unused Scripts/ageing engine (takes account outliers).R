cat("Initiating ageing engine...\n\n NEEDS IMPROVEMENT \n\n")
breaks <- c(0, 1, seq(5, 85, 5), 130)
labels <- c("<1   ", "01-04", "05-09", "10-14", "15-19", "20-24", 
            "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
            "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
POP[, agegroup := cut(age, 
                      breaks = breaks, 
                      labels = labels, 
                      include.lowest = T, 
                      right = F, 
                      ordered_result = T)]
setkey(POP, age, sex, qimd)


# Slow, obsolete method sys bp example
# for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) {
#     POP[age==j, 
#         omsysval := sample(POPinit[age==j & sex==.BY[[1]] & qimd==.BY[[2]], omsysval], .N, replace=T), 
#         by=list(sex, qimd)]
# }
#hist(POP[age==60, omsysval])

# Systolic BP 
POP[,group := paste(qimd,sex,agegroup, sep='')] # I use agegroup instead of age because otherwise the groups are not identical (due to age progression in the POP and not in the POPinit )
POPinit[,group := paste(qimd,sex,agegroup, sep='')]
setkey(POP, group, age)
setkey(POPinit, group, age)

if (setequal(unique(POPinit[,group]), unique(POP[,group]))) { # If groups not the same ignore QIMD
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j, 
            omsysval := sample(POPinit[.BY[[1]], omsysval], .N, replace=F), 
            by=group]
    }
    
    # Cholesterol
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j, 
            cholval1 := sample(POPinit[.BY[[1]], cholval1], .N, replace=F), 
            by=group]
    }
    
    # F&V portions
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j, 
            porftvg := sample(POPinit[.BY[[1]], porftvg], .N, replace=F), 
            by=group]
    }
    
    # Alcohol
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j, 
            totalwug := sample(POPinit[.BY[[1]], totalwug], .N, replace=F), 
            by=group]
    }
    
    # Personality
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j, 
            segment := sample(POPinit[.BY[[1]], segment], .N, replace=F), 
            by=group]
    }
    
    # BMI
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j, 
            bmivalCat := sample(POPinit[.BY[[1]], bmivalCat], .N, replace=F), 
            by=group]
    }
    
    # Redmeat
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j, 
            redmeat := sample(POPinit[.BY[[1]], redmeat], .N, replace=F), 
            by=group]
    }
    
    # Exercise
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j, 
            t59su06 := sample(POPinit[.BY[[1]], t59su06], .N, replace=F), 
            by=group]
    }
} else {
    cat(paste("for ", nrow(POP[group %in% outersect(unique(POP[,group]), unique(POPinit[,group])),]), " people\n", sep=''))
    cat("Groups not equal, ignoring QIMD for part of the population!\n")
    # common groups as are handled as above
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j & group %in% unique(POPinit[,group]), 
            omsysval := sample(POPinit[.BY[[1]], omsysval], .N, replace=F), 
            by=group]
    }
    
    # Cholesterol
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j & group %in% unique(POPinit[,group]), 
            cholval1 := sample(POPinit[.BY[[1]], cholval1], .N, replace=F), 
            by=group]
    }
    
    # F&V portions
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j & group %in% unique(POPinit[,group]), 
            porftvg := sample(POPinit[.BY[[1]], porftvg], .N, replace=F), 
            by=group]
    }
    
    # Alcohol
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j & group %in% unique(POPinit[,group]), 
            totalwug := sample(POPinit[.BY[[1]], totalwug], .N, replace=F), 
            by=group]
    }
    
    # Personality
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j & group %in% unique(POPinit[,group]), 
            segment := sample(POPinit[.BY[[1]], segment], .N, replace=F), 
            by=group]
    }
    
    # BMI
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j & group %in% unique(POPinit[,group]), 
            bmivalCat := sample(POPinit[.BY[[1]], bmivalCat], .N, replace=F), 
            by=group]
    }
    
    # Redmeat
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j & group %in% unique(POPinit[,group]), 
            redmeat := sample(POPinit[.BY[[1]], redmeat], .N, replace=F), 
            by=group]
    }
    
    # Exercise
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j & group %in% unique(POPinit[,group]), 
            t59su06 := sample(POPinit[.BY[[1]], t59su06], .N, replace=F), 
            by=group]
    }
    
    # Re-group with less variables
    POP[,group := paste(sex,agegroup, sep='')]
    POPinit[,group := paste(sex,agegroup, sep='')]
    if (setequal(unique(POPinit[,group]), unique(POP[,group]))==F) {
        cat(nrow(POP[group %in% outersect(unique(POP[,group]), unique(POPinit[,group])),]))
        cat("Also ignoring sex information!\n")
        POP[,group := paste(agegroup, sep='')]
        POPinit[,group := paste(agegroup, sep='')]
    } 
    setkey(POP, group, age)
    setkey(POPinit, group, age)
    
    # Handle groups not containing in POPinit groups
    # Systolic blood pressure
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j & group %in% outersect(unique(POP[,group]), unique(POPinit[,group])), 
            omsysval := sample(POPinit[.BY[[1]], omsysval], .N, replace=F), 
            by=group]
    }
    
    # Cholesterol
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j & group %in% outersect(unique(POP[,group]), unique(POPinit[,group])), 
            cholval1 := sample(POPinit[.BY[[1]], cholval1], .N, replace=F), 
            by=group]
    }
    
    # F&V portions
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j & group %in% outersect(unique(POP[,group]), unique(POPinit[,group])), 
            porftvg := sample(POPinit[.BY[[1]], porftvg], .N, replace=F), 
            by=group]
    }
    
    # Alcohol
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j & group %in% outersect(unique(POP[,group]), unique(POPinit[,group])), 
            totalwug := sample(POPinit[.BY[[1]], totalwug], .N, replace=F), 
            by=group]
    }
    
    # Personality
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j & group %in% outersect(unique(POP[,group]), unique(POPinit[,group])), 
            segment := sample(POPinit[.BY[[1]], segment], .N, replace=F), 
            by=group]
    }
    
    # BMI
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j & group %in% outersect(unique(POP[,group]), unique(POPinit[,group])), 
            bmivalCat := sample(POPinit[.BY[[1]], bmivalCat], .N, replace=F), 
            by=group]
    }
    
    # Redmeat
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j & group %in% outersect(unique(POP[,group]), unique(POPinit[,group])), 
            redmeat := sample(POPinit[.BY[[1]], redmeat], .N, replace=F), 
            by=group]
    }
    
    # Exercise
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j & group %in% outersect(unique(POP[,group]), unique(POPinit[,group])), 
            t59su06 := sample(POPinit[.BY[[1]], t59su06], .N, replace=F), 
            by=group]
    }
} 

# Diabetes
POP[,group := paste(qimd,sex,agegroup,bmivalCat, sep='')]
POPinit[,group := paste(qimd,sex,agegroup,bmivalCat, sep='')]
setkey(POP, group, age)
setkey(POPinit, group, age)

if (setequal(unique(POPinit[,group]), unique(POP[,group]))) { 
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j, 
            diabete2r := sample(POPinit[.BY[[1]], diabete2r], .N, replace=F), 
            by=group]
    }
} else {
    cat(paste("for ", nrow(POP[group %in% outersect(unique(POP[,group]), unique(POPinit[,group])),]), " people\n", sep=''))
    cat("Groups not equal, ignoring QIMD for part of the population(BMI)!!\n")
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j & group %in% unique(POPinit[,group]), 
            diabete2r := sample(POPinit[.BY[[1]], diabete2r], .N, replace=F), 
            by=group]
    }
   
    POP[,group := paste(sex,agegroup,bmivalCat, sep='')]
    POPinit[,group := paste(sex,agegroup,bmivalCat, sep='')]
    if (setequal(unique(POPinit[,group]), unique(POP[,group]))==F) {
        cat("Also ignoring sex information!!\n")
        POP[,group := paste(agegroup,bmivalCat, sep='')]
        POPinit[,group := paste(agegroup,bmivalCat, sep='')]
    }
    if (setequal(unique(POPinit[,group]), unique(POP[,group]))==F) {
        cat("Also ignoring age information!!\n")
        POP[,group := paste(bmivalCat, sep='')]
        POPinit[,group := paste(bmivalCat, sep='')]
    }
    setkey(POP, group, age)
    setkey(POPinit, group, age)
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j & group %in% outersect(unique(POP[,group]), unique(POPinit[,group])), 
            diabete2r := sample(POPinit[.BY[[1]], diabete2r], .N, replace=F), 
            by=group]
    }
}

# SysBP dependent
POP[, omsysvalCat := cut(omsysval, 
                         breaks = c(0, 1, 130, 160, 300), 
                         labels = c("not applicable", "normotensive", "hypertensive", "severely hypertensive"), 
                         include.lowest = T, 
                         right = F, 
                         ordered_result = T)]

POP[,group := paste(qimd,sex,agegroup,omsysvalCat, sep='')]
POPinit[,group := paste(qimd,sex,agegroup,omsysvalCat, sep='')]
setkey(POP, group, age)
setkey(POPinit, group, age)
if (setequal(unique(POPinit[,group]), unique(POP[,group]))) { 
   
    # BP medication
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j, 
            bpmedc := sample(POPinit[.BY[[1]], bpmedc], .N, replace=F), 
            by=group]
    }
    
    
    # Diastolic BP
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j, 
            omdiaval := sample(POPinit[.BY[[1]], omdiaval], .N, replace=F), 
            by=group]
    }
    
    
    # Salt
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j, 
            saltWt := sample(POPinit[.BY[[1]], saltWt], .N, replace=F), 
            by=group]
    }
    
} else {  
    cat(paste("for ", nrow(POP[group %in% outersect(unique(POP[,group]), unique(POPinit[,group])),]), " people\n", sep=''))
    cat("Groups not equal, ignoring QIMD for part of the population (SBP)!!!\n")
    # BP medication
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j & group %in% unique(POPinit[,group]), 
            bpmedc := sample(POPinit[.BY[[1]], bpmedc], .N, replace=F), 
            by=group]
    }
    
    
    # Diastolic BP
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j & group %in% unique(POPinit[,group]), 
            omdiaval := sample(POPinit[.BY[[1]], omdiaval], .N, replace=F), 
            by=group]
    }
    
    
    # Salt
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j & group %in% unique(POPinit[,group]), 
            saltWt := sample(POPinit[.BY[[1]], saltWt], .N, replace=F), 
            by=group]
    }
    
    POP[,group := paste(sex,agegroup,omsysvalCat, sep='')]
    POPinit[,group := paste(sex,agegroup,omsysvalCat, sep='')]
    
    if (setequal(unique(POPinit[,group]), unique(POP[,group]))==F) {
        cat("Also ignoring sex!!!\n")
        POP[,group := paste(agegroup,omsysvalCat, sep='')]
        POPinit[,group := paste(agegroup,omsysvalCat, sep='')]
    }
    
    if (setequal(unique(POPinit[,group]), unique(POP[,group]))==F) {
        cat("Also ignoring age!!!\n")
        POP[,group := paste(omsysvalCat, sep='')]
        POPinit[,group := paste(omsysvalCat, sep='')]
    }
    setkey(POP, group, age)
    setkey(POPinit, group, age)
    
    # BP medication
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j & group %in% outersect(unique(POP[,group]), unique(POPinit[,group])), 
            bpmedc := sample(POPinit[.BY[[1]], bpmedc], .N, replace=F), 
            by=group]
    }
    
    
    # Diastolic BP
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j & group %in% outersect(unique(POP[,group]), unique(POPinit[,group])), 
            omdiaval := sample(POPinit[.BY[[1]], omdiaval], .N, replace=F), 
            by=group]
    }
    
    
    # Salt
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j & group %in% outersect(unique(POP[,group]), unique(POPinit[,group])), 
            saltWt := sample(POPinit[.BY[[1]], saltWt], .N, replace=F), 
            by=group]
    }
}



# Fruit portion
POP    [, group := paste(qimd, sex, agegroup, porftvg, sep='')]
POPinit[, group := paste(qimd, sex, agegroup, porftvg, sep='')]
setkey(POP, group, age)
setkey(POPinit, group, age)
if (setequal(unique(POPinit[,group]), unique(POP[,group]))) { 
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j, 
            frtpor := sample(POPinit[.BY[[1]], frtpor], .N, replace=F), 
            by=group]
    }
} else {
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j & group %in% unique(POPinit[,group]), 
            frtpor := sample(POPinit[.BY[[1]], frtpor], .N, replace=F), 
            by=group]
    }
    
    cat(paste("for ", nrow(POP[group %in% outersect(unique(POP[,group]), unique(POPinit[,group])),]), " people\n", sep=''))
    cat("Groups not equal, ignoring QIMD for part of the population(F&V)!!!!\n")
    POP[,group := paste(sex,agegroup,porftvg, sep='')]
    POPinit[,group := paste(sex,agegroup,porftvg, sep='')]
    if (setequal(unique(POPinit[,group]), unique(POP[,group])) == F) {
        cat("Also ignoring sex!!!!\n")
        POP[,group := paste(agegroup,porftvg, sep='')]
        POPinit[,group := paste(agegroup,porftvg, sep='')] 
    }
    if (setequal(unique(POPinit[,group]), unique(POP[,group])) == F) {
        cat("Also ignoring age!!!!\n")
        POP[,group := paste(porftvg, sep='')]
        POPinit[,group := paste(porftvg, sep='')] 
    }
    setkey(POP, group, age)
    setkey(POPinit, group, age)
    
    for (j in c(5, 15, 30, 40, 50, 60, 65, 70, 75)) { # suitable for domc in future time
        POP[age==j & group %in% outersect(unique(POP[,group]), unique(POPinit[,group])), 
            frtpor := sample(POPinit[.BY[[1]], frtpor], .N, replace=F), 
            by=group]
    }
}

# Smoking
POP[cigst1 %in% c("2", "3"), `:=`(endsmoke = endsmoke + 1)]
POP[cigst1 == "4", `:=`(packyears = packyears + cigdyalCat/20)]


POP[,group := NULL]
POPinit[,group := NULL]
setkey(POP, age, sex, agegroup, qimd)
setkey(POPinit, age, sex, agegroup, qimd)


