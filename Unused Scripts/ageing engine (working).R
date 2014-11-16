cat("Initiating lag/ageing engine...\n\n")
setkey(POP, age, sex, qimd)

# Slow, obsolete method sys bp example
# for (j in mature.age) {
#     POP[age==j, 
#         omsysval := sample(SPOP2011[age==j & sex==.BY[[1L]] & qimd==.BY[[2]], omsysval], .N, replace=T), 
#         by=list(sex, qimd)]
# }
#hist(POP[age==60, omsysval])
if (i >0) {
    # Systolic BP 
    POP[age<20, group:= "undefined"]
    SPOP2011[age<20, group:= "undefined"]
    setkey(POP, group, age)
    setkey(SPOP2011, group, age)
    mature.age <- c(20, 50) # define ages that physiological factors will evolve
    if (setequal(unique(SPOP2011[,group]), unique(POP[,group]))) { # If groups not the same ignore QIMD
        for (j in mature.age) { # suitable for domc in future time
            # SBP
            POP[age==j, 
                omsysval := sample(SPOP2011[.BY[[1L]], omsysval], .N, replace=T), 
                by=group]
        }
        
        # Cholesterol
        for (j in mature.age) {
            POP[age==j, 
                cholval := sample(SPOP2011[.BY[[1L]], cholval], .N, replace=T), 
                by=group]
        }
        
        # F&V portions
        for (j in mature.age) { 
            POP[age==j, 
                porftvg := sample(SPOP2011[.BY[[1L]], porftvg], .N, replace=T), 
                by=group]
        }
        
        # Alcohol
        #     for (j in mature.age) { # suitable for domc in future time
        #         POP[age==j, 
        #             totalwug := sample(SPOP2011[.BY[[1L]], totalwug], .N, replace=T), 
        #             by=group]
        #     }
        
        # Personality
        for (j in mature.age) { # suitable for domc in future time
            POP[age==j, 
                segment := sample(SPOP2011[.BY[[1L]], segment], .N, replace=T), 
                by=group]
        }
        
        # BMI
        for (j in mature.age) { # suitable for domc in future time
            POP[age==j, 
                bmivalCat := sample(SPOP2011[.BY[[1L]], bmivalCat], .N, replace=T), 
                by=group]
        }
        
        # Redmeat
        for (j in mature.age) { # suitable for domc in future time
            POP[age==j, 
                redmeat := sample(SPOP2011[.BY[[1L]], redmeat], .N, replace=T), 
                by=group]
        }
        
        # Exercise
        for (j in mature.age) { # suitable for domc in future time
            POP[age==j, 
                t59su06 := sample(SPOP2011[.BY[[1L]], t59su06], .N, replace=T), 
                by=group]
        }
    } else {
        # SBP
        cat(paste("No match for ", nrow(POP[group %in% outersect(unique(POP[,group]), unique(SPOP2011[,group])),]), " people\n", sep=''))
        # common groups as are handled as above
        for (j in mature.age) {
            POP[age==j & group %in% unique(SPOP2011[,group]), 
                omsysval := sample(SPOP2011[.BY[[1L]], omsysval], .N, replace=T), 
                by=group]
        }
        
        # Cholesterol
        for (j in mature.age) { 
            POP[age==j & group %in% unique(SPOP2011[,group]), 
                cholval := sample(SPOP2011[.BY[[1L]], cholval], .N, replace=T), 
                by=group]
        }
        
        # F&V portions
        for (j in mature.age) { 
            POP[age==j & group %in% unique(SPOP2011[,group]), 
                porftvg := sample(SPOP2011[.BY[[1L]], porftvg], .N, replace=T), 
                by=group]
        }
        
        #     # Alcohol
        #     for (j in mature.age) { # suitable for domc in future time
        #         POP[age==j & group %in% unique(SPOP2011[,group]), 
        #             totalwug := sample(SPOP2011[.BY[[1L]], totalwug], .N, replace=T), 
        #             by=group]
        #     }
        
        # Personality
        for (j in mature.age) { 
            POP[age==j & group %in% unique(SPOP2011[,group]), 
                segment := sample(SPOP2011[.BY[[1L]], segment], .N, replace=T), 
                by=group]
        }
        
        # BMI
        for (j in mature.age) {
            POP[age==j & group %in% unique(SPOP2011[,group]), 
                bmivalCat := sample(SPOP2011[.BY[[1L]], bmivalCat], .N, replace=T), 
                by=group]
        }
        
        # Redmeat
        for (j in mature.age) { 
            POP[age==j & group %in% unique(SPOP2011[,group]), 
                redmeat := sample(SPOP2011[.BY[[1L]], redmeat], .N, replace=T), 
                by=group]
        }
        
        # Exercise
        for (j in mature.age) { 
            POP[age==j & group %in% unique(SPOP2011[,group]), 
                t59su06 := sample(SPOP2011[.BY[[1L]], t59su06], .N, replace=T), 
                by=group]
        }
    } 
    
    # Diabetes
    # POP[,group := paste(qimd,sex,agegroup,bmivalCat, sep='')]
    # SPOP2011[,group := paste(qimd,sex,agegroup,bmivalCat, sep='')]
    # setkey(POP, group, age)
    # setkey(SPOP2011, group, age)
    # 
    # if (setequal(unique(SPOP2011[,group]), unique(POP[,group]))) { 
    #     for (j in mature.age) { # suitable for domc in future time
    #         POP[age==j, 
    #             diabtotr := sample(SPOP2011[.BY[[1L]], diabtotr], .N, replace=T), 
    #             by=group]
    #     }
    # } else {
    #     cat(paste("No match for ", nrow(POP[group %in% outersect(unique(POP[,group]), unique(SPOP2011[,group])),]), " people\n", sep=''))
    #     for (j in mature.age) { # suitable for domc in future time
    #         POP[age==j & group %in% unique(SPOP2011[,group]), 
    #             diabtotr := sample(SPOP2011[.BY[[1L]], diabtotr], .N, replace=T), 
    #             by=group]
    #     }
    # }
    
    # SysBP dependent
    POP[, omsysvalCat := cut(omsysval, 
                             breaks = c(0, 130, 160, Inf), 
                             labels = c("normotensive", "hypertensive", "severely hypertensive"), 
                             include.lowest = T, 
                             right = F, 
                             ordered_result = T)]
    
    POP[,group := paste(qimd,sex,agegroup,omsysvalCat, sep='')]
    SPOP2011[,group := paste(qimd,sex,agegroup,omsysvalCat, sep='')]
    POP[age<20, group:= "undefined"]
    SPOP2011[age<20, group:= "undefined"]
    setkey(POP, group, age)
    setkey(SPOP2011, group, age)
    if (setequal(unique(SPOP2011[,group]), unique(POP[,group]))) { 
        
        # BP medication
        for (j in mature.age) { # suitable for domc in future time
            POP[age==j, 
                bpmedc := sample(SPOP2011[.BY[[1L]], bpmedc], .N, replace=T), 
                by=group]
        }
        
        
        #     # Diastolic BP
        #     for (j in mature.age) { # suitable for domc in future time
        #         POP[age==j, 
        #             omdiaval := sample(SPOP2011[.BY[[1L]], omdiaval], .N, replace=T), 
        #             by=group]
        #     }
        
        
        # Salt
        for (j in mature.age) { # suitable for domc in future time
            POP[age==j, 
                saltWt := sample(SPOP2011[.BY[[1L]], saltWt], .N, replace=T), 
                by=group]
        }
        
    } else {  
        cat(paste("No match for ", nrow(POP[group %in% outersect(unique(POP[,group]), unique(SPOP2011[,group])),]), " people\n", sep=''))
        # BP medication
        for (j in mature.age) { # suitable for domc in future time
            POP[age==j & group %in% unique(SPOP2011[,group]), 
                bpmedc := sample(SPOP2011[.BY[[1L]], bpmedc], .N, replace=T), 
                by=group]
        }
        
        
        #     # Diastolic BP
        #     for (j in mature.age) { # suitable for domc in future time
        #         POP[age==j & group %in% unique(SPOP2011[,group]), 
        #             omdiaval := sample(SPOP2011[.BY[[1L]], omdiaval], .N, replace=T), 
        #             by=group]
        #     }
        
        
        # Salt
        for (j in mature.age) { # suitable for domc in future time
            POP[age==j & group %in% unique(SPOP2011[,group]), 
                saltWt := sample(SPOP2011[.BY[[1L]], saltWt], .N, replace=T), 
                by=group]
        }
    }
    
    
    
    # Fruit portion
    POP     [, group := paste(sex, agegroup, porftvg, sep='')]
    SPOP2011[, group := paste(sex, agegroup, porftvg, sep='')]
    POP[age<20, group:= "undefined"]
    SPOP2011[age<20, group:= "undefined"]
    setkey(POP, group, age)
    setkey(SPOP2011, group, age)
    if (setequal(unique(SPOP2011[,group]), unique(POP[,group]))) { 
        for (j in mature.age) { # suitable for domc in future time
            POP[age==j, 
                frtpor := sample(SPOP2011[.BY[[1L]], frtpor], .N, replace=T), 
                by=group]
        }
    } else {
        cat(paste("No match for ", nrow(POP[group %in% outersect(unique(POP[,group]), unique(SPOP2011[,group])),]), " people\n", sep=''))
        for (j in mature.age) { # suitable for domc in future time
            POP[age==j & group %in% unique(SPOP2011[,group]), 
                frtpor := sample(SPOP2011[.BY[[1L]], frtpor], .N, replace=T), 
                by=group]
        }
    }
    
}
#****************** Lagtimes/Ageing implementation **********************************#
# BMI estimation
if (i > 0) POP <- ageing.distr("bmival") # to match distribution shape to that of SPOP2011
POP[between(age, ageL, ageH), bmival.pct := scale(bmival, scale=F), by= group]# to move the mean of the distribution according to RF trends
POP[between(age, ageL, ageH), bmival.cvdlag := bmival.pct + pred.bmi(i, age, sex, qimd, cvd.lag, .N), by= group]
POP[between(age, ageL, ageH), bmival.calag := bmival.pct + pred.bmi(i, age, sex, qimd, cancer.lag, .N), by= group]

# SBP estimation
if (i > 0) POP <- ageing.distr("omsysval")
POP[between(age, ageL, ageH), omsysval.pct := scale(omsysval, scale=F), by= group]
POP[between(age, ageL, ageH), omsysval.cvdlag := omsysval.pct + pred.sbp(i, age, sex, qimd, bmival.cvdlag, cvd.lag, .N), by= group] 

# CHOL estimation
if (i > 0) POP <- ageing.distr("cholval")
POP[between(age, ageL, ageH), cholval.pct := scale(cholval, scale=F), by= group]
POP[between(age, ageL, ageH), cholval.cvdlag := cholval.pct + pred.chol(i, age, sex, qimd, bmival.cvdlag, cvd.lag, .N), by= group] 

# F&V estimation
POP[between(age, ageL, ageH), porftvg.cvdlag := pred.fv(i, age, sex, qimd, cvd.lag)]
POP[between(age, ageL, ageH), porftvg.calag := pred.fv(i, age, sex, qimd, cancer.lag)]

# Smoking initiation/cessation simulation
if (i > 0) {
    POP[cigst1 %in% c("2", "3"), `:=`(endsmoke = endsmoke + 1)]
    POP[cigst1 == "4", `:=`(packyears = packyears + cigdyalCat/20)]
    
    POP[between(age, ageL, ageH) & cigst1 != 4, cigst1.temp := pred.smok.incid(age, sex, qimd, .N)]
    POP[cigst1.temp == T, cigst1 := "4"]
    POP[cigst1.temp == T, cigdyalCat := sample(SPOP2011[group == .BY[[1L]] & cigst1 == "4", cigdyalCat], .N, replace=F), by=group]
    POP[cigst1.temp == T, packyears := cigdyalCat/20]
    
    POP[between(age, ageL, ageH) & cigst1 == "4", cigst1.temp := pred.smok.cess(year = i, age, sex, qimd, .N)]
    POP[cigst1.temp == T, `:=`(cigst1 = "3", endsmoke = 0)]
    POP[,cigst1.temp := NULL]
}

POP[, cigst1.cvdlag := "1"]
POP[cigst1 %in% c("2", "3"), cigst1.cvdlag := cigst1]
POP[cigst1 %in% c("2", "3") & endsmoke < cvd.lag, cigst1.cvdlag := "4"]
POP[cigst1 == "4", cigst1.cvdlag := cigst1]
POP[cigst1 == "4" & packyears * 20 /cigdyalCat < cvd.lag, cigst1.cvdlag := "1"]

POP[, cigst1.calag := "1"]
POP[cigst1 %in% c("2", "3"), cigst1.calag := cigst1]
POP[cigst1 %in% c("2", "3") & endsmoke < cancer.lag, cigst1.calag := "4"]
POP[cigst1 == "4", cigst1.calag := cigst1]
POP[cigst1 == "4" & packyears * 20 /cigdyalCat < cancer.lag, cigst1.calag := "1"]

# DIAB estimation
# to predict diabetics that where healthy x years ago you need to apply current.prevalence-x*(diab.incid - mortality)
if (i==0) { # will need special case when cvd.lag=0
    Diabincid <- merge(Diabincid, Lifetable2012[, list(agegroup, sex, mq)], by= c("agegroup", "sex"))
    Diabincid[, diab.incid.mort := diab.incid * (1- mq)]
    POP[, agegroup := agegroup.fn(age, -cvd.lag)]
    x <- POP[, .N, by = list(agegroup, sex)]
    Diabincid <- merge(Diabincid, x, by= c("agegroup", "sex"))
    Diabincid[, diab.incid.mort.cou := round(diab.incid.mort * N, 0)]
    POP[, agegroup := agegroup.fn(age)]
    x <- POP[diabtotr==2, .N, by = list(agegroup, sex)]
    Diabincid <- merge(Diabincid, x, by= c("agegroup", "sex"))
    Diabincid[, pct.diab.hea:= diab.incid.mort.cou/N.y]
    POP <- merge(POP, Diabincid[, list(agegroup, sex, pct.diab.hea)], by = c("agegroup", "sex"), all.x = T)
    POP[between(age, ageL, ageH) & diabtotr == 1, diabtotr.cvdlag := 1]
    POP[between(age, ageL, ageH) & diabtotr == 2, diabtotr.cvdlag := ifelse(dice(.N) < pct.diab.hea * cvd.lag, 1, 2), by = c("agegroup", "sex")]
    POP[, diabtotr.cvdlag := factor(diabtotr.cvdlag)]
    POP[, `:=` (pct.diab.hea = NULL)]
    rm(x)
} else if (i<cvd.lag) {
    POP[,diabtotr.cvdlag := as.numeric(as.character(diabtotr.cvdlag))]
    POP[between(age, ageL, ageH) & diabtotr == 2 & diabtotr.cvdlag == 1, diabtotr.cvdlag := ifelse(dice(.N) < 1/(cvd.lag + 1-i), 2, 1)]
    POP[, diabtotr.cvdlag := factor(diabtotr.cvdlag)]
} else if (i == cvd.lag) {
    POP[, diabtotr.cvdlag := diabtotr]
} else {
    POP[between(age, ageL, ageH) & diabtotr.cvdlag == 1, diabtotr.cvdlag := ifelse(pred.diab.incid(year=i-cvd.lag, age=age, sex=sex, qimd=qimd, bmival=bmival.cvdlag, lag=cvd.lag, n=.N), 2L, 1L)]
} 

agegroup.fn(POP)
agegroup.fn(SPOP2011)
setkey(POP, age, sex, agegroup, qimd)
setkey(SPOP2011, age, sex, agegroup, qimd)


