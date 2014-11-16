cat("Initiating lag/ageing engine...\n\n")
setkey(POP, age, sex, qimd)

if (i > 0) {
    cat ("Systolic BP\n") 
    agegroup.fn(POP)
    agegroup.fn(SPOP2011)
    POP[age < 20, group:= "undefined"]
    SPOP2011[age < 20, group:= "undefined"]
    setkey(POP, group, age)
    setkey(SPOP2011, group, age)
    mature.age <- c(20, 50) # define ages that physiological factors will evolve
    if (setequal(unique(SPOP2011[,group]), unique(POP[,group]))) { # If groups not the same ignore QIMD
        #         for (j in mature.age) { 
        #             # SBP
        #             POP[age==j, 
        #                 omsysval := sample(SPOP2011[.BY[[1L]], omsysval], .N, replace=T), 
        #                 by=group]
        #         }
        
        # Cholesterol
        #         for (j in mature.age) {
        #             POP[age==j, 
        #                 cholval := sample(SPOP2011[.BY[[1L]], cholval], .N, replace=T), 
        #                 by=group]
        #         }
        
        cat("F&V portions\n")
        for (j in mature.age) { 
            POP[age == j, 
                porftvg := sample(SPOP2011[group == .BY[[1L]], porftvg], .N, replace=T), 
                by = group]
        }
        
        # Alcohol
        #     for (j in mature.age) { 
        #         POP[age==j, 
        #             totalwug := sample(SPOP2011[.BY[[1L]], totalwug], .N, replace=T), 
        #             by=group]
        #     }
        
        # Personality
#         for (j in mature.age) { 
#             POP[age==j, 
#                 segment := sample(SPOP2011[group == .BY[[1L]], segment], .N, replace=T), 
#                 by=group]
#         }
        
                
        cat("Redmeat\n")
        for (j in mature.age) { 
            POP[age==j, 
                redmeat := sample(SPOP2011[group == .BY[[1L]], redmeat], .N, replace=T), 
                by=group]
        }
        
#         # Exercise
#         for (j in mature.age) { 
#             POP[age==j, 
#                 t59su06 := sample(SPOP2011[group == .BY[[1L]], t59su06], .N, replace=T), 
#                 by=group]
#         }
    } else {
        # SBP
        cat(paste("No match for ", nrow(POP[group %in% outersect(unique(POP[,group]), unique(SPOP2011[,group])),]), " people\n", sep=''))
        # common groups as are handled as above
        for (j in mature.age) {
            POP[age==j & group %in% unique(SPOP2011[,group]), 
                omsysval := sample(SPOP2011[group == .BY[[1L]], omsysval], .N, replace=T), 
                by=group]
        }
        
        # Cholesterol
        for (j in mature.age) { 
            POP[age==j & group %in% unique(SPOP2011[,group]), 
                cholval := sample(SPOP2011[group == .BY[[1L]], cholval], .N, replace=T), 
                by=group]
        }
        
        # F&V portions
        for (j in mature.age) { 
            POP[age==j & group %in% unique(SPOP2011[,group]), 
                porftvg := sample(SPOP2011[group == .BY[[1L]], porftvg], .N, replace=T), 
                by=group]
        }
        
        #     # Alcohol
        #     for (j in mature.age) { 
        #         POP[age==j & group %in% unique(SPOP2011[,group]), 
        #             totalwug := sample(SPOP2011[.BY[[1L]], totalwug], .N, replace=T), 
        #             by=group]
        #     }
        
        # Personality
#         for (j in mature.age) { 
#             POP[age==j & group %in% unique(SPOP2011[,group]), 
#                 segment := sample(SPOP2011[group == .BY[[1L]], segment], .N, replace=T), 
#                 by=group]
#         }
        
       
        # Redmeat
        for (j in mature.age) { 
            POP[age==j & group %in% unique(SPOP2011[,group]), 
                redmeat := sample(SPOP2011[group == .BY[[1L]], redmeat], .N, replace=T), 
                by=group]
        }
        
#         # Exercise
#         for (j in mature.age) { 
#             POP[age==j & group %in% unique(SPOP2011[,group]), 
#                 t59su06 := sample(SPOP2011[group == .BY[[1L]], t59su06], .N, replace=T), 
#                 by=group]
#         }
    } 
    
    # Diabetes
    # POP[,group := paste(qimd,sex,agegroup,bmivalCat, sep='')]
    # SPOP2011[,group := paste(qimd,sex,agegroup,bmivalCat, sep='')]
    # setkey(POP, group, age)
    # setkey(SPOP2011, group, age)
    # 
    # if (setequal(unique(SPOP2011[,group]), unique(POP[,group]))) { 
    #     for (j in mature.age) { 
    #         POP[age==j, 
    #             diabtotr := sample(SPOP2011[.BY[[1L]], diabtotr], .N, replace=T), 
    #             by=group]
    #     }
    # } else {
    #     cat(paste("No match for ", nrow(POP[group %in% outersect(unique(POP[,group]), unique(SPOP2011[,group])),]), " people\n", sep=''))
    #     for (j in mature.age) { 
    #         POP[age==j & group %in% unique(SPOP2011[,group]), 
    #             diabtotr := sample(SPOP2011[.BY[[1L]], diabtotr], .N, replace=T), 
    #             by=group]
    #     }
    # }
    
    # SysBP dependent
cat("SysBP dependent\n")
    POP[, omsysvalCat := cut(omsysval, 
                             breaks = c(0, 130, 160, Inf), 
                             labels = c("normotensive", "hypertensive", "severely hypertensive"), 
                             include.lowest = T, 
                             right = F, 
                             ordered_result = T)]
    
    POP     [,group := paste0(qimd,sex,agegroup,omsysvalCat)]
    SPOP2011[,group := paste0(qimd,sex,agegroup,omsysvalCat)]
    POP[age<20, group:= "undefined"]
    SPOP2011[age<20, group:= "undefined"]
    setkey(POP, group, age)
    setkey(SPOP2011, group, age)
    if (setequal(unique(SPOP2011[,group]), unique(POP[,group]))) { 
        
#         # BP medication
#         for (j in mature.age) { 
#             POP[age==j, 
#                 bpmedc := sample(SPOP2011[group == .BY[[1L]], bpmedc], .N, replace=T), 
#                 by=group]
#         }
        
        
        #     # Diastolic BP
        #     for (j in mature.age) { 
        #         POP[age==j, 
        #             omdiaval := sample(SPOP2011[.BY[[1L]], omdiaval], .N, replace=T), 
        #             by=group]
        #     }
        
        
        cat("Salt\n")
        for (j in mature.age) { 
            POP[age==j, 
                saltWt := sample(SPOP2011[group == .BY[[1L]], saltWt], .N, replace=T), 
                by=group]
        }
        
    } else {  
        cat(paste("No match for ", nrow(POP[group %in% outersect(unique(POP[,group]), unique(SPOP2011[,group])),]), " people\n", sep=''))
#         # BP medication
#         for (j in mature.age) { 
#             POP[age==j & group %in% unique(SPOP2011[,group]), 
#                 bpmedc := sample(SPOP2011[group == .BY[[1L]], bpmedc], .N, replace=T), 
#                 by=group]
#         }
        
        
        #     # Diastolic BP
        #     for (j in mature.age) { 
        #         POP[age==j & group %in% unique(SPOP2011[,group]), 
        #             omdiaval := sample(SPOP2011[.BY[[1L]], omdiaval], .N, replace=T), 
        #             by=group]
        #     }
        
        
        # Salt
        for (j in mature.age) { 
            POP[age==j & group %in% unique(SPOP2011[,group]), 
                saltWt := sample(SPOP2011[group == .BY[[1L]], saltWt], .N, replace=T), 
                by=group]
        }
    }
    
    
    
    # Fruit portion
    #     POP     [, group := paste(sex, agegroup, porftvg, sep='')]
    #     SPOP2011[, group := paste(sex, agegroup, porftvg, sep='')]
    #     POP[age<20, group:= "undefined"]
    #     SPOP2011[age<20, group:= "undefined"]
    #     setkey(POP, group, age)
    #     setkey(SPOP2011, group, age)
    #     if (setequal(unique(SPOP2011[,group]), unique(POP[,group]))) { 
    #         for (j in mature.age) { 
    #             POP[age==j, 
    #                 frtpor := sample(SPOP2011[.BY[[1L]], frtpor], .N, replace=T), 
    #                 by=group]
    #         }
    #     } else {
    #         cat(paste("No match for ", nrow(POP[group %in% outersect(unique(POP[,group]), unique(SPOP2011[,group])),]), " people\n", sep=''))
    #         for (j in mature.age) { 
    #             POP[age==j & group %in% unique(SPOP2011[,group]), 
    #                 frtpor := sample(SPOP2011[.BY[[1L]], frtpor], .N, replace=T), 
    #                 by=group]
    #         }
    #     }
    agegroup.fn(POP)
    agegroup.fn(SPOP2011)
}
#****************************************** Lagtimes/Ageing implementation **********************************#
cat("BMI estimation\n")
if (i > 0) POP <- ageing.distr("bmival") # to match distribution shape to that of SPOP2011
POP[between(age, ageL, ageH), bmival.pct := scale(bmival, scale=F), by= group]# to move the mean of the distribution according to RF trends
POP[between(age, ageL, ageH), bmival.cvdlag := bmival.pct + pred.bmi(i, age, sex, qimd, cvd.lag), by= group]
POP[between(age, ageL, ageH), bmival.calag := bmival.pct + pred.bmi(i, age, sex, qimd, cancer.lag), by= group]

cat("SBP estimation\n")
if (i > 0) POP <- ageing.distr("omsysval")
POP[between(age, ageL, ageH), omsysval.pct := scale(omsysval, scale=F), by= group]
POP[between(age, ageL, ageH), omsysval.cvdlag := omsysval.pct + pred.sbp(i, age, sex, qimd, bmival.cvdlag, cvd.lag), by= group] 


cat("CHOL estimation\n")
if (i > 0) POP <- ageing.distr("cholval")
POP[between(age, ageL, ageH), cholval.pct := scale(cholval, scale=F), by= group]
POP[between(age, ageL, ageH), cholval.cvdlag := cholval.pct + pred.chol(i, age, sex, qimd, bmival.cvdlag, cvd.lag), by= group] 

cat("F&V estimation\n")
POP[between(age, ageL, ageH), porftvg.cvdlag := pred.fv(i, age, sex, qimd, bmival.cvdlag, cvd.lag)]
POP[between(age, ageL, ageH), frtpor.cvdlag := pred.fvrate(i, age, sex, qimd, porftvg.cvdlag, cvd.lag)]

POP[between(age, ageL, ageH), porftvg.calag := pred.fv(i, age, sex, qimd, bmival.calag, cancer.lag)]
POP[between(age, ageL, ageH), frtpor.calag := pred.fvrate(i, age, sex, qimd, porftvg.calag, cancer.lag)]

cat("Smoking initiation/cessation simulation\n")
if (i > 0) {
    POP[cigst1 %in% c("2", "3"), `:=`(endsmoke = endsmoke + 1)]
    POP[cigst1 == "4", `:=`(packyears = packyears + cigdyalCat/20)]
}

cat("Smoking initiation\n")
POP[between(age, 16, ageH) & cigst1 != 4, cigst1.temp := pred.smok.incid(i, age, sex, qimd)] # start from age 20 to accurately simulate smoking histories
POP[cigst1.temp == T, cigdyalCat := sample(SPOP2011[group == .BY[[1L]] & cigst1 == "4", cigdyalCat], .N, replace=T), by=group]
POP[cigst1.temp == T, packyears := ifelse(cigst1 %in% c("2", "3"), packyears + cigdyalCat/20, cigdyalCat/20)]
POP[cigst1.temp == T, cigst1 := "4"]

cat("Smoking cessation\n")
POP[between(age, 16, ageH) & cigst1 == "4", cigst1.temp := pred.smok.cess(i, age, sex, qimd)]
POP[cigst1.temp == T, `:=`(cigst1 = "3", endsmoke = 0)]
POP[,cigst1.temp := NULL]

cat("Smoking lag\n")
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

cat("DIAB estimation\n")
# to predict diabetics that where healthy x years ago you need to apply current.prevalence-x*(diab.incid - mortality)
if (i == 0) { # will need special case when cvd.lag=0
    
    # Diabetes incidence from Holden SE, Barnett AH, Peters JR, et al. The incidence of type 2 diabetes in the United Kingdom from 1991 to 2010. Diabetes Obes Metab 2013;15:844–52. doi:10.1111/dom.12123
    # NOTE: this is for type 2 diabetes only. For this ages I am concerned with this is absolutely fine. very few new diabetes I patients older than 35 
    Diabincid <- fread("./Lagtimes/diabincid.csv",
                       sep = ",",
                       header = T, 
                       stringsAsFactors = T)
    Diabincid[, sex := factor(sex)]
    
    Diabincid <- merge(Diabincid, Lifetable2012[, list(agegroup, sex, mq)], by= c("agegroup", "sex"))
    Diabincid[, diab.incid.mort := diab.incid * (1- mq*1.6)] # 1.6 is the rr of dying of diabetes
    POP[, agegroup := agegroup.fn(age, -cvd.lag)]
    x <- POP[, .N, by = list(agegroup, sex)]
    Diabincid <- merge(Diabincid, x, by= c("agegroup", "sex"))
    Diabincid[, diab.incid.mort.count := round(diab.incid.mort * N, 0)]
    POP[, agegroup := agegroup.fn(age)]
    x <- POP[diabtotr==2, .N, by = list(agegroup, sex)]
    Diabincid <- merge(Diabincid, x, by= c("agegroup", "sex"))
    Diabincid[, pct.diab.hea:= diab.incid.mort.count/N.y]
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
    POP[between(age, ageL, ageH) & diabtotr.cvdlag == 1, diabtotr.cvdlag := ifelse(pred.diab.incid(year=i, age=age, sex=sex, qimd=qimd, bmival=bmival.cvdlag, lag=cvd.lag), 2L, 1L)]
} 

cat("DIAB finished\n")
agegroup.fn(POP)
agegroup.fn(SPOP2011)
setkey(POP, age, sex, agegroup, qimd)
setkey(SPOP2011, age, sex, agegroup, qimd)

cat("Export cont. risk factors\n") 
export.contRF("bmival.cvdlag")
export.contRF("bmival.calag")
export.contRF("omsysval.cvdlag")
export.contRF("cholval.cvdlag")

cat("Export cat. risk factors\n") 
export.catRF("cigst1.cvdlag")
export.catRF("cigst1.calag")
export.catRF("porftvg.cvdlag")
export.catRF("porftvg.calag")
export.catRF("frtpor.cvdlag")
export.catRF("frtpor.calag")
export.catRF("diabtotr.cvdlag")
export.catRF("expsmokCat")










