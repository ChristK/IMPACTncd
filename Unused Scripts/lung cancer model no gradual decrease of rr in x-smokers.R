cat("Loading lung cancer (C34) model...\n\n")
length.of.POP <- length(POP)
POP <- merge(POP, C34incid, by = c("agegroup", "sex"), all.x = T)
setkey(POP, id)

# RR for tobacco from Gandini S, Botteri E, Iodice S, Boniol M, Lowenfels AB, Maisonneuve P, et al.
# Tobacco smoking and cancer: A meta-analysis. Int J Cancer. 2008 Jan 1;122(1):155–64.
POP[, `:=`(lung.ca.tob.rr = 1)]
# POP[cigst1=='1' | cigdyalCat==0 | between(packyears,0,1), lung.ca.tob.rr:= 1] POP[cigst1=='2',
# lung.ca.tob.rr:= 1] # asumption. Lower CI from Table I for former smoker
POP[endsmoke > 5 & cigst1 == "3", `:=`(lung.ca.tob.rr = stochRR(.N, 3.85, 5.34))]  # Table I from above paper


# OR taken from table 4 in Khuder SA. Effect of cigarette smoking on major histological types of lung
# cancer: a meta-analysis. Lung Cancer. 2001 Mar;31(2–3):139–48.  OR were converted to RR using the
# formula from Zhang J, Yu KF. What’s the relative risk?: A method of correcting the odds ratio in
# cohort studies of common outcomes. JAMA. 1998 Nov 18;280(19):1690–1.  Ex-smokers who quited 5 or
# less than 5 years ago are considered active smokers Actual Lung ca incidence in 2011 in england
# between ages 40 and 80 was 25128
POP[(cigst1 == "3" | cigst1 == "4") & endsmoke < 6 & between(packyears, 1, 19), `:=`(lung.ca.tob.rr =  
    stochRR(.N, 6.79/(1 - p0 + p0 * 6.79), 7.94/(1 - p0 + p0 * 7.94)))]
POP[(cigst1 == "3" | cigst1 == "4") & endsmoke < 6 & between(packyears, 19, 56), `:=`(lung.ca.tob.rr =  
    stochRR(.N, 16.99/(1 - p0 + p0 * 16.99), 26.38/(1 - p0 + p0 * 26.38)))]
POP[(cigst1 == "3" | cigst1 == "4") & endsmoke < 6 & packyears > 56, `:=`(lung.ca.tob.rr = stochRR(.N, 
    109.3/(1 - p0 + p0 * 109.3), 298.1/(1 - p0 + p0 * 298.1)))]

# RR for ETS from Taylor R, Najafi F, Dobson A. Meta-analysis of studies of passive smoking and lung
# cancer: effects of study type and continent. Int J Epidemiol. 2007 Jan 10;36(5):1048–59. Table 4
# for Europe
POP[, `:=`(lung.ca.ets.rr = 1)]
POP[cigst1 != "4" & expsmokCat > 0, `:=`(lung.ca.ets.rr = stochRR(.N, 1.31, 1.52))]


# RR for fruit from Norat T, Aune D, Chan D, Romaguera D. Fruits and Vegetables: Updating the
# Epidemiologic Evidence for the WCRF/AICR Lifestyle Recommendations for Cancer Prevention. In:
# Zappia V, Panico S, Russo GL, Budillon A, Ragione FD, editors. Advances in Nutrition and Cancer
# [Internet]. Springer Berlin Heidelberg; 2014 [cited 2014 Mar 21]. Available from:
# http://link.springer.com/chapter/10.1007/978-3-642-38007-5_3. p97
POP[, `:=`(lung.ca.fru.rr = 1)]  # It seems that Parkin used a recommended level of about 2 portions of fruit
POP[frtpor < 98, `:=`(lung.ca.fru.rr = 1 + (1 - stochRR(.N, 0.94, 0.97)) * (2 - frtpor))]


# Estimate prevalence of lung cancer only in first run when i does not exist yet
if (!exists("i")){
    cat("Estimating lung cancer prevalence since 2002...\n\n")
    casesM <- list(18397, 18080, 18597, 18501, 18951, 18736, 19081, 18830, 19129) # create list with annual new cases since 2002 from males
    names(casesM) <- c(2002 : 2010)
    casesF <- list(12229, 12722, 12794, 13420, 13852, 14181, 14789, 15010, 15324) # create list with annual new cases since 2002 from female
    names(casesF) <- c(2002 : 2010)
    
    for (j in 1:9) {
        POP[lung.ca.incidence == 0, v:= dice(.N) <= (2 * p0 * lung.ca.tob.rr * lung.ca.ets.rr * lung.ca.fru.rr)] # select people of all ages to assign them as lung ca prevalence from 2010 (I doubled p0 to allow for an overhead)
        TempM = copy(POP[v == T & sex == "1"]) # create a temporary data.table for men
        TempF = copy(POP[v == T & sex == "2"]) # create a temporary data.table for women
        TempM = copy(sample.df(TempM, (casesM[[j]] * pop.fraction))) # Multiplying with 2010 pop.fraction for all years since 2002 is wrong but the error is negligible
        TempF = copy(sample.df(TempF, (casesF[[j]] * pop.fraction))) # as above
        Temp <- data.table(rbind_all(list(TempM, TempF)), key=c("agegroup", "sex")) # compine males and females
        rm(TempM, TempF)
        Temp <- Temp[between(age, ageL, ageH)] # restrict their age to user specified limits
        Temp <- merge(Temp, C34surv, by = c("agegroup", "sex"), all.x = T)
        Temp[, v:= dice(.N) <= .SD[,48-j,with=F]] # Mark those who died (==F) before 2011 
        Temp <- Temp[v==T] # and keep only the alive (==T) ones
        setkey(Temp, id)
        Temp = Temp[Temp, list(id)] # keep only their ids
        setkey(POP, id)
        POP[Temp, lung.ca.incidence:= 2001 + j] # Finally assign these ids to lung.ca.incidence as the actual year
    }
    rm(casesM, casesF)
    POP[, v:= NULL]
}


# P= P0 * lung.ca.tob.rr * lung.ca.ets.rr * lung.ca.fru.rr
cat("Estimating lung cancer incidence...\n\n")
POP[between(age, ageL, ageH) & lung.ca.incidence == 0, v := dice(.N) <= (p0 * lung.ca.tob.rr * lung.ca.ets.rr * 
    lung.ca.fru.rr)]  # v is a temporary var because data.table cannot assign a number to a logical column. ??is this a bug?

if (!exists("i")) {
    POP[v == T, lung.ca.incidence := 2011]
} else {
    POP[v == T, lung.ca.incidence := 2011+i]  
}

POP[, v := NULL]

if (!exists("Out.Inc.C34")) {
    Out.Inc.C34 = copy(POP[lung.ca.incidence == 2011])
} else {
    Temp = copy(POP[lung.ca.incidence == 2011+i])
    Out.Inc.C34 <- rbind_all(list(Out.Inc.C34, Temp))
}

Out.Inc.C34 <- data.table(Out.Inc.C34, key="id")

# Estimate lung cancer mortality (people die of lung cancer up to 10 years after diagnosis)
cat("Estimating lung cancer mortality...\n\n")
POP <- merge(POP, C34surv, by = c("agegroup", "sex"), all.x = T)
setkey(POP, agegroup, sex)

if (!exists("i")){ # for 2011 that i doesn't exists yet
    for (j in 1:10) {
        POP[lung.ca.incidence == 2001+j, v:= dice(.N) <= .SD[,length.of.POP + 36 - j, with = F]] # T= dead, F=alive NEED TO UPDATE 67 WHEN I INSERT MORE PARAMETERS TO THE SYNTHPOP
    }
    Out.Mort.C34 = copy(POP[v == T])
    Out.Mort.C34[, `:=` (yearofdeath = 2011, v = NULL)]
} else {
    for (j in 1:10) {
        POP[lung.ca.incidence == 2001+i+j, v:= dice(.N) <= .SD[,length.of.POP + 36 - j, with = F]] # T= dead, F=alive
    }
    Temp = copy(POP[v == T])
    Temp[, `:=` (yearofdeath = 2011+i, v = NULL)]
    Out.Mort.C34 = rbind_all(list(Out.Mort.C34, Temp))
}

POP <- POP[v == F | is.na(v)==T,]
Out.Mort.C34 <- data.table(Out.Mort.C34, key="id")
rm(length.of.POP)
POP[, `:=` (incidence = NULL, p0 = NULL, lung.ca.tob.rr = NULL, lung.ca.ets.rr = NULL, lung.ca.fru.rr = NULL, v = NULL, X1 = NULL,
            X2 = NULL, X3 = NULL, X4 = NULL, X5 = NULL, X6 = NULL, X7 = NULL, X8 = NULL, X9 = NULL, X10 = NULL, X11 = NULL, X12 = NULL,
            X13 = NULL, X14 = NULL, X15 = NULL, X16 = NULL, X17 = NULL, X18 = NULL, X19 = NULL, X20 = NULL, p1 = NULL, p2 = NULL, 
            p3 = NULL, p4 = NULL, p5 = NULL, p6 = NULL, p7 = NULL, p8 = NULL, p9 = NULL, p10 = NULL)] 
