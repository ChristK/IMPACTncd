cat("Loading lung cancer (C34) model...\n\n")
length.of.POP <- length(POP)
POP <- merge(POP, C34incid, by = c("agegroup", "sex"), all.x = T)
setkey(POP, cigst1)

# RR for tobacco from Gandini S, Botteri E, Iodice S, Boniol M, Lowenfels AB, Maisonneuve P, et al.
# Tobacco smoking and cancer: A meta-analysis. Int J Cancer. 2008 Jan 1;122(1):155–64.
POP[, lung.ca.tob.rr := 1]
# OR = (1 + 0.00499 * packyears * exp(2.89 * log(cigdyalCat) - 0.504 * (log(cigdyalCat))^2))
# RR for ex-smokers decrease by exp(-0.06) per year of absense. 0.06 was set arbitrarily but roughly based on
# 1 Khuder SA, Mutgi AB. Effect of smoking cessation on major histologic types of lung cancer*. Chest 2001;120:1577–83. doi:10.1378/chest.120.5.1577
# table 4
POP[cigst1 == "3", lung.ca.tob.rr := exp(- endsmoke * 0.06) * (1 + 0.00499 * packyears * exp(2.89 * log(cigdyalCat) - 0.504 * (log(cigdyalCat))^2))/(1 - p0tobonly + p0tobonly * (1 + 0.00499 * packyears * exp(2.89 * log(cigdyalCat) - 0.504 * (log(cigdyalCat))^2)))]


# OR estimated from Lubin JH, Caporaso NE. Cigarette Smoking and Lung Cancer: Modeling Total
# Exposure and Intensity. Cancer Epidemiol Biomarkers Prev. 2006 Jan 3;15(3):517–523. 
# Table 1, M1

POP[cigst1 == "4", lung.ca.tob.rr := (1 + 0.00499 * packyears * exp(2.89 * log(cigdyalCat) - 0.504 * (log(cigdyalCat))^2))/(1 - p0tobonly + p0tobonly * (1 + 0.00499 * packyears * exp(2.89 * log(cigdyalCat) - 0.504 * (log(cigdyalCat))^2)))]


POP[cigst1 == "2", `:=`(lung.ca.tob.rr = stochRR(.N, 1.31, 1.52))] # Set arbitrary as ets for ex non regular smokers
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
POP[frtpor < 98, `:=`(lung.ca.fru.rr = 1 + (stochRR(.N, 1 - 0.94, 1 - 0.97)) * (frtpor-2))]


# Estimate prevalence of lung cancer only in first run when i does not exist yet
if (!exists("i")){
    cat("Estimating lung cancer prevalence since 2002...\n\n")
    casesM <- list(18397, 18080, 18597, 18501, 18951, 18736, 19081, 18830, 19129) # create list with annual new cases since 2002 from males
    names(casesM) <- c(2002 : 2010)
    casesF <- list(12229, 12722, 12794, 13420, 13852, 14181, 14789, 15010, 15324) # create list with annual new cases since 2002 from female
    names(casesF) <- c(2002 : 2010)
    
    for (j in 1:9) {
        POP[lung.ca.incidence == 0, v:= dice(.N) <= (4 * p0 * lung.ca.tob.rr * lung.ca.ets.rr * lung.ca.fru.rr)] # select people of all ages to assign them as lung ca prevalence from 2010 (I tripled p0 to allow for an overhead)
        TempM = copy(POP[v == T & sex == "1"]) # create a temporary data.table for men
        TempF = copy(POP[v == T & sex == "2"]) # create a temporary data.table for women
        TempM = copy(sample.df(TempM, (casesM[[j]] * pop.fraction))) # Multiplying with 2010 pop.fraction for all years since 2002 is wrong but the error is negligible
        TempF = copy(sample.df(TempF, (casesF[[j]] * pop.fraction))) # as above
        Temp <- data.table(rbind_all(list(TempM, TempF)), key=c("agegroup", "sex")) # compine males and females
        rm(TempM, TempF)
        Temp <- Temp[between(age, ageL, ageH)] # restrict their age to user specified limits
        Temp <- merge(Temp, C34surv, by = c("agegroup", "sex"), all.x = T)
        Temp[, v:= dice(.N) <= .SD[,length.of.POP + 17 - j,with = F]] # Mark those who died (==F) before 2011. For j=9 X1 is used
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
        POP[lung.ca.incidence == 2001+j, v:= dice(.N) <= .SD[,length.of.POP + 37 - j, with = F]] # T= dead, F=alive 
    }
    Out.Mort.C34 = copy(POP[v == T])
    Out.Mort.C34[, `:=` (yearofdeath = 2011, v = NULL)]
} else {
    for (j in 1:10) {
        POP[lung.ca.incidence == 2001+i+j, v:= dice(.N) <= .SD[,length.of.POP + 37 - j, with = F]] # T= dead, F=alive
    }
    Temp = copy(POP[v == T])
    Temp[, `:=` (yearofdeath = 2011+i, v = NULL)]
    Out.Mort.C34 = rbind_all(list(Out.Mort.C34, Temp))
}

POP =copy(POP[v == F | is.na(v)==T,])
Out.Mort.C34 <- data.table(Out.Mort.C34, key="id")
rm(length.of.POP)
POP[, `:=` (incidence = NULL, p0tobonly = NULL, p0 = NULL, lung.ca.tob.rr = NULL, lung.ca.ets.rr = NULL, lung.ca.fru.rr = NULL, v = NULL, X1 = NULL,
            X2 = NULL, X3 = NULL, X4 = NULL, X5 = NULL, X6 = NULL, X7 = NULL, X8 = NULL, X9 = NULL, X10 = NULL, X11 = NULL, X12 = NULL,
            X13 = NULL, X14 = NULL, X15 = NULL, X16 = NULL, X17 = NULL, X18 = NULL, X19 = NULL, X20 = NULL, p1 = NULL, p2 = NULL, 
            p3 = NULL, p4 = NULL, p5 = NULL, p6 = NULL, p7 = NULL, p8 = NULL, p9 = NULL, p10 = NULL)] 
