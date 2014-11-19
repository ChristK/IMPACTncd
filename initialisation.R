# preample
dependencies <- function(x) {
    for (j in x) {
        # require returns T invisibly if it was able to load package
        if (!require(j, character.only = T)) {
            # If package was not able to be loaded then re-install
            install.packages(j, dependencies = T)
            # Load package after installing
            require(j, character.only = T)
        }
    }
}

# Then try/install packages...
dependencies(c("data.table", 
               "dplyr",
               "demography", 
               "truncnorm", 
               "stringr", 
               "reshape2", 
               "compiler",
               "survey",
               "ggplot2",
               "randtoolbox",
               "doParallel",
               "doRNG",
               "foreach"))

enableJIT(3) #set to 1,2 or 3 to enable different precompiling levels

options(survey.lonely.psu = "adjust") #Lonely PSU (center any single-PSU strata around the sample grand mean rather than the stratum mean)
#require(devtools)
#install_github("Rdatatable/data.table",  build_vignettes = F)
# OR install_local("~/R/data.table-master.zip") #after manually download from github

# max projection horizon (limited by fertility)
if (init.year + yearstoproject > 2061) yearstoproject <- 2061 - init.year

# Define end() function to beep end print a message
if (Sys.info()[1] == "Linux") {
  end <- function(...){
    cat("All done! \a\n")}
} else {
  end <- function(...){
    for(i in 3){
      system("rundll32 user32.dll,MessageBeep -1")
      Sys.sleep(.5)
    }
    cat("All done! \a\n")
  }
}

end <- function(...) {
    cat("All done! \a\n")
    sink(file = "./Output/simulation parameters.txt",
         append = T, 
         type = "output",
         split = F)
    cat(paste0("Simulation ended succesfuly at: ", Sys.time(), "\n"))
    sink()
    if (Sys.info()[1] == "Windows") {
        system("rundll32 user32.dll,MessageBeep -1")
        Sys.sleep(.5)
    }
}

# # Create a vector of random numbers, using the SIMD-oriented Fast Mersenne Twister algorithms by
# # Matsumoto & Saito (2008)
# dice <- function(n = .N) {
#     rand <- SFMT(n, dim = 1, mexp = 19937, usepset = T, withtorus = F, usetime = T)
#     return(rand)
# }

# Define RNG for parallel use with doRNG
RNGkind("L'Ecuyer-CMRG")
dice <- function(n = .N) runif(n)

# define function for stochastic RR
stochRR <- function(n = .N, m, ci) { # lognormal
    if (m < 1) {
        a = -Inf
        b = 0
    } else {
        a = 0
        b = Inf
    }
    ifelse(m == ci, rr <- rep(log(m), n), rr <- rtruncnorm(n = n, a = a, b = b, mean = log(m), sd = abs(log(m) - log(ci))/1.96))
    return(exp(rr))  
}

stochRRnorm <- function(n = .N, m, ci) { # normal distr
    if (m < 1) {
        a = 0
        b = 1
    } else {
        a = 1
        b = Inf
    }
    ifelse(m == ci, rr <- rep(m, n), rr <- rtruncnorm(n = n, a = a, b = b, mean = m, sd = abs(m - ci)/1.96))
    return(rr)  
}

# function to calculate mortality based on 1st and 5th year survival
hyperbola <- function(y1, y5, x) {
    b = (5 * y5 - y1)/4
    a = y1 - b
    y = b + a/x
    return(y)
}
hyperbola <- cmpfun(hyperbola) # compiled version

# Define function for sampling. Taken from sample man pages 
resample <- function(x, ...) {
    x <- na.omit(x)
    x[sample.int(length(x), ...)]
}

# Define operator %!in%, meaning !%in%
'%!in%' <- function(x,y)!('%in%'(x,y))

# Define outersect. Like setdiff but symmetrical. I.e. setdiff(a,b) is not the same as setdiff(b,a). outersect solve this by calculating both
outersect <- function(x, y, ...) {
    big.vec <- c(x, y, ...)
    duplicates <- big.vec[duplicated(big.vec)]
    setdiff(big.vec, unique(duplicates))
}

# Define function to split agegroups and create groups
agegroup.fn <- function(x, lagtime = 0) {
    breaks                   <- c(0, 1, seq(5, 85, 5), Inf)
    labels                   <- c("<1   ", "01-04", "05-09",
                                  "10-14", "15-19", "20-24", 
                                  "25-29", "30-34", "35-39", 
                                  "40-44", "45-49", "50-54",
                                  "55-59", "60-64", "65-69",
                                  "70-74", "75-79", "80-84", 
                                  "85+")
    if (is.numeric(x)) { 
        agegroup = cut(x + lagtime, 
                       breaks = breaks, 
                       labels = labels, 
                       include.lowest = T, 
                       right = F, 
                       ordered_result = T)
        return(invisible(agegroup))    
    } else {
        if (is.data.table(x)) {
            x[, agegroup := cut(as.numeric(as.character(age)) + lagtime, 
                                breaks = breaks, 
                                labels = labels, 
                                include.lowest = T, 
                                right = F, 
                                ordered_result = T)]
            x[, group := as.factor(paste0(qimd, sex, agegroup))]
            return(invisible(x))
        } else return(print("only datatables and vectors are eligible inputs"))
    }
}

# Define function to split agegroups and create groups
agegroup.part <- function(x, lagtime = 0) {
    breaks                   <- c(seq(20, 85, 5), Inf)
    labels                   <- c("20-24", "25-29", "30-34", 
                                  "35-39", "40-44", "45-49",
                                  "50-54", "55-59", "60-64",
                                  "65-69", "70-74", "75-79",
                                  "80-84", "85+")
    if (is.numeric(x)) { 
        agegroup = cut(x + lagtime, 
                       breaks = breaks, 
                       labels = labels, 
                       include.lowest = T, 
                       right = F, 
                       ordered_result = T)
        return(invisible(agegroup))    
    } else {
        if (is.data.table(x)) {
            x[, agegroup := cut(as.numeric(as.character(age)) + lagtime, 
                                breaks = breaks, 
                                labels = labels, 
                                include.lowest = T, 
                                right = F, 
                                ordered_result = T)]
            return(invisible(x))
        } else return(print("only datatables and vectors are eligible inputs"))
    }
}

# Define function to clear labels form SPSS labelled imports
clear.labels <- function(x) {
    if(is.list(x)) {
        for(i in 1 : length(x)) class(x[[i]]) <- setdiff(class(x[[i]]), 'labelled') 
        for(i in 1 : length(x)) attr(x[[i]],"label") <- NULL
    }
    else {
        class(x) <- setdiff(class(x), "labelled")
        attr(x, "label") <- NULL
    }
    return(invisible(x))
}


# Define function for bmi projection (predicts mean bmi)
pred.bmi <- function(year, age, sex, qimd, lag = cvd.lag) {
    if (is.factor(sex)==F) {
        sex <-  factor(sex, 
                       levels = c(1,2), 
                       ordered = F)
    }
    if (is.ordered(qimd)==F) {
        qimd <- factor(qimd, 
                       levels = c(1,2,3,4,5), 
                       ordered = T)
    }
    pr <- data.frame(predict(bmi.svylm, 
                             data.frame(year = year-lag, age = age-lag, sex = sex, qimd = qimd), 
                             type = "response", 
                             se.fit=T))
    return(rnorm(nrow(pr), pr[[1]], pr[[2]]))
}

# test
# summary(pred.bmi(sample(c(0:50), n, replace = T), 
#                  sample(c(20,85), n, replace = T), 
#                  sample(c(1,2), n, replace = T), 
#                  sample(c(1:5), n, replace = T),
#                  sample(c(1,10), n, replace = T)))

# Define function for sbp projection (for DT needs the by= to work correctly with mean(bmival)) (predicts mean sbp)
pred.sbp <- function(year, age, sex, qimd, bmival, lag = cvd.lag) {
    if (is.factor(sex)==F) {
        sex <-  factor(sex, 
                       levels = c(1,2), 
                       ordered = F)
    }
    if (is.ordered(qimd)==F) {
        qimd <- factor(qimd, 
                       levels = c(1,2,3,4,5), 
                       ordered = T)
    }
    bmival[bmival>50] <- 50 # otherwise predicts NAN values
    pr <- data.frame(predict(sbp.svylm, 
                             data.frame(year = year-lag, age = age-lag, sex = sex, qimd = qimd, bmival = bmival), type = "response", se.fit=T))
    #return(pr[[1]])
    #return(rnorm(nrow(pr), pr[[1]], pr[[2]]/4))
    return(rtruncnorm(nrow(pr), a=70, b= 220, pr[[1]], pr[[2]]))
}

#test
# summary(pred.sbp(sample(c(0:50), n, replace = T), 
#                  sample(c(20,85), n, replace = T), 
#                  sample(c(1,2), n, replace = T), 
#                  sample(c(1:5), n, replace = T),
#                  runif(n, 10, 90),
#                  sample(c(1,10), n, replace = T)))

# Define function for chol projection (for ages above 30)
pred.chol <- function(year, age, sex, qimd, bmival, lag = cvd.lag) {
    if (is.factor(sex)==F) {
        sex <-  factor(sex, 
                       levels = c(1,2), 
                       ordered = F)
    }
    if (is.ordered(qimd)==F) {
        qimd <- factor(qimd, 
                       levels = c(1,2,3,4,5), 
                       ordered = T)
    }
    bmival[bmival>50] <- 50 # otherwise predicts NAN values
    pr <- data.frame(predict(chol.svylm, data.frame(year = year-lag, age = age-lag, sex = sex, qimd = qimd, bmival = bmival), type = "response", se.fit=T))
    #return(pr[[1]])
    return(rtruncnorm(nrow(pr), a = 2.5, b = 12,  pr[[1]], pr[[2]]))
}

# test
# summary(pred.chol(sample(c(0:50), n, replace = T), 
#                   sample(c(20,85), n, replace = T), 
#                   sample(c(1,2), n, replace = T), 
#                   sample(c(1:5), n, replace = T),
#                   runif(n, 10, 90),
#                   sample(c(1,10), n, replace = T)))

# Define function for diab prevalence projection 
pred.diab <- function(year, age, sex, qimd, bmival) {
    if (is.factor(sex)==F) {
        sex <-  factor(sex, 
                       levels = c(1,2), 
                       ordered = F)
    }
    if (is.ordered(qimd)==F) {
        qimd <- factor(qimd, 
                       levels = c(1,2,3,4,5), 
                       ordered = T)
    }
    bmival[bmival>50] <- 50 # otherwise predicts NAN values
    pr <- data.frame(predict(diab.svylr, data.frame(year = year, age = age, sex = sex, qimd = qimd, bmival = bmival), type = "response", se.fit=T))
    #return(rtruncnorm(nrow(pr), a = 0, b = 1, mean=pr[[1]], sd=pr[[2]])) 
    return(pr[[1]])
}

# plot(pred.diab(10, 20:70, 1,1,40,0), ylim=c(0,1))
# test
# summary(pred.diab(sample(c(0:50), n, replace = T), 
#                   sample(c(20,85), n, replace = T), 
#                   sample(c(1,2), n, replace = T), 
#                   sample(c(1:5), n, replace = T),
#                   runif(n, 10, 90),
#                   sample(c(1,10), n, replace = T)))

# Define function to extract mq from Lifetable2012
# death.pr <- function(age1, sex1) {
#     agegroup1 <- agegroup.fn(age1)
#     sex1 <- factor(sex1, 
#                   levels = c(1,2), 
#                   ordered = F)
#     x <- data.table(agegroup=agegroup1, sex=sex1)
#     x <- merge(x, Lifetable2012, by=c("agegroup", "sex"), all.x = T)
#     return(x[,mq])
# }

# function to estimate diabetes incidence
# rr = the rr of dying because of diabetes from Group TDS. Is the Current Definition for Diabetes Relevant to Mortality Risk From All Causes and Cardiovascular and Noncardiovascular Diseases? Dia Care. 2003 Jan 3;26(3):688–96. 
pred.diab.incid <- function(year, age, sex, qimd, bmival, lag ) { 
    prev0 <- pred.diab(year = year - lag, age = age - lag, sex = sex, qimd = qimd, bmival = bmival)
    prev1 <- pred.diab(year = year - lag + 1, age = age - lag + 1, sex = sex, qimd = qimd, bmival = bmival)
    tc <- ifelse ((prev1 - prev0) <= 0, 0, (prev1 - prev0)) # incidence in year = year. Produces values above 1 occassionaly
    return(as.logical(rbinom(length(tc), 1, tc)))
    
}

pred.diab.incid.lag <- function(year, age, sex, qimd, bmival, lag, duration = 1, n) { 
    prev0 <- pred.diab(year = year-lag, age = age-lag, sex = sex, qimd = qimd, bmival = bmival)
    prev1 <- pred.diab(year = year-lag+duration, age = age-lag+duration, sex = sex, qimd = qimd, bmival = bmival)
    tc <- prev0 / prev1 # derived from bayes theorem P(diab2008|diab2011)= P(diab2011|diab2008)*P(diab2008)/P(diab2011) and P(diab2011|diab2008) = 1)
    return(as.factor(ifelse(dice(n) <tc, 2,1)))
}


# Gives the annual probability of a never smoker to become  smoker next year
# all other remain never smokers
pred.nev0sm1 <- function(year, age, qimd) {
    qimd <- ifelse(qimd == 1, "1", "2")
    qimd <- ordered(qimd, levels=1:2)
 
    pnev0sm1 <- data.frame(predict(smok.start.svylr, data.frame(year = year, age = age, qimd = qimd), type = "response", se.fit=T))
    return(rtruncnorm(nrow(pnev0sm1), 0, 1, pnev0sm1[[1]], pnev0sm1[[2]]))
}

# plot(pred.nev0sm1(16:60, "1"), ylim=c(0,0.2))
# lines(pred.nev0sm1(16:60, "3"), ylim=c(0,0.2))

# Predicts the annual probability of a smoker to become ex-smoker
pred.sm0ex1 <- function(year, age, sex, qimd) {
    if (is.factor(sex)==F) {
        sex <-  factor(sex, 
                       levels = c(1,2), 
                       ordered = F)
    }
    if (is.ordered(qimd)==F) {
        qimd <- factor(qimd, 
                       levels = c(1,2,3,4,5), 
                       ordered = T)
    }
    sm0ex1 <- data.frame(predict(smok.cess.svylr, data.frame(year = year, age = age, sex = sex, qimd = qimd), type = "response", se.fit=T))
    return(rtruncnorm(nrow(sm0ex1), 0, 1, sm0ex1[[1]], sm0ex1[[2]]))
    #return(sm0ex1[[1]])
}

# for (jj in 1:5) {
#     plot(16:90, pred.sm0ex1(0, 16:90, 1, jj), ylim=c(0,0.4))
# }

# Predicts probability of ex-smoker to become active smoker (relapse) (only works for 1<endsmoke<10). Else should be 0
pred.ex0sm1 <- function(endsmoke, sex, qimd) {
    if (is.factor(sex)==F) {
        sex <-  factor(sex, 
                       levels = c(1,2), 
                       ordered = F)
    }
    if (is.ordered(qimd)==F) {
        qimd <- factor(qimd, 
                       levels = c(1,2,3,4,5), 
                       ordered = T)
    }
    
    ex0sm1 <- data.frame(predict(smok.cess.success, data.frame(endsmoke = endsmoke-1, sex = sex, qimd = qimd), type="response", se.fit=F))
    ex0sm1[ex0sm1[1]>0.95, 1] <-1
    ex1sm2 <- data.frame(predict(smok.cess.success, data.frame(endsmoke = endsmoke+0, sex = sex, qimd = qimd), type="response", se.fit=F))
    pr <- ex0sm1[[1]] - ex1sm2[[1]]
    return(pr)
}
#pred.ex0sm1(1:10, 1, 1)

# predicts the active smoker prevalence
pred.sm0prev <- function(year, age, sex, qimd) {
    if (is.factor(sex)==F) {
        sex <-  factor(sex, 
                       levels = c(1,2), 
                       ordered = F)
    }
    if (is.ordered(qimd)==F) {
        qimd <- factor(qimd, 
                       levels = c(1,2,3,4,5), 
                       ordered = T)
    }
    sm0prev <- data.frame(predict(smok.active.svylr, data.frame(year = year, age = age, sex = sex, qimd = qimd), type = "response", se.fit=T))
    return(rtruncnorm(nrow(sm0prev), 0, 1, sm0prev[[1]], sm0prev[[2]]))
    #return(sm0prev[[1]])
}
# for (jj in 1:5) {
#     plot(pred.sm0prev(0:50, 20, 1, jj), ylim=c(0,0.8))
# }



# Define function for F&V
pred.fv <- function(year, age, sex, qimd, bmival, lag = cvd.lag) {
    if (is.factor(sex)==F) {
        sex <-  factor(sex, 
                       levels = c(1,2), 
                       ordered = F)
    }
    if (is.ordered(qimd)==F) {
        qimd <- factor(qimd, 
                       levels = c(1,2,3,4,5), 
                       ordered = T)
    }
    bmival[bmival>50] <- 50 # otherwise predicts NAN values
    cc <- data.frame(predict(fv.svylr, 
                             data.frame(year= year-lag, age = age-lag, sex = sex, qimd = qimd, bmival = bmival), 
                             type="response", se.fit=T))
    cc <- rtruncnorm(nrow(cc), a=0, b=Inf, mean=cc[[1]], sd=cc[[2]])
    cc <- rpois(length(cc), cc)
    cc[cc>9] <- 9
    return(cc)  
}

# test
# summary(factor(pred.fv(sample(c(0:50), n, replace = T), 
#                        sample(c(20,85), n, replace = T), 
#                        sample(c(1,2), n, replace = T), 
#                        sample(c(1:5), n, replace = T),
#                        runif(n, 10, 90),
#                        sample(c(1,10), n, replace = T))))/n
# SPOP2011[age>19, summary(factor(porftvg))/.N]

# Define function for Fruit consumption
pred.fvrate <- function(year, age, sex, qimd, porftvg, lag = cvd.lag) {
    if (is.factor(sex)==F) {
        sex <-  factor(sex, 
                       levels = c(1,2), 
                       ordered = F)
    }
    if (is.ordered(qimd)==F) {
        qimd <- factor(qimd, 
                       levels = c(1,2,3,4,5), 
                       ordered = T)
    }
    cc <- data.frame(predict(fvrate.svylr, 
                             data.frame(year= year-lag, age = age-lag, sex = sex, qimd = qimd, porftvg = porftvg), 
                             type="response", se.fit=T))
    cc<- rtruncnorm(nrow(cc), a = 0, b = 1, mean=cc[[1]], sd=cc[[2]])
    return(round(porftvg*cc))
}

# test
# summary(factor(pred.fvrate(sample(c(0), n, replace = T), 
#                            sample(c(20,85), n, replace = T), 
#                            sample(c(1,2), n, replace = T), 
#                            sample(c(1:5), n, replace = T),
#                            sample(c(0:9), n, replace = T),
#                            sample(c(1,10), n, replace = T))))

# Define function for percentile rank (dplyr provides similar functions)
perc.rank <- function(x) rank(x,  ties.method = "random")/length(x)

# Define function to match continuous distributions of each group with the one in SPOP2011 to simulate ageing 
ageing.distr <- function(risk.factor) {
    temp = copy(SPOP2011[, c(risk.factor, "group"), with = F])
    nam <- paste0(risk.factor, ".rank")
    temp[, (nam) := perc.rank(get(risk.factor)), by = group]
    setkeyv(temp, c("group", nam))
    
    POP[, (nam) := perc.rank(get(risk.factor)), by = group]
    POP[, (risk.factor) := NULL]
    setkeyv(POP, c("group", nam))
    return(temp[POP, roll = "nearest"])
}
#example POP <- ageing.distr("bmival")


# Define function to export annual summaries of RF
pop.summ <-  function(N, ...) {
    return(list("year" = 2011 + i,
                "scenario" = gsub(".R", "", scenarios.list[[iterations]]),
                "mc" = haha,
                "pop" = N))
}

cont.summ <- function(rf, name, ...) {
    mylist <- list()
    mylist[[paste0(name, ".mean")]] <- mean(rf, na.rm=T)
    mylist[[paste0(name, ".sd")]] <- sd(rf, na.rm=T)
    #mylist[[paste0(name, ".median")]] <- median(rf, na.rm=T) # disabled to improve spead
    #mylist[[paste0(name, ".mad")]] <- mad(rf, na.rm=T)
    #mylist[[paste0(name, ".iqr")]] <- IQR(rf, na.rm=T)
    return(mylist)
}

cat.summ <- function(rf, name, ...) {
    absol <-summary(factor(rf, exclude = c(NA, NaN, "99"), ...))
    #pct <- prop.table(absol)
    absol <- absol[names(absol)!="NA's"]
    #ct <- pct[names(pct)!="NA's"]
    names(absol) <- paste0(name, ".", names(absol))
    #names(pct) <- paste0(name, ".", names(pct), ".pct")
    #return(as.list(c(absol, pct)))
    return(as.list(absol))
}

output.rf  <- function(x, ...) {
    with(x, return(c(pop.summ(nrow(x)),
                     cont.summ(bmival, "bmi"),
                     cont.summ(bmival.cvdlag, "bmi.cvd"),
                     cont.summ(bmival.calag, "bmi.ca"),
                     cont.summ(omsysval, "sbp"),
                     cont.summ(omsysval.cvdlag, "sbp.cvd"),
                     cont.summ(cholval, "tc"),
                     cont.summ(cholval.cvdlag, "tc.cvd"),
                     cat.summ(cigst1.cvdlag, "smok.cvd", levels = 1:4, labels = c("never", "ex.2", "ex.3", "active")),
                     cat.summ(cigst1.calag, "smok.ca", levels = 1:4, labels = c("never", "ex.2", "ex.3", "active")),
                     cat.summ(porftvg.cvdlag, "fv.cvd", levels = 0:9),
                     cat.summ(porftvg.calag, "fv.ca", levels = 0:9),
                     cat.summ(frtpor.cvdlag, "fruit.cvd", levels = 0:9),
                     cat.summ(frtpor.calag, "fruit.ca", levels = 0:9),
                     cat.summ(diabtotr.cvdlag, "diab.cvd", levels = 1:2, labels = c("no", "yes")),
                     cat.summ(expsmokCat, "ets", levels = 0:1))))
}

output.chd  <- function(x, ...) {
    O1 <- pop.summ(nrow(x))
    O2 <- with(x, cat.summ(chd.incidence, "chd",levels = init.year + i, labels="incidence"))
    O3 <- with(x, sum(table(factor(chd.incidence, exclude = c(0, NA)))))
    names(O3) <- "chd.prevalence"
    O4 <- with(x, sum(table(dead, exclude=c(F, NA, NaN))))
    names(O4) <- "chd.mortality"
    return(c(O1, O2, O3, O4))
}

output.stroke  <- function(x, ...) {
    O1 <- pop.summ(nrow(x))
    O2 <- with(x, cat.summ(stroke.incidence, "stroke",levels = 2011 + i, labels="incidence"))
    O3 <- with(x, sum(table(factor(stroke.incidence, exclude = c(0, NA)))))
    names(O3) <- "stroke.prevalence"
    O4 <- with(x, sum(table(dead, exclude=c(F, NA, NaN))))
    names(O4) <- "stroke.mortality"
    return(c(O1, O2, O3, O4))
}

output.other  <- function(x, ...) {
    O1 <- pop.summ(nrow(x))
    O2 <- with(x, sum(table(dead, exclude=c(F, NA, NaN))))
    names(O2) <- "other.mortality"
    return(c(O1, O2))
}


# ASFR for 2010 is the observed from ONS(same for all fertility projections), for 2011 is copy of 2012
if (Fertility.Assumption == "N") {
    Fertility <- read.csv("./Fertility/Principal fertility ONS projections.csv", header = T, 
                          colClasses = "numeric")
} else if (Fertility.Assumption == "H") {
    Fertility <- read.csv("./Fertility/High fertility ONS projections.csv", header = T, 
                          colClasses = "numeric")
} else if (Fertility.Assumption == "L") {
    Fertility <- read.csv("./Fertility/Low fertility ONS projections.csv", header = T, 
                          colClasses = "numeric")
} else stop("Fertility.Assumption was set incorrectly. Please specify fertility scenario")

Fertility = data.table(Fertility)
setnames(Fertility, c("age", 2000:2061))
setkey(Fertility, age)

# Find and load scenarios
scenarios.list <- list.files(path = "./Scenarios", pattern = glob2rx("*.R"), full.names = F, recursive = F)
n.scenarios <- length(scenarios.list)
scenarios.list <- rep(scenarios.list, each = numberofiterations)

it <- numberofiterations * n.scenarios

# specify output.txt file for simulation parameters
dir.create(path = "./Output/", recursive = T, showWarnings = F)
fileOut <- file(paste0("./Output/simulation parameters.txt"))
writeLines(c("IMPACTncd\nA dynamic microsimulation, by Dr Chris Kypridemos", "\n", 
             paste0("Simulation started at: ", Sys.time(), "\n"),
             "Simulation parameters:\n",
			 paste0("First year of the simulation = ", init.year),
             paste0("Years to project = ", yearstoproject),
             paste0("Fertility assumption = ", Fertility.Assumption),
             paste0("ageL = ", ageL),
             paste0("ageH = ", ageH),
             paste0("cvd.lag = ", cvd.lag),
             paste0("cancer.lag = ", cancer.lag),
             paste0("diseases = ", diseasestoexclude),
             paste0("Sample size = ", format(n, scientific = F)),
             paste0("Number of iterations = ", numberofiterations),
             paste0("Number of scenarios = ", n.scenarios), "\n"), fileOut)
close(fileOut)

# Import (or create) Synthetic Population
if (length(list.files("./SynthPop")) == 0) {
    cat("Building synthetic population...\nThis might take some time...\nThank you for your patience :)\n\n")
    source(file = paste0(get.dropbox.folder(), "/PhD/Models/SynthPop/Synthetic Population Script.R"))
}

