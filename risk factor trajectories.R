load(file="./Lagtimes/bmi.svylm.rda")
load(file="./Lagtimes/chol.svylm.rda")
load(file="./Lagtimes/sbp.svylm.rda")
load(file="./Lagtimes/diab.svylr.rda")
load(file="./Lagtimes/smok.active.svylr.rda")
load(file="./Lagtimes/smok.cess.svylr.rda")
load(file="./Lagtimes/smok.cess.success.rda")
load(file="./Lagtimes/smok.start.svylr.rda")
load(file="./Lagtimes/fv.svylr.rda")
load(file="./Lagtimes/fvrate.svylr.rda")

cat("Load RF trajectories\n")
# Define function for bmi projection (predicts mean bmi)
pred.bmi <- cmpfun(function(year, age, sex, qimd, porftvg, lag = cvd.lag) {
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
                           data.frame(year = year-lag,
                                      age = age-lag, 
                                      sex = sex, 
                                      qimd = qimd,
                                      porftvg = porftvg), 
                           type = "response", 
                           se.fit=T))
  return(rnorm(nrow(pr), pr[[1]], pr[[2]]))
}
)

# test
# summary(pred.bmi(sample(c(0:50), n, replace = T), 
#                  sample(c(20,85), n, replace = T), 
#                  sample(c(1,2), n, replace = T), 
#                  sample(c(1:5), n, replace = T),
#                  sample(c(1,10), n, replace = T)))

# Define function for sbp projection (for DT needs the by= to work correctly with mean(bmival)) (predicts mean sbp)
pred.sbp <- cmpfun(function(year, age, sex, qimd, bmival, cigst1, porftvg, lag = cvd.lag) {
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
  if (is.factor(cigst1)==F) {
    cigst1 <-  factor(cigst1, 
                      levels = c(1:4), 
                      ordered = F)
  }
  bmival[bmival>50] <- 50 # otherwise predicts NAN values
  pr <- data.frame(predict(sbp.svylm, 
                           data.frame(year = year-lag, 
                                      age = age-lag, 
                                      sex = sex,
                                      qimd = qimd, 
                                      bmival = bmival,
                                      cigst1 = cigst1,
                                      porftvg = porftvg), type = "response", se.fit=T))
  #return(pr[[1]])
  #return(rnorm(nrow(pr), pr[[1]], pr[[2]]/4))
  return(rtruncnorm(nrow(pr), a=70, b= 220, pr[[1]], pr[[2]]))
}
)

#test
# summary(pred.sbp(sample(c(0:50), n, replace = T), 
#                  sample(c(20,85), n, replace = T), 
#                  sample(c(1,2), n, replace = T), 
#                  sample(c(1:5), n, replace = T),
#                  runif(n, 10, 90),
#                  sample(c(1,10), n, replace = T)))

# Define function for chol projection (for ages above 30)
pred.chol <- cmpfun(function(year, age, sex, qimd, bmival, porftvg, lag = cvd.lag) {
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
  pr <- data.frame(predict(chol.svylm, data.frame(year = year-lag,
                                                  age = age-lag, 
                                                  sex = sex, 
                                                  qimd = qimd, 
                                                  bmival = bmival, 
                                                  porftvg = porftvg),
                           type = "response", 
                           se.fit=T))
  #return(pr[[1]])
  return(rtruncnorm(nrow(pr), a = 2.5, b = 12,  pr[[1]], pr[[2]]))
}
)

# test
# summary(pred.chol(sample(c(-10:50), n, replace = T), 
#                   sample(c(20,85), n, replace = T), 
#                   sample(c(1,2), n, replace = T), 
#                   sample(c(1:5), n, replace = T),
#                   runif(n, 10, 50),runif(n, 10, 20),
#                   sample(c(1,10), n, replace = T)))

# Define function for diab prevalence projection 
pred.diab <- cmpfun(function(year, age, sex, qimd, bmival) {
  if (
    is.factor(sex) == F
  ) {
    sex <-  factor(
      sex, 
      levels = c(1,2), 
      ordered = F
    )
  }
  if (
    is.ordered(qimd) == F
  ) {
    qimd <- factor(
      qimd, 
      levels = c(1,2,3,4,5), 
      ordered = T
    )
  }
  
  bmival[bmival>40] <- 40 # otherwise predicts NAN values
  
  pr <- data.frame(
    predict(
      diab.svylr, data.frame(
        year = year, 
        age = age, 
        sex = sex, 
        qimd = qimd, 
        bmival = bmival
      ), type = "response", 
      se.fit = T)
  )
  #return(rtruncnorm(nrow(pr), a = 0, b = 1, mean=pr[[1]], sd=pr[[2]])) 
  return(pr[[1]])
}
)

# function to estimate diabetes incidence
# rr = the rr of dying because of diabetes from Group TDS. Is the Current Definition for Diabetes Relevant to Mortality Risk From All Causes and Cardiovascular and Noncardiovascular Diseases? Dia Care. 2003 Jan 3;26(3):688–96. 
pred.diab.incid <- cmpfun(function(year, age, sex, qimd, bmival, lag ) { 
  prev0 <- pred.diab(year = year - lag, age = age - lag, sex = sex, qimd = qimd, bmival = bmival)
  prev1 <- pred.diab(year = year - lag + 1, age = age - lag + 1, sex = sex, qimd = qimd, bmival = bmival)
  tc <- ifelse ((prev1 - prev0) <= 0, 0, (prev1 - prev0)) # incidence in year = year. Produces values above 1 occassionaly
  return(as.logical(rbinom(length(tc), 1, tc)))
}
)

pred.diab.incid.lag <- cmpfun(function(year, age, sex, qimd, bmival, lag, duration = lag, n) { 
  prev0 <- pred.diab(year = year-lag, age = age-lag, sex = sex, qimd = qimd, bmival = bmival)
  prev1 <- pred.diab(year = year-lag + duration, age = age-lag+duration, sex = sex, qimd = qimd, bmival = bmival)
  tc <- prev0 / prev1 # derived from bayes theorem P(diab2008|diab2011)= P(diab2011|diab2008)*P(diab2008)/P(diab2011) and P(diab2011|diab2008) = 1)
  return(as.factor(ifelse(dice(n) < tc, "2","1")))
}
)

# Gives the annual probability of a never smoker to become  smoker next year
# all other remain never smokers
pred.nev0sm1 <- cmpfun(function(year, age, qimd, lag) {
  qimd <- ifelse(qimd == 1, "1", "2")
  qimd <- ordered(qimd, levels=1:2)
  
  pnev0sm1 <- data.frame(predict(smok.start.svylr, data.frame(year = year-lag, age = age-lag, qimd = qimd), type = "response", se.fit=T))
  return(rtruncnorm(nrow(pnev0sm1), 0, 1, pnev0sm1[[1]], pnev0sm1[[2]]))
}
)

# plot(pred.nev0sm1(16:60, "1"), ylim=c(0,0.2))
# lines(pred.nev0sm1(16:60, "3"), ylim=c(0,0.2))

# Predicts the annual probability of a smoker to become ex-smoker
pred.sm0ex1 <- cmpfun(function(year, age, sex, qimd, lag) {
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
  sm0ex1 <- data.frame(predict(smok.cess.svylr, data.frame(year = year-lag, age = age-lag, sex = sex, qimd = qimd), type = "response", se.fit=T))
  return(rtruncnorm(nrow(sm0ex1), 0, 1, sm0ex1[[1]], sm0ex1[[2]]))
  #return(sm0ex1[[1]])
}
)

# for (jj in 1:5) {
#     plot(16:90, pred.sm0ex1(0, 16:90, 1, jj), ylim=c(0,0.4))
# }

# Predicts probability of ex-smoker to become active smoker (relapse) (only works for 1<endsmoke<10). Else should be 0
pred.ex0sm1 <- cmpfun(function(endsmoke, sex, qimd) {
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
)
#pred.ex0sm1(1:10, 1, 1)

# predicts the active smoker prevalence
pred.sm0prev <- cmpfun(function(year, age, sex, qimd) {
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
)
# for (jj in 1:5) {
#     plot(pred.sm0prev(0:50, 20, 1, jj), ylim=c(0,0.8))
# }



# Define function for F&V
FV.intervention <- 0
pred.fv <- cmpfun(function(year, age, sex, qimd, lag = cvd.lag) {
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
  cc <- data.frame(predict(fv.svylr, 
                           data.frame(year= year - lag,
                                      age = age - lag, 
                                      sex = sex, 
                                      qimd = qimd), 
                           type="response", se.fit=T))
  #cc <- rtruncnorm(nrow(cc), a=0, b=9, mean=cc[[1]], sd=cc[[2]])
  cc <- rpois(nrow(cc), cc[[1]] + FV.intervention)# to be used for modelling interventions
  cc[cc>9] <- 9
  return(cc)  
}
)

# test
# summary(factor(pred.fv(sample(c(0:50), n, replace = T), 
#                        sample(c(20,85), n, replace = T), 
#                        sample(c(1,2), n, replace = T), 
#                        sample(c(1:5), n, replace = T),
#                        runif(n, 10, 90),
#                        sample(c(1,10), n, replace = T))))/n
# SPOP2011[age>19, summary(factor(porftvg))/.N]

# Define function for Fruit consumption
pred.fvrate <- cmpfun(function(year, age, sex, qimd, porftvg, lag = cvd.lag) {
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
)

# test
# summary(factor(pred.fvrate(sample(c(0), n, replace = T), 
#                            sample(c(20,85), n, replace = T), 
#                            sample(c(1,2), n, replace = T), 
#                            sample(c(1:5), n, replace = T),
#                            sample(c(0:9), n, replace = T),
#                            sample(c(1,10), n, replace = T))))
