# BMI (identity gaussian)
pred.bmi <- cmpfun(function(year, age, sex, qimd, a30to06m, lag = cvd.lag) {
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
                                      a30to06m.imp = a30to06m), 
                           type = "response", 
                           se.fit=T))
  return(pr[[1]])
  #return(rnorm(nrow(pr), pr[[1]], pr[[2]]))
}
)
# time trend for all qimds
load(file="./Lagtimes/bmi.svylm.rda")
plot(pred.bmi(  0:50, 50, 1, 1, 5), ylim=c(25,35))
points(pred.bmi(0:50, 50, 1, 3, 5), ylim=c(25,35), col="blue")
points(pred.bmi(0:50, 50, 1, 5, 5), ylim=c(25,35), col="red")

bmi.svylm$coefficients["year"] <- 0.95 * bmi.svylm$coefficients["year"]  
lines(pred.bmi(0:50, 50, 1, 1, 5), ylim=c(0,35))
lines(pred.bmi(0:50, 50, 1, 3, 5), ylim=c(0,35), col="blue")
lines(pred.bmi(0:50, 50, 1, 5, 5), ylim=c(0,35), col="red")

# time trend for all qimds
load(file="./Lagtimes/bmi.svylm.rda")
plot(pred.bmi(0:50, 50, 1, 1, 5), ylim=c(0,35))
points(pred.bmi(0:50, 50, 1, 3, 5), ylim=c(0,35), col="blue")
points(pred.bmi(0:50, 50, 1, 5, 5), ylim=c(0,35), col="red")

yr <- grep("year", names(bmi.svylm$coefficients)) 
bmi.svylm$coefficients[yr] <- bmi.svylm$coefficients[yr] * 2
lines(pred.bmi(0:50, 50, 1, 1, 5), ylim=c(0,35))
lines(pred.bmi(0:50, 50, 1, 3, 5), ylim=c(0,35), col="blue")
lines(pred.bmi(0:50, 50, 1, 5, 5), ylim=c(0,35), col="red")

# time trend for to decrease inequalities
load(file="./Lagtimes/bmi.svylm.rda")
plot(pred.bmi(0:50, 30, 1, 1), ylim=c(0,30))
points(pred.bmi(0:50, 30, 1, 3), ylim=c(0,30), col="blue")
points(pred.bmi(0:50, 30, 1, 5), ylim=c(0,30), col="red")

yr <- grep("year:qimd", names(bmi.svylm$coefficients)) 
bmi.svylm$coefficients[yr] <- bmi.svylm$coefficients[yr] / 4
lines(pred.bmi(0:50, 30, 1, 1), ylim=c(0,35))
lines(pred.bmi(0:50, 30, 1, 3), ylim=c(0,30), col="blue")
lines(pred.bmi(0:50, 30, 1, 5), ylim=c(0,35), col="red")

# time trend for to completely ignore inequalities
load(file="./Lagtimes/bmi.svylm.rda")
plot(pred.bmi(0:50, 30, 1, 1), ylim=c(0,30))
points(pred.bmi(0:50, 30, 1, 3), ylim=c(0,30), col="blue")
points(pred.bmi(0:50, 30, 1, 5), ylim=c(0,30), col="red")

yr <- grep("qimd", names(bmi.svylm$coefficients)) 
bmi.svylm$coefficients[yr] <- 0
lines(pred.bmi(0:50, 30, 1, 1), ylim=c(0,35))
lines(pred.bmi(0:50, 30, 1, 3), ylim=c(0,35), col="blue")
lines(pred.bmi(0:50, 30, 1, 5), ylim=c(0,35), col="red")

# SBP 
pred.sbp <- cmpfun(function(year, age, sex, qimd, bmival, cigst1, a30to06m, lag = cvd.lag) {
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
  
  cigst2 <- factor(ifelse((cigst1 == "4"), "1", "0"), levels = c(0,1))
  
  bmival[bmival>35] <- 35 # otherwise predicts NAN values
  pr <- data.frame(predict(sbp.svylm, 
                           data.frame(year = year-lag, 
                                      age = age-lag, 
                                      sex = sex,
                                      qimd = qimd, 
                                      bmival = bmival,
                                      cigst2 = cigst2,
                                      a30to06m.imp = a30to06m), type = "response", se.fit=T))
  return(pr[[1]])
  #return(rnorm(nrow(pr), pr[[1]], pr[[2]]))
  #return(rtruncnorm(nrow(pr), a=70, b= 220, pr[[1]], pr[[2]]))
}
)

# time trend for all qimds
load(file="./Lagtimes/sbp.svylm.rda")
plot(pred.sbp  (0:50, 50, 1, 1, 30,1,5), ylim=c(80,140))
points(pred.sbp(0:50, 50, 1, 3, 30,1,5), col="blue")
points(pred.sbp(0:50, 50, 1, 5, 30,1,5), col="red")

sbp.svylm$coefficients["(Intercept)"] <- sbp.svylm$coefficients["(Intercept)"]  - 5
lines(pred.sbp(0:50, 50, 1, 1, 30,1,5))
lines(pred.sbp(0:50, 50, 1, 3, 30,1,5), col="blue")
lines(pred.sbp(0:50, 50, 1, 5, 30,1,5), col="red")

sbp.svylm$coefficients["year"] <- 
  sbp.svylm$coefficients["year"] * 0.9
lines(pred.sbp(0:50, 50, 1, 1, 30,1,5))
lines(pred.sbp(0:50, 50, 1, 3, 30,1,5), col="blue")
lines(pred.sbp(0:50, 50, 1, 5, 30,1,5), col="red")

load(file="./Lagtimes/sbp.svylm.rda")
plot(pred.sbp(0:50, 50, 1, 1, 30), ylim=c(90,130))
points(pred.sbp(0:50, 50, 1, 3, 30), col="blue")
points(pred.sbp(0:50, 50, 1, 5, 30), col="red")

yr <- grep("year", names(sbp.svylm$coefficients))
sbp.svylm$coefficients[grep("year", names(sbp.svylm$coefficients))] <- sbp.svylm$coefficients[grep("year", names(sbp.svylm$coefficients))] * 0.5
lines(pred.sbp(0:50, 50, 1, 1, 30,1,5))
lines(pred.sbp(0:50, 50, 1, 3, 30,1,5), col="blue")
lines(pred.sbp(0:50, 50, 1, 5, 30,1,5), col="red")

# Chol (inverse gaussian)
pred.chol <- cmpfun(function(year, age, sex, qimd, bmival, porftvg, a30to06m, lag = cvd.lag) {
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
  bmival[bmival>40] <- 40 # otherwise predicts NAN values
  pr <- data.frame(predict(chol.svylm, data.frame(year = year-lag,
                                                  age = age-lag, 
                                                  sex = sex, 
                                                  qimd = qimd, 
                                                  bmival = bmival, 
                                                  porftvg.imp = porftvg,
                                                  a30to06m.imp = a30to06m),
                           type = "response", 
                           se.fit=T))
  return(pr[[1]])
  #return(rtruncnorm(nrow(pr), a = 2.5, b = 12,  pr[[1]], pr[[2]]))
  #return(rnorm(nrow(pr), pr[[1]], pr[[2]]))
}
)
# time trend for all qimds
load(file="./Lagtimes/chol.svylm.rda")
plot(pred.chol  (0:50, 50, 1, 1, 30, 5, 5), ylim=c(3,6), xlim=c(0,30))
points(pred.chol(0:50, 50, 1, 3, 30, 5, 5), col="blue")
points(pred.chol(0:50, 50, 1, 5, 30, 5, 5), col="red")

chol.svylm$coefficients["(Intercept)"] <- chol.svylm$coefficients["(Intercept)"] - 0.5
lines(pred.chol(0:50, 50, 1, 1, 30, 5, 5))
lines(pred.chol(0:50, 50, 1, 3, 30, 5, 5), col="blue")
lines(pred.chol(0:50, 50, 1, 5, 30, 5, 5), col="red")

chol.svylm$coefficients["year"] <- 
  chol.svylm$coefficients["year"] * 0.95
lines(pred.chol(0:50, 50, 1, 1, 30, 5, 5))
lines(pred.chol(0:50, 50, 1, 3, 30, 5, 5), col="blue")
lines(pred.chol(0:50, 50, 1, 5, 30, 5, 5), col="red")

chol.svylm$coefficients[grep("year", names(chol.svylm$coefficients))] <- chol.svylm$coefficients[grep("year", names(chol.svylm$coefficients))] /2
points(x = 16, pred.chol(16, 66, 1, 1, 30, 5))
lines(pred.chol(0:50, 50, 1, 1, 30, 5))
lines(pred.chol(0:50, 50, 1, 3, 30, 5), col="blue")
lines(pred.chol(0:50, 50, 1, 5, 30, 5), col="red")

load(file="./Lagtimes/chol.svylm.rda")
plot(pred.chol(0:50, 50, 1, 1, 30), ylim=c(3,6))
points(pred.chol(0:50, 50, 1, 3, 30), col="blue")
points(pred.chol(0:50, 50, 1, 5, 30), col="red")

yr <- grep("year", names(chol.svylm$coefficients))
chol.svylm$coefficients[yr] <- chol.svylm$coefficients[yr] * 2
lines(pred.chol(0:50, 50, 1, 1, 30))
lines(pred.chol(0:50, 50, 1, 3, 30), col="blue")
lines(pred.chol(0:50, 50, 1, 5, 30), col="red")

# F&V
pred.fv <- function(year, age, sex, qimd, lag = cvd.lag) {
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
   cc <- rpois(nrow(cc), cc[[1]])
   cc <- cc + 0
   cc[cc>9] <- 9
  return(cc)  
}

load(file="./Lagtimes/fv.svylr.rda")

plot(pred.fv  (0:50, 20:70, 1, 1), ylim=c(0,9))
points(pred.fv(0:50, 50, 1, 3), col="blue")
points(pred.fv(0:50, 50, 1, 5), col="red")

fv.svylr$coefficients["(Intercept)"] <- fv.svylr$coefficients["(Intercept)"]  + 0.2035
lines(pred.fv(0:50, 50, 1, 1))
lines(pred.fv(0:50, 50, 1, 3), col="blue")
lines(pred.fv(0:50, 50, 1, 5), col="red")




