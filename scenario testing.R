# BMI (identity gaussian)
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
    return(pr[[1]])
}


# time trend for all qimds
load(file="./Lagtimes/bmi.svylm.rda")
plot(pred.bmi(0:50, 50, 1, 1), ylim=c(0,35))
points(pred.bmi(0:50, 50, 1, 3), ylim=c(0,35), col="blue")
points(pred.bmi(0:50, 50, 1, 5), ylim=c(0,35), col="red")

bmi.svylm$coefficients["year"] <- bmi.svylm$coefficients["year"] * 2
lines(pred.bmi(0:50, 50, 1, 1), ylim=c(0,35))
lines(pred.bmi(0:50, 50, 1, 3), ylim=c(0,35), col="blue")
lines(pred.bmi(0:50, 50, 1, 5), ylim=c(0,35), col="red")

# time trend for all qimds
load(file="./Lagtimes/bmi.svylm.rda")
plot(pred.bmi(0:50, 50, 1, 1), ylim=c(0,35))
points(pred.bmi(0:50, 50, 1, 3), ylim=c(0,35), col="blue")
points(pred.bmi(0:50, 50, 1, 5), ylim=c(0,35), col="red")

yr <- grep("year", names(bmi.svylm$coefficients)) 
bmi.svylm$coefficients[yr] <- bmi.svylm$coefficients[yr] * 2
lines(pred.bmi(0:50, 50, 1, 1), ylim=c(0,35))
lines(pred.bmi(0:50, 50, 1, 3), ylim=c(0,35), col="blue")
lines(pred.bmi(0:50, 50, 1, 5), ylim=c(0,35), col="red")

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

# SBP (inverse gaussian)
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
    return(pr[[1]])
}


# time trend for all qimds
load(file="./Lagtimes/sbp.svylm.rda")
plot(pred.sbp(0:50, 50, 1, 1, 30), ylim=c(80,130))
points(pred.sbp(0:50, 50, 1, 3, 30), col="blue")
points(pred.sbp(0:50, 50, 1, 5, 30), col="red")

sbp.svylm$coefficients["year"] <- sbp.svylm$coefficients["year"] * 2
lines(pred.sbp(0:50, 50, 1, 1, 30))
lines(pred.sbp(0:50, 50, 1, 3, 30), col="blue")
lines(pred.sbp(0:50, 50, 1, 5, 30), col="red")


load(file="./Lagtimes/sbp.svylm.rda")
plot(pred.sbp(0:50, 50, 1, 1, 30), ylim=c(90,130))
points(pred.sbp(0:50, 50, 1, 3, 30), col="blue")
points(pred.sbp(0:50, 50, 1, 5, 30), col="red")

yr <- grep("year", names(sbp.svylm$coefficients))
sbp.svylm$coefficients[yr] <- sbp.svylm$coefficients[yr] * 2
lines(pred.sbp(0:50, 50, 1, 1, 30))
lines(pred.sbp(0:50, 50, 1, 3, 30), col="blue")
lines(pred.sbp(0:50, 50, 1, 5, 30), col="red")

# Chol (inverse gaussian)
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
    return(pr[[1]])
}

# time trend for all qimds
load(file="./Lagtimes/chol.svylm.rda")
plot(pred.chol(0:50, 50, 1, 1, 30), ylim=c(3,6))
points(pred.chol(0:50, 50, 1, 3, 30), col="blue")
points(pred.chol(0:50, 50, 1, 5, 30), col="red")

chol.svylm$coefficients["year"] <- chol.svylm$coefficients["year"] * 2
lines(pred.chol(0:50, 50, 1, 1, 30))
lines(pred.chol(0:50, 50, 1, 3, 30), col="blue")
lines(pred.chol(0:50, 50, 1, 5, 30), col="red")

load(file="./Lagtimes/chol.svylm.rda")
plot(pred.chol(0:50, 50, 1, 1, 30), ylim=c(3,6))
points(pred.chol(0:50, 50, 1, 3, 30), col="blue")
points(pred.chol(0:50, 50, 1, 5, 30), col="red")

yr <- grep("year", names(chol.svylm$coefficients))
chol.svylm$coefficients[yr] <- chol.svylm$coefficients[yr] * 2
lines(pred.chol(0:50, 50, 1, 1, 30))
lines(pred.chol(0:50, 50, 1, 3, 30), col="blue")
lines(pred.chol(0:50, 50, 1, 5, 30), col="red")