#cmpfile("./risk factor trajectories.R")
## IMPACTncd: A decision support tool for primary prevention of NCDs
## Copyright (C) 2015  Chris Kypridemos

## IMPACTncd is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program; if not, see <http://www.gnu.org/licenses/>
## or write to the Free Software Foundation, Inc., 51 Franklin Street,
## Fifth Floor, Boston, MA 02110-1301  USA.


load(file="./Lagtimes/bmi.svylm.rda")
load(file="./Lagtimes/chol.svylm.rda")
load(file="./Lagtimes/sbp.svylm.rda")
load(file="./Lagtimes/diab.svylr.rda")
#load(file="./Lagtimes/smok.active.svylr.rda")
#load(file="./Lagtimes/smok.exactive.svylr.rda")
load(file="./Lagtimes/smok.cess.svylr.rda")
#load(file="./Lagtimes/smok.cess.success.log.rda")
#load(file="./Lagtimes/smok.cess.success.parabola.rda")
load(file="./Lagtimes/smok.start.svylr.rda")
#load(file="./Lagtimes/smok.cigdyal.svylm.rda")
load(file="./Lagtimes/cigdyal.svylr.rda")
load(file="./Lagtimes/fv.svylr.rda")
load(file="./Lagtimes/pa.svylr.rda")
load(file="./Lagtimes/salt.rq.rda")
load(file="./Lagtimes/bpmed.svylr.rda")

cat("Load RF trajectories\n")

# PA prediction ---------------------------------------
PA.intervention <- 0L
PA.intervention.success <- 1 # eg 0.8 means 80% will increase pa by the intervention days
pred.pa <-
  cmpfun(
    function(year, age, sex, qimd, percentil.rank, lag = cvd.lag, fortune = 0.1) {
      percentil.rank <- bound(jitter(percentil.rank, amount = fortune)) 
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
  
  newdata <- data.table(
    year = year - lag,
    age  = age  - lag, 
    sex  = sex, 
    qimd = qimd
  )
  #code adapted from method getAnywhere(predict.polr)
  Terms <- delete.response(pa.svylr$terms)
  m <- model.frame(Terms, newdata, na.action = function(x) x, 
                   xlev = pa.svylr$xlevels)
  if (!is.null(cl <- attr(Terms, "dataClasses"))) 
    .checkMFClasses(cl, m)
  X <- model.matrix(Terms, m, contrasts = pa.svylr$contrasts)
  xint <- match("(Intercept)", colnames(X), nomatch = 0L)
  if (xint > 0L) 
    X <- X[, -xint, drop = FALSE]
  n <- nrow(X)
  q <- length(pa.svylr$zeta)
  eta <- drop(X %*% pa.svylr$coefficients)
  cc <- as.data.table(plogis(matrix(pa.svylr$zeta, n, q, byrow = TRUE) - 
                            eta))
  set(cc, NULL, "V8", 1)
  #if (paired) set.seed(seed[[counter[[iterations]]]] + year)
  set(cc, NULL, "d", percentil.rank)
  for (k in 1:8) set(cc, NULL, k, cc[, Reduce(`<`, .SD), .SDcol = c(k, 9)])
  cc[, a30 :=  Reduce(`+`, .SD), .SDcol = 1:8]
  
  if (PA.intervention != 0L) {
    #if (paired) set.seed(seed[[counter[[iterations]]]] + year + 1L)
    cc[a30 <= 7L - PA.intervention &
         rbinom(.N, 1, PA.intervention.success) == 1,
       a30:= a30 + PA.intervention
       ]
  }
  return(cc[, a30])  
} 
)

# F&V prediction --------------------------------------
FV.intervention <- 0L
FV.intervention.success <- 1 # eg 0.8 means 80% will increase f&v by the intervention portions
pred.fv <- 
  cmpfun(
    function(year, age, sex, qimd, percentil.rank, lag = cvd.lag, fortune = 0.1) {
  percentil.rank <- bound(jitter(percentil.rank, amount = fortune)) 
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
  
  newdata<- data.table(
    year = year - lag,
    age  = age  - lag, 
    sex  = sex, 
    qimd = qimd
  )
  #code adapted from method getAnywhere(predict.polr)
  Terms <- delete.response(fv.svylr$terms)
  m <- model.frame(Terms, newdata, na.action = function(x) x, 
                   xlev = fv.svylr$xlevels)
  if (!is.null(cl <- attr(Terms, "dataClasses"))) 
    .checkMFClasses(cl, m)
  X <- model.matrix(Terms, m, contrasts = fv.svylr$contrasts)
  xint <- match("(Intercept)", colnames(X), nomatch = 0L)
  if (xint > 0L) 
    X <- X[, -xint, drop = FALSE]
  n <- nrow(X)
  q <- length(fv.svylr$zeta)
  eta <- drop(X %*% fv.svylr$coefficients)
  cc <- as.data.table(plogis(matrix(fv.svylr$zeta, n, q, byrow = TRUE) - 
                            eta))
  set(cc, NULL, "V9", 1)
  #if (paired) set.seed(seed[[counter[[iterations]]]] + year)
  set(cc, NULL, "d", percentil.rank)
  for (k in 1:9) set(cc, NULL, k, cc[, Reduce(`<`, .SD), .SDcol = c(k, 10)])
  cc[, fv :=  Reduce(`+`, .SD), .SDcol = 1:9]
  
  if (FV.intervention != 0) {
    #if (paired) set.seed(seed[[counter[[iterations]]]] + year + 1L)
    cc[fv <= (8L - FV.intervention) &
         rbinom(.N, 1, FV.intervention.success) == 1,
       fv:= fv + FV.intervention]
  }
  return(cc[, fv])   
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

# Fruit prediction ------------------------
# pred.fvrate <- cmpfun(function(age, sex, qimd, porftvg, lag = cvd.lag) {
#   if (is.factor(sex)==F) {
#     sex <-  factor(sex, 
#                    levels = c(1,2), 
#                    ordered = F)
#   }
#   if (is.ordered(qimd)==F) {
#     qimd <- factor(qimd, 
#                    levels = c(1,2,3,4,5), 
#                    ordered = T)
#   }
#   cc <- predict(fvrate.svylr, 
#                 data.table(age     = age - lag, 
#                            sex     = sex, 
#                            qimd    = qimd, 
#                            porftvg = porftvg), 
#                 type="response", se.fit=F)
#   #cc<- rtruncnorm(nrow(cc), a = 0, b = 1, mean=cc[[1]], sd=cc[[2]])
#   #return(as.integer(porftvg*cc))
#   #if (paired) set.seed(seed[[counter[[iterations]]]] + year)
#   cc <- rbinom(length(cc), porftvg, cc)
#   return(cc)
# }
# )

# test
# summary(factor(pred.fvrate(sample(c(0), n, replace = T), 
#                            sample(c(20,85), n, replace = T), 
#                            sample(c(1,2), n, replace = T), 
#                            sample(c(1:5), n, replace = T),
#                            sample(c(0:9), n, replace = T),
#                            sample(c(1,10), n, replace = T))))

# Qdiabetes prediction ----------------------------------------------------
# Define function for diab prevalence projection with QRisk diabetes 
# http://www.qdiabetes.org/
pred.diab.qdrisk <- cmpfun(
  function(DTlocked = .SD,
           surv = 1L,
           lag = cvd.lag) {
    DT = copy(DTlocked)
    
    # DT[, age2 := age - lag]
    
    DT[, surv := surv]
    
    DT[between(age, 25, ageH), `:=` 
       (b_corticosteroids = dice(.N) < 0.005)]
    
    DT[, `:=` (b_cvd = F)]
    if (i == init.year - 2011) {
      DT[cvdcon %in% c("1", "2"), b_cvd := T] 
    } else {
      DT[chd.incidence > 0 | stroke.incidence > 0, `:=` (b_cvd = T)]
    }
    
    #white or not stated, indian, pakistani, baghladeshi, other asian,
    #black carribean, black african, chinese,  
    # from ONS white = 86% (census 2011)
    #DT[, ethrisk := sample(1:9, .N, T, c(0.86, rep(0.14/8, 8)))] ## not needed anymore. 
    ## origin(ethnicity) is defined in the synthetic population now.
    
    # The prob of being diab is 8%. So the probability of having at least
    # one of 3 family members with diabetes is 1 - (1-0.08)^3. I let family members vary above 2
    if (i == init.year - 2011) {
      DT[, fh_diab := 
           dice(.N) < 1 - (1 - sum(diabtotr == "2") / .N)^(2 + rpois(.N, 1))]
    } else {
      DT[, fh_diab := 
           dice(.N) < 1 -
           (1 - sum(diabtotr.cvdlag == "2") / .N)^(2 + rpois(.N, 1))]
    }
    
    #if (paired) set.seed(seed[[counter[[iterations]]]])
    DT[between(age, 25, 84), 
       (dice(.N) < QDrisk(age - lag,
                          sex,
                          b_corticosteroids,
                          b_cvd,
                          bpmed,
                          bmival.cvdlag,
                          origin,
                          fh_diab,
                          smoke_cat,
                          townsend,
                          surv)) + 1L]
    #     DT[, c("b_corticosteroids", "b_cvd", "b_treatedhyp",
    #            "bmi", "ethrisk", "fh_diab", "smoke_cat", "town") := NULL]
  }
)

# Diab prevalence  prediction ---------------------------------------------
pred.diab <- cmpfun(function(age, sex, qimd, bmival, a30to06m) {
  newdata <-
    data.table(
      age          = age, 
      sex          = sex, 
      qimd         = qimd, 
      bmival       = bmival,
      a30to06m.imp = a30to06m
    )
  
  type <-"response"
  total <- NULL
  tt <- delete.response(terms(formula(diab.svylr)))
  mf <- model.frame(tt, data = newdata)
  mm <- model.matrix(tt, mf)
  if (!is.null(total) && attr(tt, "intercept")) {
    mm[, attr(tt, "intercept")] <- mm[, attr(tt, "intercept")] * 
      total
  }
  eta <- drop(mm %*% coef(diab.svylr))
  eta <- switch(type, link = eta, response = diab.svylr$family$linkinv(eta))
  
  #return(rtruncnorm(nrow(pr), a = 0, b = 1, mean=pr[[1]], sd=pr[[2]])) 
}
)

# Diabetes incid ----------------------------------------------------------
# rr = the rr of dying because of diabetes from Group TDS. Is the Current Definition for Diabetes Relevant to Mortality Risk From All Causes and Cardiovascular and Noncardiovascular Diseases? Dia Care. 2003 Jan 3;26(3):688–96. 
# I1 = P1 - P0 + mortality of diabetics
pred.diab.incid <- cmpfun(function(year, age1, sex1, qimd1, bmival, a30to06m) { 
  if (
    is.factor(sex1) == F
  ) {
    sex1 <-  factor(
      sex1, 
      levels = c(1,2), 
      ordered = F
    )
  }
  if (
    is.ordered(qimd1) == F
  ) {
    qimd1 <- factor(
      qimd1, 
      levels = c(1,2,3,4,5), 
      ordered = T
    )
  }
  
  bmival[bmival>50] <- 50 # otherwise predicts NAN values
  bmival[bmival<16] <- 16 
  
  prev0 <- pred.diab(age = age1,     sex = sex1, qimd = qimd1, bmival = bmival, a30to06m = a30to06m)
  prev1 <- pred.diab(age = age1 + 1, sex = sex1, qimd = qimd1, bmival = bmival, a30to06m = a30to06m) 
  mort  <- data.table (id = seq_along(age1), age = age1, sex = sex1, qimd = qimd1, key = c("age", "sex", "qimd"))
  mort  <- Lifetable.diab[mort][, .SD, .SDcols = c(names(mort), paste0(i + init.year))] 
  setkey(mort, age, sex, qimd)
  mort  <- mort[setnames(POP[between(age, 20, 84), list(.SD[diabtotr == "2", .N]/.N), by = .(age, sex, qimd)], "V1", "actualprev")]
  mort[, mortal := .SD[, 5L, with = F] * actualprev * 1.6] # mortality * 1.6 (rr from DECODE)
  setkey(mort, id)
  tc <- prev1 - prev0 + mort[, mortal]
  #if (paired) set.seed(seed[[counter[[iterations]]]] + year)
  tc <- rbinom(length(tc), 1L, bound(tc)) + 1L
  return(as.character(tc))
  #return(prev1 - prev0)
}
)
#table(pred.diab.incid(0, rep(84, 1000), 1, 3, 28, 1))

# Diabetes lag ------------------------------------------------------------
pred.diab.incid.lag <- cmpfun(function(age, sex, qimd, bmival, a30to06m, lag) { 
  prev0 <- pred.diab(age = age - lag,            sex = sex, qimd = qimd, bmival = bmival,    a30to06m = a30to06m)
  prev1 <- pred.diab(age = age,                  sex = sex, qimd = qimd, bmival = bmival,    a30to06m = a30to06m)
  tc <- prev0 / prev1 # derived from bayes theorem P(diab2008|diab2011)= P(diab2011|diab2008)*P(diab2008)/P(diab2011) and P(diab2011|diab2008) = 1)
  tc <- rbinom(tc, 1L, bound(tc)) + 1L
  return(as.character(tc))
  #return(prev0 / prev1)
}
)

# Smoke initiation --------------------------------------------------------
# Gives the annual probability of a never smoker to become  smoker next year
# all other remain never smokers
pred.nev0sm1 <- 
  cmpfun(
    function(year, age, sex, qimd, rank, policy.effect=0, millenia=FALSE) {
      #qimd <- mapvalues(qimd,  c(1:5 ), c(1,2,2,2,2))
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
      new.data <- data.table(year = year, 
                             age = age, 
                             sex = sex, 
                             qimd = qimd)
      pnev0sm1 <- 
        as.data.table(
          predict(smok.start.svylr,
                  new.data,
                  type = "response", se.fit=T)) # prev of ever smoker
      
      if (millenia == TRUE) {
        pnev0sm1 <- cbind.dt(pnev0sm1, new.data)
        if (pnev0sm1[(year + 2011 - age) >= 2000, .N] > 0) {
          pnev0sm1[(year + 2011 - age) >= 2000, 
                   prv := (1 - policy.effect) * rnorm(.N, response, SE)]
        }
        if (pnev0sm1[(year + 2011 - age) < 2000, .N] > 0) {
          pnev0sm1[(year + 2011 - age) < 2000, 
                   prv := rnorm(.N, response, SE)]
        }
        return(1L + (rank > (1 - pnev0sm1[, prv])))
      } 
      
      if (millenia == FALSE) {
        return(1L + (rank > (1 - pnev0sm1[, (1 - policy.effect) * rnorm(.N, response, SE)]))) #2 or T means they start smoking.
      }
    }
  )
# plot(pred.nev0sm1(0, 16:90, 1, 1, 1), ylim=c(0,1))
# lines(pred.nev0sm1(16:60, "3"), ylim=c(0,0.2))

# Smoke cessation ---------------------------------------------------------
# Predicts the annual probability of a smoker to become ex-smoker and vice versa
pred.sm0ex1 <- 
  cmpfun(
    function(year, age, sex, qimd, rank, policy.effect = 0, millenia = FALSE) {
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
      
      new.data <- data.table(
        year = year, 
        age = age, 
        sex = sex,
        qimd = qimd)    
      
      sm0ex1 <- as.data.table(
        predict(
          smok.cess.svylr,
          new.data, 
          type   = "response", 
          se.fit = T) # preval of ex-smokers among ever smokers
      )
      
      if (millenia == TRUE) {
        sm0ex1 <- cbind.dt(sm0ex1, new.data)
        if (sm0ex1[(year + 2011 - age) >= 2000, .N] > 0) {
          sm0ex1[(year + 2011 - age) >= 2000, 
                 prv := (1 - policy.effect) * (1 - rnorm(.N, response, SE))]
        }
        if (sm0ex1[(year + 2011 - age) < 2000, .N] > 0) {
          sm0ex1[(year + 2011 - age) < 2000, 
                 prv := 1 - rnorm(.N, response, SE)]
        }
        return(3L + ((1 - rank) < sm0ex1[, prv]))
      } 
      
      if (millenia == FALSE) {
        return(3L + ((1 - rank) < sm0ex1[,  (1 - policy.effect) * (1 - rnorm(.N, response, SE))])) #4==T means remain active smokers.
      }
    }
  )
#pred.sm0ex1(0, 16:90, 1, 1, 1)[]
# for (jj in 1:5) {
#     plot(16:90, pred.sm0ex1(0, 16:90, 1, jj), ylim=c(0,0.4))
# }

# Smoke relapse -----------------------------------------------------------

# # Predicts probability of ex-smoker to become active smoker (relapse) (only works for 1<endsmoke<10). Else should be 0
# pred.ex0sm1 <- cmpfun(
#   function(endsmoke, sex, qimd, type = c("parabola", "log")) {
#     if (is.factor(sex)==F) {
#       sex <-  factor(sex, 
#                      levels = c(1,2), 
#                      ordered = F)
#     }
#     if (is.ordered(qimd)==F) {
#       qimd <- factor(qimd, 
#                      levels = c(1,2,3,4,5), 
#                      ordered = T)
#     }
#     
#     if (type == "parabola") {
#       ex0sm1 <-
#         as.data.table(
#           predict(
#             smok.cess.success.parabola,
#             data.table(endsmoke = bound(endsmoke, 1, 20), sex = sex, qimd = qimd), 
#             type = "response",
#             se.fit = T))
#     }
#     if (type == "log") {
#       ex0sm1 <- 
#         as.data.table(
#           predict(
#             smok.cess.success.log, 
#             data.table(endsmoke = bound(endsmoke, 1, 20), qimd = qimd),
#             type = "response", 
#             se.fit = T))
#     }
#     #pr <- rbinom(length(ex0sm1), 1, ex0sm1)
#     return(ex0sm1[, rbinom(.N, 1, bound(rnorm(.N, fit, se.fit)))])
#     #return(pr)
#   }
# )
# #pred.ex0sm1(1:10, 1, 1, "parabola")
# 
# # Smoke prevalence --------------------------------------------------------
# # predicts the active smoker prevalence for age 20
# pred.sm0prev <-
#   cmpfun(
#     function(year, sex, qimd) {
#       if (is.factor(sex)==F) {
#         sex <-  factor(sex, 
#                        levels = c(1,2), 
#                        ordered = F)
#       }
#       if (is.ordered(qimd)==F) {
#         qimd <- factor(qimd, 
#                        levels = c(1,2,3,4,5), 
#                        ordered = T)
#       }
#       
#       sm0prev <- as.data.table(predict(smok.active.svylr, data.table(year = year, sex = sex, qimd = qimd), type = "response", se.fit=T))
#       #return(sm0prev)
#       return(sm0prev[, rbinom(.N, 1, bound(rnorm(.N, response, SE)))])
#     }
#   )
# 
# # for (jj in 1:5) {
# #     plot(pred.sm0prev(-10:10, 20, 1, jj), ylim=c(0, .8), main = as.character(jj))
# # }
# 
# # Ex-Smoke prevalence --------------------------------------------------------
# # predicts the ex-smoker prevalence for age 20
# pred.ex0prev <-
#   cmpfun(
#     function(year, sex) {
#       if (is.factor(sex)==F) {
#         sex <-  factor(sex, 
#                        levels = c(1,2), 
#                        ordered = F)
#       }
#       
#       ex0prev <- as.data.table(predict(smok.exactive.svylr, data.table(year = year, sex = sex), type = "response", se.fit=T))
#       return(ex0prev[, rbinom(.N, 1, bound(rnorm(.N, response, SE)))])
#     }
#   )

# Smoke cigdyal --------------------------------------------------------
# predicts number of cigars for active smokers
# To ignore rank use dice(.N)
pred.cigdyal <- 
  cmpfun(
    function(year, age, sex, qimd, percentil.rank, policy.effect = 0, fortune = 0.05) {
      # add randomness to rank
      percentil.rank <- bound(jitter(percentil.rank, amount = fortune)) 
      
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
      
      newdata <- data.table(
        year     = year,
        age      = age, 
        sex      = sex, 
        qimd     = qimd
      )
      
      #code adapted from method getAnywhere(predict.polr)
      Terms <- delete.response(cigdyal.svylr$terms)
      m <- model.frame(Terms, newdata, na.action = function(x) x, 
                       xlev = cigdyal.svylr$xlevels)
      if (!is.null(cl <- attr(Terms, "dataClasses"))) 
        .checkMFClasses(cl, m)
      X <- model.matrix(Terms, m, contrasts = cigdyal.svylr$contrasts)
      xint <- match("(Intercept)", colnames(X), nomatch = 0L)
      if (xint > 0L) 
        X <- X[, -xint, drop = FALSE]
      n <- nrow(X)
      q <- length(cigdyal.svylr$zeta)
      eta <- drop(X %*% cigdyal.svylr$coefficients)
      cc <- as.data.table(plogis(matrix(cigdyal.svylr$zeta, n, q, byrow = TRUE) - 
                                   eta))
      set(cc, NULL, "V8", 1)
      #if (paired) set.seed(seed[[counter[[iterations]]]] + year)
      set(cc, NULL, "dd", percentil.rank)
      for (k in 1:8) set(cc, NULL, k, cc[, Reduce(`<`, .SD), .SDcol = c(k, 9)])
      cc[, cigdyal :=  Reduce(`+`, .SD), .SDcol = 1:8]
      #cc[is.na(cigdyal), cigdyal:=0]
      cc[, cigdyal := as.integer(runif(.N, cigdyal*4, cigdyal*4+4 ))]
      cc[cigdyal<1, cigdyal := 1]
      cc[, cigdyal := cigdyal * (1 - policy.effect)]
      cc[cigdyal<1, cigdyal := 1]
      return(cc[, cigdyal])   
    }
  ) 
# for (jj in 1:5) {
#     plot(pred.cigdyal(-10:10, 20, 1, 5, jj), ylim=c(0, 50), main = as.character(jj))
# }

# BMI prediction ----------------------------------------------------------
# Define function for bmi projection (predicts mean bmi)
pred.bmi <- 
  cmpfun(
    function(year, age, sex, qimd, a30to06m, lag = cvd.lag) {
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
      pr <- data.frame(
        predict(
          bmi.svylm, 
          data.table(
            year         = year - lag,
            age          = age  - lag, 
            sex          = sex, 
            qimd         = qimd,
            a30to06m.imp = a30to06m
          ), 
          type = "response", 
          se.fit=T
        )
      )
      #return(pr[[1]])
      return(rnorm(nrow(pr), pr[[1]], pr[[2]]))
    }
  )

# test
# summary(pred.bmi(sample(c(0:50), n, replace = T), 
#                  sample(c(20,85), n, replace = T), 
#                  sample(c(1,2), n, replace = T), 
#                  sample(c(1:5), n, replace = T),
#                  sample(c(1,10), n, replace = T)))

# Salt prediction ---------------------------------------------------------
# Returns a dataframe of 24h salt percentiles by year, age, sex, qimd 
pred.salt <- 
  cmpfun(
    function(year, lag = cancer.lag) {
      # year <- 
        # switch(EXPR = as.character((year - lag)),
        #        "-16" =  lag - 8,
        #        "-15" =  lag - 8.5,
        #        "-14" =  lag - 9,
        #        "-13" =  lag - 9.5,
        #        "-12" =  lag - 9,
        #        "-11" =  lag - 8.5,
        #        "-10" =  lag - 8,
        #        "-9"  =  lag - 7.7,
        #        "-8"  =  lag - 7.5,
        #        year
        # )
      if (lag>4) lag <- 4
      
      tmp <- expand.grid(
        year = year-lag,
        age  = (19 - lag):(ageH - lag),
        #age  = (ageL-lag):(ageH-lag),
        sex  = factor(1:2),
        qimd = ordered(1:5)
      )
      cc <- predict(salt.rq, tmp)^3
      
      tmp <- setDT(cbind(tmp, cc))
      tmp[, `:=` (year = NULL, age = age + lag)]
      setnames(tmp,
               paste0("tau= ", sprintf("%.2f", c(0.01, 1:19/20, 0.99))),
               paste0(c(0.01, 1:19/20, 0.99)))
      
      tmp <- melt(tmp, 1:3, 
                  variable.name = "percentile",
                  value.name = "salt.u", 
                  variable.factor = F)
      
      tmp[, percentile := as.numeric(percentile)]
      tmp[, salt.l := shift(salt.u, 1, 1, "lag"),
          by = .(age, sex, qimd)]
      tmp[salt.u<salt.l, salt.t := salt.u] # logic to reverse column l u columns
      tmp[salt.u<salt.l, `:=` (salt.u = salt.l, salt.l = salt.t)]
      if ("salt.t" %in% names(tmp)) tmp[, salt.t := NULL]
      return(tmp)
    }
  )

# SBP prediction ----------------------------------------------------------
# Define function for sbp projection (for DT needs the by= to work correctly with mean(bmival)) (predicts mean sbp)
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
  
  # cigst2 <- factor(as.integer(cigst1 == "4"))
  cigst2 <- mapvalues(cigst1,  c(4:1 ), c(1,0,0,0))
  bmival[bmival>50] <- 50 # otherwise predicts NAN values
  pr <- data.frame(
    predict(
      sbp.svylm, 
      data.table(
        year = year-lag, 
        age = age-lag, 
        sex = sex,
        qimd = qimd, 
        bmival = bmival,
        cigst2 = cigst2,
        a30to06m.imp = a30to06m),
      type = "response", se.fit = T
    )
  )
  #return(pr[[1]])
  return(rnorm(nrow(pr), pr[[1]], pr[[2]]))
  #return(rtruncnorm(nrow(pr), a=70, b= 220, pr[[1]], pr[[2]]))
}
)

#test
# summary(pred.sbp(sample(c(0:50),  n, replace = T), 
#                  sample(c(20,85), n, replace = T), 
#                  sample(c(1,2),   n, replace = T), 
#                  sample(c(1:5),   n, replace = T),
#                  runif(n, 10, 90),
#                  sample(c(1,10),  n, replace = T)))

# Chol prediction ---------------------------------------------------------
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
  bmival[bmival>40] <- 40 # otherwise predicts NAN values
  pr <- data.frame(
    predict(
      chol.svylm, 
      data.table(
        year = year-lag,
        age = age-lag, 
        sex = sex, 
        qimd = qimd, 
        bmival = bmival, 
        porftvg.imp = porftvg),
      type = "response", 
      se.fit=T
    )
  )
  #return(pr[[1]])
  #return(rtruncnorm(nrow(pr), a = 2.5, b = 12,  pr[[1]], pr[[2]]))
  return(rnorm(nrow(pr), pr[[1]], pr[[2]]))
}
)

# test
# summary(pred.chol(sample(c(-10:50), n, replace = T), 
#                   sample(c(20,85), n, replace = T), 
#                   sample(c(1,2), n, replace = T), 
#                   sample(c(1:5), n, replace = T),
#                   runif(n, 10, 50),runif(n, 10, 20),
#                   sample(c(1,10), n, replace = T)))

# BP medication prediction ---------------------------------------------
pred.bpmed <- cmpfun(function(n, age, sex, qimd, omsysval) {
  newdata <-
    data.table(
      age          = age, 
      sex          = sex,
      qimd         = qimd,
      omsysval     = omsysval)
  
  type <-"response"
  total <- NULL
  tt <- delete.response(terms(formula(bpmed.svylr)))
  mf <- model.frame(tt, data = newdata)
  mm <- model.matrix(tt, mf)
  if (!is.null(total) && attr(tt, "intercept")) {
    mm[, attr(tt, "intercept")] <- mm[, attr(tt, "intercept")] * 
      total
  }
  eta <- drop(mm %*% coef(bpmed.svylr))
  eta <- switch(type, link = eta, response = bpmed.svylr$family$linkinv(eta))
  rbinom(n, 1, eta)
  #return(rtruncnorm(nrow(pr), a = 0, b = 1, mean=pr[[1]], sd=pr[[2]])) 
}
)

# Undiagnosed DM prediction ---------------------------------------------
if (length(grep("health check", scenarios.list[[iterations]]))) {
  load(file="./Lagtimes/undiag.diab.svylr.rda")
  
  pred.undiag.diab <- cmpfun(
    function(n, qimd, year) {
      if (is.ordered(qimd)==F) {
        qimd <- factor(qimd, 
                       levels = c(1,2,3,4,5), 
                       ordered = T)
      }
      newdata <-
        data.table(
          qimd = qimd)
      
      type <-"response"
      total <- NULL
      tt <- delete.response(terms(formula(undiag.diab.svylr)))
      mf <- model.frame(tt, data = newdata)
      mm <- model.matrix(tt, mf)
      if (!is.null(total) && attr(tt, "intercept")) {
        mm[, attr(tt, "intercept")] <- mm[, attr(tt, "intercept")] * 
          total
      }
      eta <- drop(mm %*% coef(undiag.diab.svylr))
      eta <- switch(type, link = eta, response = undiag.diab.svylr$family$linkinv(eta))
      rbinom(n, 1, eta)
      #return(rtruncnorm(nrow(pr), a = 0, b = 1, mean=pr[[1]], sd=pr[[2]])) 
    }
  )
}

# COPD prevalence prediction -------------------------------
if ("C34" %in% diseasestoexclude) {
  load(file="./Lagtimes/copd.svylr.rda")
  CPS <- fread("./Cancer Statistics/CPSII C34 nonsmokers deaths.csv")
  CPS[, sex := factor(sex)]
  CPS[, rr.sm := predict(loess(rr~age, span=0.7)), by=sex]
  pred.copd <- cmpfun(
    function(n, age, qimd, cigst1) {
      if (!is.factor(cigst1)) {
        cigst1 <- factor(cigst1, 
                         levels = c(1,2,3,4), 
                         ordered = F)
      }
      if (!is.ordered(qimd)) {
        qimd <- factor(qimd, 
                       levels = c(1,2,3,4,5), 
                       ordered = T)
      }
      newdata <-
        data.table(
          age          = age, 
          qimd         = qimd,
          cigst1       = cigst1)
      
      type <-"response"
      total <- NULL
      tt <- delete.response(terms(formula(copd.svylr)))
      mf <- model.frame(tt, data = newdata)
      mm <- model.matrix(tt, mf)
      if (!is.null(total) && attr(tt, "intercept")) {
        mm[, attr(tt, "intercept")] <- mm[, attr(tt, "intercept")] * 
          total
      }
      eta <- drop(mm %*% coef(copd.svylr))
      eta <- switch(type, link = eta, response = copd.svylr$family$linkinv(eta))
      rbinom(n, 1, eta)
    }
  )
  
  # PLCO lung cancer model to estimate risk from -----
  # Tammemägi M, et al. Evaluation of the Lung Cancer Risks at which to Screen Ever- and Never-Smokers:
  # Screening Rules Applied to the PLCO and NLST Cohorts. PLoS medicine. 2014;11(12): e1001764.
  # not to be applied on never smokers
  tob.cum.risk <- 
    cmpfun(
      function(age, sex, sec, bmi, cigst1, cigdyal, numsmok, smokyrs, endsmoke, origin, i) { 
        copd.h <- pred.copd(length(age), age, sec, 1) # hypothetical if never smokers. Biased as based on prevalence not incidence
        copd.a <- pred.copd(length(age), age, sec, cigst1) # actual
        
        dt <- data.table(age, sex, i)
        
        # History of cancer: from Maddams J, et al. Projections of cancer
        # prevalence in the United  Kingdom, 2010-2040. 
        #Br J Cancer 2012; 107: 1195-1202 (table 5, scenario 1)
        dt <- data.table(age, sex, i)
        if (i < 9) {# before 2020
          dt[age < 45             & sex =="1", hist.of.ca := rbinom(.N, 1, 381/1e5)]
          dt[between(age, 45, 64) & sex =="1", hist.of.ca := rbinom(.N, 1, 2669/1e5)]
          dt[age > 64             & sex =="1", hist.of.ca := rbinom(.N, 1, 12656/1e5)]
          dt[age < 45             & sex =="2", hist.of.ca := rbinom(.N, 1, 525/1e5)]
          dt[between(age, 45, 64) & sex =="2", hist.of.ca := rbinom(.N, 1, 4952/1e5)]
          dt[age > 64             & sex =="2", hist.of.ca := rbinom(.N, 1, 12801/1e5)]
        }
        if (i >= 9 & i < 19) {# after 2020
          dt[age < 45             & sex =="1", hist.of.ca := rbinom(.N, 1, 391/1e5)]
          dt[between(age, 45, 64) & sex =="1", hist.of.ca := rbinom(.N, 1, 3037/1e5)]
          dt[age > 64             & sex =="1", hist.of.ca := rbinom(.N, 1, 15558/1e5)]
          dt[age < 45             & sex =="2", hist.of.ca := rbinom(.N, 1, 567/1e5)]
          dt[between(age, 45, 64) & sex =="2", hist.of.ca := rbinom(.N, 1, 5914/1e5)]
          dt[age > 64             & sex =="2", hist.of.ca := rbinom(.N, 1, 15909/1e5)]
        }
        if (i >= 19 & i < 29) {# after 2030
          dt[age < 45             & sex =="1", hist.of.ca := rbinom(.N, 1, 424/1e5)]
          dt[between(age, 45, 64) & sex =="1", hist.of.ca := rbinom(.N, 1, 3459/1e5)]
          dt[age > 64             & sex =="1", hist.of.ca := rbinom(.N, 1, 18698/1e5)]
          dt[age < 45             & sex =="2", hist.of.ca := rbinom(.N, 1, 655/1e5)]
          dt[between(age, 45, 64) & sex =="2", hist.of.ca := rbinom(.N, 1, 7299/1e5)]
          dt[age > 64             & sex =="2", hist.of.ca := rbinom(.N, 1, 19261/1e5)]
        }
        if (i >= 29 ) {# after 2040
          dt[age < 45             & sex =="1", hist.of.ca := rbinom(.N, 1, 431/1e5)]
          dt[between(age, 45, 64) & sex =="1", hist.of.ca := rbinom(.N, 1, 3632/1e5)]
          dt[age > 64             & sex =="1", hist.of.ca := rbinom(.N, 1, 23301/1e5)]
          dt[age < 45             & sex =="2", hist.of.ca := rbinom(.N, 1, 690/1e5)]
          dt[between(age, 45, 64) & sex =="2", hist.of.ca := rbinom(.N, 1, 8419/1e5)]
          dt[age > 64             & sex =="2", hist.of.ca := rbinom(.N, 1, 24852/1e5)]
        }
        
        # Assuming hypothetical never smokers having 70% of the risk for cancer
        dt[, hist.of.ca.h := hist.of.ca * rbinom(.N, 1, 0.7)]
        
        # Family history of lung cancer: from Maddams J, et al. Projections of cancer
        # prevalence in the United  Kingdom, 2010-2040. 
        # Br J Cancer 2012; 107: 1195-1202 (table 3, scenario 1)
        # The prob of having ca is 120/1e5. So the probability of having at least
        # one of 3 family members with diabetes is 1 - (1-120/1e5)^3. I let family
        # members vary between 2 and 4
        if (i < 9) { # before 2020
          dt[, fam.hist.of.ca := 
               dice(.N) < 1 -
               (1-104/1e5)^(2 + rpois(.N, 1))]
        }
        if (i >= 9 & i < 19) { # after 2020
          dt[, fam.hist.of.ca := 
               dice(.N) < 1 -
               (1-120/1e5)^(2 + rpois(.N, 1))]
        }
        if (i >= 19 & i < 29) { # after 2030
          dt[, fam.hist.of.ca := 
               dice(.N) < 1 -
               (1-148/1e5)^(2 + rpois(.N, 1))]
        }
        if (i >= 29) { # after 2040
          dt[, fam.hist.of.ca := 
               dice(.N) < 1 -
               (1-188/1e5)^(2 + rpois(.N, 1))]
        }
        
        sec <- 2L - as.integer(as.character(sec)) # originaly education but I use it as proxy for 
        # sec. Also centered around 4
        
        origin[origin == 1 | origin == 9] <- 0
        origin[between(origin, 2, 5) | origin == 8] <- -0.5241286
        origin[origin == 6 | origin == 7] <- 0.3211605
        
        # Probability of cancer if never smoker
        # risk.h <- exp((age - 62L) * 0.079597 -
        #                 sec * 0.0879289 -
        #                 (bmi - 27) * 0.028948 +
        #                 copd.h * 0.3457265 +
        #                 dt$hist.of.ca.h * 0.4845208 +
        #                 dt$fam.hist.of.ca * 0.5856777 +
        #                 origin - 7.02198)
        # 
        # risk.h <- risk.h / (1 + risk.h)
        
        dt[, age:= age + cvd.lag]
        dt[CPS, on=c("age", "sex"), risk.h := rr.sm/1e5]
        
        cigst1 <- c(2.542472, 2.799727)[1L + (cigst1 == "4")]
        cigdyal <- cigdyal + numsmok
        #cigdyal[cigdyal < 6] <- 6 # necessary, other wise smoking less than 6 cigarettes was protective
        
        # Probability of cancer for active smoker
        # risk.a <- exp((age - 62L) * 0.079597 - 
        #                 sec * 0.0879289 -
        #                 (bmi - 27) * 0.028948 + 
        #                 copd.a * 0.3457265 + 
        #                 dt$hist.of.ca * 0.4845208 + 
        #                 dt$fam.hist.of.ca * 0.5856777 + 
        #                 origin - 7.02198 + cigst1 -
        #                 ((((cigdyal)/100)^-1) - 4.021541613) *
        #                 0.1815486 +
        #                 (smokyrs - 27) * 0.0305566 -
        #                 (endsmoke - 8.593417626) *
        #                 0.0321362 * (cigst1 == 2.542472))
        
        risk.a <- exp((age - 62L) * 0.079597 - 7.02198 + cigst1 -
                        (((cigdyal/100)^-1) - 4.021541613) *
                        0.1815486 +
                        (smokyrs - 27) * 0.0305566 -
                        (endsmoke - 8.593417626) *
                        0.0321362 * (cigst1 == 2.542472))
        
        risk.a <- risk.a / (1 + risk.a)
        return(risk.a / dt$risk.h) # Sampling error is not considered because not clearly reported and sample size was ~ 200.000
        #return(cbind(risk.a, risk.h))
      }
    )
}

