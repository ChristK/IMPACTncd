
# 3.887
system.time({
  POP[between(age, 20, 84), 
      bmival.cvdlag := 
        pred.bmi(i, age, sex, qimd, a30to06m.cvdlag, cvd.lag)]
  

})


system.time({
  X <- data.table(model.matrix(Terms, m, contrasts = pa.svylr$contrasts)) # 2sec
  X[, "(Intercept)" := NULL] #
})
system.time(
  POP[, list(mean(bmival), sd(bmival))]
)

POP[, meansd(bmival), by=sex]
POP[, list(mean(bmival), sd(bmival)), by=sex]

tableC(xx)
table(xx)

output.rf2  <- cmpfun(function(dt, strata, l = 0, h = 100, ...) {
  dt[between(age, l, h), list( "year"            = init.year + i,
                               "scenario"        = gsub(".R", "", scenarios.list[[iterations]]),
                               "mc"              = haha,
                               "pop"             = .N,
                               "bmi.cvd.mean"    = mean(bmival.cvdlag, na.rm = T),
                               "bmi.cvd.sd"      = stats::sd(bmival.cvdlag, na.rm = T),
                               "fv.cvd.0"        = sum(porftvg.cvdlag == 0, na.rm = T),
                               "fv.cvd.1"        = sum(porftvg.cvdlag == 1, na.rm = T),
                               "fv.cvd.2"        = sum(porftvg.cvdlag == 2, na.rm = T),
                               "fv.cvd.3"        = sum(porftvg.cvdlag == 3, na.rm = T),
                               "fv.cvd.4"        = sum(porftvg.cvdlag == 4, na.rm = T),
                               "fv.cvd.5"        = sum(porftvg.cvdlag == 5, na.rm = T),
                               "fv.cvd.6"        = sum(porftvg.cvdlag == 6, na.rm = T),
                               "fv.cvd.7"        = sum(porftvg.cvdlag == 7, na.rm = T),
                               "fv.cvd.8"        = sum(porftvg.cvdlag == 8, na.rm = T),
                               "fv.ca.0"         = sum(porftvg.calag == 0, na.rm = T),
                               "fv.ca.1"         = sum(porftvg.calag == 1, na.rm = T),
                               "fv.ca.2"         = sum(porftvg.calag == 2, na.rm = T),
                               "fv.ca.3"         = sum(porftvg.calag == 3, na.rm = T),
                               "fv.ca.4"         = sum(porftvg.calag == 4, na.rm = T),
                               "fv.ca.5"         = sum(porftvg.calag == 5, na.rm = T),
                               "fv.ca.6"         = sum(porftvg.calag == 6, na.rm = T),
                               "fv.ca.7"         = sum(porftvg.calag == 7, na.rm = T),
                               "fv.ca.8"         = sum(porftvg.calag == 8, na.rm = T)
  ), by = strata]
}
)

output.rf3  <- cmpfun(function(dt, strata, l = 0, h = 100, ...) {
  dt[between(age, l, h), c(init.year + i,
                           gsub(".R", "", scenarios.list[[iterations]]),
                           haha,
                           .N,
                           meansd(bmival.cvdlag),
                           meansd(bmival.calag),
                           meansd(omsysval.cvdlag),
                           meansd(cholval.cvdlag),
                           meansd(salt24h.cvdlag),
                           meansd(salt24h.calag),
                           meansd(packyears),
                           fvsum(porftvg.cvdlag, porftvg.calag),
                           smoksum(cigst1.cvdlag, cigst1.calag,
                                   expsmokCat, diabtotr.cvdlag, a30to06m.cvdlag)),
     by = strata]
}
)
output.rf3(POP, c("qimd", "sex", "agegroup"), 30, 85)
output.rf2(POP, c("qimd", "sex", "agegroup"), 30, 85)


system.time(
  output.rf3(POP, c(NULL), 30, 85)
)
system.time(
  output.rf(POP, c(NULL), 30, 85)
)
system.time({
  a <- vector("list", 2)
  a[[1]] <- output.rf3(POP, c("sex"), 20, 84)
  
  a[[2]] <- output.rf(POP, c("sex"), 20, 84)

  setnames(a[[1]], paste0("V", 1:50), output.rf.names)
  setcolorder(a[[1]], a[[1]][,order(names(a[[1]]))])
  setcolorder(a[[2]], a[[2]][,order(names(a[[2]]))])
  
  identical(a[[1]], a[[2]])
  
  for (k in 1:53) {
  print(identical(a[[1]][, k, with =F], a[[2]][, k, with =F]))
    print(names(a[[1]])[[k]])
  }
  View(a[[1]])
  POP[between(age, 20, 84)&sex==1, table(cigst1.cvdlag)]
}
)

system.time({
  b <- vector("list", 5)
  b[[1]] <- output.rf(POP, c("qimd", "sex", "agegroup"), 20, 84)
  
  b[[2]] <- output.rf(POP, c("sex", "agegroup"), 20, 84)
  
  b[[3]] <- output.rf(POP, c("qimd", "sex"), ageL, ageH) 
  
  b[[4]] <- output.rf(POP, c("sex"), ageL, ageH)
  
  b[[5]] <- output.rf(POP, c(), ageL, ageH)
  
}
)



compare <- microbenchmark(output.rf3(POP, c("group"), 30, 85),
                          output.rf(POP, c("group"), 30, 85), 
                          times = 10)
autoplot(compare)

newdata <- 
  POP[between(age, 20, 84), list(
    "year" = i,
    "age"  = age, 
    "sex"  = sex, 
    "qimd" = qimd
  )]


pred.pa2 <- cmpfun(function(year, age, sex, qimd, lag = cvd.lag) {
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
  #   
  newdata<- data.table(
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
  X <- data.table(model.matrix(Terms, m, contrasts = pa.svylr$contrasts)) # 2sec
  X[, "(Intercept)" := NULL] # 2sec
  n <- nrow(X)
  q <- length(pa.svylr$zeta)
  for (k in 1:length(pa.svylr$coefficients)) {
    set(X, NULL, k, X[, k , with = F] * pa.svylr$coefficients[[k]])
  }
  
  X <- X[, .(eta=Reduce(`+`, .SD))]
  X[, paste0("V", 1:7) := as.list(pa.svylr$zeta)]
  X <- X[, .(
    plogis(V1 - eta),
    plogis(V2 - eta),
    plogis(V3 - eta),
    plogis(V4 - eta),
    plogis(V5 - eta),
    plogis(V6 - eta),
    plogis(V7 - eta)
  )]

  set(X, NULL, "V8", 1L)
  set(X, NULL, "d", dice(nrow(X)))
  for (k in 1:8) set(X, NULL, k, X[, Reduce(`<`, .SD), .SDcol = c(k, 9)])
  X[, a30 :=  Reduce(`+`, .SD), .SDcol = 1:8]
  
  if (PA.intervention != 0L) {
    PA.intervention2 <- 
      sample(
        c(0L, PA.intervention),
        nrow(X), T,
        prob = c(1-PA.intervention.success, PA.intervention.success)
      )
    X[a30 <= 7L - PA.intervention, a30:= a30 + PA.intervention2]
  }
  return(X[, a30])  
} 
)


