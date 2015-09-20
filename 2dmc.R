#cmpfile("./2dmc.R")
cat("Sample RR values for 2d Monte Carlo\n\n")
# coefficients for salt model from the MC simulation
load(file="./Lagtimes/salt.rq.coef.rda")
salt.rq$coefficients <- salt.rq.coef[[counter[[iterations]]]]
#salt.rq$coefficients <- apply(simplify2array(salt.rq.coef), 1:2, mean) # mean of MC

if ("CHD" %in% diseasestoexclude) {
  tobacco.rr.chd <- chd.tobacco.rr.l[.id == counter[[iterations]]]
  chd.ets.rr.mc  <- chd.ets.rr.l[[counter[[iterations]]]]
  sbp.rr.chd     <- chd.sbp.rr.l[.id == counter[[iterations]]]
  chol.rr.chd    <- chd.chol.rr.l[.id == counter[[iterations]]]
  chd.bmi.rr.mc  <- chd.bmi.rr.l[.id == counter[[iterations]]]
  chd.diab.rr.mc <- chd.diab.rr.l[.id == counter[[iterations]]]
  chd.fv.rr.mc   <- chd.fv.rr.l[[counter[[iterations]]]]
  pa.rr.chd      <- chd.pa.rr.l[.id == counter[[iterations]]]
}

if ("stroke" %in% diseasestoexclude) {
  tobacco.rr.stroke <- stroke.tobacco.rr.l[.id == counter[[iterations]]]
  stroke.ets.rr.mc  <- stroke.ets.rr.l[[counter[[iterations]]]]
  sbp.rr.stroke     <- stroke.sbp.rr.l[.id == counter[[iterations]]]
  chol.rr.stroke    <- stroke.chol.rr.l[.id == counter[[iterations]]]
  stroke.bmi.rr.mc  <- stroke.bmi.rr.l[.id == counter[[iterations]]]
  stroke.diab.rr.mc <- stroke.diab.rr.l[.id == counter[[iterations]]]
  stroke.fv.rr.mc   <- stroke.fv.rr.l[[counter[[iterations]]]]
  pa.rr.stroke      <- stroke.pa.rr.l[.id == counter[[iterations]]]
}

if ("C16" %in% diseasestoexclude) {
  c16.salt.optim  <- c16.salt.optim.l[[counter[[iterations]]]]
  c16.salt.mr     <- c16.salt.mr.l[[counter[[iterations]]]]
  c16.tob.rr.mc   <- c16.tob.rr.mc.l[[counter[[iterations]]]]
  c16.extob.rr.mc <- c16.extob.rr.mc.l[[counter[[iterations]]]]
  c16.fv.rr.mc    <- c16.fv.rr.mc.l[.id == counter[[iterations]]]
  c16.salt.rr.mc  <- c16.salt.rr.mc.l[.id == counter[[iterations]]]
}

