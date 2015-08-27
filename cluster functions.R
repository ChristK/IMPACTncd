#cmpfile("./cluster functions.R")
haha <- paste(sample(c(rep(0:9,each=5),LETTERS,letters),12,replace=T),collapse='')   

# Define function for output dir
output.dir <- cmpfun(function() {
  paste0("./Output/", gsub(".R", "", scenarios.list[[iterations]]), "/", haha , "/")
}
)


dir.create(path = output.dir(), recursive = T) # create a unique directory for each run of each scenario

# Define list of function to run for each diseases models
#cmpfile("./chd model.R")
#cmpfile("./stroke model.R")
#cmpfile("./lung cancer model.R")
#cmpfile("./other model.R")

diseases <- list(
  chd = function() 
    if ("CHD" %in% get("diseasestoexclude", parent.frame())) loadcmp(file = "./chd model.Rc", my.env),
  stroke = function() 
    if ("stroke" %in% get("diseasestoexclude", parent.frame())) loadcmp(file = "./stroke model.Rc", my.env),
  lung.ca = function() 
    if ("C34" %in% get("diseasestoexclude", parent.frame())) loadcmp(file = "./lung cancer model.Rc", my.env),
  gastric.ca = function() 
    if ("C16" %in% get("diseasestoexclude", parent.frame())) loadcmp(file = "./gastric cancer model.Rc", my.env),
  other = function() 
    loadcmp(file = "./other model.Rc", my.env)
)

# Define function to match continuous distributions of each group with the one in SPOP2011 to simulate ageing 
# ageing.distr <- cmpfun(function(risk.factor, env = my.env) {
#   risk.factor <- substitute(risk.factor)
#   temp <- SPOP2011[eval(risk.factor)>0, list(eval(risk.factor), group)]
#   nam <- paste0(risk.factor, ".rank")
#   temp[, (nam) := (frank(eval(risk.factor), na.last = F, ties.method="random")-1)/(.N - 1),
#        by = group]
#   setkeyv(temp, c("group", nam))
#   
#   POP[, (nam) := (frank(eval(risk.factor), na.last = F, ties.method="random")-1)/(.N - 1),
#       by = group]
#   POP[, eval(risk.factor) := NULL]
#   setkeyv(POP, c("group", nam))
#   POP <- temp[POP, roll = "nearest"]
#   assign("POP", POP, envir = env)
# }
# )

ageing.distr <- # smaller fortune increase the variability of the join
  cmpfun(
    function(DT.ref, risk.factor, fortune = 0.001, DT = POP, env = my.env) {
      risk.factor <- substitute(risk.factor)
      nam  <- paste0(risk.factor, ".rank")
      DT[, (nam) := 
           (frank(
             eval(risk.factor),
             na.last = F, 
             ties.method = "random") -1) / (.N - 1),
         by = group]
      DT[, eval(risk.factor) := NULL]
      setkeyv(DT, c("group", nam))
      DT.ref <- DT.ref[, sample_frac(.SD, fortune, T), by = group]
      setkeyv(DT.ref, c("group", nam))
      assign("POP", DT.ref[DT, roll = "nearest"], envir = env)
    }
  )

# Define function to export annual summaries of RF
pop.summ <-  cmpfun(function(N, ...) {
  return(list("year" = 2011 + i,
              "scenario" = gsub(".R", "", scenarios.list[[iterations]]),
              "mc" = haha,
              "pop" = N))
}
)

cont.summ <- cmpfun(function(rf, name, ...) {
  mylist <- list()
  mylist[[paste0(name, ".mean")]] <- mean(rf, na.rm = T, trim = 0.05)
  mylist[[paste0(name, ".sd")]] <- stats::sd(rf, na.rm = T)
  #mylist[[paste0(name, ".median")]] <- median(rf, na.rm=T) # disabled to improve spead
  #mylist[[paste0(name, ".mad")]] <- mad(rf, na.rm=T)
  #mylist[[paste0(name, ".iqr")]] <- IQR(rf, na.rm=T)
  return(mylist)
}
)

cat.summ <- cmpfun(function(rf, name, ...) {
  absol <- table(factor(rf, exclude = c(NA, NaN, "99", "999"), ...))
  #pct <- prop.table(absol)
  absol <- absol[names(absol)!="NA's"]
  #ct <- pct[names(pct)!="NA's"]
  names(absol) <- paste0(name, ".", names(absol))
  #names(pct) <- paste0(name, ".", names(pct), ".pct")
  #return(as.list(c(absol, pct)))
  return(as.list(absol))
}
)

# Old output function. SLOW
# output.rf  <- cmpfun(function(x, ...) {
#   with(x, return(c(pop.summ(nrow(x)),
#                    #cont.summ(bmival, "bmi"),
#                    cont.summ(bmival.cvdlag, "bmi.cvd"),
#                    cont.summ(bmival.calag, "bmi.ca"),
#                    #cont.summ(omsysval, "sbp"),
#                    cont.summ(omsysval.cvdlag, "sbp.cvd"),
#                    #cont.summ(cholval, "tc"),
#                    cont.summ(cholval.cvdlag, "tc.cvd"),
#                    cont.summ(salt24h.calag, "salt.ca"),
#                    cat.summ(cigst1.cvdlag, "smok.cvd",
#                             levels = 1:4, 
#                             labels = c("never", "ex.2", "ex.3", "active")),
#                    cat.summ(cigst1.calag, "smok.ca", 
#                             levels = 1:4, 
#                             labels = c("never", "ex.2", "ex.3", "active")),
#                    cat.summ(porftvg.cvdlag, "fv.cvd", levels = 0:8),
#                    cat.summ(porftvg.calag, "fv.ca", levels = 0:8),
#                    cat.summ(frtpor.cvdlag, "fruit.cvd", levels = 0:8),
#                    cat.summ(frtpor.calag, "fruit.ca", levels = 0:8),
#                    cat.summ(a30to06m.cvdlag, "pa.cvd", levels = 0:7),
#                    cat.summ(a30to06m.calag, "pa.ca", levels = 0:7),
#                    cat.summ(diabtotr.cvdlag, "diab.cvd",
#                             levels = 1:2, 
#                             labels = c("no", "yes")),
#                    cat.summ(expsmokCat, "ets",
#                             levels = 0:1))))
# }
# )

output.rf  <- cmpfun(function(dt, strata, l = 0, h = 100, ...) {
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
output.rf.names <- 
  c("year", "scenario", "mc" , "pop", "bmi.cvd.mean", "bmi.cvd.sd", "bmi.ca.mean",
    "bmi.ca.sd", "sbp.cvd.mean", "sbp.cvd.sd", "tc.cvd.mean", "tc.cvd.sd",
    "salt.cvd.mean", "salt.cvd.sd", "salt.ca.mean", "salt.ca.sd",
    "packyears.mean", "packyears.sd", paste0("fv.cvd.", 0:8), 
    paste0("fv.ca.", 0:8), "smok.cvd.never", "smok.cvd.active",
    "smok.ca.never", "smok.ca.active", "ets.yes", "diab.cvd.yes", 
    paste0("pa.cvd.", 0:7)  
  )

# output.rf  <- cmpfun(function(dt, strata, l = 0, h = 100, ...) {
#   dt[between(age, l, h), list("year"            = init.year + i,
#             "scenario"        = gsub(".R", "", scenarios.list[[iterations]]),
#             "mc"              = haha,
#             "pop"             = .N,
#             "bmi.cvd.mean"    = mean(bmival.cvdlag, na.rm = T),
#             "bmi.cvd.sd"      = stats::sd(bmival.cvdlag, na.rm = T),
#             "bmi.ca.mean"     = mean(bmival.calag, na.rm = T),
#             "bmi.ca.sd"       = stats::sd(bmival.calag, na.rm = T),
#             "sbp.cvd.mean"    = mean(omsysval.cvdlag, na.rm = T),
#             "sbp.cvd.sd"      = stats::sd(omsysval.cvdlag, na.rm = T),
#             "tc.cvd.mean"     = mean(cholval.cvdlag, na.rm = T),
#             "tc.cvd.sd"       = stats::sd(cholval.cvdlag, na.rm = T),
#             "salt.cvd.mean"   = mean(salt24h.cvdlag, na.rm = T),
#             "salt.cvd.sd"     = stats::sd(salt24h.cvdlag, na.rm = T),
#             "salt.ca.mean"    = mean(salt24h.calag, na.rm = T),
#             "salt.ca.sd"      = stats::sd(salt24h.calag, na.rm = T),
#             "packyears.mean"  = mean(packyears, na.rm = T),
#             "packyears.sd"    = stats::sd(packyears, na.rm = T),
#             "smok.cvd.never"  = sum(cigst1.cvdlag == 1, na.rm = T),
#             #"smok.cvd.ex.2"   = sum(cigst1.cvdlag == 2, na.rm = T),
#             #"smok.cvd.ex.3"   = sum(cigst1.cvdlag == 3, na.rm = T),
#             "smok.cvd.active" = sum(cigst1.cvdlag == 4, na.rm = T),
#             "smok.ca.never"   = sum(cigst1.calag == 1, na.rm = T),
#             #"smok.ca.ex.2"    = sum(cigst1.calag == 2, na.rm = T),
#             #"smok.ca.ex.3"    = sum(cigst1.calag == 3, na.rm = T),
#             "smok.ca.active"  = sum(cigst1.calag == 4, na.rm = T),
#             "fv.cvd.0"        = sum(porftvg.cvdlag == 0, na.rm = T),
#             "fv.cvd.1"        = sum(porftvg.cvdlag == 1, na.rm = T),
#             "fv.cvd.2"        = sum(porftvg.cvdlag == 2, na.rm = T),
#             "fv.cvd.3"        = sum(porftvg.cvdlag == 3, na.rm = T),
#             "fv.cvd.4"        = sum(porftvg.cvdlag == 4, na.rm = T),
#             "fv.cvd.5"        = sum(porftvg.cvdlag == 5, na.rm = T),
#             "fv.cvd.6"        = sum(porftvg.cvdlag == 6, na.rm = T),
#             "fv.cvd.7"        = sum(porftvg.cvdlag == 7, na.rm = T),
#             "fv.cvd.8"        = sum(porftvg.cvdlag == 8, na.rm = T),
#             "fv.ca.0"         = sum(porftvg.calag == 0, na.rm = T),
#             "fv.ca.1"         = sum(porftvg.calag == 1, na.rm = T),
#             "fv.ca.2"         = sum(porftvg.calag == 2, na.rm = T),
#             "fv.ca.3"         = sum(porftvg.calag == 3, na.rm = T),
#             "fv.ca.4"         = sum(porftvg.calag == 4, na.rm = T),
#             "fv.ca.5"         = sum(porftvg.calag == 5, na.rm = T),
#             "fv.ca.6"         = sum(porftvg.calag == 6, na.rm = T),
#             "fv.ca.7"         = sum(porftvg.calag == 7, na.rm = T),
#             "fv.ca.8"         = sum(porftvg.calag == 8, na.rm = T),
#             #             "fruit.cvd.0"     = sum(frtpor.cvdlag == 0, na.rm = T),
#             #             "fruit.cvd.1"     = sum(frtpor.cvdlag == 1, na.rm = T),
#             #             "fruit.cvd.2"     = sum(frtpor.cvdlag == 2, na.rm = T),
#             #             "fruit.cvd.3"     = sum(frtpor.cvdlag == 3, na.rm = T),
#             #             "fruit.cvd.4"     = sum(frtpor.cvdlag == 4, na.rm = T),
#             #             "fruit.cvd.5"     = sum(frtpor.cvdlag == 5, na.rm = T),
#             #             "fruit.cvd.6"     = sum(frtpor.cvdlag == 6, na.rm = T),
#             #             "fruit.cvd.7"     = sum(frtpor.cvdlag == 7, na.rm = T),
#             #             "fruit.cvd.8"     = sum(frtpor.cvdlag == 8, na.rm = T),
#             #             "fruit.ca.0"     = sum(frtpor.calag == 0, na.rm = T),
#             #             "fruit.ca.1"     = sum(frtpor.calag == 1, na.rm = T),
#             #             "fruit.ca.2"     = sum(frtpor.calag == 2, na.rm = T),
#             #             "fruit.ca.3"     = sum(frtpor.calag == 3, na.rm = T),
#             #             "fruit.ca.4"     = sum(frtpor.calag == 4, na.rm = T),
#             #             "fruit.ca.5"     = sum(frtpor.calag == 5, na.rm = T),
#             #             "fruit.ca.6"     = sum(frtpor.calag == 6, na.rm = T),
#             #             "fruit.ca.7"     = sum(frtpor.calag == 7, na.rm = T),
#             #             "fruit.ca.8"     = sum(frtpor.calag == 8, na.rm = T),
#             "pa.cvd.0"        = sum(a30to06m.cvdlag == 0, na.rm = T),
#             "pa.cvd.1"        = sum(a30to06m.cvdlag == 1, na.rm = T),
#             "pa.cvd.2"        = sum(a30to06m.cvdlag == 2, na.rm = T),
#             "pa.cvd.3"        = sum(a30to06m.cvdlag == 3, na.rm = T),
#             "pa.cvd.4"        = sum(a30to06m.cvdlag == 4, na.rm = T),
#             "pa.cvd.5"        = sum(a30to06m.cvdlag == 5, na.rm = T),
#             "pa.cvd.6"        = sum(a30to06m.cvdlag == 6, na.rm = T),
#             "pa.cvd.7"        = sum(a30to06m.cvdlag == 7, na.rm = T),
#             #             "pa.ca.0"         = sum(a30to06m.calag == 0, na.rm = T),
#             #             "pa.ca.1"         = sum(a30to06m.calag == 1, na.rm = T),
#             #             "pa.ca.2"         = sum(a30to06m.calag == 2, na.rm = T),
#             #             "pa.ca.3"         = sum(a30to06m.calag == 3, na.rm = T),
#             #             "pa.ca.4"         = sum(a30to06m.calag == 4, na.rm = T),
#             #             "pa.ca.5"         = sum(a30to06m.calag == 5, na.rm = T),
#             #             "pa.ca.6"         = sum(a30to06m.calag == 6, na.rm = T),
#             #             "pa.ca.7"         = sum(a30to06m.calag == 7, na.rm = T),
#             "diab.cvd.yes"    = sum(diabtotr.cvdlag == 2, na.rm = T),
#             #"diab.cvd.no"     = sum(diabtotr.cvdlag == 1, na.rm = T),
#             "ets.yes"         = sum(expsmokCat == 1, na.rm = T)#,
#             #"ets.no"          = sum(expsmokCat == 0, na.rm =T)
#   ), by = strata]
# }
# )

output.chd  <- cmpfun(function(dt, strata, l = ageL, h = ageH, ...) {
  dt[between(age, l, h), 
     list("year"            = 2011 + i,
          "scenario"        = gsub(".R", "", scenarios.list[[iterations]]),
          "mc"              = haha,
          "pop"             = .N,
          "chd.incidence"   = sum(chd.incidence == init.year + i, na.rm = T),
          "chd.prevalence"  = sum(chd.incidence > 0, na.rm = T),
          "chd.mortality"   = sum(dead, na.rm = T)
     ), by = strata]
}
)
output.stroke  <- cmpfun(function(dt, strata, l = ageL, h = ageH, ...) {
  dt[between(age, l, h), 
     list("year"            = 2011 + i,
          "scenario"        = gsub(".R", "", scenarios.list[[iterations]]),
          "mc"              = haha,
          "pop"             = .N,
          "stroke.incidence"   = sum(stroke.incidence == init.year + i, na.rm = T),
          "stroke.prevalence"  = sum(stroke.incidence > 0, na.rm = T),
          "stroke.mortality"   = sum(dead, na.rm = T)
     ), by = strata]
}
)
# NEED TO add remission
output.c16  <- cmpfun(function(dt, strata, l = ageL, h = ageH, ...) {
  dt[between(age, l, h), 
     list("year"            = 2011 + i,
          "scenario"        = gsub(".R", "", scenarios.list[[iterations]]),
          "mc"              = haha,
          "pop"             = .N,
          "c16.incidence"   = sum(c16.incidence == init.year + i, na.rm = T),
          "c16.prevalence"  = sum(c16.incidence > 0, na.rm = T),
          "c16.mortality"   = sum(dead, na.rm = T)
     ), by = strata]
}
)

output.other  <- cmpfun(function(dt, strata, ...) {
  dt[, list("year"            = 2011 + i,
            "scenario"        = gsub(".R", "", scenarios.list[[iterations]]),
            "mc"              = haha,
            "pop"             = .N,
            "other.mortality" = sum(dead, na.rm = T)
  ), by = strata]
}
)

# output.chd  <- cmpfun(function(x, ...) {
#   O1 <- pop.summ(nrow(x))
#   O2 <- with(x, cat.summ(chd.incidence, "chd",levels = init.year + i, labels="incidence"))
#   O3 <- with(x, sum(table(factor(chd.incidence, exclude = c(0, NA)))))
#   names(O3) <- "chd.prevalence"
#   O4 <- with(x, sum(table(dead, exclude=c(F, NA, NaN))))
#   names(O4) <- "chd.mortality"
#   return(c(O1, O2, O3, O4))
# }
# )

# output.stroke  <- cmpfun(function(x, ...) {
#   O1 <- pop.summ(nrow(x))
#   O2 <- with(x, cat.summ(stroke.incidence, "stroke", 
#                          levels = 2011 + i, labels="incidence"))
#   O3 <- with(x, sum(table(factor(stroke.incidence, exclude = c(0, NA)))))
#   names(O3) <- "stroke.prevalence"
#   O4 <- with(x, sum(table(dead, exclude=c(F, NA, NaN))))
#   names(O4) <- "stroke.mortality"
#   return(c(O1, O2, O3, O4))
# }
# )

# output.c16  <- cmpfun(function(x, ...) {
#   O1 <- pop.summ(nrow(x))
#   O2 <- with(x, cat.summ(c16.incidence, "c16", 
#                          levels = init.year + i, labels="incidence"))
#   O3 <- with(x, sum(table(factor(c16.incidence, exclude = c(0, NA)))))
#   names(O3) <- "c16.prevalence"
#   O4 <- with(x, sum(table(dead, exclude=c(F, NA, NaN))))
#   names(O4) <- "c16.mortality"
#   O5 <- with(x, sum(table(c16.remission, exclude = c(0, NA))))
#   names(O5) <- "c16.remission"
#   return(c(O1, O2, O3, O4, O5))
# }
# )

# output.other  <- cmpfun(function(x, ...) {
#   O1 <- pop.summ(nrow(x))
#   O2 <- with(x, sum(table(dead, exclude=c(F, NA, NaN))))
#   names(O2) <- "other.mortality"
#   return(c(O1, O2))
# }
# )

# convert changes in lipids consumption to cholestrol
# keys.formula <- function(ds=0, dp=0, dz=0){
#   dchol = (1.35*(2*ds-dp)+1.5*dz) * 0.0259 ## dchol in mmol/L
#   return(dchol)
# }
