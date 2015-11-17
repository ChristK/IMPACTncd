#cmpfile("./cluster functions.R")
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


if (paired == T) {
  haha <- counter[[iterations]] + paired.mem
} else {
  haha <- paste(sample(c(rep(0:9,each=5),LETTERS,letters),12,replace=T),collapse='')   
}

# Define function for output dir
output.dir <-
  cmpfun(
    function() {
      paste0("./Output/",
             gsub(".Rc", "", scenarios.list[[iterations]]), 
             "/",
             haha,
             "/")
    }
  )


dir.create(path = output.dir(), recursive = T) # create a unique directory for each run of each scenario

# from Mozaffarian NEJM 
salt.sbp.reduct <- cmpfun(
  function(salt.difference, age, sbp, n) {
    y = rnorm(n, -3.735, 0.73) + rnorm(n, -0.105, 0.029) * (age - 50) +
      rnorm(n, -1.874, 0.884) * (sbp > 140) +
      rnorm(n, -2.489, 1.188) * rbinom(n, 1, 0.1)
    return(salt.difference * y /5.85)
  }
)

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
              "scenario" = gsub(".Rc", "", scenarios.list[[iterations]]),
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
  dt[between(age, l, h), c(2011 + i,
                           gsub(".Rc", "", scenarios.list[[iterations]]),
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

output.chd  <- cmpfun(function(dt, strata, l = ageL, h = ageH, ...) {
  dt[between(age, l, h), 
     list("year"            = 2011 + i,
          "scenario"        = gsub(".Rc", "", scenarios.list[[iterations]]),
          "mc"              = haha,
          "pop"             = .N,
          "chd.incidence"   = sum(chd.incidence == 2011 + i, na.rm = T),
          "chd.prevalence"  = sum(chd.incidence > 0, na.rm = T),
          "chd.mortality"   = sum(dead, na.rm = T)
     ), by = strata]
}
)
output.stroke  <- cmpfun(function(dt, strata, l = ageL, h = ageH, ...) {
  dt[between(age, l, h), 
     list("year"            = 2011 + i,
          "scenario"        = gsub(".Rc", "", scenarios.list[[iterations]]),
          "mc"              = haha,
          "pop"             = .N,
          "stroke.incidence"   = sum(stroke.incidence == 2011 + i, na.rm = T),
          "stroke.prevalence"  = sum(stroke.incidence > 0, na.rm = T),
          "stroke.mortality"   = sum(dead, na.rm = T)
     ), by = strata]
}
)
output.cvd  <- cmpfun(function(dt, strata, l = ageL, h = ageH, ...) {
  dt[between(age, l, h), 
     list("year"            = 2011 + i,
          "scenario"        = gsub(".Rc", "", scenarios.list[[iterations]]),
          "mc"              = haha,
          "pop"             = .N,
          #"cvd.incidence"   = sum(chd.incidence == (2011 + i) | stroke.incidence == (2011 + i), na.rm = T), # proper incidence is difficult because of mortality, but the bias is minimal
          "cvd.prevalence"  = sum(chd.incidence > 0 | stroke.incidence > 0, na.rm = T)
     ), by = strata]
}
)
# NEED TO add remission
output.c16  <- cmpfun(function(dt, strata, l = ageL, h = ageH, ...) {
  dt[between(age, l, h), 
     list("year"            = 2011 + i,
          "scenario"        = gsub(".Rc", "", scenarios.list[[iterations]]),
          "mc"              = haha,
          "pop"             = .N,
          "c16.incidence"   = sum(c16.incidence == 2011 + i, na.rm = T),
          "c16.prevalence"  = sum(c16.incidence > 0, na.rm = T),
          "c16.mortality"   = sum(dead, na.rm = T)
     ), by = strata]
}
)

output.other  <- cmpfun(function(dt, strata, ...) {
  dt[, list("year"            = 2011 + i,
            "scenario"        = gsub(".Rc", "", scenarios.list[[iterations]]),
            "mc"              = haha,
            "pop"             = .N,
            "other.mortality" = sum(dead, na.rm = T)
  ), by = strata]
}
)
