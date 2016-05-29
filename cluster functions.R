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
  haha <- paste0(
    sample(
      c(rep(0:9, each = 5), LETTERS, letters), 
      12,
      replace = T), 
    collapse = "")   
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
    if (paired) set.seed(seed[[counter[[iterations]]]])
    y = rnorm(n, -3.735, 0.73) + rnorm(n, -0.105, 0.029) * (age - 50) +
      rnorm(n, -1.874, 0.884) * (sbp > 140) +
      rnorm(n, -2.489, 1.188) * rbinom(n, 1, 0.1)
    return(salt.difference * y /5.85)
  }
)

diseases <- vector("list", length(diseasestoexclude) + 1)
names(diseases) <- c(diseasestoexclude, "other")
if ("CHD" %in% diseasestoexclude) diseases$CHD <- function() loadcmp(file = "./chd model.Rc", my.env)
if ("stroke" %in% diseasestoexclude) diseases$stroke <- function() loadcmp(file = "./stroke model.Rc", my.env)
if ("C34" %in% diseasestoexclude) diseases$C34 <- function() loadcmp(file = "./lung cancer model.Rc", my.env)
if ("C16" %in% diseasestoexclude) diseases$C16 <- function() loadcmp(file = "./gastric cancer model.Rc", my.env)
diseases$other <- function() loadcmp(file = "./other model.Rc", my.env)

ageing.distr <- # smaller fortune increase the variability of the join
  cmpfun(
    function(DT.ref, risk.factor, fortune = 0.05) {
      if (risk.factor == "cholval") {
        wtr <- quote((frank(cholval,
                            na.last = F, 
                            ties.method = "random") - 1) / (.N - 1))
      }
      if (risk.factor == "omsysval") {
        wtr <- quote((frank(omsysval,
                            na.last = F, 
                            ties.method = "random") - 1) / (.N - 1))
      }
      if (risk.factor == "bmival") {
        wtr <- quote((frank(bmival,
                            na.last = F, 
                            ties.method = "random") - 1) / (.N - 1))
      }
      nam  <- paste0(substitute(risk.factor), ".rank")
      POP[, (nam) := 
            eval(wtr),
          by = group]
      POP[, (risk.factor) := NULL]
      setkeyv(POP, c("group", nam))
      tt <- DT.ref[, .SD[sample(.N, .N*fortune)], by = group]
      setkeyv(tt, c("group", nam))
      assign("POP", tt[POP, roll = "nearest"], envir = my.env)
    }
  )

# Define function to export annual summaries of RF
pop.summ <-  cmpfun(
  function(N, ...) {
    return(list("year" = 2011 + i,
                "scenario" = gsub(".Rc", "", scenarios.list[[iterations]]),
                "mc" = haha,
                "pop" = N))
  }
)

output.rf  <- cmpfun(
  function(dt, strata, l = 0, h = 100, ...) {
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
                             meansd(packyears.cvdlag),
                             fvsum(porftvg.cvdlag, porftvg.calag),
                             smoksum(cigst1.cvdlag, cigst1,
                                     expsmok.cvdlag, diabtotr.cvdlag,
                                     a30to06m.cvdlag)),
       by = strata]
  }
)

output.rf.names <- 
  c("year", "scenario", "mc" , "pop", "bmi.cvd.mean", "bmi.cvd.sd", "bmi.ca.mean",
    "bmi.ca.sd", "sbp.cvd.mean", "sbp.cvd.sd", "tc.cvd.mean", "tc.cvd.sd",
    "salt.cvd.mean", "salt.cvd.sd", "salt.ca.mean", "salt.ca.sd",
    "packyears.mean", "packyears.sd", "packyears.cvd.mean",
    "packyears.cvd.sd", paste0("fv.cvd.", 0:8), 
    paste0("fv.ca.", 0:8), "smok.cvd.never", "smok.cvd.active",
    "smok.curr.never", "smok.curr.active", "ets.yes", "diab.cvd.yes", 
    paste0("pa.cvd.", 0:7)  
  )

output.chd  <- cmpfun(
  function(dt, strata, l = ageL, h = ageH, ...
  ) {
    dt[between(age, l, h), 
       list("year"              = 2011 + i,
            "scenario"          = gsub(".Rc", "", scenarios.list[[iterations]]),
            "mc"                = haha,
            "pop"               = .N,
            "chd.incidence"     = sum(chd.incidence == 2011 + i, na.rm = T),
            "chd.incidence.cvd" = sum(chd.incidence == 2011 + i & 
                                        stroke.incidence == 0, na.rm = T),
            "chd.prevalence"    = sum(chd.incidence > 0, na.rm = T),
            "chd.mortality"     = sum(dead, na.rm = T)
       ), by = strata]
  }
)
output.stroke  <- cmpfun(function(dt, strata, l = ageL, h = ageH, ...) {
  dt[between(age, l, h), 
     list("year"                 = 2011 + i,
          "scenario"             = gsub(".Rc", "", scenarios.list[[iterations]]
          ),
          "mc"                   = haha,
          "pop"                  = .N,
          "stroke.incidence"     = sum(stroke.incidence == 2011 + i, na.rm = T),
          "stroke.incidence.cvd" = sum(stroke.incidence == 2011 + i & 
                                         chd.incidence == 0, na.rm = T),
          "stroke.prevalence"    = sum(stroke.incidence > 0, na.rm = T),
          "stroke.mortality"     = sum(dead, na.rm = T)
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
          "c16.prevalence"  = sum(c16.incidence > 0 & is.na(c16.remission), na.rm = T),
          "c16.mortality"   = sum(dead, na.rm = T)
     ), by = strata]
}
)

output.c34  <- cmpfun(function(dt, strata, l = ageL, h = ageH, ...) {
  dt[between(age, l, h), 
     list("year"            = 2011 + i,
          "scenario"        = gsub(".Rc", "", scenarios.list[[iterations]]),
          "mc"              = haha,
          "pop"             = .N,
          "c34.incidence"   = sum(c34.incidence == 2011 + i, na.rm = T),
          "c34.prevalence"  = sum(c34.incidence > 0 & is.na(c34.remission), na.rm = T),
          "c34.mortality"   = sum(dead, na.rm = T)
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

# Define origin(ethnicity)
pred.origin <- cmpfun(function(age, sex, qimd) {
  newdata <- data.table(
    age  = age/100, 
    sex  = sex, 
    qimd = as.character(qimd)
  )
  #code adapted from method getAnywhere(predict.multinom)
  Terms <- delete.response(origin.multinom$terms)
  m <- model.frame(Terms, newdata, na.action = na.omit, 
                   xlev = origin.multinom$xlevels)
  if (!is.null(cl <- attr(Terms, "dataClasses"))) 
    .checkMFClasses(cl, m)
  X <- model.matrix(Terms, m, contrasts = origin.multinom$contrasts)
  class(origin.multinom) <- "nnet"
  cc <- data.table(predict(origin.multinom, X))
  for (k in 2:8) set(cc, NULL, k, cc[, Reduce(`+`, .SD), .SDcol = c(k - 1, k)])
  set(cc, NULL, 9L, 1L)
  if (paired) set.seed(seed[[counter[[iterations]]]])
  set(cc, NULL, "d", dice(nrow(cc)))
  for (k in 1:8) set(cc, NULL, k, cc[, Reduce(`<`, .SD), .SDcol = c(k, 10)])
  return(cc[, Reduce(`+`, .SD), .SDcol = 1:9])  
} 
)


# Seeded random -----------------------------------------------------------
# if (paired == T) {
#   
#   sample.ss <- cmpfun(
#     function(x, size, replace = FALSE, prob = NULL, plu = 0, ...) {
#     set.seed(seed[[counter[[iterations]]]] + plu)
#     base:::sample(x, size, replace, prob)
#     }
#   )
#   
#   rnorm.ss <- cmpfun(
#     function(x, size, replace = FALSE, prob = NULL, ...) {
#       set.seed(seed[[counter[[iterations]]]])
#       base:::sample(x, size, replace, prob)
#     }
#   )
# }
