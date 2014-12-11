#cmpfile("./cluster functions")
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
    other = function() 
      loadcmp(file = "./other model.Rc", my.env)
)

# Define function to match continuous distributions of each group with the one in SPOP2011 to simulate ageing 
ageing.distr <- cmpfun(function(risk.factor) {
  temp <- SPOP2011[, c(risk.factor, "group"), with = F]
  nam <- paste0(risk.factor, ".rank")
  temp[, (nam) := percent_rank(get(risk.factor)), by = group]
  setkeyv(temp, c("group", nam))
  
  POP[, (nam) := percent_rank(get(risk.factor)), by = group]
  POP[, (risk.factor) := NULL]
  setkeyv(POP, c("group", nam))
  return(temp[POP, roll = "nearest"])
}
)
#example POP <- ageing.distr("bmival")


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
  mylist[[paste0(name, ".mean")]] <- mean(rf, na.rm = T)
  mylist[[paste0(name, ".sd")]] <- sd(rf, na.rm = T)
  #mylist[[paste0(name, ".median")]] <- median(rf, na.rm=T) # disabled to improve spead
  #mylist[[paste0(name, ".mad")]] <- mad(rf, na.rm=T)
  #mylist[[paste0(name, ".iqr")]] <- IQR(rf, na.rm=T)
  return(mylist)
}
)

cat.summ <- cmpfun(function(rf, name, ...) {
  absol <-summary(factor(rf, exclude = c(NA, NaN, "99"), ...))
  #pct <- prop.table(absol)
  absol <- absol[names(absol)!="NA's"]
  #ct <- pct[names(pct)!="NA's"]
  names(absol) <- paste0(name, ".", names(absol))
  #names(pct) <- paste0(name, ".", names(pct), ".pct")
  #return(as.list(c(absol, pct)))
  return(as.list(absol))
}
)

output.rf  <- cmpfun(function(x, ...) {
  with(x, return(c(pop.summ(nrow(x)),
                   cont.summ(bmival, "bmi"),
                   cont.summ(bmival.cvdlag, "bmi.cvd"),
                   cont.summ(bmival.calag, "bmi.ca"),
                   cont.summ(omsysval, "sbp"),
                   cont.summ(omsysval.cvdlag, "sbp.cvd"),
                   cont.summ(cholval, "tc"),
                   cont.summ(cholval.cvdlag, "tc.cvd"),
                   cat.summ(cigst1.cvdlag, "smok.cvd",
                            levels = 1:4, 
                            labels = c("never", "ex.2", "ex.3", "active")),
                   cat.summ(cigst1.calag, "smok.ca", 
                            levels = 1:4, 
                            labels = c("never", "ex.2", "ex.3", "active")),
                   cat.summ(porftvg.cvdlag, "fv.cvd", levels = 0:9),
                   cat.summ(porftvg.calag, "fv.ca", levels = 0:9),
                   cat.summ(frtpor.cvdlag, "fruit.cvd", levels = 0:9),
                   cat.summ(frtpor.calag, "fruit.ca", levels = 0:9),
                   cat.summ(diabtotr.cvdlag, "diab.cvd",
                            levels = 1:2, 
                            labels = c("no", "yes")),
                   cat.summ(expsmokCat, "ets",
                            levels = 0:1))))
}
)

output.chd  <- cmpfun(function(x, ...) {
  O1 <- pop.summ(nrow(x))
  O2 <- with(x, cat.summ(chd.incidence, "chd",levels = init.year + i, labels="incidence"))
  O3 <- with(x, sum(table(factor(chd.incidence, exclude = c(0, NA)))))
  names(O3) <- "chd.prevalence"
  O4 <- with(x, sum(table(dead, exclude=c(F, NA, NaN))))
  names(O4) <- "chd.mortality"
  return(c(O1, O2, O3, O4))
}
)

output.stroke  <- cmpfun(function(x, ...) {
  O1 <- pop.summ(nrow(x))
  O2 <- with(x, cat.summ(stroke.incidence, "stroke",levels = 2011 + i, labels="incidence"))
  O3 <- with(x, sum(table(factor(stroke.incidence, exclude = c(0, NA)))))
  names(O3) <- "stroke.prevalence"
  O4 <- with(x, sum(table(dead, exclude=c(F, NA, NaN))))
  names(O4) <- "stroke.mortality"
  return(c(O1, O2, O3, O4))
}
)

output.other  <- cmpfun(function(x, ...) {
  O1 <- pop.summ(nrow(x))
  O2 <- with(x, sum(table(dead, exclude=c(F, NA, NaN))))
  names(O2) <- "other.mortality"
  return(c(O1, O2))
}
)
