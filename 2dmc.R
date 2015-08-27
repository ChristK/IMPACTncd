#cmpfile("./2dmc.R")
cat("Sample RR values for 2d Monte Carlo\n\n")

if ("CHD" %in% diseasestoexclude) {
  tobacco.rr.chd[, rr := stochRRabov1(1, mean.rr, ci.rr), by = .(mean.rr, ci.rr)]
  chd.ets.rr.mc <-  stochRRabov1(1, 1.26, 1.38)
  sbp.rr.chd[, rr := stochRRbelow1(1, mean.rr, ci.rr), by = .(mean.rr, ci.rr)]
  chol.rr.chd[, rr := stochRRbelow1(1, mean.rr, ci.rr), by = .(mean.rr, ci.rr)]
  chd.bmi.rr.mc <- 
    data.table(
      agegroup = unique(agegroup.fn(0:90)),
      rr       = c(rep(1, 5), 
                   rep(stochRRabov1(1, 1.213256, 1.278837), 8),
                   rep(stochRRabov1(1, 1.058372, 1.115581), 2),
                   rep(1, 4))
    , key = "agegroup"
    )
  chd.diab.rr.mc <- 
    data.table(
      agegroup        = rep(unique(agegroup.fn(0:90)),2),
      diabtotr.cvdlag = rep(c("1", "2"), each=19),
      rr              = c(rep(1, 24), 
                          rep(stochRRabov1(1, 2.51, 2.8), 8),
                          rep(stochRRabov1(1, 2.01, 2.26), 2),
                          rep(stochRRabov1(1, 1.78, 2.05), 4))
    , key = c("agegroup", "diabtotr.cvdlag")
    )
  chd.fv.rr.mc <- stochRRbelow1(1, 0.96, 0.99)
  pa.rr.chd[, rr := stochRRabov1(1, mean.rr, ci.rr), by = .(mean.rr, ci.rr)]
}

if ("stroke" %in% diseasestoexclude) {
  tobacco.rr.stroke[, rr := stochRRabov1(1, mean.rr, ci.rr), by = .(mean.rr, ci.rr)]
  stroke.ets.rr.mc <- stochRRabov1(1, 1.25, 1.38)
  sbp.rr.stroke[, rr := stochRRbelow1(1, mean.rr, ci.rr), by = .(mean.rr, ci.rr)]
  chol.rr.stroke[, rr := stochRRbelow1(1, mean.rr, ci.rr), by = .(mean.rr, ci.rr)]
  stroke.bmi.rr.mc <- 
    data.table(
      agegroup = unique(agegroup.fn(0:90)),
      rr       = c(rep(1, 5), 
                   rep(stochRRabov1(1, 1.183667, 1.261833), 8),
                   rep(stochRRabov1(1, 1.077667, 1.148833), 2),
                   rep(1, 4))
    , key = "agegroup"
    )
  stroke.diab.rr.mc <- 
    data.table(
      agegroup        = rep(unique(agegroup.fn(0:90)),2),
      diabtotr.cvdlag = rep(c("1", "2"), each=19),
      rr              = c(rep(1, 24), 
                          rep(stochRRabov1(1, 3.74, 4.58), 8),
                          rep(stochRRabov1(1, 2.06, 2.58), 2),
                          rep(stochRRabov1(1, 1.8, 2.27), 4))
    , key = c("agegroup", "diabtotr.cvdlag")
    )
  stroke.fv.rr.mc <- stochRRbelow1(1, 0.95, 0.97)
  pa.rr.stroke[, rr := stochRRabov1(1, mean.rr, ci.rr), by = .(mean.rr, ci.rr)]
}

if ("C16" %in% diseasestoexclude) {
  c16.salt.optim <- rnorm(1, 4, 0.3) # optimal level for salt around 4 g/day
  c16.tob.rr.mc <- stochRRabov1(1, 1.04, 1.01)
  c16.extob.rr.mc <- stochRRbelow1(1, 0.961, 1)
  
  c16.fv.rr.mc <- 
    data.table(
      agegroup = unique(agegroup.fn(0:90)),
      rr       = c(rep(1, 5), 
                   rep(stochRRbelow1(1, 0.94, 1), 10),
                   rep(stochRRbelow1(1, 0.95, 1), 2),
                   rep(stochRRbelow1(1, 0.97, 1), 2))
    , key = "agegroup"
    )
  
  c16.salt.rr.mc <- 
    data.table(
      agegroup = unique(agegroup.fn(0:90)),
      rr       = c(rep(1, 5), 
                   rep(stochRRabov1(1, 1.08, 1), 10),
                   rep(stochRRabov1(1, 1.06, 1), 2),
                   rep(stochRRabov1(1, 1.04, 1), 2))
      , key = "agegroup"
    )
  
  # Salt effect on sbp
  # Calculate the effect of salt on SBP. From He FJ, MacGregor GA. 
  # Effect of modest salt reduction on blood pressure: a meta-analysis
  # of randomized trials. Implications for public health. 
  # Journal of Human Hypertension 2002;16:761. 
  # 100mmol reduction of salt (5.85 g) causes 7.11 mmHg reduction in SBP
  # in hypertensives and 3.57 in normotensive. 
  # no ci reported but both have p<0.001 so from Altman DG, Bland JM. 
  # How to obtain the confidence interval
  # from a P value. The BMJ 2011   z = -0.862 + sqrt(0.743 - 2.404 * log(0.001)) 
  # and se = effect/z (ie 7.11/z)
  salt.sbp.norm.mc <- rtruncnorm(1, 0, Inf, 3.57, 3.57/(-0.862 + sqrt(0.743 - 2.404 * log(0.001))))
  salt.sbp.htn.mc <- rtruncnorm(1, 0, Inf, 7.11, 7.11/(-0.862 + sqrt(0.743 - 2.404 * log(0.001))))
  
  salt.sbp.norm <- function(salt.difference) {
    return(salt.difference * salt.sbp.norm.mc /5.85)
  }
  
  salt.sbp.htn <- function(salt.difference) {
    return(salt.difference * salt.sbp.htn.mc /5.85)
  }
}
