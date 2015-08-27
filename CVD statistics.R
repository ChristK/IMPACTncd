#cmpfile("./CVD statistics.R")
# CVD statistics
if ("CHD" %in% diseasestoexclude) {
  tobacco.rr.chd  <- setkey(
    fread(
      "./CVD Statistics/tobacco.rrchd.csv", 
      stringsAsFactors = F, 
      colClasses = c("factor", "factor",
                     "factor", "numeric",
                     "numeric")
    ),
    agegroup, sex, cigst1.cvdlag
  )
  
  sbp.rr.chd <- setkey(
    fread(
      "./CVD Statistics/sbp.rrchd.csv", 
      stringsAsFactors = F, 
      colClasses = c("factor", "factor",
                     "numeric", "numeric")
    ),
    agegroup, sex
  )
  
  chol.rr.chd <- setkey(
    fread(
      "./CVD Statistics/chol.rrchd.csv", 
      stringsAsFactors = F, 
      colClasses = c("factor", "numeric",
                     "numeric")
    ),
    agegroup
  )
  
  pa.rr.chd <- setkey(
    fread(
      "./CVD Statistics/pa.rrchd.csv", 
      stringsAsFactors = F, 
      colClasses = c("factor", "integer",
                     "numeric", "numeric")
    ),
    agegroup, a30to06m.cvdlag
  )
  
  fatality30chd <- setkey(
    fread(
      "./CVD Statistics/fatality30chd.csv", 
      stringsAsFactors = T, 
      colClasses = c("factor", "factor", 
                     "numeric")
    ),
    agegroup, sex
  )

  xx <-  suppressWarnings(
    rbind(fread("./CVD Statistics/CHD DISMOD Males.csv", sep = ",", header = T, stringsAsFactors = F,
                skip = 3, nrows = 100)[, sex := 1],
          fread("./CVD Statistics/CHD DISMOD Females.csv", sep = ",", header = T, stringsAsFactors = F, 
                skip = 3, nrows = 100)[, sex := 2]
    )[, `:=` (sex = factor(sex), age = as.integer(Age))]
  )
  
  
  CHDincid <- setnames(copy(xx[, c(15, 14, 6), with = F]), "Incidence (rates)", "incidence")[, incidence := as.numeric(incidence)]
  
  CHDpreval <- setnames(copy(xx[, c(15, 14, 7), with = F]), "Prevalence (rates)", "prevalence")[, prevalence := as.numeric(prevalence)]
  
  CHDsurv <- setnames(copy(xx[, c(15, 14, 9), with = F]), "Case fatality (rates)", "fatality")[, fatality := as.numeric(fatality)]

  if (init.year < 2011) {
    CHDsurv[, fatality := fatality * 
               ((100 + fatality.annual.improvement.chd) / 100)^(2011 - init.year)
             ]
  }
}

# Do I have to separate between ischaemic and haemorrhagic? The risk factors seems more ore less the same.
if ("stroke" %in% diseasestoexclude) {
  tobacco.rr.stroke <- setkey(
    fread(
      "./CVD Statistics/tobacco.rrstroke.csv", 
      stringsAsFactors = F, 
      colClasses = c("factor", "factor", 
                     "factor", "numeric",
                     "numeric")
    ),
    agegroup, sex, cigst1.cvdlag
  )
  
  sbp.rr.stroke <- setkey(
    fread(
      "./CVD Statistics/sbp.rrstroke.csv", 
      stringsAsFactors = F, 
      colClasses = c("factor", "factor",
                     "numeric", "numeric")
    ),
    agegroup, sex
  )
  
  chol.rr.stroke <- setkey(
    fread(
      "./CVD Statistics/chol.rrstroke.csv", 
      stringsAsFactors = F, 
      colClasses = c("factor", "numeric", 
                     "numeric")
    ),
    agegroup
  )
  
  pa.rr.stroke <- setkey(
    fread(
      "./CVD Statistics/pa.rrstroke.csv", 
      stringsAsFactors = F, 
      colClasses = c("factor", "integer",
                     "numeric", "numeric")
    ),
    agegroup, a30to06m.cvdlag
  )

  fatality30stroke <- setkey(
    fread("./CVD Statistics/fatality30stroke.csv", 
          stringsAsFactors = F, 
          colClasses = c("factor", "factor",
                         "numeric")
    ),
    agegroup, sex
  )
 
#    strokeincid <- fread(
#     "./CVD Statistics/strokeincid.csv", 
#     sep = ",",
#     header = T, 
#     stringsAsFactors = F)[,
#                           `:=` (sex = factor(sex), 
#                                 agegroup = ordered(agegroup))]
#   setkey(strokeincid, agegroup, sex)
# 
#   strokepreval <- fread(
#     "./CVD Statistics/strokepreval.csv", 
#     sep = ",",
#     header = T, 
#     stringsAsFactors = F)[,
#                           `:=` (sex = factor(sex), 
#                                 agegroup = ordered(agegroup))]
# 
#   strokesurv <- fread(
#     "./CVD Statistics/strokesurv.csv", 
#     sep = ",", 
#     header = T, 
#     stringsAsFactors = F)[, 
#                           `:=` (sex = factor(sex), 
#                                 agegroup = ordered(agegroup))] 
  xx <-  suppressWarnings(
    rbind(fread("./CVD Statistics/Stroke DISMOD Males.csv", sep = ",", header = T, stringsAsFactors = F,
                skip = 3, nrow = 100)[, sex := 1],
          fread("./CVD Statistics/Stroke DISMOD Females.csv", sep = ",", header = T, stringsAsFactors = F, 
                skip = 3, nrow = 100)[, sex := 2]
    )[, `:=` (sex = factor(sex), age = as.integer(Age))]
  )
  
  
  strokeincid <- setnames(copy(xx[, c(15, 14, 6), with = F]), "Incidence (rates)", "incidence")[, incidence := as.numeric(incidence)]
  
  strokepreval <- setnames(copy(xx[, c(15, 14, 7), with = F]), "Prevalence (rates)", "prevalence")[, prevalence := as.numeric(prevalence)]
  
  strokesurv <- setnames(copy(xx[, c(15, 14, 9), with = F]), "Case fatality (rates)", "fatality")[, fatality := as.numeric(fatality)]
  
  if (init.year < 2011) {
    strokesurv[, fatality := fatality * 
              ((100 + fatality.annual.improvement.stroke) / 100)^(2011 - init.year)
            ]
  }
}
