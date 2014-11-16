require("ggplot2")
require("binom")

# Define function to summarise output RF
MC.mean <- function(m, sd, n, ...) {
  sd <- ifelse(is.na(sd), 2 * m, sd)
  m <- rnorm(n, m, sd)
  return(list(mean= mean(m, na.rm=T),
              lui = quantile(m, probs = 0.025, na.rm = T),
              uui = quantile(m, probs = 0.975, na.rm = T)))
}

# create a closure to produce the functions for graphs
closure.table.cat <- function (dt, type, rf, ...) {
  rf <- substitute(rf)
  grp <- c("year", "scenario")
  
  if (type == "S") grp <- c(grp, "sex")
  if (type == "SQ") grp <- c(grp, "sex", "qimd")
  if (type == "SA") grp <- c(grp, "sex", "agegroup")
  if (type == "SAQ") grp <- c(grp, "sex", "agegroup", "qimd")
  
  function() {
    dt <- dt[group == type,
             MC.mean(eval(rf)/pop, 
                     sqrt((eval(rf)/pop)*(1-eval(rf)/pop)/pop),
                     .N),
             by=grp]
    
    return(dt)  
  }
}

closure.table.cont <- function (dt, type, rf.m, rf.sd, ...) {
  rf.m <- substitute(rf.m)
  rf.sd <- substitute(rf.sd)
  grp <- c("year", "scenario")
  
  if (type == "S") grp <- c(grp, "sex")
  if (type == "SQ") grp <- c(grp, "sex", "qimd")
  if (type == "SA") grp <- c(grp, "sex", "agegroup")
  if (type == "SAQ") grp <- c(grp, "sex", "agegroup", "qimd")
  
  function() {
    dt <- dt[group == type,
             MC.mean(eval(rf.m), 
                     eval(rf.sd)/sqrt(pop),
                     .N),
             by=grp]
   return(dt)  
  }
}

closure.graph.cat <- function (dt, type, rf, lag, yaxis, title, yscale = 1) {
  rf <- substitute(rf)
  grp <- c("year", "scenario")
  
  if (type == "S") grp <- c(grp, "sex")
  if (type == "SQ") grp <- c(grp, "sex", "qimd")
  if (type == "SA") grp <- c(grp, "sex", "agegroup")
  if (type == "SAQ") grp <- c(grp, "sex", "agegroup", "qimd")
  
  function() {
    dt <- dt[group == type,
             MC.mean(eval(rf)/pop, 
                     sqrt((eval(rf)/pop)*(1-eval(rf)/pop)/pop),
                     .N),
             by=grp]
    dt[, `:=` (year = year - lag, 
               mean = mean *yscale,
               lui = lui * yscale,
               uui = uui * yscale)]
    g <- ggplot(dt,
                aes(x=year,
                    y=mean, 
                    colour=scenario, 
                    ymax=max(mean) * 1.05, 
                    ymin = 0)
    ) + 
      geom_errorbar(
        aes(
          ymin = lui, 
          ymax = uui
        ),
        width=.1
      ) +
      geom_line() +
      geom_point(size=3) +
      ylab(ifelse(yscale == 1, yaxis, paste0(yaxis, " per ", format(yscale, scientific = F)))) +
      scale_x_continuous(name="Year")
    
    if (type == "S") g <- g + facet_grid(sex ~ .) + ggtitle(paste0(title, " (ages ", ageL, " - ", ageH, ")")) 
    if (type == "SQ") g <- g + facet_grid(sex ~ qimd) + ggtitle(paste0(title, " by QIMD"))
    if (type == "SA") g <- g + facet_grid(sex ~ agegroup) + ggtitle(paste0(title, " by Age Group"))
    if (type == "SAQ") g <- g + facet_grid(sex + qimd ~ agegroup)+ ggtitle(paste0(title, "\nby Sex, Age Group and QIMD"))
    return(g)  
  }
}

closure.graph.cont <- function (dt, type, rf.m, rf.sd, lag = cvd.lag, yaxis, title) {
  rf.m <- substitute(rf.m)
  rf.sd <- substitute(rf.sd)
  grp <- c("year", "scenario")
  
  if (type == "S") grp <- c(grp, "sex")
  if (type == "SQ") grp <- c(grp, "sex", "qimd")
  if (type == "SA") grp <- c(grp, "sex", "agegroup")
  if (type == "SAQ") grp <- c(grp, "sex", "agegroup", "qimd")
  
  function() {
    dt <- dt[group == type,
             MC.mean(eval(rf.m), 
                     eval(rf.sd)/sqrt(pop),
                     .N),
             by=grp]
    dt[, year := year - lag]
    g <- ggplot(dt,
                aes(x=year,
                    y=mean, 
                    colour=scenario, 
                    ymax=max(mean)*1.05, 
                    ymin = 0)
    ) + 
      geom_errorbar(
        aes(
          ymin = lui, 
          ymax = uui
        ),
        width=.1
      ) +
      geom_line() +
      geom_point(size=3) +
      ylab(yaxis) +
      scale_x_continuous(name="Year")
    
    if (type == "S") g <- g + facet_grid(sex ~ .) + ggtitle(paste0(title, " (ages ", ageL, " - ", ageH, ")")) 
    if (type == "SQ") g <- g + facet_grid(sex ~ qimd) + ggtitle(paste0(title, " by QIMD"))
    if (type == "SA") g <- g + facet_grid(sex ~ agegroup) + ggtitle(paste0(title, " by Age Group"))
    if (type == "SAQ") g <- g + facet_grid(sex + qimd ~ agegroup)+ ggtitle(paste0(title, "\nby Sex, Age Group and QIMD"))
    return(g)  
  }
}

# Example (tt is a function)
# tt <- closure.graph.cat(chd.burden, "S", chd.incidence, 0, "Incidence", "CHD Incidence", 100000 )
# tt()

Graphs.fn <- list()
for (type in c("S", "SQ", "SA", "SAQ")) {
  # Risk Factors
  Graphs.fn[[paste0("smoking.", type)]] <- closure.graph.cat(riskfactors,
                                                             type,
                                                             smok.cvd.active, 
                                                             0,
                                                             "Prevalence",
                                                             "Smoking Prevalence")
  
  Graphs.fn[[paste0("ets.", type)]] <- closure.graph.cat(riskfactors,
                                                         type,
                                                         ets.1, 
                                                         0,
                                                         "Prevalence",
                                                         "Environmental Tobacco Smoking")
  
  Graphs.fn[[paste0("diabetes.", type)]] <- closure.graph.cat(riskfactors,
                                                              type,
                                                              diab.cvd.yes, 
                                                              0,
                                                              "Prevalence",
                                                              "Diabetes Prevalence")
  
  Graphs.fn[[paste0("fv.", type)]] <- closure.graph.cat(riskfactors,
                                                        type,
                                                        fv.cvd.6+fv.cvd.7+fv.cvd.8+fv.cvd.9, 
                                                        0,
                                                        "Prevalence",
                                                        "Five or more F&V portions a day")
  
  Graphs.fn[[paste0("bmi.", type)]] <- closure.graph.cont(riskfactors,
                                                          type,
                                                          bmi.cvd.mean, 
                                                          bmi.cvd.sd,
                                                          0,
                                                          "BMI (kg/m^2)",
                                                          "BMI")
  
  Graphs.fn[[paste0("tc.", type)]] <- closure.graph.cont(riskfactors,
                                                         type,
                                                         tc.cvd.mean, 
                                                         tc.cvd.sd,
                                                         0,
                                                         "Total Cholesterol (mmol/l)",
                                                         "Total Cholesterol")
  
  Graphs.fn[[paste0("sbp.", type)]] <- closure.graph.cont(riskfactors,
                                                          type,
                                                          sbp.cvd.mean, 
                                                          sbp.cvd.sd,
                                                          0,
                                                          "Systolic Blood Pressure (mmHg)",
                                                          "Systolic Blood Pressure")
  # Life expectancy
  Graphs.fn[[paste0("le0.", type)]] <- closure.graph.cont(life.exp0,
                                                        type,
                                                        mean,
                                                        sd,
                                                        0,
                                                        "Age (years)",
                                                        "Life Expectancy at Birth")
  
  Graphs.fn[[paste0("le65.", type)]] <- closure.graph.cont(life.exp65,
                                                          type,
                                                          mean,
                                                          sd,
                                                          0,
                                                          "Age (years)",
                                                          "Life Expectancy at 65")
  Graphs.fn[[paste0("hle.", type)]] <- closure.graph.cont(hlife.exp,
                                                           type,
                                                           mean,
                                                           sd,
                                                           0,
                                                           "Age (years)",
                                                           "Healthy Life Expectancy")
  
  # CHD
  if ("CHD" %in% diseasestoexclude) {
    Graphs.fn[[paste0("chdincid.", type)]] <- closure.graph.cat(chd.burden,
                                                               type,
                                                               chd.incidence, 
                                                               0,
                                                               "Incidence",
                                                               "CHD Incidence",
                                                               100000)
  
    Graphs.fn[[paste0("chdpreval.", type)]] <- closure.graph.cat(chd.burden,
                                                                type,
                                                                chd.prevalence, 
                                                                0,
                                                                "Prevalence",
                                                                "CHD Prevalence",
                                                                100000)
   
    Graphs.fn[[paste0("chdmortal.", type)]] <- closure.graph.cat(chd.burden,
                                                                 type,
                                                                 chd.mortality, 
                                                                 0,
                                                                 "Mortality",
                                                                 "CHD Mortality",
                                                                 100000)
  }
  
  # Stroke
  if ("stroke" %in% diseasestoexclude) {
    Graphs.fn[[paste0("strokeincid.", type)]] <- closure.graph.cat(stroke.burden,
                                                                type,
                                                                stroke.incidence, 
                                                                0,
                                                                "Incidence",
                                                                "Stroke Incidence",
                                                                100000)
    
    Graphs.fn[[paste0("strokepreval.", type)]] <- closure.graph.cat(stroke.burden,
                                                                 type,
                                                                 stroke.prevalence, 
                                                                 0,
                                                                 "Prevalence",
                                                                 "Stroke Prevalence",
                                                                 100000)
    
    Graphs.fn[[paste0("strokemortal.", type)]] <- closure.graph.cat(stroke.burden,
                                                                 type,
                                                                 stroke.mortality, 
                                                                 0,
                                                                 "Mortality",
                                                                 "Stroke Mortality",
                                                                 100000)
  }
 
}

Tables.fn <- list()
for (type in c("S", "SQ", "SA", "SAQ")) {
  # Risk Factors
  Tables.fn[[paste0("smoking.", type)]] <- closure.table.cat(riskfactors,
                                                             type,
                                                             smok.cvd.active, 
                                                             0,
                                                             "Prevalence",
                                                             "Smoking Prevalence")
  
  Tables.fn[[paste0("ets.", type)]] <- closure.table.cat(riskfactors,
                                                         type,
                                                         ets.1, 
                                                         0,
                                                         "Prevalence",
                                                         "Environmental Tobacco Smoking")
  
  Tables.fn[[paste0("diabetes.", type)]] <- closure.table.cat(riskfactors,
                                                              type,
                                                              diab.cvd.yes, 
                                                              0,
                                                              "Prevalence",
                                                              "Diabetes Prevalence")
  
  Tables.fn[[paste0("fv.", type)]] <- closure.table.cat(riskfactors,
                                                        type,
                                                        fv.cvd.6+fv.cvd.7+fv.cvd.8+fv.cvd.9, 
                                                        0,
                                                        "Prevalence",
                                                        "Five or more F&V portions a day")
  
  Tables.fn[[paste0("bmi.", type)]] <- closure.table.cont(riskfactors,
                                                          type,
                                                          bmi.cvd.mean, 
                                                          bmi.cvd.sd,
                                                          0,
                                                          "BMI (kg/m^2)",
                                                          "BMI")
  
  Tables.fn[[paste0("tc.", type)]] <- closure.table.cont(riskfactors,
                                                         type,
                                                         tc.cvd.mean, 
                                                         tc.cvd.sd,
                                                         0,
                                                         "Total Cholesterol (mmol/l)",
                                                         "Total Cholesterol")
  
  Tables.fn[[paste0("sbp.", type)]] <- closure.table.cont(riskfactors,
                                                          type,
                                                          sbp.cvd.mean, 
                                                          sbp.cvd.sd,
                                                          0,
                                                          "Systolic Blood Pressure (mmHg)",
                                                          "Systolic Blood Pressure")
  # Life expectancy
  Tables.fn[[paste0("le0.", type)]] <- closure.table.cont(life.exp0,
                                                          type,
                                                          mean,
                                                          sd,
                                                          0,
                                                          "Age (years)",
                                                          "Life Expectancy at Birth")
  
  Tables.fn[[paste0("le65.", type)]] <- closure.table.cont(life.exp65,
                                                           type,
                                                           mean,
                                                           sd,
                                                           0,
                                                           "Age (years)",
                                                           "Life Expectancy at 65")
  
  Tables.fn[[paste0("hle.", type)]] <- closure.table.cont(hlife.exp,
                                                          type,
                                                          mean,
                                                          sd,
                                                          0,
                                                          "Age (years)",
                                                          "Healthy Life Expectancy")
  
  # CHD
  if ("CHD" %in% diseasestoexclude) {
    Tables.fn[[paste0("chdincid.", type)]] <- closure.table.cat(chd.burden,
                                                                type,
                                                                chd.incidence, 
                                                                0,
                                                                "Incidence",
                                                                "CHD Incidence",
                                                                100000)
    
    Tables.fn[[paste0("chdpreval.", type)]] <- closure.table.cat(chd.burden,
                                                                 type,
                                                                 chd.prevalence, 
                                                                 0,
                                                                 "Prevalence",
                                                                 "CHD Prevalence",
                                                                 100000)
    
    Tables.fn[[paste0("chdmortal.", type)]] <- closure.table.cat(chd.burden,
                                                                 type,
                                                                 chd.mortality, 
                                                                 0,
                                                                 "Mortality",
                                                                 "CHD Mortality",
                                                                 100000)
  }
  
  # Stroke
  if ("stroke" %in% diseasestoexclude) {
    Tables.fn[[paste0("strokeincid.", type)]] <- closure.table.cat(stroke.burden,
                                                                   type,
                                                                   stroke.incidence, 
                                                                   0,
                                                                   "Incidence",
                                                                   "Stroke Incidence",
                                                                   100000)
    
    Tables.fn[[paste0("strokepreval.", type)]] <- closure.table.cat(stroke.burden,
                                                                    type,
                                                                    stroke.prevalence, 
                                                                    0,
                                                                    "Prevalence",
                                                                    "Stroke Prevalence",
                                                                    100000)
    
    Tables.fn[[paste0("strokemortal.", type)]] <- closure.table.cat(stroke.burden,
                                                                    type,
                                                                    stroke.mortality, 
                                                                    0,
                                                                    "Mortality",
                                                                    "Stroke Mortality",
                                                                    100000)
  }
  
}
