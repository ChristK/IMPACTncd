#cmpfile("./post simulation functions.R")
require("ggplot2")
require("binom")
require("scales")

weighted.var.se <- function(x, w, na.rm=FALSE)
  #  Computes the variance of a weighted mean following Cochran 1977 definition
{
  if (na.rm) { w <- w[i <- !is.na(x)]; x <- x[i] }
  n = length(w)
  xWbar = weighted.mean(x,w,na.rm=na.rm)
  wbar = mean(w)
  out = n/((n-1)*sum(w)^2)*(sum((w*x-wbar*xWbar)^2)-2*xWbar*sum((w-wbar)*(w*x-wbar*xWbar))+xWbar^2*sum((w-wbar)^2))
  return(out)
}

# Define function to summarise output RF
MC.mean <- function(m, sd, n, lim = Inf, ...) {
  sd <- ifelse(is.na(sd), 2 * m, sd) # if mean of one value then sd is technically NA. This is an assumption sd =2*m
  m <- rtruncnorm(n, 0, lim, m, sd)
  return(list(mean = mean(m, na.rm = T, trim = 0.05),
              lui = quantile(m, probs = 0.025, na.rm = T),
              uui = quantile(m, probs = 0.975, na.rm = T)))
}

# Standard populations
direct.standard <- data.table(agegroup = ordered(c("<1   ", "01-04", "05-09",
                                                   "10-14", "15-19", "20-24", 
                                                   "25-29", "30-34", "35-39", 
                                                   "40-44", "45-49", "50-54",
                                                   "55-59", "60-64", "65-69",
                                                   "70-74", "75-79", "80-84", 
                                                   "85+")),
                              who.pop = c(8.86 * 1/5, 8.86 * 4/5, 8.69,
                                          8.60, 8.47, 8.22, 7.93, 7.61, 
                                          7.15, 6.59 ,6.04 ,5.37 ,4.55, 
                                          3.72, 2.96, 2.21, 1.52, 0.91,
                                          0.63
                              ),
                              esp.pop = c(1000, 4000, 5500, 
                                          5500, 5500, 6000, 
                                          6000, 6500, 7000, 
                                          7000, 7000, 7000, 
                                          6500, 6000, 5500, 
                                          5000, 4000, 2500, 
                                          2500 
                              )
)

# Set cutoff to exlude subgroups with population less than cut off from graphs
# Else it produces wrong CI

cutoff <- 0


direct.standard[, who.pop := who.pop / 100] # per 1
direct.standard[, esp.pop := esp.pop / 100000] # per 1
setkey(direct.standard, agegroup)

age.limit <- levels(factor((agegroup.fn(ageL:ageH)))) 

# create a closure to produce the functions for graphs
closure.table.cat <- function (dt, type, rf, ...) {
  rf <- substitute(rf)
  grp <- c("year", "scenario")
  dt2 <- NULL
  
  if (type == "S")   grp <- c(grp, "sex")
  if (type == "SQ")  grp <- c(grp, "sex", "qimd")
  if (type == "SA")  grp <- c(grp, "sex", "agegroup")
  if (type == "SAQ") grp <- c(grp, "sex", "agegroup", "qimd")

  function() {
    if ("pop" %in% names(dt)) {
      dt <- dt[pop > cutoff, ]
    }
    
    dt1 <- dt[group == type,
              MC.mean(eval(rf)/pop, 
                      sqrt((eval(rf)/pop)*(1-eval(rf)/pop)/pop),
                      .N),
              by = grp]
    
    if (type %in% c("SA", "SAQ")) {
      dt <- dt[agegroup %in% age.limit,]
      if (type == "SA") grp <- c("year", "scenario", "sex")
      if (type == "SAQ") grp <- c("year", "scenario", "sex", "qimd")
      dt2 <- merge(direct.standard, dt, by = "agegroup", all.y = T)
      dt2 <- dt2[group == type,]
      dt2 <- dt2[,
                 `:=` (
                   who.pop.mean = sum(eval(rf) * who.pop/pop, na.rm = T) / sum(who.pop, na.rm = T),
                   who.pop.se = sqrt((sum(eval(rf) * (who.pop/pop)^2, na.rm = T)) / sum(who.pop, na.rm = T)),
                   esp.pop.mean = sum(eval(rf) * esp.pop/pop, na.rm = T) / sum(esp.pop, na.rm = T),
                   esp.pop.se = sqrt((sum(eval(rf) * (esp.pop/pop)^2, na.rm = T)) / sum(esp.pop, na.rm = T))
                 ),
                 by= c(grp, "mc")] # from http://www.iarc.fr/en/publications/pdfs-online/epi/sp155/ci5v8-chap8.pdf
      
      dt3 <- dt2[, MC.mean(who.pop.mean, who.pop.se, .N), by = grp]
      dt3 <- unique(dt3, by = grp)
      dt3[, agegroup := "who.pop"]
      
      dt4 <- dt2[, MC.mean(esp.pop.mean, esp.pop.se, .N), by = grp]
      dt4 <- unique(dt4, by = grp)
      dt4[, agegroup := "esp.pop"]
      
      dt2 <- rbind(dt3, dt4, fill = T)
    }
    
    dt <- rbind(dt1, dt2, fill = T)  
    setkeyv(dt, grp)
    
    return(dt)  
  }
}

closure.table.cont <- function (dt, type, rf.m, rf.sd, ...) {
  rf.m <- substitute(rf.m)
  rf.sd <- substitute(rf.sd)
  grp <- c("year", "scenario")
  dt2 <- NULL
  
  if (type == "S")   grp <- c(grp, "sex")
  if (type == "SQ")  grp <- c(grp, "sex", "qimd")
  if (type == "SA")  grp <- c(grp, "sex", "agegroup")
  if (type == "SAQ") grp <- c(grp, "sex", "agegroup", "qimd")
  
  function() {
    
    if ("pop" %in% names(dt)) {
      dt <-   dt[pop > cutoff, ]
    } else {
      dt[, pop := n]
    }
    
    dt1 <- dt[group == type,
              MC.mean(eval(rf.m), 
                      eval(rf.sd)/sqrt(pop),
                      .N),
              by=grp]
    
    if (type %in% c("SA", "SAQ")) {
      dt <- dt[agegroup %in% age.limit,]
      if (type == "SA") grp <- c("year", "scenario", "sex")
      if (type == "SAQ") grp <- c("year", "scenario", "sex", "qimd")
      dt2 <- merge(direct.standard, dt, by = "agegroup", all.y = T)
      dt2 <- dt2[group == type,]
      dt2 <- dt2[,
                 `:=` (
                   who.pop.mean = sum(eval(rf.m) * who.pop, na.rm = T) / sum(who.pop, na.rm = T),
                   who.pop.se = sqrt((sum(eval(rf.m) * pop * (who.pop/pop)^2, na.rm = T)) / sum(who.pop, na.rm = T)),
                   esp.pop.mean = sum(eval(rf.m) * esp.pop, na.rm = T) / sum(esp.pop, na.rm = T),
                   esp.pop.se = sqrt((sum(eval(rf.m) * pop * (esp.pop/pop)^2, na.rm = T)) / sum(esp.pop, na.rm = T))
                 ),
                 by= c(grp, "mc")] #from http://www.iarc.fr/en/publications/pdfs-online/epi/sp155/ci5v8-chap8.pdf
      
      dt3 <- dt2[, MC.mean(who.pop.mean, who.pop.se, .N), by = grp]
      dt3 <- unique(dt3, by = grp)
      dt3[, agegroup := "who.pop"]
      
      dt4 <- dt2[, MC.mean(esp.pop.mean, esp.pop.se, .N), by = grp]
      dt4 <- unique(dt4, by = grp)
      dt4[, agegroup := "esp.pop"]
      
      dt2 <- rbind(dt3, dt4, fill = T)
      
    }
    dt <- rbind(dt1, dt2, fill = T)  
    setkeyv(dt, grp)
    
    return(dt)  
  }
}

closure.graph.cat <- function (dt, type, rf, lag, yaxis, title, yscale = 1) {
  rf <- substitute(rf)
  grp <- c("year", "scenario")
  
  if (type == "S")   grp <- c(grp, "sex")
  if (type == "SQ")  grp <- c(grp, "sex", "qimd")
  if (type == "SA")  grp <- c(grp, "sex", "agegroup")
  if (type == "SAQ") grp <- c(grp, "sex", "agegroup", "qimd")
  
  function() {
    if ("pop" %in% names(dt)) {
      dt <- dt[pop > cutoff, ]
    }
    
    dt <- dt[group == type,
             MC.mean(eval(rf)/pop, 
                     sqrt((eval(rf)/pop)*(1-eval(rf)/pop)/pop),
                     .N),
             by=grp]
    
    dt[, `:=` (year = year - lag, 
               mean = mean * yscale,
               lui = lui * yscale,
               uui = uui * yscale)]
    
    g <- ggplot(dt,
                aes(x = year,
                    y = mean, 
                    colour = scenario, 
                    ymax = max(mean) * 1.01) 
                #ymin = 0)
    ) + 
      geom_ribbon(
        aes(
          ymin = lui, 
          ymax = uui,
          fill = scenario,
          linetype = NA
        ),
        stat = "identity",
        alpha = 0.3
      ) +
      #geom_smooth(alpha=0.9, method='loess', se = F) +
      geom_point(size = 2, stat = "identity") +
      ylab(ifelse(yscale == 1, yaxis, paste0(yaxis, " per ", format(yscale, scientific = F)))) +
      scale_x_continuous(name="Year") +
      theme(axis.text.x  = element_text(angle=90, vjust=0.5))
    
    if (type == "P") g <- g + ggtitle(paste0(title, " (ages ", ageL, " - ", ageH, ")")) 
    if (type == "S") g <- g + facet_grid(sex ~ .) + ggtitle(paste0(title, " by sex (ages ", ageL, " - ", ageH, ")")) 
    if (type == "SQ") g <- g + 
        facet_grid(sex ~ qimd) + 
        ggtitle(paste0(title, " by sex and QIMD (ages ", ageL, " - ", ageH, ")"))
    if (type == "SA") g <- g + facet_grid(sex ~ agegroup) + ggtitle(paste0(title, " by sex and age group"))
    if (type == "SAQ") g <- g + facet_grid(sex + qimd ~ agegroup)+ ggtitle(paste0(title, " by sex, age group and QIMD"))
    return(g)  
  }
}

closure.graph.std <- function (dt, type, rf, lag, yaxis, title, yscale = 1, std = "WHO") {
  if (type %!in% c("SA", "SAQ")) return(function(){})
  
  rf <- substitute(rf)
  grp <- c("year", "scenario")
  
  if (type == "SA") grp <- c(grp, "sex")
  if (type == "SAQ") grp <- c(grp, "sex", "qimd")
  
  function() {
    if ("pop" %in% names(dt)) {
      dt <-   dt[pop > cutoff, ]
    }
    
    dt <- merge(direct.standard, dt, by = "agegroup", all.y = T)
    dt <- dt[agegroup %in% age.limit,]
    dt <- dt[group == type,]
    
    if (std == "WHO") {
      dt <- dt[,
               `:=` (
                 std.mean = sum(eval(rf)* who.pop/pop, na.rm = T) / sum(who.pop, na.rm = T),
                 std.se = sqrt((sum(eval(rf) * (who.pop/pop)^2, na.rm = T)) / sum(who.pop, na.rm = T))
               ),
               by= c(grp, "mc")] #from http://www.iarc.fr/en/publications/pdfs-online/epi/sp155/ci5v8-chap8.pdf
    }
    
    if (std == "European") {
      dt <- dt[,
               `:=` (
                 std.mean = sum(eval(rf)* esp.pop/pop, na.rm = T) / sum(esp.pop, na.rm = T),
                 std.se = sqrt((sum(eval(rf) * (esp.pop/pop)^2, na.rm = T)) / sum(esp.pop, na.rm = T))
               ),
               by= c(grp, "mc")] #from http://www.iarc.fr/en/publications/pdfs-online/epi/sp155/ci5v8-chap8.pdf
    }
    
    dt <- dt[, MC.mean(std.mean, std.se, .N), by = grp]
    dt <- unique(dt, by = grp)
    
    dt[, `:=` (year = year - lag, 
               mean = mean * yscale,
               lui = lui * yscale,
               uui = uui * yscale)]
    
    g <- ggplot(dt,
                aes(x = year,
                    y = mean, 
                    colour = scenario, 
                    ymax = max(mean) * 1.01) 
                # ymin = min(lui) * 0.95)
                # ymin = 0)
    ) + 
      geom_ribbon(
        aes(
          ymin = lui, 
          ymax = uui,
          fill = scenario,
          linetype = NA
        ),
        stat = "identity",
        alpha = 0.3
      ) +
      #geom_line() +
      geom_point(size = 2, stat = "identity") +
      ylab(ifelse(yscale == 1, yaxis, paste0(yaxis, " per ", format(yscale, scientific = F)))) +
      scale_x_continuous(name="Year") +
      theme(axis.text.x  = element_text(angle=90, vjust=0.5))
    
    if (type == "SA") g <- g + facet_grid(sex ~ .) + 
        ggtitle(paste0(title, " by sex (ages ", ageL, " - ", ageH, ")\nStandardised against the ", std, " standard population"))
    if (type == "SAQ") g <- g + facet_grid(sex ~ qimd) + 
        ggtitle(paste0(title, " by sex and QIMD (ages ", ageL, " - ", ageH, ")\nStandardised against the ", std, " standard population"))
    return(g)  
  }
}

closure.graph.cont <- function (dt, type, rf.m, rf.sd, lag = cvd.lag, yaxis, title) {
  rf.m <- substitute(rf.m)
  rf.sd <- substitute(rf.sd)
  grp <- c("year", "scenario")
  
  if (type == "S")   grp <- c(grp, "sex")
  if (type == "SQ")  grp <- c(grp, "sex", "qimd")
  if (type == "SA")  grp <- c(grp, "sex", "agegroup")
  if (type == "SAQ") grp <- c(grp, "sex", "agegroup", "qimd")
  
  function() {
    if ("pop" %in% names(dt)) {
      dt <-   dt[pop > cutoff, ]
    } else {
      dt[, pop := n]
    }
    
    dt <- dt[group == type,
             MC.mean(eval(rf.m), 
                     eval(rf.sd)/sqrt(pop),
                     .N),
             by=grp]
    dt[, year := year - lag]
    g <- ggplot(dt,
                aes(x=year,
                    y=mean, 
                    colour = scenario, 
                    ymax = max(mean)*1.01) 
                # ymin = 0)
    ) + 
      geom_ribbon(
        aes(
          ymin = lui, 
          ymax = uui,
          fill = scenario,
          linetype = NA
        ),
        stat = "identity",
        alpha = 0.3
      ) +
      #geom_line() +
      geom_point(size = 2, stat = "identity") +
      ylab(yaxis) +
      scale_x_continuous(name="Year") +
      theme(axis.text.x  = element_text(angle=90, vjust=0.5))
    
    if (type == "P") g <- g + ggtitle(paste0(title, " (ages ", ageL, " - ", ageH, ")")) 
    if (type == "S") g <- g + facet_grid(sex ~ .) + ggtitle(paste0(title, " by sex (ages ", ageL, " - ", ageH, ")")) 
    if (type == "SQ") g <- g + 
        facet_grid(sex ~ qimd) + 
        ggtitle(paste0(title, " by sex and QIMD (ages ", ageL, " - ", ageH, ")"))
    if (type == "SA") g <- g + facet_grid(sex ~ agegroup) + ggtitle(paste0(title, " by sex and age group"))
    if (type == "SAQ") g <- g + 
        facet_grid(sex + qimd ~ agegroup) + 
        ggtitle(paste0(title, " by sex, age group and QIMD"))
    return(g)  
  }
}

closure.graph.std.cont <- function (dt, type, rf, lag = cvd.lag, yaxis, title, std = "WHO", yscale = 1) {
  if (type %!in% c("SA", "SAQ")) return(function(){})
  
  rf <- substitute(rf)
  grp <- c("year", "scenario")
  
  if (type == "SA")  grp <- c(grp, "sex")
  if (type == "SAQ") grp <- c(grp, "sex", "qimd")
  
  function() {
    if ("pop" %in% names(dt)) {
      dt <-   dt[pop > cutoff, ]
    } else {
      dt[, pop := n]
    }
    
    dt <- merge(direct.standard, dt, by = "agegroup", all.y = T)
    dt <- dt[agegroup %in% age.limit,]
    dt <- dt[group == type,]
    
    if (std == "WHO") {
      dt <- dt[,
               `:=` (
                 std.mean = sum(eval(rf) * who.pop, na.rm = T) / sum(who.pop, na.rm = T),
                 std.se = sqrt((sum(eval(rf) * pop * (who.pop/pop)^2, na.rm = T)) / sum(who.pop, na.rm = T))
               ),
               by= c(grp, "mc")] #from http://www.iarc.fr/en/publications/pdfs-online/epi/sp155/ci5v8-chap8.pdf
    }
    
    if (std == "European") {
      dt <- dt[,
               `:=` (
                 std.mean = sum(eval(rf) * esp.pop, na.rm = T) / sum(esp.pop, na.rm = T),
                 std.se = sqrt((sum(eval(rf) * pop * (esp.pop/pop)^2, na.rm = T)) / sum(esp.pop, na.rm = T))
               ),
               by= c(grp, "mc")] # from http://www.iarc.fr/en/publications/pdfs-online/epi/sp155/ci5v8-chap8.pdf
      # adjusted by me for continuous variables
    }
    
    dt <- dt[, MC.mean(std.mean, std.se, .N), by = grp]
    dt <- unique(dt, by = grp)
    
    dt[, `:=` (year = year - lag, 
               mean = mean * yscale,
               lui = lui * yscale,
               uui = uui * yscale)]
    
    g <- ggplot(dt,
                aes(x = year,
                    y = mean, 
                    colour = scenario, 
                    ymax = max(mean) * 1.01) 
                # ymin = min(lui) * 0.95)
                # ymin = 0)
    ) + 
      geom_ribbon(
        aes(
          ymin = lui, 
          ymax = uui,
          fill = scenario,
          linetype = NA
        ),
        stat = "identity",
        alpha = 0.3
      ) +
      #geom_line() +
      geom_point(size = 2, stat = "identity") +
      ylab(ifelse(yscale == 1, yaxis, paste0(yaxis, " per ", format(yscale, scientific = F)))) +
      scale_x_continuous(name="Year") +
      theme(axis.text.x  = element_text(angle=90, vjust=0.5))
    
    if (type == "SA") g <- g + facet_grid(sex ~ .) + 
        ggtitle(paste0(title, " by sex (ages ", ageL, " - ", ageH, ")\nStandardised against the ", std, " standard population"))
    if (type == "SAQ") g <- g + facet_grid(sex ~ qimd) + 
        ggtitle(paste0(title, " by sex and QIMD (ages ", ageL, " - ", ageH, ")\nStandardised against the ", std, " standard population"))
    return(g)  
  }
}

closure.table.SII <- function(dt, indicator, age.adj = F, ...){
  indicator <- substitute(indicator)
  function() {
    setkey(dt, scenario, mc, sex, year, qimd)
    dt[, qimd2 := cumsum(as.numeric(pop))/sum(as.numeric(pop)),
        by= .(sex, year, scenario, mc)]
    dt[, qimd2 := (qimd2 + c(0, qimd2[seq_len(.N-1)]))/2,
        by= .(sex, year, scenario, mc)]
    
    if (age.adj == T) {
    dt <- dt[, glm(
          I(100000* eval(indicator)/pop) ~ qimd2 + agegroup, weights = as.numeric(pop), 
          family="gaussian"(link="identity"))$coefficients["qimd2"],
by = .(sex, scenario, year, mc)][, list(SII = mean(V1, na.rm = T),
                                        lui = quantile(V1, probs = 0.025, na.rm = T),
                                        uui = quantile(V1, probs = 0.975, na.rm = T)),
                                  by = .(sex, scenario, year)]
    } else {
      dt <- dt[, glm(
        I(100000* eval(indicator)/pop) ~ qimd2, 
        family="gaussian"(link="identity"))$coefficients["qimd2"],
        by = .(sex, scenario, year, mc)][, list(SII = mean(V1, na.rm = T),
                                                lui = quantile(V1, probs = 0.025, na.rm = T),
                                                uui = quantile(V1, probs = 0.975, na.rm = T)),
                                         by = .(sex, scenario, year)]
    }

    return(dt)
  }
}

# Kunst and Mackenbach method/definition
closure.table.RII <- function(dt, indicator, age.adj = F, ...){
  indicator <- substitute(indicator)
  function() {
    setkey(dt, scenario, mc, sex, year, qimd)
    dt[, qimd2 := cumsum(as.numeric(pop))/sum(as.numeric(pop)),
        by= .(sex, year, scenario, mc)]
    dt[, qimd2 := (qimd2 + c(0, qimd2[seq_len(.N-1)]))/2,
        by= .(sex, year, scenario, mc)]
   
    if (age.adj == T) {
    dt <- dt[, glm(
      cbind(eval(indicator), pop-eval(indicator)) ~ qimd2 + agegroup,
      family="binomial"(link="log"))$coefficients["qimd2"], 
      by = .(sex, scenario, year, mc)][, list(RII = mean(exp(V1), na.rm = T),
                                              lui = quantile(exp(V1), probs = 0.025, na.rm = T),
                                              uui = quantile(exp(V1), probs = 0.975, na.rm = T)),
                                        by = .(sex, scenario, year)]
    } else {
      dt <- dt[, glm(
        cbind(eval(indicator), pop-eval(indicator)) ~ qimd2,
        family="binomial"(link="log"))$coefficients["qimd2"], 
        by = .(sex, scenario, year, mc)][, list(RII = mean(exp(V1), na.rm = T),
                                                lui = quantile(exp(V1), probs = 0.025, na.rm = T),
                                                uui = quantile(exp(V1), probs = 0.975, na.rm = T)),
                                         by = .(sex, scenario, year)]
    }
    
    return(dt)
  }
}

closure.table.le.SII <- function(dt, indicator, ...){
  indicator <- substitute(indicator)
  function() {
    setkey(dt, scenario, mc, sex, year, qimd)
    dt[, qimd2:= cumsum(as.numeric(n))/sum(as.numeric(n)), by = .(sex, year, scenario, mc)]
    dt[, qimd2 := (qimd2 + c(0, qimd2[seq_len(.N-1)]))/2, by = .(sex, year, scenario, mc)]
    
    dt <- dt[, glm(
      eval(indicator) ~ qimd2, 
      weights = as.numeric(n),
      family = "gaussian"(link="identity"))$coefficients["qimd2"],
      by = .(sex, scenario, year, mc)][, list(SII = -mean(V1, na.rm = T),
                                              lui = -quantile(V1, probs = 0.025, na.rm = T),
                                              uui = -quantile(V1, probs = 0.975, na.rm = T)),
                                        by = .(sex, scenario, year)]
    return(dt)
  }
}

closure.graph.SII <- function(dt, indicator, title, age.adj = F, ...){
  indicator <- substitute(indicator)
  function() {
    setkey(dt, scenario, mc, sex, year, qimd)
    dt[, qimd2:= cumsum(as.numeric(pop))/sum(as.numeric(pop)), by= .(sex, year, scenario, mc)]
    dt[, qimd2 := (qimd2 + c(0, qimd2[seq_len(.N-1)]))/2, by= .(sex, year, scenario, mc)]
    
    if (age.adj == T) {
      dt <- dt[, glm(
        I(100000* eval(indicator)/pop) ~ qimd2 + agegroup, weights = as.numeric(pop), 
        family="gaussian"(link="identity"))$coefficients["qimd2"],
        by = .(sex, scenario, year, mc)][, list(SII = mean(V1, na.rm = T),
                                                lui = quantile(V1, probs = 0.025, na.rm = T),
                                                uui = quantile(V1, probs = 0.975, na.rm = T)),
                                         by = .(sex, scenario, year)]
    } else {
      dt <- dt[, glm(
        I(100000* eval(indicator)/pop) ~ qimd2, 
        family="gaussian"(link="identity"))$coefficients["qimd2"],
        by = .(sex, scenario, year, mc)][, list(SII = mean(V1, na.rm = T),
                                                lui = quantile(V1, probs = 0.025, na.rm = T),
                                                uui = quantile(V1, probs = 0.975, na.rm = T)),
                                         by = .(sex, scenario, year)]
    }
    
    g <- ggplot(dt,
                aes(x = year,
                    y = SII, 
                    colour = scenario, 
                    ymax = max(SII) * 1.01) 
                #ymin = 0)
    ) + 
      geom_ribbon(
        aes(
          ymin = lui, 
          ymax = uui,
          fill = scenario,
          linetype = NA
        ),
        stat = "identity",
        alpha = 0.3
      ) +
      #geom_smooth(alpha=0.9, method='loess', se = F) +
      geom_point(size = 2, stat = "identity") +
      ylab("Slop Index of Inequality per 100000")+
      scale_x_continuous(name="Year") +
      theme(axis.text.x  = element_text(angle=90, vjust=0.5)) +
      facet_grid(sex ~ .) + 
      ggtitle(paste0("Absolute socioeconomic inequality of ", title, "\nby sex (ages ", ageL, " - ", ageH, ")"))
    return(g)
  }
}

closure.graph.RII <- function(dt, indicator, title, age.adj = F, ...){
  indicator <- substitute(indicator)
  function() {
    setkey(dt, scenario, mc, sex, year, qimd)
    dt[, qimd2:= cumsum(as.numeric(pop))/sum(as.numeric(pop)), by= .(sex, year, scenario, mc)]
    dt[, qimd2 := (qimd2 + c(0, qimd2[seq_len(.N-1)]))/2, by= .(sex, year, scenario, mc)]
    
    if (age.adj == T) {
      dt <- dt[, glm(
        cbind(eval(indicator), pop-eval(indicator)) ~ qimd2 + agegroup,
        family="binomial"(link="log"))$coefficients["qimd2"], 
        by = .(sex, scenario, year, mc)][, list(RII = mean(exp(V1), na.rm = T),
                                                lui = quantile(exp(V1), probs = 0.025, na.rm = T),
                                                uui = quantile(exp(V1), probs = 0.975, na.rm = T)),
                                         by = .(sex, scenario, year)]
    } else {
      dt <- dt[, glm(
        cbind(eval(indicator), pop-eval(indicator)) ~ qimd2,
        family="binomial"(link="log"))$coefficients["qimd2"], 
        by = .(sex, scenario, year, mc)][, list(RII = mean(exp(V1), na.rm = T),
                                                lui = quantile(exp(V1), probs = 0.025, na.rm = T),
                                                uui = quantile(exp(V1), probs = 0.975, na.rm = T)),
                                         by = .(sex, scenario, year)]
    }
    
    g <- ggplot(dt,
                aes(x = year,
                    y = RII, 
                    colour = scenario, 
                    ymax = max(RII) * 1.01) 
                #ymin = 0)
    ) + 
      geom_ribbon(
        aes(
          ymin = lui, 
          ymax = uui,
          fill = scenario,
          linetype = NA
        ),
        stat = "identity",
        alpha = 0.3
      ) +
      #geom_smooth(alpha=0.9, method='loess', se = F) +
      geom_point(size = 2, stat = "identity") +
      ylab("Relative Index of Inequality")+
      scale_x_continuous(name="Year") +
      theme(axis.text.x  = element_text(angle=90, vjust=0.5)) +
      facet_grid(sex ~ .) + 
      ggtitle(paste0("Relative socioeconomic inequality of ", title, "\nby sex (ages ", ageL, " - ", ageH, ")"))
    return(g)
  }
}

closure.graph.le.RII <- function(dt, indicator, title, ...){
  indicator <- substitute(indicator)
  function() {
    setkey(dt, scenario, mc, sex, year, qimd)
    dt[, qimd2:= cumsum(as.numeric(n))/sum(as.numeric(n)), by= .(sex, year, scenario, mc)]
    dt[, qimd2 := (qimd2 + c(0, qimd2[seq_len(.N-1)]))/2, by= .(sex, year, scenario, mc)]
    
    dt <- dt[, glm(
      eval(indicator) ~ qimd2, 
      weights = as.numeric(n),
      family = "gaussian"(link="log"))$coefficients["qimd2"],
      by = .(sex, scenario, year, mc)][, list(RII = mean(exp(-V1), na.rm = T),
                                              lui = quantile(exp(-V1), probs = 0.025, na.rm = T),
                                              uui = quantile(exp(-V1), probs = 0.975, na.rm = T)),
                                        by = .(sex, scenario, year)]
    
    g <- ggplot(dt,
                aes(x = year,
                    y = RII, 
                    colour = scenario, 
                    ymax = max(RII) * 1.01) 
                #ymin = 0)
    ) + 
      geom_ribbon(
        aes(
          ymin = lui, 
          ymax = uui,
          fill = scenario,
          linetype = NA
        ),
        stat = "identity",
        alpha = 0.3
      ) +
      #geom_smooth(alpha=0.9, method='loess', se = F) +
      geom_point(size = 2, stat = "identity") +
      ylab("Relative Index of Inequality")+
      scale_x_continuous(name="Year") +
      theme(axis.text.x  = element_text(angle=90, vjust=0.5)) +
      facet_grid(sex ~ .) + 
      ggtitle(paste0("Relative socioeconomic inequality in ", title, ""))
    return(g)
  }
}

closure.table.le.RII <- function(dt, indicator, ...){
  indicator <- substitute(indicator)
  function() {
    setkey(dt, scenario, mc, sex, year, qimd)
    dt[, qimd2:= cumsum(as.numeric(n))/sum(as.numeric(n)), by= .(sex, year, scenario, mc)]
    dt[, qimd2 := (qimd2 + c(0, qimd2[seq_len(.N-1)]))/2, by= .(sex, year, scenario, mc)]
    
    dt <- dt[, glm(
      eval(indicator) ~ qimd2, 
      weights = as.numeric(n),
      family = "gaussian"(link="log"))$coefficients["qimd2"],
      by = .(sex, scenario, year, mc)][, list(RII = mean(exp(-V1), na.rm = T),
                                              lui = quantile(exp(-V1), probs = 0.025, na.rm = T),
                                              uui = quantile(exp(-V1), probs = 0.975, na.rm = T)),
                                        by = .(sex, scenario, year)]
    
    
    return(dt)
  }
}

closure.graph.le.SII <- function(dt, indicator, title, ...){
  indicator <- substitute(indicator)
  function() {
    setkey(dt, scenario, mc, sex, year, qimd)
    dt[, qimd2:= cumsum(as.numeric(n))/sum(as.numeric(n)), by= .(sex, year, scenario, mc)]
    dt[, qimd2 := (qimd2 + c(0, qimd2[seq_len(.N-1)]))/2, by= .(sex, year, scenario, mc)]
    
    dt <- dt[, glm(
      eval(indicator) ~ qimd2, 
      weights = as.numeric(n),
      family = "gaussian"(link="identity"))$coefficients["qimd2"],
      by = .(sex, scenario, year, mc)][, list(SII = -mean(V1, na.rm = T),
                                              lui = -quantile(V1, probs = 0.025, na.rm = T),
                                              uui = -quantile(V1, probs = 0.975, na.rm = T)),
                                        by = .(sex, scenario, year)]
    
    g <- ggplot(dt,
                aes(x = year,
                    y = SII, 
                    colour = scenario, 
                    ymax = max(SII) * 1.01) 
                #ymin = 0)
    ) + 
      geom_ribbon(
        aes(
          ymin = lui, 
          ymax = uui,
          fill = scenario,
          linetype = NA
        ),
        stat = "identity",
        alpha = 0.3
      ) +
      #geom_smooth(alpha=0.9, method='loess', se = F) +
      geom_point(size = 2, stat = "identity") +
      ylab("Slope Index of Inequality in years")+
      scale_x_continuous(name="Year") +
      theme(axis.text.x  = element_text(angle=90, vjust=0.5)) +
      facet_grid(sex ~ .) + 
      ggtitle(paste0("Absolute socioeconomic inequality in ", title, ""))
    return(g)
  }
}

# Example (tt is a function)
# tt <- closure.graph.cat(chd.burden, "S", chd.incidence, 0, "Incidence", "CHD Incidence", 100000 )
# tt()

Graphs.fn <- list()
for (type in c("S", "SQ", "SA", "SAQ", "P")) {
  # Risk Factors
  Graphs.fn[[paste0("smoking.", type)]] <- closure.graph.cat(riskfactors,
                                                             type,
                                                             smok.cvd.active, 
                                                             cvd.lag,
                                                             "Prevalence",
                                                             "Smoking prevalence")
  
  Graphs.fn[[paste0("smoking.", type, ".WHO" )]] <- closure.graph.std(riskfactors,
                                                                      type,
                                                                      smok.cvd.active, 
                                                                      cvd.lag,
                                                                      "Prevalence",
                                                                      "Smoking prevalence",
                                                                      1,
                                                                      "WHO")
  
  Graphs.fn[[paste0("smoking.", type, ".ESP" )]] <- closure.graph.std(riskfactors,
                                                                      type,
                                                                      smok.cvd.active, 
                                                                      cvd.lag,
                                                                      "Prevalence",
                                                                      "Smoking prevalence",
                                                                      1,
                                                                      "European")
  
  Graphs.fn[[paste0("ets.", type)]] <- closure.graph.cat(riskfactors,
                                                         type,
                                                         ets.yes, 
                                                         cvd.lag,
                                                         "Prevalence",
                                                         "Environmental tobacco smoking")
  
  Graphs.fn[[paste0("ets.", type, ".WHO" )]] <- closure.graph.std(riskfactors,
                                                                  type,
                                                                  ets.yes, 
                                                                  cvd.lag,
                                                                  "Prevalence",
                                                                  "Environmental tobacco smoking",
                                                                  1,
                                                                  "WHO")
  
  Graphs.fn[[paste0("ets.", type, ".ESP" )]] <- closure.graph.std(riskfactors,
                                                                  type,
                                                                  ets.yes, 
                                                                  cvd.lag,
                                                                  "Prevalence",
                                                                  "Environmental tobacco smoking",
                                                                  1,
                                                                  "European")
  
  Graphs.fn[[paste0("diabetes.", type)]] <- closure.graph.cat(riskfactors,
                                                              type,
                                                              diab.cvd.yes, 
                                                              cvd.lag,
                                                              "Prevalence",
                                                              "Diabetes prevalence")
  
  Graphs.fn[[paste0("diabetes.", type, ".WHO" )]] <- closure.graph.std(riskfactors,
                                                                       type,
                                                                       diab.cvd.yes, 
                                                                       cvd.lag,
                                                                       "Prevalence",
                                                                       "Diabetes prevalence",
                                                                       1,
                                                                       "WHO")
  
  Graphs.fn[[paste0("diabetes.", type, ".ESP" )]] <- closure.graph.std(riskfactors,
                                                                       type,
                                                                       diab.cvd.yes, 
                                                                       cvd.lag,
                                                                       "Prevalence",
                                                                       "Diabetes prevalence",
                                                                       1,
                                                                       "European")
  
  Graphs.fn[[paste0("fv.cvd", type)]] <- closure.graph.cat(riskfactors,
                                                        type,
                                                        fv.cvd.5+fv.cvd.6+fv.cvd.7+fv.cvd.8, 
                                                        cvd.lag,
                                                        "Prevalence",
                                                        "Five or more F&V portions a day")
  
  Graphs.fn[[paste0("fv.cvd", type, ".WHO" )]] <- closure.graph.std(riskfactors,
                                                                 type,
                                                                 fv.cvd.5+fv.cvd.6+fv.cvd.7+fv.cvd.8, 
                                                                 cvd.lag,
                                                                 "Prevalence",
                                                                 "Five or more F&V portions a day",
                                                                 1,
                                                                 "WHO")
  
  Graphs.fn[[paste0("fv.cvd", type, ".ESP" )]] <- closure.graph.std(riskfactors,
                                                                 type,
                                                                 fv.cvd.5+fv.cvd.6+fv.cvd.7+fv.cvd.8, 
                                                                 cvd.lag,
                                                                 "Prevalence",
                                                                 "Five or more F&V portions a day",
                                                                 1,
                                                                 "European")
  
  
  Graphs.fn[[paste0("fv.ca", type)]] <- closure.graph.cat(riskfactors,
                                                        type,
                                                        fv.ca.5+fv.ca.6+fv.ca.7+fv.ca.8, 
                                                        ca.lag,
                                                        "Prevalence",
                                                        "Five or more F&V portions a day")
  
  Graphs.fn[[paste0("fv.ca", type, ".WHO" )]] <- closure.graph.std(riskfactors,
                                                                 type,
                                                                 fv.ca.5+fv.ca.6+fv.ca.7+fv.ca.8, 
                                                                 ca.lag,
                                                                 "Prevalence",
                                                                 "Five or more F&V portions a day",
                                                                 1,
                                                                 "WHO")
  
  Graphs.fn[[paste0("fv.ca", type, ".ESP" )]] <- closure.graph.std(riskfactors,
                                                                 type,
                                                                 fv.ca.5+fv.ca.6+fv.ca.7+fv.ca.8, 
                                                                 ca.lag,
                                                                 "Prevalence",
                                                                 "Five or more F&V portions a day",
                                                                 1,
                                                                 "European")
  
  Graphs.fn[[paste0("pa.", type)]] <- closure.graph.cat(riskfactors,
                                                        type,
                                                        pa.cvd.5+pa.cvd.6+pa.cvd.7, 
                                                        cvd.lag,
                                                        "Prevalence",
                                                        "Five or more days of PA per week")
  
  Graphs.fn[[paste0("pa.", type, ".WHO" )]] <- closure.graph.std(riskfactors,
                                                                 type,
                                                                 pa.cvd.5+pa.cvd.6+pa.cvd.7, 
                                                                 cvd.lag,
                                                                 "Prevalence",
                                                                 "Five or more days of PA per week",
                                                                 1,
                                                                 "WHO")
  
  Graphs.fn[[paste0("pa.", type, ".ESP" )]] <- closure.graph.std(riskfactors,
                                                                 type,
                                                                 pa.cvd.5+pa.cvd.6+pa.cvd.7, 
                                                                 cvd.lag,
                                                                 "Prevalence",
                                                                 "Five or more days of PA per week",
                                                                 1,
                                                                 "European")
  
  Graphs.fn[[paste0("bmi.cvd.", type)]] <- closure.graph.cont(riskfactors,
                                                          type,
                                                          bmi.cvd.mean, 
                                                          bmi.cvd.sd,
                                                          cvd.lag,
                                                          "Body mass index (kg/m^2)",
                                                          "Body mass index")
  
  Graphs.fn[[paste0("bmi.cvd.", type, ".WHO" )]] <- closure.graph.std.cont(riskfactors,
                                                                       type,
                                                                       bmi.cvd.mean, 
                                                                       cvd.lag,
                                                                       "Body mass index (kg/m^2)",
                                                                       "Body mass index",
                                                                       "WHO")
  
  Graphs.fn[[paste0("bmi.cvd.", type, ".ESP" )]] <- closure.graph.std.cont(riskfactors,
                                                                       type,
                                                                       bmi.cvd.mean, 
                                                                       cvd.lag,
                                                                       "Body mass index (kg/m^2)",
                                                                       "Body mass index",
                                                                       "European")
  
  Graphs.fn[[paste0("bmi.ca.", type)]] <- closure.graph.cont(riskfactors,
                                                              type,
                                                              bmi.ca.mean, 
                                                              bmi.ca.sd,
                                                              cancer.lag,
                                                              "Body mass index (kg/m^2)",
                                                              "Body mass index")
  
  Graphs.fn[[paste0("bmi.ca.", type, ".WHO" )]] <- closure.graph.std.cont(riskfactors,
                                                                           type,
                                                                           bmi.ca.mean, 
                                                                           cancer.lag,
                                                                           "Body mass index (kg/m^2)",
                                                                           "Body mass index",
                                                                           "WHO")
  
  Graphs.fn[[paste0("bmi.ca.", type, ".ESP" )]] <- closure.graph.std.cont(riskfactors,
                                                                           type,
                                                                           bmi.ca.mean, 
                                                                           cancer.lag,
                                                                           "Body mass index (kg/m^2)",
                                                                           "Body mass index",
                                                                           "European")
  
  
  Graphs.fn[[paste0("salt.ca.", type)]] <- closure.graph.cont(riskfactors,
                                                           type,
                                                           salt.ca.mean, 
                                                           salt.ca.sd,
                                                           cancer.lag,
                                                           "Salt consumption (g)",
                                                           "Salt consumption (cancer lag)")
  
  Graphs.fn[[paste0("salt.ca.", type, ".WHO" )]] <- closure.graph.std.cont(riskfactors,
                                                                        type,
                                                                        salt.ca.mean, 
                                                                        cancer.lag,
                                                                        "Salt consumption (g)",
                                                                        "Salt consumption (cancer lag)",
                                                                        "WHO")
  
  Graphs.fn[[paste0("salt.ca.", type, ".ESP" )]] <- closure.graph.std.cont(riskfactors,
                                                                        type,
                                                                        salt.ca.mean, 
                                                                        cancer.lag,
                                                                        "Salt consumption (g)",
                                                                        "Salt consumption (cancer lag)",
                                                                        "European")
  
  Graphs.fn[[paste0("salt.cvd.", type)]] <- closure.graph.cont(riskfactors,
                                                              type,
                                                              salt.cvd.mean, 
                                                              salt.cvd.sd,
                                                              cancer.lag,
                                                              "Salt consumption (g)",
                                                              "Salt consumption (cvd lag)")
  
  Graphs.fn[[paste0("salt.cvd.", type, ".WHO" )]] <- closure.graph.std.cont(riskfactors,
                                                                           type,
                                                                           salt.cvd.mean, 
                                                                           cancer.lag,
                                                                           "Salt consumption (g)",
                                                                           "Salt consumption (cvd lag)",
                                                                           "WHO")
  
  Graphs.fn[[paste0("salt.cvd.", type, ".ESP" )]] <- closure.graph.std.cont(riskfactors,
                                                                           type,
                                                                           salt.cvd.mean, 
                                                                           cancer.lag,
                                                                           "Salt consumption (g)",
                                                                           "Salt consumption (cvd lag)",
                                                                           "European")
  
  Graphs.fn[[paste0("tc.", type)]] <- closure.graph.cont(riskfactors,
                                                         type,
                                                         tc.cvd.mean, 
                                                         tc.cvd.sd,
                                                         cvd.lag,
                                                         "Total cholesterol (mmol/l)",
                                                         "Total cholesterol")
  
  Graphs.fn[[paste0("tc.", type, ".WHO" )]] <- closure.graph.std.cont(riskfactors,
                                                                      type,
                                                                      tc.cvd.mean, 
                                                                      cvd.lag,
                                                                      "Total cholesterol (mmol/l)",
                                                                      "Total cholesterol",
                                                                      "WHO")
  
  Graphs.fn[[paste0("tc.", type, ".ESP" )]] <- closure.graph.std.cont(riskfactors,
                                                                      type,
                                                                      tc.cvd.mean, 
                                                                      cvd.lag,
                                                                      "Total cholesterol (mmol/l)",
                                                                      "Total cholesterol",
                                                                      "European")
  
  Graphs.fn[[paste0("sbp.", type)]] <- closure.graph.cont(riskfactors,
                                                          type,
                                                          sbp.cvd.mean, 
                                                          sbp.cvd.sd,
                                                          cvd.lag,
                                                          "Systolic blood pressure (mmHg)",
                                                          "Systolic blood pressure")
  
  Graphs.fn[[paste0("sbp.", type, ".WHO" )]] <- closure.graph.std.cont(riskfactors,
                                                                       type,
                                                                       sbp.cvd.mean, 
                                                                       cvd.lag,
                                                                       "Systolic blood pressure (mmHg)",
                                                                       "Systolic blood pressure",
                                                                       "WHO")
  
  Graphs.fn[[paste0("sbp.", type, ".ESP" )]] <- closure.graph.std.cont(riskfactors,
                                                                       type,
                                                                       sbp.cvd.mean, 
                                                                       cvd.lag,
                                                                       "Systolic Blood Pressure (mmHg)",
                                                                       "Systolic Blood Pressure",
                                                                       "European")
  
  # Life expectancy
  if (type %in% c("S", "SQ")) {
    Graphs.fn[[paste0("le0.", type)]] <- closure.graph.cont(life.exp0,
                                                            type,
                                                            mean,
                                                            sd,
                                                            0,
                                                            "Age (years)",
                                                            "Life expectancy at birth")
    
    Graphs.fn[[paste0("le65.", type)]] <- closure.graph.cont(life.exp65,
                                                             type,
                                                             mean,
                                                             sd,
                                                             0,
                                                             "Age (years)",
                                                             "Life expectancy at 65")
  }
  
  if (type == "S") {
    
    Graphs.fn[[paste0("le0.SII.", type)]] <- closure.graph.le.SII(life.exp0[group == "SQ"],
                                                                  mean,
                                                                  "life expectancy at birth")
    
    Graphs.fn[[paste0("le0.RII.", type)]] <- closure.graph.le.RII(life.exp0[group == "SQ"],
                                                                  mean,
                                                                  "life expectancy at birth")
    
    Graphs.fn[[paste0("le65.SII.", type)]] <- closure.graph.le.SII(life.exp65[group == "SQ"],
                                                                   mean,
                                                                   "life expectancy at 65")
    
    Graphs.fn[[paste0("le65.RII.", type)]] <- closure.graph.le.RII(life.exp65[group == "SQ"],
                                                                   mean,
                                                                   "life expectancy at 65")
    
  }
  
  if (is.null(diseasestoexclude) == F) {
    if (type %in% c("S", "SQ")) {
      Graphs.fn[[paste0("hle.", type)]] <- closure.graph.cont(hlife.exp,
                                                              type,
                                                              mean,
                                                              sd,
                                                              0,
                                                              "Age (years)",
                                                              "Healthy life expectancy at birth")
    }
    
    if (type == "S") {
      
      Graphs.fn[[paste0("hle.SII.", type)]] <- closure.graph.le.SII(hlife.exp[group == "SQ"],
                                                                    mean,
                                                                    "healthy life expectancy at birth")
      
      Graphs.fn[[paste0("hle.RII.", type)]] <- closure.graph.le.RII(hlife.exp[group == "SQ"],
                                                                    mean,
                                                                    "healthy life xpectancy at birth")
      
    }
  }
  
  # CHD
  if ("CHD" %in% diseasestoexclude) {
    Graphs.fn[[paste0("chdincid.", type)]] <- closure.graph.cat(chd.burden,
                                                                type,
                                                                chd.incidence, 
                                                                0,
                                                                "Incidence",
                                                                "CHD incidence",
                                                                100000)
    
    Graphs.fn[[paste0("chdincid.", type, ".WHO" )]] <- closure.graph.std(chd.burden,
                                                                         type,
                                                                         chd.incidence, 
                                                                         0,
                                                                         "Incidence",
                                                                         "CHD incidence",
                                                                         100000,
                                                                         "WHO")
    
    Graphs.fn[[paste0("chdincid.", type, ".ESP" )]] <- closure.graph.std(chd.burden,
                                                                         type,
                                                                         chd.incidence, 
                                                                         0,
                                                                         "Incidence",
                                                                         "CHD incidence",
                                                                         100000,
                                                                         "European")
    
    Graphs.fn[[paste0("chdpreval.", type)]] <- closure.graph.cat(chd.burden,
                                                                 type,
                                                                 chd.prevalence, 
                                                                 0,
                                                                 "Prevalence",
                                                                 "CHD prevalence",
                                                                 100000)
    
    Graphs.fn[[paste0("chdpreval.", type, ".WHO" )]] <- closure.graph.std(chd.burden,
                                                                          type,
                                                                          chd.prevalence, 
                                                                          0,
                                                                          "Prevalence",
                                                                          "CHD prevalence",
                                                                          100000,
                                                                          "WHO")
    
    Graphs.fn[[paste0("chdpreval.", type, ".ESP" )]] <- closure.graph.std(chd.burden,
                                                                          type,
                                                                          chd.prevalence, 
                                                                          0,
                                                                          "Prevalence",
                                                                          "CHD prevalence",
                                                                          100000,
                                                                          "European")
    
    Graphs.fn[[paste0("chdmortal.", type)]] <- closure.graph.cat(chd.burden,
                                                                 type,
                                                                 chd.mortality, 
                                                                 0,
                                                                 "Mortality",
                                                                 "CHD mortality",
                                                                 100000)
    
    Graphs.fn[[paste0("chdmortal.", type, ".WHO" )]] <- closure.graph.std(chd.burden,
                                                                          type,
                                                                          chd.mortality, 
                                                                          0,
                                                                          "Mortality",
                                                                          "CHD mortality",
                                                                          100000,
                                                                          "WHO")
    
    Graphs.fn[[paste0("chdmortal.", type, ".ESP" )]] <- closure.graph.std(chd.burden,
                                                                          type,
                                                                          chd.mortality, 
                                                                          0,
                                                                          "Mortality",
                                                                          "CHD mortality",
                                                                          100000,
                                                                          "European")
    if (type == "S") {
      Graphs.fn[[paste0("chdincid.SII.", type)]] <- closure.graph.SII(chd.burden[group=="SQ"],
                                                                      chd.incidence,
                                                                      "CHD incidence",
                                                                      F)
      
      Graphs.fn[[paste0("chdpreval.SII.", type)]] <- closure.graph.SII(chd.burden[group=="SQ"],
                                                                       chd.prevalence,
                                                                       "CHD prevalence",
                                                                       F)
      
      Graphs.fn[[paste0("chdmortal.SII.", type)]] <- closure.graph.SII(chd.burden[group=="SQ"],
                                                                       chd.mortality,
                                                                       "CHD mortality",
                                                                       F)
      
      Graphs.fn[[paste0("chdincid.RII.", type)]] <- closure.graph.RII(chd.burden[group=="SQ"],
                                                                      chd.incidence,
                                                                      "CHD incidence",
                                                                      F)
      
      Graphs.fn[[paste0("chdpreval.RII.", type)]] <- closure.graph.RII(chd.burden[group=="SQ"],
                                                                       chd.prevalence,
                                                                       "CHD prevalence",
                                                                       F)
      
      Graphs.fn[[paste0("chdmortal.RII.", type)]] <- closure.graph.RII(chd.burden[group=="SQ"],
                                                                       chd.mortality,
                                                                       "CHD mortality",
                                                                       F)
      
      Graphs.fn[[paste0("chdincid.SII.adj.", type)]] <- closure.graph.SII(chd.burden[group=="SAQ"],
                                                                      chd.incidence,
                                                                      "CHD incidence (age adjusted)",
                                                                      T)
      
      Graphs.fn[[paste0("chdpreval.SII.adj.", type)]] <- closure.graph.SII(chd.burden[group=="SAQ"],
                                                                       chd.prevalence,
                                                                       "CHD prevalence (age adjusted)",
                                                                       T)
      
      Graphs.fn[[paste0("chdmortal.SII.adj.", type)]] <- closure.graph.SII(chd.burden[group=="SAQ"],
                                                                       chd.mortality,
                                                                       "CHD mortality (age adjusted)",
                                                                       T)
      
      Graphs.fn[[paste0("chdincid.RII.adj.", type)]] <- closure.graph.RII(chd.burden[group=="SAQ"],
                                                                      chd.incidence,
                                                                      "CHD incidence (age adjusted)",
                                                                      T)
      
      Graphs.fn[[paste0("chdpreval.RII.adj.", type)]] <- closure.graph.RII(chd.burden[group=="SAQ"],
                                                                       chd.prevalence,
                                                                       "CHD prevalence (age adjusted)",
                                                                       T)
      
      Graphs.fn[[paste0("chdmortal.RII.adj.", type)]] <- closure.graph.RII(chd.burden[group=="SAQ"],
                                                                       chd.mortality,
                                                                       "CHD mortality (age adjusted)",
                                                                       T)
    }
  }
  
  # Stroke
  if ("stroke" %in% diseasestoexclude) {
    Graphs.fn[[paste0("strokeincid.", type)]] <- closure.graph.cat(stroke.burden,
                                                                   type,
                                                                   stroke.incidence, 
                                                                   0,
                                                                   "Incidence",
                                                                   "Stroke incidence",
                                                                   100000)
    
    Graphs.fn[[paste0("strokeincid.", type, ".WHO" )]] <- closure.graph.std(stroke.burden,
                                                                            type,
                                                                            stroke.incidence, 
                                                                            0,
                                                                            "Incidence",
                                                                            "Stroke incidence",
                                                                            100000,
                                                                            "WHO")
    
    Graphs.fn[[paste0("strokeincid.", type, ".ESP" )]] <- closure.graph.std(stroke.burden,
                                                                            type,
                                                                            stroke.incidence, 
                                                                            0,
                                                                            "Incidence",
                                                                            "Stroke incidence",
                                                                            100000,
                                                                            "European")
    
    Graphs.fn[[paste0("strokepreval.", type)]] <- closure.graph.cat(stroke.burden,
                                                                    type,
                                                                    stroke.prevalence, 
                                                                    0,
                                                                    "Prevalence",
                                                                    "Stroke prevalence",
                                                                    100000)
    
    Graphs.fn[[paste0("strokepreval.", type, ".WHO" )]] <- closure.graph.std(stroke.burden,
                                                                             type,
                                                                             stroke.prevalence, 
                                                                             0,
                                                                             "Prevalence",
                                                                             "Stroke prevalence",
                                                                             100000,
                                                                             "WHO")
    
    Graphs.fn[[paste0("strokepreval.", type, ".ESP" )]] <- closure.graph.std(stroke.burden,
                                                                             type,
                                                                             stroke.prevalence, 
                                                                             0,
                                                                             "Prevalence",
                                                                             "Stroke prevalence",
                                                                             100000,
                                                                             "European")
    
    Graphs.fn[[paste0("strokemortal.", type)]] <- closure.graph.cat(stroke.burden,
                                                                    type,
                                                                    stroke.mortality, 
                                                                    0,
                                                                    "Mortality",
                                                                    "Stroke mortality",
                                                                    100000)
    
    Graphs.fn[[paste0("strokemortal.", type, ".WHO" )]] <- closure.graph.std(stroke.burden,
                                                                             type,
                                                                             stroke.mortality, 
                                                                             0,
                                                                             "Mortality",
                                                                             "Stroke mortality",
                                                                             100000,
                                                                             "WHO")
    
    Graphs.fn[[paste0("strokemortal.", type, ".ESP" )]] <- closure.graph.std(stroke.burden,
                                                                             type,
                                                                             stroke.mortality, 
                                                                             0,
                                                                             "Mortality",
                                                                             "Stroke mortality",
                                                                             100000,
                                                                             "European")
    if (type == "S") {
      
      Graphs.fn[[paste0("strokeincid.SII.", type)]] <- closure.graph.SII(stroke.burden[group=="SQ"],
                                                                         stroke.incidence,
                                                                         "stroke incidence",
                                                                         F)
      
      Graphs.fn[[paste0("strokepreval.SII.", type)]] <- closure.graph.SII(stroke.burden[group=="SQ"],
                                                                          stroke.prevalence,
                                                                          "stroke prevalence",
                                                                          F)
      
      Graphs.fn[[paste0("strokemortal.SII.", type)]] <- closure.graph.SII(stroke.burden[group=="SQ"],
                                                                          stroke.mortality,
                                                                          "stroke mortality",
                                                                          F)
      
      Graphs.fn[[paste0("strokeincid.RII.", type)]] <- closure.graph.RII(stroke.burden[group=="SQ"],
                                                                         stroke.incidence,
                                                                         "stroke incidence",
                                                                         F)
      
      Graphs.fn[[paste0("strokepreval.RII.", type)]] <- closure.graph.RII(stroke.burden[group=="SQ"],
                                                                          stroke.prevalence,
                                                                          "stroke prevalence",
                                                                          F)
      
      Graphs.fn[[paste0("strokemortal.RII.", type)]] <- closure.graph.RII(stroke.burden[group=="SQ"],
                                                                          stroke.mortality,
                                                                          "stroke mortality",
                                                                          F)
      
      Graphs.fn[[paste0("strokeincid.SII.adj.", type)]] <- closure.graph.SII(stroke.burden[group=="SAQ"],
                                                                         stroke.incidence,
                                                                         "stroke incidence (age adjusted)",
                                                                         T)
      
      Graphs.fn[[paste0("strokepreval.SII.adj.", type)]] <- closure.graph.SII(stroke.burden[group=="SAQ"],
                                                                          stroke.prevalence,
                                                                          "stroke prevalence (age adjusted)",
                                                                          T)
      
      Graphs.fn[[paste0("strokemortal.SII.adj.", type)]] <- closure.graph.SII(stroke.burden[group=="SAQ"],
                                                                          stroke.mortality,
                                                                          "stroke mortality (age adjusted)",
                                                                          T)
      
      Graphs.fn[[paste0("strokeincid.RII.adj.", type)]] <- closure.graph.RII(stroke.burden[group=="SAQ"],
                                                                         stroke.incidence,
                                                                         "stroke incidence (age adjusted)",
                                                                         T)
      
      Graphs.fn[[paste0("strokepreval.RII.adj.", type)]] <- closure.graph.RII(stroke.burden[group=="SAQ"],
                                                                          stroke.prevalence,
                                                                          "stroke prevalence (age adjusted)",
                                                                          T)
      
      Graphs.fn[[paste0("strokemortal.RII.adj.", type)]] <- closure.graph.RII(stroke.burden[group=="SAQ"],
                                                                          stroke.mortality,
                                                                          "stroke mortality (age adjusted)",
                                                                          T)
    }
  }
  
  if ("C16" %in% diseasestoexclude) {
    Graphs.fn[[paste0("c16incid.", type)]] <- closure.graph.cat(c16.burden,
                                                                type,
                                                                c16.incidence, 
                                                                0,
                                                                "Incidence",
                                                                "Gastric ca incidence",
                                                                100000)
    
    Graphs.fn[[paste0("c16incid.", type, ".WHO" )]] <- closure.graph.std(c16.burden,
                                                                         type,
                                                                         c16.incidence, 
                                                                         0,
                                                                         "Incidence",
                                                                         "Gastric ca incidence",
                                                                         100000,
                                                                         "WHO")
    
    Graphs.fn[[paste0("c16incid.", type, ".ESP" )]] <- closure.graph.std(c16.burden,
                                                                         type,
                                                                         c16.incidence, 
                                                                         0,
                                                                         "Incidence",
                                                                         "Gastric ca incidence",
                                                                         100000,
                                                                         "European")
    
    Graphs.fn[[paste0("c16preval.", type)]] <- closure.graph.cat(c16.burden,
                                                                 type,
                                                                 c16.prevalence, 
                                                                 0,
                                                                 "Prevalence",
                                                                 "Gastric ca prevalence",
                                                                 100000)
    
    Graphs.fn[[paste0("c16preval.", type, ".WHO" )]] <- closure.graph.std(c16.burden,
                                                                          type,
                                                                          c16.prevalence, 
                                                                          0,
                                                                          "Prevalence",
                                                                          "Gastric ca prevalence",
                                                                          100000,
                                                                          "WHO")
    
    Graphs.fn[[paste0("c16preval.", type, ".ESP" )]] <- closure.graph.std(c16.burden,
                                                                          type,
                                                                          c16.prevalence, 
                                                                          0,
                                                                          "Prevalence",
                                                                          "Gastric ca prevalence",
                                                                          100000,
                                                                          "European")
    
    Graphs.fn[[paste0("c16mortal.", type)]] <- closure.graph.cat(c16.burden,
                                                                 type,
                                                                 c16.mortality, 
                                                                 0,
                                                                 "Mortality",
                                                                 "Gastric ca mortality",
                                                                 100000)
    
    Graphs.fn[[paste0("c16mortal.", type, ".WHO" )]] <- closure.graph.std(c16.burden,
                                                                          type,
                                                                          c16.mortality, 
                                                                          0,
                                                                          "Mortality",
                                                                          "Gastric ca mortality",
                                                                          100000,
                                                                          "WHO")
    
    Graphs.fn[[paste0("c16mortal.", type, ".ESP" )]] <- closure.graph.std(c16.burden,
                                                                          type,
                                                                          c16.mortality, 
                                                                          0,
                                                                          "Mortality",
                                                                          "Gastric ca mortality",
                                                                          100000,
                                                                          "European")
    if (type == "S") {
      Graphs.fn[[paste0("c16incid.SII.", type)]] <- closure.graph.SII(c16.burden[group=="SQ"],
                                                                      c16.incidence,
                                                                      "Gastric ca incidence",
                                                                      F)
      
      Graphs.fn[[paste0("c16preval.SII.", type)]] <- closure.graph.SII(c16.burden[group=="SQ"],
                                                                       c16.prevalence,
                                                                       "Gastric ca prevalence",
                                                                       F)
      
      Graphs.fn[[paste0("c16mortal.SII.", type)]] <- closure.graph.SII(c16.burden[group=="SQ"],
                                                                       c16.mortality,
                                                                       "Gastric ca mortality",
                                                                       F)
      
      Graphs.fn[[paste0("c16incid.RII.", type)]] <- closure.graph.RII(c16.burden[group=="SQ"],
                                                                      c16.incidence,
                                                                      "Gastric ca incidence",
                                                                      F)
      
      Graphs.fn[[paste0("c16preval.RII.", type)]] <- closure.graph.RII(c16.burden[group=="SQ"],
                                                                       c16.prevalence,
                                                                       "Gastric ca prevalence",
                                                                       F)
      
      Graphs.fn[[paste0("c16mortal.RII.", type)]] <- closure.graph.RII(c16.burden[group=="SQ"],
                                                                       c16.mortality,
                                                                       "Gastric ca mortality",
                                                                       F)
      
      Graphs.fn[[paste0("c16incid.SII.adj.", type)]] <- closure.graph.SII(c16.burden[group=="SAQ"],
                                                                          c16.incidence,
                                                                          "Gastric ca incidence (age adjusted)",
                                                                          T)
      
      Graphs.fn[[paste0("c16preval.SII.adj.", type)]] <- closure.graph.SII(c16.burden[group=="SAQ"],
                                                                           c16.prevalence,
                                                                           "Gastric ca prevalence (age adjusted)",
                                                                           T)
      
      Graphs.fn[[paste0("c16mortal.SII.adj.", type)]] <- closure.graph.SII(c16.burden[group=="SAQ"],
                                                                           c16.mortality,
                                                                           "Gastric ca mortality (age adjusted)",
                                                                           T)
      
      Graphs.fn[[paste0("c16incid.RII.adj.", type)]] <- closure.graph.RII(c16.burden[group=="SAQ"],
                                                                          c16.incidence,
                                                                          "Gastric ca incidence (age adjusted)",
                                                                          T)
      
      Graphs.fn[[paste0("c16preval.RII.adj.", type)]] <- closure.graph.RII(c16.burden[group=="SAQ"],
                                                                           c16.prevalence,
                                                                           "Gastric ca prevalence (age adjusted)",
                                                                           T)
      
      Graphs.fn[[paste0("c16mortal.RII.adj.", type)]] <- closure.graph.RII(c16.burden[group=="SAQ"],
                                                                           c16.mortality,
                                                                           "Gastric ca mortality (age adjusted)",
                                                                           T)
    }
  }
}


Tables.fn <- list()
for (type in c("S", "SQ", "SA", "SAQ", "P")) {
  # Risk Factors
  Tables.fn[[paste0("smoking.", type)]] <- closure.table.cat(riskfactors,
                                                             type,
                                                             smok.cvd.active, 
                                                             0,
                                                             "Prevalence",
                                                             "Smoking Prevalence")
  
  Tables.fn[[paste0("ets.", type)]] <- closure.table.cat(riskfactors,
                                                         type,
                                                         ets.yes, 
                                                         0,
                                                         "Prevalence",
                                                         "Environmental Tobacco Smoking")
  
  Tables.fn[[paste0("diabetes.", type)]] <- closure.table.cat(riskfactors,
                                                              type,
                                                              diab.cvd.yes, 
                                                              0,
                                                              "Prevalence",
                                                              "Diabetes Prevalence")
  
  Tables.fn[[paste0("fv.cvd", type)]] <- closure.table.cat(riskfactors,
                                                        type,
                                                        fv.cvd.5+fv.cvd.6+fv.cvd.7+fv.cvd.8, 
                                                        0,
                                                        "Prevalence",
                                                        "Five or more F&V portions a day")
  
  Tables.fn[[paste0("fv.ca.", type)]] <- closure.table.cat(riskfactors,
                                                        type,
                                                        fv.ca.5+fv.ca.6+fv.ca.7+fv.ca.8, 
                                                        0,
                                                        "Prevalence",
                                                        "Five or more F&V portions a day")
  
  
  Tables.fn[[paste0("pa.", type)]] <- closure.table.cat(riskfactors,
                                                        type,
                                                        pa.cvd.5+pa.cvd.6+pa.cvd.7, 
                                                        0,
                                                        "Prevalence",
                                                        "Five or more days of PA per week")
  
  Tables.fn[[paste0("bmi.cvd.", type)]] <- closure.table.cont(riskfactors,
                                                          type,
                                                          bmi.cvd.mean, 
                                                          bmi.cvd.sd,
                                                          0,
                                                          "BMI (kg/m^2)",
                                                          "BMI")
  
  Tables.fn[[paste0("bmi.ca.", type)]] <- closure.table.cont(riskfactors,
                                                          type,
                                                          bmi.ca.mean, 
                                                          bmi.ca.sd,
                                                          0,
                                                          "BMI (kg/m^2)",
                                                          "BMI")
  
  Tables.fn[[paste0("salt.ca.", type)]] <- closure.table.cont(riskfactors,
                                                           type,
                                                           salt.ca.mean, 
                                                           salt.ca.sd,
                                                           0,
                                                           "Salt (g)",
                                                           "Salt (cancer lag)")
  
  Tables.fn[[paste0("salt.cvd.", type)]] <- closure.table.cont(riskfactors,
                                                              type,
                                                              salt.cvd.mean, 
                                                              salt.cvd.sd,
                                                              0,
                                                              "Salt (g)",
                                                              "Salt (cvd lag)")
  
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
  if (type %in% c("S", "SQ")) {
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
  }
  
  if (type == "S") {
    
    Tables.fn[[paste0("le0.SII.", type)]] <- closure.table.le.SII(life.exp0[group == "SQ"],
                                                                  mean)
    
    Tables.fn[[paste0("le0.RII.", type)]] <- closure.table.le.RII(life.exp0[group == "SQ"],
                                                                  mean)
    
    Tables.fn[[paste0("le65.SII.", type)]] <- closure.table.le.SII(life.exp65[group == "SQ"],
                                                                   mean)
    
    Tables.fn[[paste0("le65.RII.", type)]] <- closure.table.le.RII(life.exp65[group == "SQ"],
                                                                   mean)
    
  }
  
  if (is.null(diseasestoexclude) == F) {
    if (type %in% c("S", "SQ")) {
      Tables.fn[[paste0("hle.", type)]] <- closure.table.cont(hlife.exp,
                                                              type,
                                                              mean,
                                                              sd,
                                                              0,
                                                              "Age (years)",
                                                              "Healthy Life Expectancy")
    }
    
    if (type == "S") {
      
      Tables.fn[[paste0("hle.SII.", type)]] <- closure.table.le.SII(hlife.exp[group == "SQ"],
                                                                    mean)
      
      Tables.fn[[paste0("hle.RII.", type)]] <- closure.table.le.RII(hlife.exp[group == "SQ"],
                                                                    mean)
      
    }
  }
  
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
    if (type == "S") {
      Tables.fn[[paste0("chdincid.SII.", type)]] <- closure.table.SII(chd.burden[group=="SQ"],
                                                                      chd.incidence,
                                                                      F)
      
      Tables.fn[[paste0("chdpreval.SII.", type)]] <- closure.table.SII(chd.burden[group=="SQ"],
                                                                       chd.prevalence,
                                                                       F)
      
      Tables.fn[[paste0("chdmortal.SII.", type)]] <- closure.table.SII(chd.burden[group=="SQ"],
                                                                       chd.mortality,
                                                                       F)
      
      Tables.fn[[paste0("chdincid.RII.", type)]] <- closure.table.RII(chd.burden[group=="SQ"],
                                                                      chd.incidence,
                                                                      F)
      
      Tables.fn[[paste0("chdpreval.RII.", type)]] <- closure.table.RII(chd.burden[group=="SQ"],
                                                                       chd.prevalence,
                                                                       F)
      
      Tables.fn[[paste0("chdmortal.RII.", type)]] <- closure.table.RII(chd.burden[group=="SQ"],
                                                                       chd.mortality,
                                                                       F)
      
      Tables.fn[[paste0("chdincid.SII.adj.", type)]] <- closure.table.SII(chd.burden[group=="SAQ"],
                                                                      chd.incidence,
                                                                      T)
      
      Tables.fn[[paste0("chdpreval.SII.adj.", type)]] <- closure.table.SII(chd.burden[group=="SAQ"],
                                                                       chd.prevalence,
                                                                       T)
      
      Tables.fn[[paste0("chdmortal.SII.adj.", type)]] <- closure.table.SII(chd.burden[group=="SAQ"],
                                                                       chd.mortality,
                                                                       T)
      
      Tables.fn[[paste0("chdincid.RII.adj.", type)]] <- closure.table.RII(chd.burden[group=="SAQ"],
                                                                      chd.incidence,
                                                                      T)
      
      Tables.fn[[paste0("chdpreval.RII.adj.", type)]] <- closure.table.RII(chd.burden[group=="SAQ"],
                                                                       chd.prevalence,
                                                                       T)
      
      Tables.fn[[paste0("chdmortal.RII.adj.", type)]] <- closure.table.RII(chd.burden[group=="SAQ"],
                                                                       chd.mortality,
                                                                       T)
    }
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
    if (type == "S") {
      Tables.fn[[paste0("strokeincid.SII.", type)]] <- closure.table.SII(stroke.burden[group=="SQ"],
                                                                         stroke.incidence,
                                                                         F)
      
      Tables.fn[[paste0("strokepreval.SII.", type)]] <- closure.table.SII(stroke.burden[group=="SQ"],
                                                                          stroke.prevalence,
                                                                          F)
      
      Tables.fn[[paste0("strokemortal.SII.", type)]] <- closure.table.SII(stroke.burden[group=="SQ"],
                                                                          stroke.mortality,
                                                                          F)
      
      Tables.fn[[paste0("strokeincid.RII.", type)]] <- closure.table.RII(stroke.burden[group=="SQ"],
                                                                         stroke.incidence,
                                                                         F)
      
      Tables.fn[[paste0("strokepreval.RII.", type)]] <- closure.table.RII(stroke.burden[group=="SQ"],
                                                                          stroke.prevalence,
                                                                          F)
      
      Tables.fn[[paste0("strokemortal.RII.", type)]] <- closure.table.RII(stroke.burden[group=="SQ"],
                                                                          stroke.mortality,
                                                                          F)
      
      Tables.fn[[paste0("strokeincid.SII.adj.", type)]] <- closure.table.SII(stroke.burden[group=="SAQ"],
                                                                         stroke.incidence,
                                                                         T)
      
      Tables.fn[[paste0("strokepreval.SII.adj.", type)]] <- closure.table.SII(stroke.burden[group=="SAQ"],
                                                                          stroke.prevalence,
                                                                          T)
      
      Tables.fn[[paste0("strokemortal.SII.adj.", type)]] <- closure.table.SII(stroke.burden[group=="SAQ"],
                                                                          stroke.mortality,
                                                                          T)
      
      Tables.fn[[paste0("strokeincid.RII.adj.", type)]] <- closure.table.RII(stroke.burden[group=="SAQ"],
                                                                         stroke.incidence,
                                                                         T)
      
      Tables.fn[[paste0("strokepreval.RII.adj.", type)]] <- closure.table.RII(stroke.burden[group=="SAQ"],
                                                                          stroke.prevalence,
                                                                          T)
      
      Tables.fn[[paste0("strokemortal.RII.adj.", type)]] <- closure.table.RII(stroke.burden[group=="SAQ"],
                                                                          stroke.mortality,
                                                                          T)
    }
  }
  
  if ("C16" %in% diseasestoexclude) {
    Tables.fn[[paste0("c16incid.", type)]] <- closure.table.cat(c16.burden,
                                                                type,
                                                                c16.incidence, 
                                                                0,
                                                                "Incidence",
                                                                "Gastric ca Incidence",
                                                                100000)
    
    Tables.fn[[paste0("c16preval.", type)]] <- closure.table.cat(c16.burden,
                                                                 type,
                                                                 c16.prevalence, 
                                                                 0,
                                                                 "Prevalence",
                                                                 "Gastric ca Prevalence",
                                                                 100000)
    
    Tables.fn[[paste0("c16mortal.", type)]] <- closure.table.cat(c16.burden,
                                                                 type,
                                                                 c16.mortality, 
                                                                 0,
                                                                 "Mortality",
                                                                 "Gastric ca Mortality",
                                                                 100000)
    if (type == "S") {
      Tables.fn[[paste0("c16incid.SII.", type)]] <- closure.table.SII(c16.burden[group=="SQ"],
                                                                      c16.incidence,
                                                                      F)
      
      Tables.fn[[paste0("c16preval.SII.", type)]] <- closure.table.SII(c16.burden[group=="SQ"],
                                                                       c16.prevalence,
                                                                       F)
      
      Tables.fn[[paste0("c16mortal.SII.", type)]] <- closure.table.SII(c16.burden[group=="SQ"],
                                                                       c16.mortality,
                                                                       F)
      
      Tables.fn[[paste0("c16incid.RII.", type)]] <- closure.table.RII(c16.burden[group=="SQ"],
                                                                      c16.incidence,
                                                                      F)
      
      Tables.fn[[paste0("c16preval.RII.", type)]] <- closure.table.RII(c16.burden[group=="SQ"],
                                                                       c16.prevalence,
                                                                       F)
      
      Tables.fn[[paste0("c16mortal.RII.", type)]] <- closure.table.RII(c16.burden[group=="SQ"],
                                                                       c16.mortality,
                                                                       F)
      
      Tables.fn[[paste0("c16incid.SII.adj.", type)]] <- closure.table.SII(c16.burden[group=="SAQ"],
                                                                          c16.incidence,
                                                                          T)
      
      Tables.fn[[paste0("c16preval.SII.adj.", type)]] <- closure.table.SII(c16.burden[group=="SAQ"],
                                                                           c16.prevalence,
                                                                           T)
      
      Tables.fn[[paste0("c16mortal.SII.adj.", type)]] <- closure.table.SII(c16.burden[group=="SAQ"],
                                                                           c16.mortality,
                                                                           T)
      
      Tables.fn[[paste0("c16incid.RII.adj.", type)]] <- closure.table.RII(c16.burden[group=="SAQ"],
                                                                          c16.incidence,
                                                                          T)
      
      Tables.fn[[paste0("c16preval.RII.adj.", type)]] <- closure.table.RII(c16.burden[group=="SAQ"],
                                                                           c16.prevalence,
                                                                           T)
      
      Tables.fn[[paste0("c16mortal.RII.adj.", type)]] <- closure.table.RII(c16.burden[group=="SAQ"],
                                                                           c16.mortality,
                                                                           T)
    }
  }
}

GraphsfromTables <- cmpfun(function(x) {
  pd <- position_dodge(.7) 
  dt <- get(x, pos = .GlobalEnv$Tables)
  age.lim <- T
  
  if (grepl(glob2rx("*.SA*"), x)) {
    dt <- dt[agegroup %in% levels(agegroup.fn(20:100)[]),]
  } 
  
  if (grepl(glob2rx("smoking*"), x)) {
    title <- "Smoking prevalence"
    yaxis <- "Prevalence"
    yscale <- 1
    pct <- T
  } 
  if (grepl(glob2rx("ets*"), x)) {
    title <- "Environmental tobacco smoking prevalence\n"
    yaxis <- "Prevalence"
    yscale <- 1
    pct <- T
  } 
  if (grepl(glob2rx("diabetes*"), x)) {
    title <- "Diabetes prevalence"
    yaxis <- "Prevalence"
    yscale <- 1
    pct <- T
  } 
  if (grepl(glob2rx("fv*"), x)) {
    title <- "Five or more F&V portions a day"
    yaxis <- "Prevalence"
    yscale <- 1
    pct <- T
  } 
  if (grepl(glob2rx("pa*"), x)) {
    title <- "Five or more active days per week"
    yaxis <- "Prevalence"
    yscale <- 1
    pct <- T
  } 
  if (grepl(glob2rx("bmi*"), x)) {
    title <- "Mean body mass index"
    yaxis <- expression(paste("Body mass index (Kg/", m^bold("2"), ")"))
    yscale <- 1
    pct <- F
  } 
  if (grepl(glob2rx("tc*"), x)) {
    title <- "Mean plasma total cholesterol"
    yaxis <- "Total cholestrol (mmol/l)"
    yscale <- 1
    pct <- F
  } 
  if (grepl(glob2rx("salt*"), x)) {
    title <- "Salt consumption"
    yaxis <- "Salt consumption (g)"
    yscale <- 1
    pct <- F
  } 
  if (grepl(glob2rx("sbp*"), x)) {
    title <- "Mean systolic blood pressure"
    yaxis <- "Systolic blood pressure (mmHg)"
    yscale <- 1
    pct <- F
  } 
  if (grepl(glob2rx("le0.S"), x)) {
    title <- "Life expectancy at birth"
    yaxis <- "Age (years)"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("le65.S"), x)) {
    title <- "Life expectancy at 65"
    yaxis <- "Age (years)"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("le0.SII.S"), x)) {
    title <- "Absolute inequality in\nlife expectancy at birth"
    yaxis <- "Slope index of inequality (years)"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("le0.RII.S"), x)) {
    title <- "Relative inequality in\nlife expectancy at birth"
    yaxis <- "Relative index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("le65.SII.S"), x)) {
    title <- "Absolute inequality in\nlife expectancy at 65"
    yaxis <- "Slope index of inequality (years)"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("le65.RII.S"), x)) {
    title <- "Relative inequality in\nlife expectancy at 65"
    yaxis <- "Relative index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("hle.S"), x)) {
    title <- "Healthy life expectancy at birth"
    yaxis <- "Age (years)"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("hle.SII.S"), x)) {
    title <- "Absolute inequality in\nhealthy life expectancy at birth"
    yaxis <- "Slope index of inequality (years)"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("hle.RII.S"), x)) {
    title <- "Relative inequality in\nhealthy life expectancy at birth"
    yaxis <- "Relative index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("chdincid.*"), x)) {
    title <- "CHD incidence"
    yaxis <- "Incidence"
    yscale <- 100000
    pct <- F
    age.lim <- T
  } 
  if (grepl(glob2rx("chdpreval.*"), x)) {
    title <- "CHD prevalence"
    yaxis <- "Prevalence"
    yscale <- 100000
    pct <- F
    age.lim <- T
  } 
  if (grepl(glob2rx("chdmortal.*"), x)) {
    title <- "CHD mortality"
    yaxis <- "Mortality"
    yscale <- 100000
    pct <- F
    age.lim <- T
  } 
  if (grepl(glob2rx("chdincid.SII.S*"), x)) {
    title <- "Absolute inequality in CHD incidence"
    yaxis <- "Slope index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("chdpreval.SII.S*"), x)) {
    title <- "Absolute inequality in CHD prevalence"
    yaxis <- "Slope index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("chdmortal.SII.S*"), x)) {
    title <- "Absolute inequality in CHD mortality"
    yaxis <- "Slope index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("chdincid.RII.S*"), x)) {
    title <- "Relative inequality in CHD incidence"
    yaxis <- "Relative  index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("chdpreval.RII.S*"), x)) {
    title <- "Relative inequality in CHD prevalence"
    yaxis <- "Relative index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("chdmortal.RII.S*"), x)) {
    title <- "Relative inequality in CHD mortality"
    yaxis <- "Relative index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("chdincid.SII.a*"), x)) {
    title <- "Absolute inequality in CHD incidence (age adjusted)"
    yaxis <- "Slope index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("chdpreval.SII.a*"), x)) {
    title <- "Absolute inequality in CHD prevalence (age adjusted)"
    yaxis <- "Slope index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("chdmortal.SII.a*"), x)) {
    title <- "Absolute inequality in CHD mortality (age adjusted)"
    yaxis <- "Slope index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("chdincid.RII.a*"), x)) {
    title <- "Relative inequality in CHD incidence (age adjusted)"
    yaxis <- "Relative index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("chdpreval.RII.a*"), x)) {
    title <- "Relative inequality in CHD prevalence (age adjusted)"
    yaxis <- "Relative index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("chdmortal.RII.a*"), x)) {
    title <- "Relative inequality in CHD mortality (age adjusted)"
    yaxis <- "Relative index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("strokeincid.*"), x)) {
    title <- "Stroke incidence"
    yaxis <- "Incidence"
    yscale <- 100000
    pct <- F
    age.lim <- T
  } 
  if (grepl(glob2rx("strokepreval.*"), x)) {
    title <- "Stroke prevalence"
    yaxis <- "Prevalence"
    yscale <- 100000
    pct <- F
    age.lim <- T
  } 
  if (grepl(glob2rx("strokemortal.*"), x)) {
    title <- "Stroke mortality"
    yaxis <- "Mortality"
    yscale <- 100000
    pct <- F
    age.lim <- T
  } 
  if (grepl(glob2rx("strokeincid.SII.S*"), x)) {
    title <- "Absolute inequality in stroke incidence"
    yaxis <- "Slope index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("strokepreval.SII.S*"), x)) {
    title <- "Absolute inequality in stroke prevalence"
    yaxis <- "Slope index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("strokemortal.SII.S*"), x)) {
    title <- "Absolute inequality in stroke mortality"
    yaxis <- "Slope index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("strokeincid.RII.S*"), x)) {
    title <- "Relative inequality in stroke incidence"
    yaxis <- "Relative index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("strokepreval.RII.S*"), x)) {
    title <- "Relative inequality in stroke prevalence"
    yaxis <- "Relative index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("strokemortal.RII.S*"), x)) {
    title <- "Relative inequality in stroke mortality"
    yaxis <- "Relative index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("strokeincid.SII.a*"), x)) {
    title <- "Absolute inequality in stroke incidence (age adjusted)"
    yaxis <- "Slope index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("strokepreval.SII.a*"), x)) {
    title <- "Absolute inequality in stroke prevalence (age adjusted)"
    yaxis <- "Slope index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("strokemortal.SII.a*"), x)) {
    title <- "Absolute inequality in stroke mortality (age adjusted)"
    yaxis <- "Slope index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("strokeincid.RII.a*"), x)) {
    title <- "Relative inequality in stroke incidence (age adjusted)"
    yaxis <- "Relative index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("strokepreval.RII.a*"), x)) {
    title <- "Relative inequality in stroke prevalence (age adjusted)"
    yaxis <- "Relative index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("strokemortal.RII.a*"), x)) {
    title <- "Relative inequality in stroke mortality (age adjusted)"
    yaxis <- "Relative index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("c16incid.*"), x)) {
    title <- "Gastric ca incidence"
    yaxis <- "Incidence"
    yscale <- 100000
    pct <- F
    age.lim <- T
  } 
  if (grepl(glob2rx("c16preval.*"), x)) {
    title <- "Gastric ca prevalence"
    yaxis <- "Prevalence"
    yscale <- 100000
    pct <- F
    age.lim <- T
  } 
  if (grepl(glob2rx("c16mortal.*"), x)) {
    title <- "Gastric ca mortality"
    yaxis <- "Mortality"
    yscale <- 100000
    pct <- F
    age.lim <- T
  } 
  if (grepl(glob2rx("c16incid.SII.S*"), x)) {
    title <- "Absolute inequality in gastric ca incidence"
    yaxis <- "Slope index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("c16preval.SII.S*"), x)) {
    title <- "Absolute inequality in gastric ca prevalence"
    yaxis <- "Slope index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("c16mortal.SII.S*"), x)) {
    title <- "Absolute inequality in gastric ca mortality"
    yaxis <- "Slope index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("c16incid.RII.S*"), x)) {
    title <- "Relative inequality in gastric ca incidence"
    yaxis <- "Relative  index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("c16preval.RII.S*"), x)) {
    title <- "Relative inequality in gastric ca prevalence"
    yaxis <- "Relative index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("c16mortal.RII.S*"), x)) {
    title <- "Relative inequality in gastric ca mortality"
    yaxis <- "Relative index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("c16incid.SII.a*"), x)) {
    title <- "Absolute inequality in gastric ca incidence (age adjusted)"
    yaxis <- "Slope index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("c16preval.SII.a*"), x)) {
    title <- "Absolute inequality in gastric ca prevalence (age adjusted)"
    yaxis <- "Slope index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("c16mortal.SII.a*"), x)) {
    title <- "Absolute inequality in gastric ca mortality (age adjusted)"
    yaxis <- "Slope index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("c16incid.RII.a*"), x)) {
    title <- "Relative inequality in gastric ca incidence (age adjusted)"
    yaxis <- "Relative index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("c16preval.RII.a*"), x)) {
    title <- "Relative inequality in gastric ca prevalence (age adjusted)"
    yaxis <- "Relative index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("c16mortal.RII.a*"), x)) {
    title <- "Relative inequality in gastric ca mortality (age adjusted)"
    yaxis <- "Relative index of inequality"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("le0.SQ"), x)) {
    title <- "Life expectancy at birth"
    yaxis <- "Age (years)"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("le65.SQ"), x)) {
    title <- "Life expectancy at 65"
    yaxis <- "Age (years)"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  if (grepl(glob2rx("hle.SQ"), x)) {
    title <- "Healthy life expectancy at birth"
    yaxis <- "Age (years)"
    yscale <- 1
    pct <- F
    age.lim <- F
  } 
  
  if (length(grep("mean", names(dt)))>0 & length(grep(glob2rx("*le*.*"), x)) == 0) {
    dt[, `:=` (mean = yscale * mean,
               lui = yscale * lui,
               uui = yscale * uui)]
  }
  if (length(grep("SII", names(dt))) >0){
    setnames(dt, "SII", "mean")
  }
  if (length(grep("RII", names(dt))) >0) {
    setnames(dt, "RII", "mean")
  }
  
  if (grepl(glob2rx("*.SA*"), x)) {
    dt <- dt[is.na(mean) == F,]
  } 
  
  g <- ggplot(dt,
              aes(x = year,
                  y = mean, 
                  colour = scenario)) + 
    geom_errorbar(aes(ymin= lui, ymax = uui),
                  width = .05,
                  position = pd,
                  alpha = 3/5) +
    #geom_line(position = pd, size = 0.5, alpha = 4/4, se = F) +
    geom_point(position = pd, size = 1, alpha = 4/5) +
    geom_smooth(position = pd, size = 0.5, alpha = 4/4, se = F, method="loess", span = 0.4) +
    #geom_point(size = 2, stat = "identity") +
    ylab(ifelse(yscale == 1, yaxis, paste0(yaxis, " per ", format(yscale, scientific = F)))) +
    scale_x_continuous(name="Year") +
    theme(axis.text.x  = element_text(angle=90, vjust=0.5))
  if (pct == T) g <- g +  scale_y_continuous(labels = percent_format())
  
  if (grepl(glob2rx("*.P"), x)) {
    if (age.lim == F) {
      g <- g + ggtitle(paste0(title)) 
    } else {
      g <- g + ggtitle(paste0(title, " (ages ", ageL, " - ", ageH, ")")) 
      
    }
  }
  if (grepl(glob2rx("*.S"), x)) {
    if (age.lim == F) {
      g <- g + facet_grid(sex ~ .) + ggtitle(paste0(title, " by sex")) 
    } else {
      g <- g + facet_grid(sex ~ .) + ggtitle(paste0(title, " by sex (ages ", ageL, " - ", ageH, ")")) 
      
    }
  }
  
  if (grepl(glob2rx("*.SQ"), x)) {
    if (age.lim == F) {
      g <- g + facet_grid(sex ~ qimd) + ggtitle(paste0(title, " by sex and QIMD"))
    } else {
      g <- g + facet_grid(sex ~ qimd) + ggtitle(paste0(title, "\nby sex and QIMD (ages ", ageL, " - ", ageH, ")"))
    }
  }
  if (grepl(glob2rx("*.SA"), x)) g <- g + facet_grid(sex ~ agegroup) + ggtitle(paste0(title, " by sex and age group"))
  if (grepl(glob2rx("*.SAQ"), x)) g <- g + facet_grid(sex + qimd ~ agegroup)+ ggtitle(paste0(title, " by sex, age group and QIMD"))
  return(g)
}
)

# delete empty functions
Graphs.fn[sapply(Graphs.fn, function(x) length(formals(x)) == 0 && length(body(x)) == 1)] <- NULL
Tables.fn[sapply(Tables.fn, function(x) length(formals(x)) == 0 && length(body(x)) == 1)] <- NULL

