require("ggplot2")
require("binom")

# Define function to summarise output RF
MC.mean <- function(m, sd, n, lim = Inf, ...) {
  sd <- ifelse(is.na(sd), 2 * m, sd) # if mean of 0ne value then sd is technically NA. This is an assumption sd =2*m
  m <- rtruncnorm(n, 0, lim, m, sd)
  return(list(mean = mean(m, na.rm = T),
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

cutoff <- 500


direct.standard[, who.pop := who.pop / 100] # per 1
direct.standard[, esp.pop := esp.pop / 100000] # per 1
setkey(direct.standard, agegroup)

age.limit <- levels(factor((agegroup.fn(ageL:ageH)))) 

# create a closure to produce the functions for graphs
closure.table.cat <- function (dt, type, rf, ...) {
  rf <- substitute(rf)
  grp <- c("year", "scenario")
  dt2 <- NULL
  
  if (type == "S") grp <- c(grp, "sex")
  if (type == "SQ") grp <- c(grp, "sex", "qimd")
  if (type == "SA") grp <- c(grp, "sex", "agegroup")
  if (type == "SAQ") grp <- c(grp, "sex", "agegroup", "qimd")
  
  function() {
    
    
    if ("pop" %in% names(dt)) {
      dt <-   dt[pop > cutoff, ]
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
  
  if (type == "S") grp <- c(grp, "sex")
  if (type == "SQ") grp <- c(grp, "sex", "qimd")
  if (type == "SA") grp <- c(grp, "sex", "agegroup")
  if (type == "SAQ") grp <- c(grp, "sex", "agegroup", "qimd")
  
  function() {
    
    if ("pop" %in% names(dt)) {
      dt <-   dt[pop > cutoff, ]
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
  
  if (type == "S") grp <- c(grp, "sex")
  if (type == "SQ") grp <- c(grp, "sex", "qimd")
  if (type == "SA") grp <- c(grp, "sex", "agegroup")
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
      geom_point(size = 1, stat = "identity") +
      ylab(ifelse(yscale == 1, yaxis, paste0(yaxis, " per ", format(yscale, scientific = F)))) +
      scale_x_continuous(name="Year") +
      theme(axis.text.x  = element_text(angle=90, vjust=0.5))
    
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
      geom_point(size = 1, stat = "identity") +
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
  
  if (type == "S") grp <- c(grp, "sex")
  if (type == "SQ") grp <- c(grp, "sex", "qimd")
  if (type == "SA") grp <- c(grp, "sex", "agegroup")
  if (type == "SAQ") grp <- c(grp, "sex", "agegroup", "qimd")
  
  function() {
    if ("pop" %in% names(dt)) {
      dt <-   dt[pop > cutoff, ]
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
      geom_point(size = 1, stat = "identity") +
      ylab(yaxis) +
      scale_x_continuous(name="Year") +
      theme(axis.text.x  = element_text(angle=90, vjust=0.5))
    
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
      geom_point(size = 1, stat = "identity") +
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

closure.table.SII <- function(dt, indicator, ...){
  indicator <- substitute(indicator)
  function() {
    setkey(dt, scenario, mc, sex, year, agegroup, qimd)
    dt[, qimd2 := cumsum(pop)/sum(pop),
       by= .(sex, year, scenario, mc, agegroup)]
    dt[, qimd2 := (qimd2 + c(0, qimd2[seq_len(.N-1)]))/2,
       by= .(sex, year, scenario, mc, agegroup)]
    
    dt <- dt[,{
      SII <- predict(
        glm(
          cbind(eval(indicator), pop-eval(indicator)) ~ qimd2, 
          family="binomial"(link="identity")),
        data.frame(qimd2 = c(0, 1)),
        type = "response",
        se = T)
      SII <- (rnorm(1000, SII[[1]][[2]],SII[[2]][[2]]) -
                rnorm(1000,SII[[1]][[1]],SII[[2]][[1]])) * 100000
      list(SII = mean(SII, na.rm = T),
           lui = quantile(SII, probs = 0.025, na.rm = T),
           uui = quantile(SII, probs = 0.975, na.rm = T))
    },
    by = .(sex, scenario, year)
    ]
    return(dt)
  }
}

# Kunst and Mackenbach method/definition
closure.table.RII <- function(dt, indicator, ...){
  indicator <- substitute(indicator)
  function() {
    setkey(dt, scenario, mc, sex, year, agegroup, qimd)
    dt[, qimd2 := cumsum(pop)/sum(pop),
       by= .(sex, year, scenario, mc, agegroup)]
    dt[, qimd2 := (qimd2 + c(0, qimd2[seq_len(.N-1)]))/2,
       by= .(sex, year, scenario, mc, agegroup)]
    
    dt <- dt[,{
      RII <- predict(
        glm(
          cbind(eval(indicator), pop-eval(indicator)) ~ qimd2, 
          family="binomial"(link="logit")),
        data.frame(qimd2 = c(0, 1)),
        type = "response",
        se = T)
      RII <- (rnorm(1000, RII[[1]][[2]],RII[[2]][[2]]) /
                rnorm(1000,RII[[1]][[1]],RII[[2]][[1]]))
      list(RII = mean(RII, na.rm = T),
           lui = quantile(RII, probs = 0.025, na.rm = T),
           uui = quantile(RII, probs = 0.975, na.rm = T))
    },
    by = .(sex, scenario, year)
    ]
    return(dt)
  }
}

closure.table.le.SII <- function(dt, indicator, ...){
  indicator <- substitute(indicator)
  function() {
    setkey(dt, scenario, mc, sex, year, qimd)
    dt[, qimd2:= cumsum(pop)/sum(pop), by= .(sex, year, scenario, mc)]
    dt[, qimd2 := (qimd2 + c(0, qimd2[seq_len(.N-1)]))/2, by= .(sex, year, scenario, mc)]
    
    dt <- dt[,{
      SII <- predict(
        glm(
          eval(indicator) ~ qimd2, 
          weights = pop,
          family="gaussian"(link="identity")),
        data.frame(qimd2 = c(0, 1)),
        type = "response",
        se = T)
      SII <- (rnorm(1000, SII[[1]][[2]], SII[[2]][[2]]) -
                rnorm(1000, SII[[1]][[1]], SII[[2]][[1]]))
      list(SII = mean(SII, na.rm = T),
           lui = quantile(SII, probs = 0.025, na.rm = T),
           uui = quantile(SII, probs = 0.975, na.rm = T))
    },
    by = .(sex, scenario, year)
    ]
    return(dt)
  }
}

closure.graph.SII <- function(dt, indicator, title, ...){
  indicator <- substitute(indicator)
  function() {
    setkey(dt, scenario, mc, sex, year, agegroup, qimd)
    dt[, qimd2:= cumsum(pop)/sum(pop), by= .(sex, year, scenario, mc, agegroup)]
    dt[, qimd2 := (qimd2 + c(0, qimd2[seq_len(.N-1)]))/2, by= .(sex, year, scenario, mc, agegroup)]
    
    dt <- dt[,{
      SII <- predict(
        glm(
          cbind(eval(indicator), pop-eval(indicator)) ~ qimd2,
          family="binomial"(link="identity")),
        data.frame(qimd2 = c(0, 1)),
        type = "response",
        se = T)
      SII <- (rnorm(1000, SII[[1]][[2]],SII[[2]][[2]]) -
                rnorm(1000,SII[[1]][[1]],SII[[2]][[1]])) * 100000
      list(SII = mean(SII, na.rm = T),
           lui = quantile(SII, probs = 0.025, na.rm = T),
           uui = quantile(SII, probs = 0.975, na.rm = T))
    },
    by = .(sex, scenario, year)
    ]
    
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
      geom_point(size = 1, stat = "identity") +
      ylab("Slop Index of Inequality per 100000")+
      scale_x_continuous(name="Year") +
      theme(axis.text.x  = element_text(angle=90, vjust=0.5)) +
      facet_grid(sex ~ .) + 
      ggtitle(paste0("Absolute socioeconomic inequality of ", title, "\nby sex (ages ", ageL, " - ", ageH, ")"))
    return(g)
  }
}

closure.graph.RII <- function(dt, indicator, title, ...){
  indicator <- substitute(indicator)
  function() {
    setkey(dt, scenario, mc, sex, year, agegroup, qimd)
    dt[, qimd2:= cumsum(pop)/sum(pop), by= .(sex, year, scenario, mc, agegroup)]
    dt[, qimd2 := (qimd2 + c(0, qimd2[seq_len(.N-1)]))/2, by= .(sex, year, scenario, mc, agegroup)]
    
    dt <- dt[,{
      RII <- predict(
        glm(
          cbind(eval(indicator), pop-eval(indicator)) ~ qimd2,
          family="binomial"(link="logit")),
        data.frame(qimd2 = c(0, 1)),
        type = "response",
        se = T)
      RII <- (rnorm(1000, RII[[1]][[2]], RII[[2]][[2]]) /
                rnorm(1000, RII[[1]][[1]], RII[[2]][[1]]))
      list(RII = mean(RII, na.rm = T),
           lui = quantile(RII, probs = 0.025, na.rm = T),
           uui = quantile(RII, probs = 0.975, na.rm = T))
    },
    by = .(sex, scenario, year)
    ]
    
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
      geom_point(size = 1, stat = "identity") +
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
    dt[, qimd2:= cumsum(pop)/sum(pop), by= .(sex, year, scenario, mc)]
    dt[, qimd2 := (qimd2 + c(0, qimd2[seq_len(.N-1)]))/2, by= .(sex, year, scenario, mc)]
    
    dt <- dt[,{
      RII <- predict(
        glm(
          eval(indicator) ~ qimd2, 
          weights = pop,
          family="gaussian"(link="log")),
        data.frame(qimd2 = c(0, 1)),
        type = "response",
        se = T)
      RII <- (rnorm(1000, RII[[1]][[2]], RII[[2]][[2]]) /
                rnorm(1000, RII[[1]][[1]], RII[[2]][[1]]))
      list(RII = mean(RII, na.rm = T),
           lui = quantile(RII, probs = 0.025, na.rm = T),
           uui = quantile(RII, probs = 0.975, na.rm = T))
    },
    by = .(sex, scenario, year)
    ]
    
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
      geom_point(size = 1, stat = "identity") +
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
    dt[, qimd2:= cumsum(pop)/sum(pop), by= .(sex, year, scenario, mc)]
    dt[, qimd2 := (qimd2 + c(0, qimd2[seq_len(.N-1)]))/2, by= .(sex, year, scenario, mc)]
    
    dt <- dt[,{
      RII <- predict(
        glm(
          eval(indicator) ~ qimd2, 
          weights = pop,
          family="gaussian"(link="log")),
        data.frame(qimd2 = c(0, 1)),
        type = "response",
        se = T)
      RII <- (rnorm(1000, RII[[1]][[2]], RII[[2]][[2]]) /
                rnorm(1000, RII[[1]][[1]], RII[[2]][[1]]))
      list(RII = mean(RII, na.rm = T),
           lui = quantile(RII, probs = 0.025, na.rm = T),
           uui = quantile(RII, probs = 0.975, na.rm = T))
    },
    by = .(sex, scenario, year)
    ]
    
    return(dt)
  }
}

closure.graph.le.SII <- function(dt, indicator, title, ...){
  indicator <- substitute(indicator)
  function() {
    setkey(dt, scenario, mc, sex, year, qimd)
    dt[, qimd2:= cumsum(pop)/sum(pop), by= .(sex, year, scenario, mc)]
    dt[, qimd2 := (qimd2 + c(0, qimd2[seq_len(.N-1)]))/2, by= .(sex, year, scenario, mc)]
    
    dt <- dt[,{
      SII <- predict(
        glm(
          eval(indicator) ~ qimd2, 
          weights = pop,
          family="gaussian"(link="identity")),
        data.frame(qimd2 = c(0, 1)),
        type = "response",
        se = T)
      SII <- (rnorm(1000, SII[[1]][[2]], SII[[2]][[2]]) -
                rnorm(1000, SII[[1]][[1]], SII[[2]][[1]]))
      list(SII = mean(SII, na.rm = T),
           lui = quantile(SII, probs = 0.025, na.rm = T),
           uui = quantile(SII, probs = 0.975, na.rm = T))
    },
    by = .(sex, scenario, year)
    ]
    
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
      geom_point(size = 1, stat = "identity") +
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
for (type in c("S", "SQ", "SA", "SAQ")) {
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
                                                         ets.1, 
                                                         cvd.lag,
                                                         "Prevalence",
                                                         "Environmental tobacco smoking")
  
  Graphs.fn[[paste0("ets.", type, ".WHO" )]] <- closure.graph.std(riskfactors,
                                                                  type,
                                                                  ets.1, 
                                                                  cvd.lag,
                                                                  "Prevalence",
                                                                  "Environmental tobacco smoking",
                                                                  1,
                                                                  "WHO")
  
  Graphs.fn[[paste0("ets.", type, ".ESP" )]] <- closure.graph.std(riskfactors,
                                                                  type,
                                                                  ets.1, 
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
  
  Graphs.fn[[paste0("fv.", type)]] <- closure.graph.cat(riskfactors,
                                                        type,
                                                        fv.cvd.6+fv.cvd.7+fv.cvd.8+fv.cvd.9, 
                                                        cvd.lag,
                                                        "Prevalence",
                                                        "Five or more F&V portions a day")
  
  Graphs.fn[[paste0("fv.", type, ".WHO" )]] <- closure.graph.std(riskfactors,
                                                                 type,
                                                                 fv.cvd.6+fv.cvd.7+fv.cvd.8+fv.cvd.9, 
                                                                 cvd.lag,
                                                                 "Prevalence",
                                                                 "Five or more F&V portions a day",
                                                                 1,
                                                                 "WHO")
  
  Graphs.fn[[paste0("fv.", type, ".ESP" )]] <- closure.graph.std(riskfactors,
                                                                 type,
                                                                 fv.cvd.6+fv.cvd.7+fv.cvd.8+fv.cvd.9, 
                                                                 cvd.lag,
                                                                 "Prevalence",
                                                                 "Five or more F&V portions a day",
                                                                 1,
                                                                 "European")
  
  Graphs.fn[[paste0("bmi.", type)]] <- closure.graph.cont(riskfactors,
                                                          type,
                                                          bmi.cvd.mean, 
                                                          bmi.cvd.sd,
                                                          cvd.lag,
                                                          "Body mass index (kg/m^2)",
                                                          "Body mass index")
  
  Graphs.fn[[paste0("bmi.", type, ".WHO" )]] <- closure.graph.std.cont(riskfactors,
                                                                       type,
                                                                       bmi.cvd.mean, 
                                                                       cvd.lag,
                                                                       "Body mass index (kg/m^2)",
                                                                       "Body mass index",
                                                                       "WHO")
  
  Graphs.fn[[paste0("bmi.", type, ".ESP" )]] <- closure.graph.std.cont(riskfactors,
                                                                       type,
                                                                       bmi.cvd.mean, 
                                                                       cvd.lag,
                                                                       "Body mass index (kg/m^2)",
                                                                       "Body mass index",
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
                                                                  "Life expectancy at birth")
    
    Graphs.fn[[paste0("le0.RII.", type)]] <- closure.graph.le.RII(life.exp0[group == "SQ"],
                                                                  mean,
                                                                  "Life expectancy at birth")
    
    Graphs.fn[[paste0("le65.SII.", type)]] <- closure.graph.le.SII(life.exp65[group == "SQ"],
                                                                   mean,
                                                                   "Life expectancy at 65")
    
    Graphs.fn[[paste0("le65.RII.", type)]] <- closure.graph.le.RII(life.exp65[group == "SQ"],
                                                                   mean,
                                                                   "Life expectancy at 65")
    
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
                                                                    "Healthy life expectancy at birth")
      
      Graphs.fn[[paste0("hle.RII.", type)]] <- closure.graph.le.RII(hlife.exp[group == "SQ"],
                                                                    mean,
                                                                    "Healthy life xpectancy at birth")
      
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
      Graphs.fn[[paste0("chdincid.SII.", type)]] <- closure.graph.SII(chd.burden[group=="SAQ"],
                                                                      chd.incidence,
                                                                      "CHD incidence")
      
      Graphs.fn[[paste0("chdpreval.SII.", type)]] <- closure.graph.SII(chd.burden[group=="SAQ"],
                                                                       chd.prevalence,
                                                                       "CHD prevalence")
      
      Graphs.fn[[paste0("chdmortal.SII.", type)]] <- closure.graph.SII(chd.burden[group=="SAQ"],
                                                                       chd.mortality,
                                                                       "CHD mortality")
      
      Graphs.fn[[paste0("chdincid.RII.", type)]] <- closure.graph.RII(chd.burden[group=="SAQ"],
                                                                      chd.incidence,
                                                                      "CHD incidence")
      
      Graphs.fn[[paste0("chdpreval.RII.", type)]] <- closure.graph.RII(chd.burden[group=="SAQ"],
                                                                       chd.prevalence,
                                                                       "CHD prevalence")
      
      Graphs.fn[[paste0("chdmortal.RII.", type)]] <- closure.graph.RII(chd.burden[group=="SAQ"],
                                                                       chd.mortality,
                                                                       "CHD mortality")
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
      
      Graphs.fn[[paste0("strokeincid.SII.", type)]] <- closure.graph.SII(stroke.burden[group=="SAQ"],
                                                                         stroke.incidence,
                                                                         "stroke incidence")
      
      Graphs.fn[[paste0("strokepreval.SII.", type)]] <- closure.graph.SII(stroke.burden[group=="SAQ"],
                                                                          stroke.prevalence,
                                                                          "stroke prevalence")
      
      Graphs.fn[[paste0("strokemortal.SII.", type)]] <- closure.graph.SII(stroke.burden[group=="SAQ"],
                                                                          stroke.mortality,
                                                                          "stroke mortality")
      
      Graphs.fn[[paste0("strokeincid.RII.", type)]] <- closure.graph.RII(stroke.burden[group=="SAQ"],
                                                                         stroke.incidence,
                                                                         "stroke incidence")
      
      Graphs.fn[[paste0("strokepreval.RII.", type)]] <- closure.graph.RII(stroke.burden[group=="SAQ"],
                                                                          stroke.prevalence,
                                                                          "stroke prevalence")
      
      Graphs.fn[[paste0("strokemortal.RII.", type)]] <- closure.graph.RII(stroke.burden[group=="SAQ"],
                                                                          stroke.mortality,
                                                                          "stroke mortality")
    }
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
      Tables.fn[[paste0("chdincid.SII.", type)]] <- closure.table.SII(chd.burden[group=="SAQ"],
                                                                      chd.incidence)
      
      Tables.fn[[paste0("chdpreval.SII.", type)]] <- closure.table.SII(chd.burden[group=="SAQ"],
                                                                       chd.prevalence)
      
      Tables.fn[[paste0("chdmortal.SII.", type)]] <- closure.table.SII(chd.burden[group=="SAQ"],
                                                                       chd.mortality)
      
      Tables.fn[[paste0("chdincid.RII.", type)]] <- closure.table.RII(chd.burden[group=="SAQ"],
                                                                      chd.incidence)
      
      Tables.fn[[paste0("chdpreval.RII.", type)]] <- closure.table.RII(chd.burden[group=="SAQ"],
                                                                       chd.prevalence)
      
      Tables.fn[[paste0("chdmortal.RII.", type)]] <- closure.table.RII(chd.burden[group=="SAQ"],
                                                                       chd.mortality)
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
      Tables.fn[[paste0("strokeincid.SII.", type)]] <- closure.table.SII(stroke.burden[group=="SAQ"],
                                                                         stroke.incidence)
      
      Tables.fn[[paste0("strokepreval.SII.", type)]] <- closure.table.SII(stroke.burden[group=="SAQ"],
                                                                          stroke.prevalence)
      
      Tables.fn[[paste0("strokemortal.SII.", type)]] <- closure.table.SII(stroke.burden[group=="SAQ"],
                                                                          stroke.mortality)
      
      Tables.fn[[paste0("strokeincid.RII.", type)]] <- closure.table.RII(stroke.burden[group=="SAQ"],
                                                                         stroke.incidence)
      
      Tables.fn[[paste0("strokepreval.RII.", type)]] <- closure.table.RII(stroke.burden[group=="SAQ"],
                                                                          stroke.prevalence)
      
      Tables.fn[[paste0("strokemortal.RII.", type)]] <- closure.table.RII(stroke.burden[group=="SAQ"],
                                                                          stroke.mortality)
    }
  }
  
}


