require("data.table")
require("ggplot2")
require("binom")
source(file = "./post simulation functions.R")

# riskfactors <- fread("./output/RF/riskfactors.csv")
# life.exp <- fread("./output/Other/life.exp.csv")
# chd.burden <- fread("./output/CHD/chd.burden.csv")
# healthylife.exp <- fread("./output/CHD/healthylife.exp.csv")

load("./Output/RF/riskfactors.RData")
load("./Output/Other/life.exp.RData")
load("./Output/CHD/chd.burden.RData")
load("./Output/CHD/healthylife.exp.RData")
load("./Output/Stroke/stroke.burden.RData")
load("./Output/Stroke/healthylife.exp.RData")
load("./Output/Graphs/Graphs.rda")
load("./Output/Tables/Tables.rda")

pd <- position_dodge(.3) 



# Healthy Life Expectancy (CHD)
healthylife.exp[,.(mean=mean(age), sd=sd(age)), by=.(sex, chd.incidence, scenario, mc)][, MC.mean(mean,sd,.N), by=.(sex, chd.incidence, scenario)]

Graphs$hle.s <- ggplot(healthylife.exp[,mean_se(age), by=.(sex, chd.incidence, scenario)],
            aes(x=chd.incidence, y=y, colour=scenario, ymax=max(y)*1.05, ymin = 70)) + 
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ .) +
    ylab("Age (years)") + scale_x_continuous(name="Year") + 
    ggtitle("Healthy Life Expectancy")    
print(Graphs$hle.s)

Graphs$hle.sq <- ggplot(healthylife.exp[,mean_se(age), by=.(sex, chd.incidence, scenario, qimd)],
            aes(x=chd.incidence, y=y, colour=scenario, ymax=max(y)*1.05, ymin = 70)) + 
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ qimd) +
    ylab("Age (years)") + scale_x_continuous(name="Year") + 
    ggtitle("Healthy Life Expectancy")    
print(Graphs$hle.sq)


# Risk Factors (CHD)
# Using exact Binomial from agresti-coull
riskfactors[group=="S", binom.confint(sum(smok.cvd.active), sum(pop), method="agresti-coull"), by=.(year, scenario, sex)]

# Using Normal Approximation to the Binomial
riskfactors[group=="S", MC.mean(smok.cvd.active.pct, 
                                   sqrt(smok.cvd.active.pct*(1-smok.cvd.active.pct)/pop), .N), by=.(year, scenario, sex)]

