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

pd <- position_dodge(.3) 
Graphs <- list()

# CHD
Graphs$chdincid.s <- ggplot(chd.burden[group=="S", MC.mean(chd.incidence/pop, 
                                                                 sqrt((chd.incidence/pop)*(1-chd.incidence/pop)/pop), .N),
                                  by=.(year, scenario, sex)],
                                 aes(x=year, y=mean*100000, colour=scenario, ymax=max(mean*100000)*1.05, ymin = 0)) + 
    geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ .) +
    ylab("Incidence per 100,000") + scale_x_continuous(name="Year") +
    ggtitle("CHD Incidence (ages 30-84)")   
print(Graphs$chdincid.s)

Graphs$chdincid.sq <- ggplot(chd.burden[group=="SQ", MC.mean(chd.incidence/pop, 
                                                    sqrt((chd.incidence/pop)*(1-chd.incidence/pop)/pop), .N),
                                by=.(year, scenario, sex, qimd)],
                     aes(x=year, y=mean*100000, colour=scenario, ymax=max(mean*100000)*1.05, ymin = 0)) + 
    geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ qimd) +
    ylab("Incidence per 100,000") + scale_x_continuous(name="Year") +
    ggtitle("CHD Incidence (ages 30-84)")   
print(Graphs$chdincid.sq)

Graphs$chdpreval.s <- ggplot(chd.burden[group=="S", MC.mean(chd.prevalence/pop, 
                                                    sqrt((chd.prevalence/pop)*(1-chd.prevalence/pop)/pop), .N),
                                by=.(year, scenario, sex)],
                     aes(x=year, y=mean*100000, colour=scenario, ymax=max(mean*100000)*1.05, ymin = 0)) + 
    geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ .) +
    ylab("Prevalence per 100,000") + scale_x_continuous(name="Year") +
    ggtitle("CHD Prevalence (ages 30-84)")   
print(Graphs$chdpreval.s)

Graphs$chdpreval.sq <- ggplot(chd.burden[group=="SQ", MC.mean(chd.prevalence/pop, 
                                                      sqrt((chd.prevalence/pop)*(1-chd.prevalence/pop)/pop), .N),
                                 by=.(year, scenario, sex, qimd)],
                      aes(x=year, y=mean*100000, colour=scenario, ymax=max(mean*100000)*1.05, ymin = 0)) + 
    geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ qimd) +
    ylab("Prevalence per 100,000") + scale_x_continuous(name="Year") +
    ggtitle("CHD Prevalence (ages 30-84)")   
print(Graphs$chdpreval.sq)

Graphs$chdmortal.s <- ggplot(chd.burden[group=="S", MC.mean(chd.mortality/pop, 
                                                     sqrt((chd.mortality/pop)*(1-chd.mortality/pop)/pop), .N),
                                 by=.(year, scenario, sex)],
                      aes(x=year, y=mean*100000, colour=scenario, ymax=max(mean*100000)*1.05, ymin = 0)) + 
    geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ .) +
    ylab("Mortality per 100,000") + scale_x_continuous(name="Year") +
    ggtitle("CHD Mortality (ages 30-84)")   
print(Graphs$chdmortal.s)

Graphs$chdmortal.sq <- ggplot(chd.burden[group=="SQ", MC.mean(chd.mortality/pop, 
                                                       sqrt((chd.mortality/pop)*(1-chd.mortality/pop)/pop), .N),
                                  by=.(year, scenario, sex, qimd)],
                       aes(x=year, y=mean*100000, colour=scenario, ymax=max(mean*100000)*1.05, ymin = 0)) + 
    geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ qimd) +
    ylab("Mortality per 100,000") + scale_x_continuous(name="Year") +
    ggtitle("CHD Mortality (ages 30-84)")   
print(Graphs$chdmortal.sq)

# Stroke
Graphs$strokeincid.s <- ggplot(stroke.burden[group=="S", MC.mean(stroke.incidence/pop, 
                                                                 sqrt((stroke.incidence/pop)*(1-stroke.incidence/pop)/pop), .N),
                                             by=.(year, scenario, sex)],
                               aes(x=year, y=mean*100000, colour=scenario, ymax=max(mean*100000)*1.05, ymin = 0)) + 
    geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ .) +
    ylab("Incidence per 100,000") + scale_x_continuous(name="Year") +
    ggtitle("Stroke Incidence (ages 30-84)")   
print(Graphs$strokeincid.s)

Graphs$strokeincid.sq <- ggplot(stroke.burden[group=="SQ", MC.mean(stroke.incidence/pop, 
                                                                   sqrt((stroke.incidence/pop)*(1-stroke.incidence/pop)/pop), .N),
                                              by=.(year, scenario, sex, qimd)],
                                aes(x=year, y=mean*100000, colour=scenario, ymax=max(mean*100000)*1.05, ymin = 0)) + 
    geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ qimd) +
    ylab("Incidence per 100,000") + scale_x_continuous(name="Year") +
    ggtitle("Stroke Incidence (ages 30-84)")   
print(Graphs$strokeincid.sq)

Graphs$strokepreval.s <- ggplot(stroke.burden[group=="S", MC.mean(stroke.prevalence/pop, 
                                                                  sqrt((stroke.prevalence/pop)*(1-stroke.prevalence/pop)/pop), .N),
                                              by=.(year, scenario, sex)],
                                aes(x=year, y=mean*100000, colour=scenario, ymax=max(mean*100000)*1.05, ymin = 0)) + 
    geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ .) +
    ylab("Prevalence per 100,000") + scale_x_continuous(name="Year") +
    ggtitle("Stroke Prevalence (ages 30-84)")   
print(Graphs$strokepreval.s)

Graphs$strokepreval.sq <- ggplot(stroke.burden[group=="SQ", MC.mean(stroke.prevalence/pop, 
                                                                    sqrt((stroke.prevalence/pop)*(1-stroke.prevalence/pop)/pop), .N),
                                               by=.(year, scenario, sex, qimd)],
                                 aes(x=year, y=mean*100000, colour=scenario, ymax=max(mean*100000)*1.05, ymin = 0)) + 
    geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ qimd) +
    ylab("Prevalence per 100,000") + scale_x_continuous(name="Year") +
    ggtitle("Stroke Prevalence (ages 30-84)")   
print(Graphs$strokepreval.sq)

Graphs$strokemortal.s <- ggplot(stroke.burden[group=="S", MC.mean(stroke.mortality/pop, 
                                                                  sqrt((stroke.mortality/pop)*(1-stroke.mortality/pop)/pop), .N),
                                              by=.(year, scenario, sex)],
                                aes(x=year, y=mean*100000, colour=scenario, ymax=max(mean*100000)*1.05, ymin = 0)) + 
    geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ .) +
    ylab("Mortality per 100,000") + scale_x_continuous(name="Year") +
    ggtitle("Stroke Mortality (ages 30-84)")   
print(Graphs$strokemortal.s)

Graphs$strokemortal.sq <- ggplot(stroke.burden[group=="SQ", MC.mean(stroke.mortality/pop, 
                                                                    sqrt((stroke.mortality/pop)*(1-stroke.mortality/pop)/pop), .N),
                                               by=.(year, scenario, sex, qimd)],
                                 aes(x=year, y=mean*100000, colour=scenario, ymax=max(mean*100000)*1.05, ymin = 0)) + 
    geom_errorbar(aes(ymin= lui*100000, ymax = uui*100000), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ qimd) +
    ylab("Mortality per 100,000") + scale_x_continuous(name="Year") +
    ggtitle("Stroke Mortality (ages 30-84)")   
print(Graphs$strokemortal.sq)

# Life Expectancy
life.exp[,.(mean=mean(age), sd=sd(age)), by=.(sex, year.death, scenario, mc)][, MC.mean(mean,sd,.N), by=.(sex, year.death, scenario)]

Graphs$le.s <- ggplot(life.exp[,mean_se(age), by=.(sex, year.death, scenario)],
                      aes(x=year.death, y=y, colour=scenario, ymax=max(y)*1.05, ymin = 70)) + 
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ .) +
    ylab("Age (years)") + scale_x_continuous(name="Year") + 
    ggtitle("Life Expectancy")    
print(Graphs$le.s)

Graphs$le.sq <- ggplot(life.exp[,mean_se(age), by=.(sex, year.death, scenario, qimd)],
                       aes(x=year.death, y=y, colour=scenario, ymax=max(y)*1.05, ymin = 70)) + 
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ qimd) +
    ylab("Age (years)") + scale_x_continuous(name="Year") + 
    ggtitle("Life Expectancy")    
print(Graphs$le.sq)


# Life Expectancy
life.exp[,.(mean=mean(age), sd=sd(age)), by=.(sex, year.death, scenario, mc)][, MC.mean(mean,sd,.N), by=.(sex, year.death, scenario)]

Graphs$le.s <- ggplot(life.exp[,mean_se(age), by=.(sex, year.death, scenario)],
            aes(x=year.death, y=y, colour=scenario, ymax=max(y)*1.05, ymin = 70)) + 
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ .) +
    ylab("Age (years)") + scale_x_continuous(name="Year") + 
    ggtitle("Life Expectancy")    
print(Graphs$le.s)

Graphs$le.sq <- ggplot(life.exp[,mean_se(age), by=.(sex, year.death, scenario, qimd)],
            aes(x=year.death, y=y, colour=scenario, ymax=max(y)*1.05, ymin = 70)) + 
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ qimd) +
    ylab("Age (years)") + scale_x_continuous(name="Year") + 
    ggtitle("Life Expectancy")    
print(Graphs$le.sq)

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
Graphs$smoking.s <- ggplot(riskfactors[group=="S", MC.mean(smok.cvd.active/pop, 
                                            sqrt((smok.cvd.active/pop)*(1-smok.cvd.active/pop)/pop), .N), by=.(year, scenario, sex)],
            aes(x=year-5, y=mean, colour=scenario, ymax=max(mean)*1.05, ymin = 0)) + 
    geom_errorbar(aes(ymin= lui, ymax = uui), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ .) +
    ylab("Prevalence") + scale_x_continuous(name="Year") +
    ggtitle("Smoking Prevalence (ages 30-84)")   
print(Graphs$smoking.s)

Graphs$smoking.sq <- ggplot(riskfactors[group=="SQ", MC.mean(smok.cvd.active/pop, 
                                            sqrt((smok.cvd.active/pop)*(1-smok.cvd.active/pop)/pop), .N), 
                        by=.(year, scenario, sex, qimd)],
            aes(x=year-5, y=mean, colour=scenario, ymax=max(mean)*1.05, ymin = 0)) + 
    geom_errorbar(aes(ymin= lui, ymax = uui), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ qimd) +
    ylab("Prevalence %") + scale_x_continuous(name="Year") +
    ggtitle("Smoking Prevalence (ages 30-84)")   
print(Graphs$smoking.sq)


Graphs$diabetes.s <- ggplot(riskfactors[group=="S", MC.mean(diab.cvd.yes/pop, 
                                            sqrt((diab.cvd.yes/pop)*(1-diab.cvd.yes/pop)/pop), .N), by=.(year, scenario, sex)],
            aes(x=year-5, y=mean, colour=scenario, ymax=max(mean)*1.05, ymin = 0)) + 
    geom_errorbar(aes(ymin= lui, ymax = uui), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ .) +
    ylab("Prevalence %") + scale_x_continuous(name="Year") +
    ggtitle("Diabetes Prevalence \n(ages 30-84, including undiagnosed)")   
print(Graphs$diabetes.s)

Graphs$diabetes.sq <- ggplot(riskfactors[group=="SQ", MC.mean(diab.cvd.yes/pop, 
                                                     sqrt((diab.cvd.yes/pop)*(1-diab.cvd.yes/pop)/pop), .N), 
                                  by=.(year, scenario, sex,qimd)],
                     aes(x=year-5, y=mean, colour=scenario, ymax=max(mean)*1.05, ymin = 0)) + 
    geom_errorbar(aes(ymin= lui, ymax = uui), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ qimd) +
    ylab("Prevalence %") + scale_x_continuous(name="Year") +
    ggtitle("Diabetes Prevalence \n(ages 30-84, including undiagnosed)")   
print(Graphs$diabetes.sq)

Graphs$fv.s <- ggplot(riskfactors[group=="S", MC.mean((fv.cvd.6+fv.cvd.7+fv.cvd.8+fv.cvd.9)/pop, 
                                                     sqrt(((fv.cvd.6+fv.cvd.7+fv.cvd.8+fv.cvd.9)/
                                                               pop)*(1-(+fv.cvd.6+fv.cvd.7+fv.cvd.8+fv.cvd.9)/pop)/pop),
                                               .N), by=.(year, scenario, sex)],
                     aes(x=year-5, y=mean, colour=scenario, ymax=max(mean)*1.05, ymin = 0)) + 
    geom_errorbar(aes(ymin= lui, ymax = uui), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ .) +
    ylab("Prevalence %") + scale_x_continuous(name="Year") +
    ggtitle("Five or more F&V portions a day \n(ages 30-84)")   
print(Graphs$fv.s)

Graphs$fv.sq <- ggplot(riskfactors[group=="SQ", MC.mean((fv.cvd.6+fv.cvd.7+fv.cvd.8+fv.cvd.9)/pop, 
                                               sqrt(((fv.cvd.6+fv.cvd.7+fv.cvd.8+fv.cvd.9)/
                                                         pop)*(1-(+fv.cvd.6+fv.cvd.7+fv.cvd.8+fv.cvd.9)/pop)/pop),
                                               .N), by=.(year, scenario, sex, qimd)],
               aes(x=year-5, y=mean, colour=scenario, ymax=max(mean)*1.05, ymin = 0)) + 
    geom_errorbar(aes(ymin= lui, ymax = uui), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ qimd) +
    ylab("Prevalence %") + scale_x_continuous(name="Year") +
    ggtitle("Five or more F&V portions a day \n(ages 30-84)")   
print(Graphs$fv.sq)

Graphs$bmi.s <- ggplot(riskfactors[group=="S", MC.mean(bmi.cvd.mean, bmi.cvd.sd/sqrt(pop), .N), by=.(year, scenario, sex)],
               aes(x=year-5, y=mean, colour=scenario, ymax=max(mean)*1.05, ymin = 0)) + 
    geom_errorbar(aes(ymin= lui, ymax = uui), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ .) +
    ylab("BMI (kg/m^2)") + scale_x_continuous(name="Year") +
    ggtitle("BMI (ages 30-84)")   
print(Graphs$bmi.s)

Graphs$bmi.sq <- ggplot(riskfactors[group=="SQ", MC.mean(bmi.cvd.mean, bmi.cvd.sd/sqrt(pop), .N), by=.(year, scenario, sex, qimd)],
                aes(x=year-5, y=mean, colour=scenario, ymax=max(mean)*1.05, ymin = 0)) + 
    geom_errorbar(aes(ymin= lui, ymax = uui), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ qimd) +
    ylab("BMI (kg/m^2)") + scale_x_continuous(name="Year") +
    ggtitle("BMI (ages 30-84)")   
print(Graphs$bmi.sq)

Graphs$tc.s <- ggplot(riskfactors[group=="S", MC.mean(tc.cvd.mean, tc.cvd.sd/sqrt(pop), .N), by=.(year, scenario, sex)],
                aes(x=year-5, y=mean, colour=scenario, ymax=max(mean)*1.05, ymin = 0)) + 
    geom_errorbar(aes(ymin= lui, ymax = uui), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ .) +
    ylab("Total Cholesterol (mmol/l)") + scale_x_continuous(name="Year") +
    ggtitle("Total Cholesterol (ages 30-84)")   
print(Graphs$tc.s)

Graphs$tc.sq <- ggplot(riskfactors[group=="SQ", MC.mean(tc.cvd.mean, tc.cvd.sd/sqrt(pop), .N), by=.(year, scenario, sex, qimd)],
                 aes(x=year-5, y=mean, colour=scenario, ymax=max(mean)*1.05, ymin = 0)) + 
    geom_errorbar(aes(ymin= lui, ymax = uui), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ qimd) +
    ylab("Total Cholesterol (mmol/l)") + scale_x_continuous(name="Year") +
    ggtitle("Total Cholesterol (ages 30-84)")   
print(Graphs$tc.sq)

Graphs$sbp.s <- ggplot(riskfactors[group=="S", MC.mean(sbp.cvd.mean, sbp.cvd.sd/sqrt(pop), .N), by=.(year, scenario, sex)],
               aes(x=year-5, y=mean, colour=scenario, ymax=max(mean)*1.05, ymin = 0)) + 
    geom_errorbar(aes(ymin= lui, ymax = uui), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ .) +
    ylab("Systolic Blood Pressure (mmHg)") + scale_x_continuous(name="Year") +
    ggtitle("Systolic Blood Pressure (ages 30-84)")   
print(Graphs$sbp.s)

Graphs[["sbp.sq"]] <- ggplot(riskfactors[group=="SQ", MC.mean(sbp.cvd.mean, sbp.cvd.sd/pop, .N), by=.(year, scenario, sex, qimd)],
                aes(x=year-5, y=mean, colour=scenario, ymax=max(mean)*1.05, ymin = 0)) + 
    geom_errorbar(aes(ymin= lui, ymax = uui), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(sex ~ qimd) +
    ylab("Systolic Blood Pressure (mmHg)") + scale_x_continuous(name="Year") +
    ggtitle("Systolic Blood Pressure (ages 30-84)")   
print(Graphs$sbp.sq)

