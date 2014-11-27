require("data.table")
require("ggplot2")
require("binom")
source(file = "./post simulation functions.R")

load("./Output/CHD/chd.burden.RData")
load(file="./Validation/kirk.RData") # chd.drates

setnames(chd.drates, c("chd.death.rates", "chd.death.rates.lcl", "chd.death.rates.ucl"),
         c("mean", "lower", "upper"))
chd.drates[, group:="BAMP"]

chd.mort = copy(chd.burden[group=="SAQ" & scenario == "current trends",])
chd.mort = copy(chd.mort[agegroup!="30-34",])
chd.mort[agegroup %in% c("35-39", "40-44"), agegroup := "35-44"]
chd.mort[agegroup %in% c("45-49", "50-54"), agegroup := "45-54"]
chd.mort[agegroup %in% c("55-59", "60-64"), agegroup := "55-64"]
chd.mort[agegroup %in% c("65-69", "70-74"), agegroup := "65-74"]
chd.mort[agegroup %in% c("75-79", "80-84"), agegroup := "75-84"]

chd.mort <- chd.mort[, binom.confint(sum(chd.mortality), sum(pop), method="agresti-coull"),
           by=.(year, agegroup, sex, qimd)]
chd.mort[, `:=` (group="IMPACTncd", x=NULL, n=NULL, method=NULL)]
mort <- rbind(chd.mort,chd.drates, fill=T)
mort <- mort[agegroup!="85+" & between(year, 2000, 2060)]

pd <- position_dodge(.3) 
men1 <- ggplot(mort[sex=="Men",],
       aes(x=year, y=mean*100000, colour=group, ymax=max(mean*100000)*1.05, ymin = 0)) + 
    geom_errorbar(aes(ymin= lower*100000, ymax = upper*100000), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(agegroup ~ qimd, scales="fixed") +
    ylab("Mortality per 100,000") + scale_x_continuous(name="Year") + ylim(0,1500) + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
    ggtitle("CHD Mortality Validation(Men)") 

women1 <- ggplot(mort[sex=="Women",],
              aes(x=year, y=mean*100000, colour=group, ymax=max(mean*100000)*1.05, ymin = 0)) + 
    geom_errorbar(aes(ymin= lower*100000, ymax = upper*100000), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(agegroup ~ qimd, scales="fixed") +
    ylab("Mortality per 100,000") + scale_x_continuous(name="Year") + ylim(0,1500) + 
    ggtitle("CHD Mortality Validation(Women)")

print(men1)
print(women1)

men2 <- ggplot(mort[sex=="Men",],
              aes(x=year, y=mean*100000, colour=group, ymin = 0)) + 
  geom_errorbar(aes(ymin= lower*100000, ymax = upper*100000), width=.1) +
  geom_smooth(size = 2, alpha=1/4) +
  geom_point(size= 2, alpha=3/4) +
  facet_grid(agegroup ~ qimd, scales="free") +
  ylab("Mortality per 100,000") + scale_x_continuous(name="Year") + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
  ggtitle("CHD Mortality Validation (Men)") 

women2 <- ggplot(mort[sex=="Women",],
                 aes(x=year, y=mean*100000, colour=group, ymin = 0)) + 
  geom_errorbar(aes(ymin= lower*100000, ymax = upper*100000), width=.1) +
  geom_smooth(size = 2, alpha=1/4) +
  geom_point( size= 2, alpha=3/4) +
  facet_grid(agegroup ~ qimd, scales="free") +
  ylab("Mortality per 100,000") + scale_x_continuous(name="Year") + 
  ggtitle("CHD Mortality Validation (Women)")

print(men2)
print(women2) 

# Observed CHD mortality validation
load("./LifeTables/deaths.by.cause.RData")
'%!in%' <- function(x,y)!('%in%'(x,y))
agegrouptoexclude <- levels(factor(agegroup.fn(50:84)))
diseasestoexclude.ICD <- c(paste0("I", 20:25))
deaths.by.cause[icd %!in% diseasestoexclude.ICD, ndths := 0]
deaths.by.cause[, Mx:= sum(ndths)/pop, by=.(year, agegroup, sex)]
deaths.by.cause =copy(unique(deaths.by.cause, by=c("year", "agegroup", "sex"))[, c("icd", "ndths"):= NULL][])
deaths.by.cause[, agegroup:= ordered(agegroup, levels = c("<1   ", "01-04", "05-09",
                                                          "10-14", "15-19", "20-24", 
                                                          "25-29", "30-34", "35-39", 
                                                          "40-44", "45-49", "50-54",
                                                          "55-59", "60-64", "65-69",
                                                          "70-74", "75-79", "80-84", 
                                                          "85+"))]
deaths.by.cause[sex=="1", sex:= "Men"]
deaths.by.cause[sex=="2", sex:= "Women"]
deaths.by.cause[,sex := factor(sex)]
setkey(deaths.by.cause, year, sex, agegroup)

chd.mort = copy(chd.burden[group=="SA" & scenario == "current trends",])

ggplot(deaths.by.cause[agegroup %in% agegrouptoexclude],
       aes(x=year, y=Mx*100000, colour=agegroup, ymax=max(Mx*100000)*1.05, ymin = 0)) + 
    #geom_errorbar(aes(ymin= lower*100000, ymax = upper*100000), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3) +
    facet_grid(.~sex) +
    ylab("Mortality per 100,000") + scale_x_continuous(name="Year") + ylim(0,4500) + 
    ggtitle("CHD Mortality (observed)") 
