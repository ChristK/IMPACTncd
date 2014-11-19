require("data.table")
require("ggplot2")
require("binom")
source(file = "./post simulation functions.R")

# riskfactors <- fread("./output/RF/riskfactors.csv")
# life.exp <- fread("./output/Other/life.exp.csv")
# chd.burden <- fread("./output/CHD/chd.burden.csv")
# healthylife.exp <- fread("./output/CHD/healthylife.exp.csv")

load("./Output/RF/riskfactors.RData")
load("./Output/Other/life.exp0.RData")
load("./Output/Other/life.exp65.RData")
load("./Output/Other/hlife.exp.RData")
load("./Output/Other/other.mortality.RData")
load("./Output/CHD/chd.burden.RData")
load("./Output/Stroke/stroke.burden.RData")
load("./Output/Graphs/Graphs.rda")
load("./Output/Tables/Tables.rda")

pd <- position_dodge(.3) 

# Save graphs to pdfs A4 landscape
lapply(names(Graphs), 
       function(x) ggsave(filename=paste0(x,".pdf"),
                          plot=Graphs[[x]], 
                          path = "./Output/Graphs", 
                          width = 11.69,
                          height = 8.27))


# Using exact Binomial from agresti-coull
riskfactors[group=="S", binom.confint(sum(smok.cvd.active), sum(pop), method="agresti-coull"), by=.(year, scenario, sex)]

# Using Normal Approximation to the Binomial
riskfactors[group=="S", MC.mean(smok.cvd.active.pct, 
                                   sqrt(smok.cvd.active.pct*(1-smok.cvd.active.pct)/pop), .N), by=.(year, scenario, sex)]

