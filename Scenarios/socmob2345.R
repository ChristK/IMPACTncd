# This scenario is the liberal social mobility one
# Assumes that 20% of each of the QIMD 2, 3, 4, 5 populations  move one level upwards every year, permanently 
cat("socmob2345 scenario\n\n")

intervention.year <- 2016

# Load prediction equations
if (i == (init.year - 2011)) {
    load(file="./Lagtimes/bmi.svylm.rda")
    load(file="./Lagtimes/chol.svylm.rda")
    load(file="./Lagtimes/sbp.svylm.rda")
    load(file="./Lagtimes/diab.svylr.rda")
    load(file="./Lagtimes/smok.active.svylr.rda")
    load(file="./Lagtimes/smok.cess.svylr.rda")
    load(file="./Lagtimes/smok.cess.success.rda")
    load(file="./Lagtimes/smok.start.svylr.rda")
    load(file="./Lagtimes/fv.svylr.rda")
    load(file="./Lagtimes/fvrate.svylr.rda")
}

if (i >= (intervention.year - 2011 + cvd.lag)) {
  setkey(POP, id)
  POP[sample_frac(POP[qimd == "5", .(id)], 0.2), qimd := "4"]
  POP[sample_frac(POP[qimd == "4", .(id)], 0.2), qimd := "3"]
  POP[sample_frac(POP[qimd == "3", .(id)], 0.2), qimd := "2"]
  POP[sample_frac(POP[qimd == "2", .(id)], 0.2), qimd := "1"]
  setkey(POP, qimd, sex, agegroup)
}
