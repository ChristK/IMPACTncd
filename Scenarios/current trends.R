# This scenario is the fundumental one
# Assumes that the trends that where observed since 2001 will continue in the future
cat("current trends scenario\n\n")

# Load prediction equations
if (i == (init.year-2011)) {
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