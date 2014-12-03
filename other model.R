# Estimating deaths from other causes
cat("Estimating deaths from other causes...\n\n")
POP <- merge(POP, setnames(Lifetable[, c("age", "sex", paste0(i + 2011)), with = F], paste0(i + 2011), "qx"), by = c("age", "sex"), all.x = T)

cat("Inflate mortality for diabetics and smokers...\n\n")
POP[diabtotr.cvdlag == "2", `:=` (qx = 1.6 * qx)] # Increase the mortality of diabetics DECODE study
POP[diabtotr.cvdlag == "1", `:=` (qx = qx * (1 - 1.6 * POP[diabtotr.cvdlag == "2", .N]/nrow(POP)) / 
                                    .N / nrow(POP))] # Decrease the mortality of non-diabetics 

#1 Doll R, Peto R, Boreham J, et al. Mortality in relation to smoking: 50 years’ observations on male British doctors. 
#BMJ 2004;328:1519. doi:10.1136/bmj.38142.554479.AE table 1
# POP[cigst1 == "4", `:=` (qx = 1.8 * qx)] # Increase the mortality of smokers
# POP[cigst1 != "4", `:=` (qx = qx * (1 - 1.8 * POP[,.SD[cigst1 == "4", .N]/.N]) / POP[,.SD[cigst1 != "4", .N]/.N])] 
POP[cigst1.cvdlag == "4", `:=` (qx = smokriskofdeath * qx)]
POP[cigst1.cvdlag != "4", `:=` (qx = qx * (1 - smokriskofdeath * POP[cigst1.cvdlag == "4", .N]/nrow(POP)) / 
                           .N / nrow(POP))]

POP[, `:=`(dead = dice(.N) <= qx)]  # mark deaths from lifetable

cat("Export Other mortality summary...\n\n")
output <- vector("list", 5)

if (exists("other.mortal.rds")) output[[1]] <- other.mortal.rds

output[[2]] <- POP[between(age, ageL, ageH), output.other(.SD), by=.(qimd, sex, agegroup)]

output[[3]] <- POP[between(age, ageL, ageH), output.other(.SD), by=.(sex, agegroup)]

output[[4]] <- POP[between(age, ageL, ageH), output.other(.SD), by=.(qimd, sex)]

output[[5]] <- POP[between(age, ageL, ageH), output.other(.SD), by=.(sex)]

other.mortal.rds <- rbindlist(output, fill = T)

if (i == yearstoproject + init.year - 2012) {
  saveRDS(other.mortal.rds, file = paste0(output.dir(), "other.mortal.rds"))
}

cat("Export Other mortality individuals...\n\n")
output <- vector("list", 2)

if (exists("other.ind.mortal.rds")) output[[1]] <- other.ind.mortal.rds

output[[2]] <- POP[dead == T, .(age, sex, qimd, agegroup, eqv5, id, hserial, hpnssec8, sha)][,`:=` (year.death = 2011+i, cause.death = "Other", scenario = gsub(".R", "", scenarios.list[[iterations]]), mc = haha)]

other.ind.mortal.rds <- rbindlist(output, fill = T)

rm(output)

if (i == yearstoproject + init.year - 2012) {
saveRDS(other.ind.mortal.rds, file = paste0(output.dir(), "other.ind.mortal.rds"))
}

   
POP = copy(POP[dead == F,])  # remove dead 
POP[, `:=`(qx = NULL)]
