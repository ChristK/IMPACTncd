#cmpfile("./other model.R")
cat("Estimating deaths from other causes...\n")
cat(paste0(Sys.time(), "\n\n"))

POP <- merge(POP, 
             setnames(
  Lifetable[, c("age", "sex", "qimd", paste0(i + init.year)), with = F],
  paste0(i + init.year), "qx"), 
  by = c("age", "sex", "qimd"), 
  all.x = T)

cat("Inflate mortality for diabetics and smokers...\n\n")
POP[diabtotr.cvdlag == "2", qx := 1.6 * qx] # Increase the mortality of diabetics DECODE study
POP[diabtotr.cvdlag != "2", qx := qx * (1 - 1.6 * POP[group==.BY[[1]], prop.table(table(diabtotr.cvdlag))[2]]) / 
                                    (.N / POP[group==.BY[[1]], .N]), by=group] # Decrease the mortality of non-diabetics 

#1 Doll R, Peto R, Boreham J, et al. Mortality in relation to smoking: 50 years’ observations on male British doctors. 
#BMJ 2004;328:1519. doi:10.1136/bmj.38142.554479.AE table 1
# POP[cigst1 == "4", `:=` (qx = 1.8 * qx)] # Increase the mortality of smokers
# POP[cigst1 != "4", `:=` (qx = qx * (1 - 1.8 * POP[,.SD[cigst1 == "4", .N]/.N]) / POP[,.SD[cigst1 != "4", .N]/.N])] 
POP[cigst1.cvdlag == "4", `:=` (qx = smokriskofdeath * qx), by=group]
POP[cigst1.cvdlag != "4", `:=` (qx = qx * (1 - smokriskofdeath * 
                                             POP[group==.BY[[1]], prop.table(table(cigst1.cvdlag))][4]) / 
                           (.N / POP[group==.BY[[1]], .N])), by=group]

POP[, `:=`(dead = dice(.N) < qx, qx = NULL)]  # mark deaths from lifetable

cat("Export Other mortality summary...\n\n")
if (i == init.year-2011) other.mortal <- vector("list", yearstoproject * 4)

other.mortal[[i * 4 + 1]] <- output.other(POP, c("qimd", "sex", "agegroup"))

other.mortal[[i * 4 + 2]] <- output.other(POP, c("sex", "agegroup"))

other.mortal[[i * 4 + 3]] <- output.other(POP, c("qimd", "sex"))

other.mortal[[i * 4 + 4]] <- output.other(POP, c("sex"))

if (i == yearstoproject + init.year - 2012) {
  saveRDS(rbindlist(other.mortal, T, T),
          file = paste0(output.dir(), "other.mortal.rds"))
}

cat("Export Other mortality individuals...\n\n")
indiv.mort[[1]] <- POP[dead == T, .(age, sex, qimd, agegroup, eqv5, id, hserial, hpnssec8, sha)][,`:=` (year = 2011 + i, cause = "other", scenario = gsub(".R", "", scenarios.list[[iterations]]), mc = haha)]

POP = POP[dead == F,]  # remove dead 

