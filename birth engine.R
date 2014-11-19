# Estimate number of births by the age of mother
cat("Estimating births...\n\n")
births <- POP[between(age, 15, 46) & sex == "2"][, .N, by = age]
births <- merge(births, Fertility[, c("age", paste0(2011 + i)), with = F], by = "age")
setnames(births, paste0(2011 + i), "fert")
births[, newborns := round(N * fert/1000)]

# Create temp data.table for newborns inherit characteristics (hserial, sha, qimd,eqv5, hpnssec8) only by their mother
tt <- unique(POP[between(age, 15, 46) & sex == 2,list(hserial)]) #select hserials with fertile women
setkey(tt)
setkey(POP, hserial)
# set age of newborn as 0
# set sex, given m/f = 1.05 or f/newborn = 0.487804872
#aggregate newborns to total population

POP <- rbind(POP, 
             POP[tt, 
                 sample_n(.SD,
                          births[, sum(newborns)], 
                          replace = T)][, `:=`(age = 0, 
                                               id = as.integer((1:.N) + POP[, max(id)]), 
                                               sex= ifelse(dice(.N) < 0.487804872, "2", "1"), 
                                               cigst1 = "1",
                                               cigst1.cvdlag = "1",
                                               cigst1.calag = "1",
                                               cigdyalCat = 0,
                                               givupsk = "99",
                                               endsmoke = 0,
                                               packyears = 0,
                                               #diabtyper = "2",
                                               diabtotr = "1",
                                               diabtotr.cvdlag = "1",
                                               #lipid = "0",
                                               #bpmedc = "0",
                                               cvdcon = "3")], 
             fill = T)
agegroup.fn(POP)

# export births 
# write.csv(births, file = paste0(output.dir(), "births-", year +i, ".csv"))

rm(tt, births)
