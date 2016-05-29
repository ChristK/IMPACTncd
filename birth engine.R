#cmpfile("./birth engine.R")
## IMPACTncd: A decision support tool for primary prevention of NCDs
## Copyright (C) 2015  Chris Kypridemos
 
## IMPACTncd is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program; if not, see <http://www.gnu.org/licenses/>
## or write to the Free Software Foundation, Inc., 51 Franklin Street,
## Fifth Floor, Boston, MA 02110-1301  USA.


# Estimate number of births by the age of mother
cat("Estimating births...\n\n")
births <- POP[between(age, 15, 46) & sex == "2"][, .N, by = age]
births <- merge(births, Fertility[, c("age", paste0(2011 + i)), with = F], by = "age")
setnames(births, paste0(2011 + i), "fert")
births[, newborns := round(N * fert/1000)]

# Create temp data.table for newborns inherit characteristics (hserial, sha, qimd,eqv5, hpnssec8) only by their mother
tt <- POP[between(age, 15, 46) & sex == "2", unique(hserial)] #select hserials with fertile women
#setkey(tt)
setkey(POP, hserial)
# set age of newborn as 0
# set sex, given m/f = 1.05 or f/newborn = 0.487804872
#aggregate newborns to total population

POP <- rbind(POP, 
             POP[hserial %in% tt, 
                 sample_n(.SD,
                          births[, sum(newborns)], 
                          replace = T)
                 ][ , `:=`(age = 0, 
                           id = (1:.N) + POP[, max(id)], 
                    sex = (dice(.N) < 0.487804872) + 1L, 
                    cigst1 = "1",
                    cigst1.cvdlag = "1",
                    cigst1.calag = "1",
                    cigdyal = 0,
                    smokyrs = 0L,
                    givupsk = "99",
                    endsmoke = 0L,
                    numsmok = 0,
                    packyears = 0,
					          packyears.calag = 0,
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
