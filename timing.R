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

started.at <- proc.time()
source(file = "./life table engine.R")


cat(timetaken(started.at), "\n") 

require(microbenchmark)
compare <- microbenchmark(POP[is.na(chd.sbp.rr), chd.sbp.rr := 1],
                          POP[chd.sbp.rr==NA, chd.sbp.rr := 1], 
                          times = 100)
autoplot(compare)

system.time(
  SPOP2011[, mean(omsysval, na.rm = T), by = .(agegroup, sex, qimd)]
)

system.time(
  SPOP2011 %>% group_by(agegroup, sex, qimd) %>% summarise(mean(omsysval, na.rm = T))
)

compare <- microbenchmark(POP[, mean(omsysval, na.rm = T), by = .(agegroup, sex, qimd)],
                          POP %>% group_by(agegroup, sex, qimd) %>% summarise(mean(omsysval, na.rm = T)), 
                          times = 100)
autoplot(compare)

compare <- microbenchmark(POP[cigst1.cvdlag == "4" & age < 45 & sex == "1", chd.tob.rr := stochRR(.N, 5.51, 12.25)],
                          POP[cigst1.cvdlag == "4", chd.tob.rr := stochRR(.N, 5.51, 12.25)],
                          POP[cigst1.cvdlag == "4" & age < 45 & sex == "1", chd.tob.rr := 2], 
                          times = 100)
autoplot(compare)

system.time({
  POP[, chd.bmi.rr := 1]
  POP[between(age, ageL, 59), chd.bmi.rr := stochRRabov1(.N, 1.213256^((bmival.cvdlag - 20) / 4.56), 1.278837^((bmival.cvdlag - 20) / 4.56))]
  POP[between(age, 60, 69), chd.bmi.rr := stochRRabov1(.N, 1.058372^((bmival.cvdlag - 20) / 4.56), 1.115581^((bmival.cvdlag - 20) / 4.56))]
  POP[is.na(chd.bmi.rr), chd.bmi.rr := 1]
}
)

POP[, summary(chd.bmi.rr)]
POP[group==26, chd.ets.rr]

system.time({
  POP[, chd.chol.rr := 1]
#chol.rr.chd <- fread("./CVD Statistics/chol.rrchd.csv", stringsAsFactors = T, colClasses = c("factor", "numeric", "numeric"))
setkey(POP, agegroup)
#setkey(chol.rr.chd, agegroup)

POP[chol.rr.chd, chd.chol.rr := stochRRabov1(.N, mean.rr^(3.8 - cholval.cvdlag), ci.rr^(3.8 - cholval.cvdlag))]
POP[is.na(chd.chol.rr), chd.chol.rr := 1]
})


POP[, summary(chd.chol.rr)]

POP[tt, chd.tob.rr := 5]

