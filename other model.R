#cmpfile("./other model.R")
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


cat("Estimating deaths from other causes...\n")
cat(paste0(Sys.time(), "\n\n"))

cat("Inflate mortality for diabetics and smokers...\n\n")
#Doll R, et al. Mortality in relation to smoking: 50 years’ observations on male
#British doctors. BMJ 2004;328:1519. doi:10.1136/bmj.38142.554479.AE table 1
set(POP, NULL, "death.tob.rr", 1)
POP[cigst1.cvdlag == "4", death.tob.rr := smokriskofdeath]

set(POP, NULL, "death.diab.rr", 1)
POP[diabtotr.cvdlag == "2", death.diab.rr := 1.6] #rr from DECODE study

deathpaf <- 
  POP[between(age, ageL, ageH), 
      .(paf = 1 - 1 / (sum(death.tob.rr * death.diab.rr) / .N)), 
      by = .(age, sex, qimd)
      ]
setkey(deathpaf, age, sex, qimd)

deathrate <- setnames(
  Lifetable[, c("age", "sex", "qimd", as.character(i + 2011)), with = F],
  as.character(i + 2011), "qx")
setkey(deathrate, age, sex, qimd)

deathrate[deathpaf, qx := qx * (1 - paf)]

POP[deathrate, qx := qx, on = c("age", "sex", "qimd")]


POP[dice(.N) < qx * death.tob.rr * death.diab.rr, `:=`(dead = 1L)]  # mark deaths from lifetable

POP[, `:=` (qx = NULL,
            death.tob.rr = NULL,
            death.diab.rr = NULL)]

cat("Export Other mortality summary...\n\n")
if (i == init.year-2011) other.mortal <- vector("list", yearstoproject * 4)

other.mortal[[(2011 - init.year + i) * 4 + 1]] <-
  output.other(POP, c("qimd", "sex", "agegroup"))

other.mortal[[(2011 - init.year + i) * 4 + 2]] <-
  output.other(POP, c("sex", "agegroup"))

other.mortal[[(2011 - init.year + i) * 4 + 3]] <-
  output.other(POP, c("qimd", "sex"))

other.mortal[[(2011 - init.year + i) * 4 + 4]] <- 
  output.other(POP, c("sex"))

if (i == yearstoproject + init.year - 2012) {
  saveRDS(rbindlist(other.mortal, T, T),
          file = paste0(output.dir(), "other.mortal.rds"))
}

cat("Export Other mortality individuals...\n\n")
indiv.mort[[1]] <- 
  POP[dead == 1L, 
      .(age, sex, qimd, agegroup, eqv5, id, hserial, hpnssec8, sha
      )][,`:=` (year = 2011 + i, 
                cause = "other", 
                scenario = gsub(".R", "", scenarios.list[[iterations]]), 
                mc = haha)]

POP = POP[is.na(dead)]  # remove dead 
POP[, dead := NULL]
