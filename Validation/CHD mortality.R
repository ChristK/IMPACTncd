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


require(reshape, xlsx) # for rename function

read.rates_fnc <- function(filename, workbook, sex, imdq) {
	dat.temp <- read.xlsx(filename, workbook)
	colnames(dat.temp) <- c('Year', '35-44', '45-54', '55-64', '65-74', '75-84', '85+')
	dat.temp <- reshape(dat.temp, idvar = 'Year', varying = list(2:7), v.names = c('chd.death.rates'), timevar = 'Age', direction = 'long', times = colnames(dat.temp)[2:7])
	rownames(dat.temp) <- NULL
	dat.temp$Sex <- sex
	dat.temp$IMDQ <- imdq
	dat.temp
}

comb.rates_fnc <- function(filename, sex) {
	rbind(read.rates_fnc(filename, paste(sex, 1, sep = ""), sex, 1),
	read.rates_fnc(filename, paste(sex, 2, sep = ""), sex, 2),
	read.rates_fnc(filename, paste(sex, 3, sep = ""), sex, 3),
	read.rates_fnc(filename, paste(sex, 4, sep = ""), sex, 4), 
	read.rates_fnc(filename, paste(sex, 5, sep = ""), sex, 5))
}

comb.rates_fnc_ul <- function(filename, ul, sex) {
	rbind(read.rates_fnc(filename, paste(ul, "cl_", sex, 1 , sep = ""), sex, "1"),
	read.rates_fnc(filename, paste(ul, "cl_", sex, 2, sep = ""), sex, "2"),
	read.rates_fnc(filename, paste(ul, "cl_", sex, 3, sep = ""), sex, "3"),
	read.rates_fnc(filename, paste(ul, "cl_", sex, 4, sep = ""), sex, "4"),
	read.rates_fnc(filename, paste(ul, "cl_", sex, 5, sep = ""), sex, "5"))
}

men.file = "./Validation/2014-03-28 BAMP outputs_Mrate_duncan1.xlsx"
women.file = "./Validation/2014-02-25 BAMP outputs_Frate_duncan1.xlsx"

chd.drates.M <- comb.rates_fnc(filename = men.file, sex = "M") 
chd.drates.F <- comb.rates_fnc(filename = women.file, sex = "F")

chd.drates_lcl.M <- comb.rates_fnc_ul(filename = men.file, "l", sex = "M")
chd.drates_lcl.M <- rename(chd.drates_lcl.M, c("chd.death.rates" = "chd.death.rates.lcl"))

chd.drates_ucl.M <- comb.rates_fnc_ul(filename = men.file, "u", sex = "M")
chd.drates_ucl.M <- rename(chd.drates_ucl.M, c("chd.death.rates" = "chd.death.rates.ucl"))

chd.drates_lcl.F <- comb.rates_fnc_ul(filename = women.file, "l", sex = "F")
chd.drates_lcl.F <- rename(chd.drates_lcl.F, c("chd.death.rates"="chd.death.rates.lcl"))

chd.drates_ucl.F <- comb.rates_fnc_ul(filename = women.file, "u", sex = "F")
chd.drates_ucl.F <- rename(chd.drates_ucl.F, c("chd.death.rates" = "chd.death.rates.ucl"))

chd.drates.F_temp <- merge(chd.drates.F, chd.drates_lcl.F, by = c("Year", "Age", "Sex", "IMDQ"))
chd.drates.F <- merge(chd.drates.F_temp, chd.drates_ucl.F, by = c("Year", "Age", "Sex", "IMDQ"))

chd.drates.M_temp <- merge(chd.drates.M, chd.drates_lcl.M, by = c("Year", "Age", "Sex", "IMDQ"))
chd.drates.M <- merge(chd.drates.M_temp, chd.drates_ucl.M, by = c("Year", "Age", "Sex", "IMDQ"))

chd.drates <- rbind(chd.drates.M, chd.drates.F)

setDT(chd.drates)
setnames(chd.drates, c("Age", "IMDQ", "Sex", "Year"), c("agegroup", "qimd", "sex", "year"))
chd.drates[sex=="M", sex:="Men"]
chd.drates[sex=="F", sex:="Women"]
chd.drates[, unique(agegroup)]
save(chd.drates, file="./Validation/kirk.RData")

chd.drates$logit     <- logit(chd.drates$chd.death.rates)
chd.drates$logit.se  <- abs(logit(chd.drates$chd.death.rates.lcl) - logit(chd.drates$chd.death.rates.ucl)) / (2*1.96)

# this should make the sensitivity analysis of the CHD death rates follow the same trajectory for each iteration
# which I think means that they are completely dependent, statistically speaking for this purpose
chd.drates.sens <- matrix(nrow=dim(chd.drates)[1],ncol=niter)
q=rnorm(niter,0,1)
for (i in 1:niter) {
  chd.drates.sens[,i] = inv.logit(chd.drates$logit + q[i]*chd.drates$logit.se)
}
chd.drates$chd.death.rates.sens <- chd.drates.sens

