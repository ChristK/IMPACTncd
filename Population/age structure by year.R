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


# population <- setDT(read.table("./Population/population.txt", header=T, sep="", skip=1, strip.white = T))
# population[Age=="110+", Age := "110"]
# population[, Age := as.numeric(as.character(Age))]
# population[Age > 99, Age := 99]
# population[, Female := sum(Female), by=.(Year, Age)]
# population[, Male := sum(Male), by=.(Year, Age)]
# population[, Total:=NULL]
# population = unique(population, by=c("Year", "Age"))
# #population = copy(population[Year>1980,])
# population = melt(population, id.vars=c("Year", "Age"), variable.name = "sex", value.name = "pop")
# setnames(population, c("year", "age", "sex", "pop"))
# population[sex=="Male", sex:="1"]
# population[sex=="Female", sex:="2"]
# population[, sex:=factor(sex)]
# population[, pct:= pop/sum(pop), by=.(year)]
# saveRDS(population, file = "./Population/population.rds")

# load ONS population estimates 2001-2013
population <- fread("./Population/MYEB1_detailed_population_estimates_series_UK_(0113).csv")
population <- population[country=="E"]
setnames(population, paste0("population_", 2001:2013), paste0(2001:2013))
population <- population[, lapply(.SD, sum), by=.(Age, sex), .SDcols=c(paste0(2001:2013))]


# load ONS population projections 2014-2021
population.proj <- fread("./Population/pop projections from ONS.csv",  header = T)
population.proj[, Area := NULL]
population <- merge(population, population.proj, by=c("Age", "sex"), all=T)
setnames(population, "Age", "age")

population <- fread("./Population/population.struct.csv",  header = T)
population <- melt(population, c("sex", "age"), variable.name = "year", value.name = "pop")
population[, `:=` (year = as.numeric(as.character(year)))]
breaks                   <- c(0, 1, seq(5, 85, 5), 90,Inf)
labels                   <- c("<1", "01-04", "05-09",
                              "10-14", "15-19", "20-24", 
                              "25-29", "30-34", "35-39", 
                              "40-44", "45-49", "50-54",
                              "55-59", "60-64", "65-69",
                              "70-74", "75-79", "80-84", 
                              "85-89", "90+")

# stratify by qimd
population[, agegroup := cut(age, 
                                    breaks = breaks, 
                                    labels = labels, 
                                    include.lowest = T, 
                                    right = F, 
                                    ordered_result = T)]


xx <- fread("./Population/popullation by qimd.csv",  header = T)[, lapply(.SD, function(x) x/sum(x)), by = .(year, sex)][, qimd := qimd * 15]
xx <- melt(xx, c("year", "sex", "qimd"), variable.name = "agegroup", value.name = "pct")
levels(xx$agegroup) <- gsub("\\s","", levels(xx$agegroup))




population <- rbind(copy(population[, qimd := 1]),
                           copy(population[, qimd := 2]),
                           copy(population[, qimd := 3]),
                           copy(population[, qimd := 4]),
                           copy(population[, qimd := 5]))

setkey(xx, agegroup, sex, qimd, year)
setkey(population, agegroup, sex, qimd, year)



population <- xx[population, roll = "nearest"]
population[, pop := round(pop * pct)] 
population[, agegroup := NULL] 


write.csv(population, file = "./Population/population.struct.csv", row.names = F)
