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
write.csv(population, file = "./Population/population.struct.csv", row.names = F)
