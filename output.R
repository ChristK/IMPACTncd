dir.create(path = "./Output/RF/", recursive = T, showWarnings = F)
dir.create(path = "./Output/Other/", recursive = T, showWarnings = F)

cat("Collecting risk factors output...\n")
all.files <- as.list(list.files(path = "./Output", pattern = "riskfactors.rds", full.names = T, recursive = T)) 
riskfactors <- rbindlist(lapply(all.files, readRDS), fill=T)
riskfactors[is.na(qimd) == T & is.na(agegroup) == T, group := "S"]
riskfactors[is.na(qimd) == T & is.na(agegroup) == F, group := "SA"]
riskfactors[is.na(qimd) == F & is.na(agegroup) == T, group := "SQ"]
riskfactors[is.na(qimd) == F & is.na(agegroup) == F, group := "SAQ"]
riskfactors[sex == "1", sex := "Men"]
riskfactors[sex == "2", sex := "Women"]
#write.csv(riskfactors, file="./Output/RF/riskfactors.csv", row.names = F)
save(riskfactors, file="./Output/RF/riskfactors.RData")

cat("Collecting other causes mortality output...\n")
all.files <- as.list(list.files(path = "./Output", pattern = "other.mortal.rds", full.names = T, recursive = T)) 
other.mortality <- rbindlist(lapply(all.files, readRDS), fill=T)
other.mortality[is.na(qimd) == T & is.na(agegroup) == T, group := "S"]
other.mortality[is.na(qimd) == T & is.na(agegroup) == F, group := "SA"]
other.mortality[is.na(qimd) == F & is.na(agegroup) == T, group := "SQ"]
other.mortality[is.na(qimd) == F & is.na(agegroup) == F, group := "SAQ"]
other.mortality[sex == "1", sex := "Men"]
other.mortality[sex == "2", sex := "Women"]
#write.csv(other.mortality, file="./Output/Other/other.mortality.csv", row.names = F)
save(other.mortality, file="./Output/Other/other.mortality.RData")

cat("Calculating life expectancy...\n")
all.files <- as.list(list.files(path = "./Output", pattern = ".ind.mortal.rds", full.names = T, recursive = T)) 
life.exp <- rbindlist(lapply(all.files, readRDS), fill=T)
life.exp[sex == "1", sex := "Men"]
life.exp[sex == "2", sex := "Women"]
#write.csv(life.exp, file="./Output/Other/life.exp.csv", row.names = F)
save(life.exp, file="./Output/Other/life.exp.RData")

if ("CHD" %in% diseasestoexclude) {
    cat("Collecting CHD burden output...\n")
    dir.create(path = "./Output/CHD/", recursive = T, showWarnings = F)
    all.files <- as.list(list.files(path = "./Output", pattern = "chd.burden.rds", full.names = T, recursive = T)) 
    chd.burden <- rbindlist(lapply(all.files, readRDS), fill = T)
    chd.burden[is.na(qimd) == T & is.na(agegroup) == T, group := "S"]
    chd.burden[is.na(qimd) == T & is.na(agegroup) == F, group := "SA"]
    chd.burden[is.na(qimd) == F & is.na(agegroup) == T, group := "SQ"]
    chd.burden[is.na(qimd) == F & is.na(agegroup) == F, group := "SAQ"]
    chd.burden[sex == "1", sex := "Men"]
    chd.burden[sex == "2", sex := "Women"]
    #write.csv(chd.burden, file="./Output/CHD/chd.burden.csv", row.names = F)
    save(chd.burden, file="./Output/CHD/chd.burden.RData")
    
    cat("Calculating healthy life expectancy...\n")
    all.files <- as.list(list.files(path = "./Output", pattern = "chd.ind.incid.rds", full.names = T, recursive = T)) 
    healthylife.exp <- rbindlist(lapply(all.files, readRDS), fill=T)
    healthylife.exp[sex == "1", sex := "Men"]
    healthylife.exp[sex == "2", sex := "Women"]
    #write.csv(healthylife.exp, file="./Output/CHD/healthylife.exp.csv", row.names = F)
    save(healthylife.exp, file="./Output/CHD/healthylife.exp.RData")
}

if ("stroke" %in% diseasestoexclude) {
    cat("Collecting stroke burden output...\n")
    dir.create(path = "./Output/Stroke/", recursive = T, showWarnings = F)
    all.files <- as.list(list.files(path = "./Output", pattern = "stroke.burden.rds", full.names = T, recursive = T)) 
    stroke.burden <- rbindlist(lapply(all.files, readRDS), fill = T)
    stroke.burden[is.na(qimd) == T & is.na(agegroup) == T, group := "S"]
    stroke.burden[is.na(qimd) == T & is.na(agegroup) == F, group := "SA"]
    stroke.burden[is.na(qimd) == F & is.na(agegroup) == T, group := "SQ"]
    stroke.burden[is.na(qimd) == F & is.na(agegroup) == F, group := "SAQ"]
    stroke.burden[sex == "1", sex := "Men"]
    stroke.burden[sex == "2", sex := "Women"]
    #write.csv(stroke.burden, file="./Output/Stroke/stroke.burden.csv", row.names = F)
    save(stroke.burden, file="./Output/Stroke/stroke.burden.RData")
    
    cat("Calculating healthy life expectancy...\n")
    all.files <- as.list(list.files(path = "./Output", pattern = "stroke.ind.incid.rds", full.names = T, recursive = T)) 
    healthylife.exp <- rbindlist(lapply(all.files, readRDS), fill=T)
    healthylife.exp[sex == "1", sex := "Men"]
    healthylife.exp[sex == "2", sex := "Women"]
    #write.csv(healthylife.exp, file="./Output/stroke/healthylife.exp.csv", row.names = F)
    save(healthylife.exp, file="./Output/Stroke/healthylife.exp.RData")
}

if ("C34" %in% diseasestoexclude) {
    dir.create(path = "./Output/Lung Cancer/", recursive = T, showWarnings = F)
    
}

if (cleardirectories == T) {
    scenarios.list <- list.files(path = "./Scenarios", pattern = glob2rx("*.R"), full.names = F, recursive = F)
    scenario.dirs <- as.list(paste0("./Output/", gsub(".R", "", scenarios.list)))
    lapply(scenario.dirs, unlink, recursive =T, force = T)
}

rm(list = setdiff(ls(), lsf.str())) # remove everything but functions
