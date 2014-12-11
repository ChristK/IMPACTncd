#cmpfile("./CVD statistics.R")
# CVD statistics
if ("CHD" %in% diseasestoexclude) {
    CHDincid <- fread("./CVD Statistics/CHDincid.csv", 
                      sep = ",",
                      header = T, 
                      stringsAsFactors = T)[, `:=` (sex = factor(sex), agegroup = ordered(agegroup))]
    setkey(CHDincid, sex, agegroup)
    
    CHDpreval <- fread("./CVD Statistics/CHDpreval.csv", 
                       sep = ",",
                       header = T, 
                       stringsAsFactors = T)[, `:=` (sex = factor(sex), agegroup = ordered(agegroup))]
    setkey(CHDpreval, sex, agegroup)
    #CHDpreval[agegroup %in% c("<1   ", "01-04", "05-09", "10-14", "15-19"), prevalence :=0] # Set prevalence of younger than 20 to 0
    
    CHDsurv <- fread("./CVD Statistics/CHDsurv.csv", 
                     sep = ",", 
                     header = T, 
                     stringsAsFactors = T)[, `:=` (sex = factor(sex), agegroup = ordered(agegroup))]  
    setkey(CHDsurv, sex, agegroup)
    
    all.files <- list.files(path = "./CVD Statistics", 
                            pattern = glob2rx("chd*paf.csv"), 
                            full.names = T) # Create a list of files containing chd in their filenames
    readdata <- function(fn) {
        sex <- agegroup <- NULL
        nam <- gsub(".csv", "", fn)
        nam <- gsub("./CVD Statistics/", "", nam)
        assign(nam, fread(fn, sep=",", header = T, stringsAsFactors = T))
        dt <- get(nam)
        dt[, `:=` (sex = factor(sex), agegroup = ordered(agegroup))]
        keycols <- c("sex", "agegroup")
        setkeyv(dt, keycols)
        return(dt)
    }
    
    mylist <- sapply(all.files, readdata, simplify=F, USE.NAMES=T)
    names(mylist) <- gsub(".csv", "", names(mylist))
    names(mylist) <- gsub("./CVD Statistics/", "", names(mylist))
    
    list2env(mylist ,.GlobalEnv) # copy each object of the list to the global environment
    rm(all.files, readdata, mylist) # garbage cleaning
    
    CHDincid[chdbmipaf[chdcholpaf[chddiabpaf[chdetspaf[chdfvpaf[chdsbppaf[chdsmokepaf]]]]]], 
             p0 := incidence * (1 - bmipaf) *
                 (1 - cholpaf) * (1 - diabpaf) * 
                 (1 - etspaf) * (1 - fvpaf) * 
                 (1 - sbppaf) * (1 - tobaccopaf)]
}

# Do I have to separate between ischaemic and haemorrhagic? The risk factors seems more ore less the same.
if ("stroke" %in% diseasestoexclude) {
    strokeincid <- fread("./CVD Statistics/strokeincid.csv", 
                         sep = ",",
                         header = T, 
                         stringsAsFactors = T)[, `:=` (sex = factor(sex), agegroup = ordered(agegroup))]
    setkey(strokeincid, sex, agegroup)
   
    strokepreval <- fread("./CVD Statistics/strokepreval.csv", 
                       sep = ",",
                       header = T, 
                       stringsAsFactors = T)[, `:=` (sex = factor(sex), agegroup = ordered(agegroup))]
    setkey(strokepreval, sex, agegroup)
    
    strokesurv <- fread("./CVD Statistics/strokesurv.csv", 
                     sep = ",", 
                     header = T, 
                     stringsAsFactors = T)[, `:=` (sex = factor(sex), agegroup = ordered(agegroup))] 
    setkey(strokesurv, sex, agegroup)
    
    all.files <- list.files(path = "./CVD Statistics", 
                            pattern = glob2rx("stroke*paf.csv"), 
                            full.names = T) 
    
    readdata <- function(fn) {
        sex <- agegroup <- NULL
        nam <- gsub(".csv", "", fn)
        nam <- gsub("./CVD Statistics/", "", nam)
        assign(nam, fread(fn, sep=",", header = T, stringsAsFactors = T))
        dt <- get(nam)
        dt[, `:=` (sex = factor(sex), agegroup = ordered(agegroup))]
        keycols <- c("sex", "agegroup")
        setkeyv(dt, keycols)
        return(dt)
    }
    
    mylist <- sapply(all.files, readdata, simplify=F, USE.NAMES=T)
    names(mylist) <- gsub(".csv", "", names(mylist))
    names(mylist) <- gsub("./CVD Statistics/", "", names(mylist))
    
    list2env(mylist ,.GlobalEnv) # copy each object of the list to the global environment
    rm(all.files, readdata, mylist) # garbage cleaning
    
    strokeincid[strokebmipaf[strokecholpaf[strokediabpaf[strokeetspaf[strokefvpaf[strokesbppaf[strokesmokepaf]]]]]], 
             p0 := incidence * (1 - bmipaf) * 
                 (1 - cholpaf) * (1 - diabpaf) * 
                 (1 - etspaf) * (1 - fvpaf) * 
                 (1 - sbppaf) * (1 - tobaccopaf)]

}
