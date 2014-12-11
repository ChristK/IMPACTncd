#cmpfile("./simulation.R")
# Year 0 is mid-year 2011 (or any other init.year) Dates of Fieldwork for HSE 2011: January 2011 - March 2012
sink(file = paste0(output.dir(), "log.txt"),
     append = T, 
     type = "output",
     split = F)

for (i in (init.year-2011):(yearstoproject + init.year - 2012)) {
    cat(paste0(rep("* ", 20)), "\n\n")
    cat(paste("Simulating mid ", 2011 + i, " to mid ", 2012 + i, "...\n", sep="")) 
    cat(paste0(Sys.time(), "\n\n"))
    cat(paste0(rep("* ", 20)), "\n\n")    
        
    # Load scenario
    loadcmp(file = paste0("./Scenarios/", scenarios.list[[iterations]],"c"), my.env)
    
    # Start births engine
    #cmpfile("./birth engine.R")
    #sys.source(file = "./birth engine.R", my.env)
    loadcmp(file = "./birth engine.Rc", my.env)
    
    # Start ageing engine
    #cmpfile("./ageing engine.R")
    #sys.source(file = "./ageing engine.R", my.env)
    loadcmp(file = "./ageing engine.Rc", my.env)
    
    # Estimating incidence and mortality of modelled NCDs
    diseases <- sample(diseases) # randomly change the order of diseases each year
    lapply(diseases, function(f) f()) # run all functions in the list
    #cat(paste0(POP[,.N], " population\n"))
   
    cat("Advance age\n")
    POP[, `:=`(age = age + 1)]  # make pop older
    agegroup.fn(POP)
}

sink()
gc()
