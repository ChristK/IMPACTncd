# Start Simulation
# Define function for output dir
# ******************************************************************************************************************

# Year 0 is mid-year 2011 (or any other init.year) Dates of Fieldwork for HSE 2011: January 2011 - March 2012
sink(file = paste0(output.dir(), "log.txt"),
     append = T, 
     type = "output",
     split = F)

for (i in (init.year-2011):(yearstoproject + init.year - 2012)) {
    cat(paste0(rep("* ", 20)), "\n\n")
    cat(paste("Simulating mid ", 2011 + i, " to mid ", 2012 + i, "...\n\n", sep="")) 
    cat(paste0(rep("* ", 20)), "\n")    
        
    # Load scenario
    sys.source(file = paste0("./Scenarios/", scenarios.list[[iterations]]), my.env)
    
    # Start births engine
    cat(paste0(POP[,.N], " population\n"))
    sys.source(file = "./birth engine.R", my.env)
    cat(paste0(POP[,.N], " population\n"))
    
    # Start ageing engine
    sys.source(file = "./ageing engine.R", my.env)
    
    # Estimating incidence and mortality of modelled NCDs
    diseases <- sample(diseases) # randomly change the order of diseases each year
    lapply(diseases, function(f) f()) # run all functions in the list
    cat(paste0(POP[,.N], " population\n"))
   
cat("advance age\n")
    POP[, `:=`(age = age + 1)]  # make pop older
    cat("advance agegroups\n")
    agegroup.fn(POP)
}

sink()
gc()