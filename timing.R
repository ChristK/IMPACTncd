started.at <- proc.time()
source(file = "./life table engine.R")


cat(timetaken(started.at), "\n") 

require(microbenchmark)
compare <- microbenchmark(source(file = "./post simulation functions.R"),
                          loadcmp(file = "./post simulation functions.Rc"), 
                          times = 10)
autoplot(compare)
