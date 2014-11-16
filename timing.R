started.at <- proc.time()

source('./Models/IMPACTncd/prototype 03.R')



cat(timetaken(started.at), "\n") 

# compare <- microbenchmark(dice(100000), dice2(100000), times = 1000)
# autoplot(compare)
