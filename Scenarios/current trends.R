# This scenario is the fundamental one
# Assumes that the trends that where observed since 2001 will continue in the future
cat("current trends scenario\n\n")

# Load prediction equations
if (i == (init.year-2011)) {
  
  # Load RF trajectoy functions
  #cmpfile("./risk factor trajectories.R")
  #sys.source(file = "./risk factor trajectories.R", my.env)
  loadcmp(file = "./risk factor trajectories.Rc", my.env)
  
  # Function to apply after ageing
  post.ageing.scenario.fn <- function(i) {
    cat("Post ageing scenario function\n")
  }
}
