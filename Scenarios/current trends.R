#cmpfile("./Scenarios/current trends.R")
# This scenario is the fundamental one
# Assumes that the trends that where observed since 2001 will continue in the future
cat("current trends scenario\n\n")

if (i == (init.year-2011)) {
  # Function to apply after ageing
  post.ageing.scenario.fn <- function(i, env = my.env) {
    cat("Post ageing scenario function\n")
  }
}
