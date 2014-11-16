# Define function to summarise output RF
MC.mean <- function(m, sd, n, ...) {
    m <- rnorm(n*10000, m, sd)
    return(list(mean= mean(m, na.rm=T),
                lui = quantile(m, probs = 0.025, na.rm = T),
                uui = quantile(m, probs = 0.975, na.rm = T)))
}




