tt <- data.frame(n=rnorm(100))

tt2 <- data.frame(n=sample(tt$n, 110, replace=T))

tt$n1 <- "original"
tt2$n1 <- "sampled"
tt3 <- rbind(tt, tt2)

require(ggplot2)
print(ggplot(tt3, aes(n, fill = n1)) + geom_histogram(alpha = 0.3, aes(y = ..density..), position = 'identity'))
print(ggplot(tt3, aes(n, fill = n1)) + geom_density(alpha = 0.2))