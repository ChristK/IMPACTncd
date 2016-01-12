POP[, chd.incidence := 0L]
POP[, stroke.incidence := 0L]


POP[between(age, 40, 49) & invited == 1,
    .N] / POP[between(age, 40, 49) & eligible == 1,  .N]

POP[between(age, 50, 59) & invited == 1,
    .N] / POP[between(age, 50, 59) & eligible == 1,  .N]

POP[between(age, 60, 74) & invited == 1,
    .N] / POP[between(age, 60, 74) & eligible == 1,  .N]

POP[between(age, 70, 74) & invited == 1,
    .N] / POP[between(age, 70, 74) & eligible == 1,  .N]


POP[between(age, 40, 49) & invited == 1,
    .N] / POP[invited == 1,  .N]

POP[between(age, 50, 59) & invited == 1,
    .N] / POP[invited == 1,  .N]

POP[between(age, 60, 74) & invited == 1,
    .N] / POP[invited == 1,  .N]


# age distribution in those who receive health checks
# from table I Chang 2015
total <- 7584 + 6841  + 5984

7584  /total
6841  /total
19331 * 0.29  /total
1185  * 0.312  /total


POP[qrisk2 > 0.20 & coverage == 1 & age >59,
    .N] / POP[qrisk2 > 0.20 & coverage == 1 & age <60,  .N]
POP[qrisk2 < 0.10 & coverage == 1 & age >59,
    .N] / POP[qrisk2 < 0.10 & coverage == 1 & age <60,  .N]

p <- c(0, 0.7, 0.9, 1)
e <- c(0, 0.1, 0.2, 1)
fit.perc(p,e)

p <- c(0, 0.403, 0.667, 0.81, 0.895, 1)
e <- c(0, 0.05,  0.1,   0.15, 0.2,   1)
fit.perc(p,e)
#shape1 shape2 
#1.04  10.74 

hist(rbeta(1e4, 1.43, 16.77)) #qrisk
pbeta(0.2, 1.43, 16.77)

p <- c(0,  0.3716008, 0.7067961, 0.9814783, 1)
e <- c((40-39)/(75-39), (50-39)/(75-39), (60-39)/(75-39), (70-39)/(75-39), (74-39)/(75-39)) #age scaled
fit.perc(p,e)
hist(rbeta(1e4, 1.43, 1.94)*(75-39) + 39) #age (descaled)

x <- rbeta(1e4, 1.43, 16.77)
y <- rbeta(1e4, 1.43, 1.94)*(75-39) + 39
lm(y ~ x)

POP[, invited := 0]
tt1 <- POP[qrisk2 < 0.1 & eligible == 1, 
           sample(id, 0.6 * POP[eligible == 1, .N] * coverage, F, exp(age))]
tt2 <- POP[between(qrisk2, 0.1, 0.2) & eligible == 1 & invited == 0,
           sample(id, 0.3 * POP[eligible == 1 & invited == 0, .N] * coverage, F, exp(age))]
tt3 <- POP[qrisk2 > 0.2 & eligible == 1 & invited == 0,
           sample(id, 0.1 * POP[eligible == 1 & invited == 0, .N] * coverage, F, exp(age/5))]
POP[id %in% c(tt1, tt2, tt3), invited := 1L]
#POP[coverage == 1, hist(age)]
#POP[coverage == 1, hist(qrisk2)]
#POP[coverage == 1, mean(qrisk2)]
POP[qrisk2 > 0.20 & invited == 1,
    .N] / POP[invited == 1,  .N]
POP[qrisk2 >= 0.10 & invited == 1,
    .N] / POP[invited == 1,  .N]
POP[between(age, 40, 49) & invited == 1,
    .N] / POP[invited == 1,  .N]
POP[between(age, 50, 59) & invited == 1,
    .N] / POP[invited == 1,  .N]
POP[between(age, 60, 74) & invited == 1,
    .N] / POP[invited == 1,  .N]
POP[qrisk2 > 0.2 & invited == 1, .N, by = agegroup]



