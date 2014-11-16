POP[between(age, 16, ageH) & cigst1 == "4", 
    cigst1.temp := pred.xsmok(i, age, sex, qimd)]
POP[between(age, 16, ageH) & cigst1 == "4", summary(cigst1.temp)]
POP[between(age, 16, ageH) & cigst1 == "4", 
    cigst1.temp2 :=  dice(.N)]
POP[between(age, 16, ageH) & cigst1 == "4", summary(cigst1.temp2)]
POP[between(age, 16, ageH) & cigst1 == "4" & cigst1.temp2<cigst1.temp,  .N]
POP[between(age, 16, ageH) & cigst1 == "4" & cigst1.temp2<cigst1.temp,  .N]+
POP[between(age, 16, ageH) & cigst1 == "4" & cigst1.temp2>cigst1.temp,  .N]
POP[between(age, 16, ageH) & cigst1 == "4",  .N]

load("G:/Dropbox/PhD/Models/IMPACTncd/POPtest.RData")

POP[between(age, 16, ageH) & cigst1 == "4", 
    cigst1.temp := dice(.N) < pred.xsmok(i, age, sex, qimd)]
POP[cigst1.temp == T, .N]
POP[,summary(cigst1.temp)]
POP[,cigst1.temp := NULL]


library(data.table)
DT <- data.table(A=rep(0.3,100))
DT[, B := runif(.N) < A]

DT[B == T, .N]
DT[, summary(B)]

DT[, B := runif(.N) < A]

DT[B == T, .N]
DT[, summary(B)]

DT <- data.table(A=sample(1:2, 100, replace = T))
DT[A==1, .N]
DT[, summary(factor(A))]

DT[,A:=sample(1:2, 100, replace = T)]
DT[A==1, .N]
DT[, summary(factor(A))]
