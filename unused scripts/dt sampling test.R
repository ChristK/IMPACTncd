DT = data.table(sex=sample(1:2), chol=sample(1:1000,20), age=sample(5:8))
D1 <- DT[, sample(chol, 3), by=list(age, sex)]
D2 <- DT[DT[, sample(.I, 3), by=list(age, sex)][[2]],]
