xx <-  suppressWarnings(
  rbind(fread("./Cancer Statistics/C16 DISMOD Males.csv", sep = ",", header = T, stringsAsFactors = F,
              skip = 3, nrow = 100)[, sex := 1],
        fread("./Cancer Statistics/C16 DISMOD Females.csv", sep = ",", header = T, stringsAsFactors = F, 
              skip = 3, nrow = 100)[, sex := 2]
  )[, `:=` (sex = factor(sex), age = as.integer(Age))]
)


C16incid <- setnames(copy(xx[, c(15, 14, 6), with = F]), "Incidence (rates)", "incidence")[, incidence := as.numeric(incidence)]
C16incid[, agegroup := agegroup.fn(age)]
setkey(C16incid, agegroup, sex)
#C16incid[C16tobpaf[C16fvpaf[C16saltpaf]], 
#         p0 := incidence * (1 - tobpaf) * (1 - fvpaf) * (1 - saltpaf)]

C16incid[C16fvpaf, 
         p0 := incidence * (1 - fvpaf)]
C16incid[is.na(p0), p0 := incidence]
C16incid[, agegroup := NULL]
setkey(C16incid, NULL)


POP[C16incid, p0 := p0]
POP[C16incid, inc := incidence]


POP[between(age, ageL, ageH) &
      c16.incidence == 0 &
      dice(.N) < p0 * c16.fv.rr,# * c16.fv.rr * c16.salt.rr *b, 
    c16.incidence := init.year + i] # b is the correction factor

POP[between(age, ageL, ageH) &
      c16.incidence < 2011 &
      dice(.N) < inc, 
    c17.incidence := init.year + i] 
POP[, table(c16.incidence)]
POP[, table(c17.incidence)]

POP[, c16.incidence := 0]
POP[, c17.incidence := 0]
POP[, p0 := NULL]
POP[, inc := NULL]
