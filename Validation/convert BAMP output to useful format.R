## IMPACTncd: A decision support tool for primary prevention of NCDs
## Copyright (C) 2015  Chris Kypridemos
 
## IMPACTncd is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program; if not, see <http://www.gnu.org/licenses/>
## or write to the Free Software Foundation, Inc., 51 Franklin Street,
## Fifth Floor, Boston, MA 02110-1301  USA.


# C16 ---------------------------------------------------------------------


all.files <- as.list(
  list.files(
    path = "./Validation/C16", 
    pattern = "*pr.txt", 
    full.names = T, 
    recursive = T
  )
) 

c16.val.mort <- lapply(all.files, read.table)
c16.val.mort <- lapply(c16.val.mort, setDT)
c16.val.mort <- lapply(c16.val.mort, setnames, paste0(2002:2112))

for (i in 1:length(c16.val.mort)) {
  lui  <-  data.table(t(c16.val.mort[[i]][1:6,]), keep.rownames = T)
  mean2 <- data.table(t(c16.val.mort[[i]][13:18,]), keep.rownames = T)
  uui <-  data.table(t(c16.val.mort[[i]][25:30,]), keep.rownames = T)
  setnames(lui, c("year", "35-44", "45-54", "55-64", "65-74", "75-84", "85+"))
  lui <- melt(lui, id.vars = "year", value.name = "lui", variable.name = "agegroup")
  setnames(uui, c("year", "35-44", "45-54", "55-64", "65-74", "75-84", "85+"))
  uui <- melt(uui, id.vars = "year",value.name = "uui", variable.name = "agegroup")
  setnames(mean2, c("year", "35-44", "45-54", "55-64", "65-74", "75-84", "85+"))
  mean2 <- melt(mean2, id.vars = "year", value.name = "mean", variable.name = "agegroup")
  mean2 <- merge(mean2, lui, by = c("year", "agegroup"))
  mean2 <- merge(mean2, uui, by = c("year", "agegroup"))
  c16.val.mort[[i]] <- mean2
  rm(mean2, lui, uui)
}

for (i in 1:5) {
  c16.val.mort[[i]][, sex := "Women"]
  c16.val.mort[[i]][, qimd := i]
  }
for (i in 6:10) {
  c16.val.mort[[i]][, sex := "Men"]
  c16.val.mort[[i]][, qimd := i - 5]
}



c16.drates <- rbindlist(c16.val.mort)
c16.drates[, year := as.integer(year)]
save(c16.drates, file="./Validation/c16.drates.RData")

# C34 ---------------------------------------------------------------------
all.files <- as.list(
  list.files(
    path = "./Validation/C34", 
    pattern = "*pr.txt", 
    full.names = T, 
    recursive = T
  )
) 

c34.val.mort <- lapply(all.files, read.table)
c34.val.mort <- lapply(c34.val.mort, setDT)
c34.val.mort <- lapply(c34.val.mort, setnames, paste0(2002:2112))

for (i in 1:length(c34.val.mort)) {
  lui  <-  data.table(t(c34.val.mort[[i]][1:6,]), keep.rownames = T)
  mean2 <- data.table(t(c34.val.mort[[i]][13:18,]), keep.rownames = T)
  uui <-  data.table(t(c34.val.mort[[i]][25:30,]), keep.rownames = T)
  setnames(lui, c("year", "35-44", "45-54", "55-64", "65-74", "75-84", "85+"))
  lui <- melt(lui, id.vars = "year", value.name = "lui", variable.name = "agegroup")
  setnames(uui, c("year", "35-44", "45-54", "55-64", "65-74", "75-84", "85+"))
  uui <- melt(uui, id.vars = "year",value.name = "uui", variable.name = "agegroup")
  setnames(mean2, c("year", "35-44", "45-54", "55-64", "65-74", "75-84", "85+"))
  mean2 <- melt(mean2, id.vars = "year", value.name = "mean", variable.name = "agegroup")
  mean2 <- merge(mean2, lui, by = c("year", "agegroup"))
  mean2 <- merge(mean2, uui, by = c("year", "agegroup"))
  c34.val.mort[[i]] <- mean2
  rm(mean2, lui, uui)
}

for (i in 1:5) {
  c34.val.mort[[i]][, sex := "Women"]
  c34.val.mort[[i]][, qimd := i]
}
for (i in 6:10) {
  c34.val.mort[[i]][, sex := "Men"]
  c34.val.mort[[i]][, qimd := i - 5]
}

c34.drates <- rbindlist(c34.val.mort)
c34.drates[, year := as.integer(year)]
save(c34.drates, file="./Validation/c34.drates.RData")

# Stroke ------------------------------------------------------------------


`F1pr` <- setDT(read.table("C:/Users/ckyprid/Dropbox/PhD/Models/IMPACTncd/Validation/F1pr.txt", quote="\""))
`F2pr` <- setDT(read.table("C:/Users/ckyprid/Dropbox/PhD/Models/IMPACTncd/Validation/F2pr.txt", quote="\""))
`F3pr` <- setDT(read.table("C:/Users/ckyprid/Dropbox/PhD/Models/IMPACTncd/Validation/F3pr.txt", quote="\""))
`F4pr` <- setDT(read.table("C:/Users/ckyprid/Dropbox/PhD/Models/IMPACTncd/Validation/F4pr.txt", quote="\""))
`F5pr` <- setDT(read.table("C:/Users/ckyprid/Dropbox/PhD/Models/IMPACTncd/Validation/F5pr.txt", quote="\""))
`M5pr` <- setDT(read.table("C:/Users/ckyprid/Dropbox/PhD/Models/IMPACTncd/Validation/M5pr.txt", quote="\""))
`M4pr` <- setDT(read.table("C:/Users/ckyprid/Dropbox/PhD/Models/IMPACTncd/Validation/M4pr.txt", quote="\""))
`M3pr` <- setDT(read.table("C:/Users/ckyprid/Dropbox/PhD/Models/IMPACTncd/Validation/M3pr.txt", quote="\""))
`M2pr` <- setDT(read.table("C:/Users/ckyprid/Dropbox/PhD/Models/IMPACTncd/Validation/M2pr.txt", quote="\""))
`M1pr` <- setDT(read.table("C:/Users/ckyprid/Dropbox/PhD/Models/IMPACTncd/Validation/M1pr.txt", quote="\""))

l <- apropos(glob2rx("*pr"), mode = "list")
for (i in l){
  setnames(get(i), paste0(2002:2112))
  lui  <-  data.table(t(get(i)[1:6,]), keep.rownames = T)
  mean2 <- data.table(t( get(i)[13:18,]), keep.rownames = T)
  uui <-  data.table(t(get(i)[25:30,]), keep.rownames = T)
  setnames(lui, c("year", "35-44", "45-54", "55-64", "65-74", "75-84", "85+"))
  lui <- melt(lui, value.name = "lui", variable.name = "agegroup")
  setnames(uui, c("year", "35-44", "45-54", "55-64", "65-74", "75-84", "85+"))
  uui <- melt(uui, value.name = "uui", variable.name = "agegroup")
  setnames(mean2, c("year", "35-44", "45-54", "55-64", "65-74", "75-84", "85+"))
  mean2 <- melt(mean2, value.name = "mean", variable.name = "agegroup")
  mean2 <- merge(mean2, lui, by = c("year", "agegroup"))
  mean2 <- merge(mean2, uui, by = c("year", "agegroup"))
  assign(i, mean2)
  rm(mean2, lui, uui)
}

for (ii in 1:5) {
  for (kk in c("M", "F")) {
    nam <- paste0(kk, ii, "pr")
    get(nam)[, qimd := 6-ii]
    get(nam)[, sex := ifelse(kk=="M", "Men", "Women")]
  }
}

stroke.drates <- rbindlist(as.list(mget(l)))
stroke.drates[, year := as.integer(year)]
save(stroke.drates, file="./Validation/stroke.drates.RData")
