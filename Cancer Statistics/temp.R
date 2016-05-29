## IMPACTncd: A decision support tool for primary prevention of NCDs
## Copyright (C) 2015 Chris Kypridemos
 
## IMPACTncd is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program; if not, see <http://www.gnu.org/licenses/>
## or write to the Free Software Foundation, Inc., 51 Franklin Street,
## Fifth Floor, Boston, MA 02110-1301 USA.

C34cases6 <- 
 data.table(year = 2006,
       site = "C34",
       agegroup = unique(agegroup.fn(0:85)),
       sex = rep(c("Men", "Women"), each = 19),
       cases = c(
        0, 1, 1, 0,	1, 1, 5, 12, 45, 137,	292, 646, 1350, 2129, 2648,	3086,	3412,	2547,	1689,
        0, 1, 0, 1,	3, 3, 3, 13, 41, 117,	275, 519, 1056, 1387, 1743,	2111,	2380,	2039,	1433
       )
 )

C34cases7 <- 
 data.table(year = 2007,
       site = "C34",
       agegroup = unique(agegroup.fn(0:85)),
       sex = rep(c("Men", "Women"), each = 19),
       cases = c(
        0,	0,	0,	2,	4,	3,	8,	15,	48,	114,	269,	612,	1271,	2122,	2675,	3063,	3285,	2677,	1793,
        0,	0,	0,	0,	0,	4,	6,	14,	40,	141,	243,	513,	1026,	1579,	1814,	2084,	2327,	2108,	1662
       )
 )

C34cases8 <- 
 data.table(year = 2008,
       site = "C34",
       agegroup = unique(agegroup.fn(0:85)),
       sex = rep(c("Men", "Women"), each = 19),
       cases = c(
       0, 1, 0, 3, 2, 3, 4,	25,	36,	120, 267,	617,	1222,	2111,	2756,	3256,	3325,	2641,	1993,
       0,	0, 0,	0, 2,	6, 6,	15,	42,	97,	 286,	549,	983,	1707,	1869,	2169,	2510,	2141,	1749
       
       )
 )

C34cases9 <- 
 fread("./Cancer Statistics/2009 cases.csv", header = T)
setnames(C34cases9, c("site", "sex", paste0(unique(agegroup.fn(0:85)))))
C34cases9[, year := 2009]
C34cases9 <- melt(C34cases9, c("site", "sex", "year"), variable.name = "agegroup", value.name = "cases")[site == "C34", ]
C34cases9[, sex := ifelse(sex=="1", "Men", "Women")]


C34cases10 <- 
 fread("./Cancer Statistics/2010 cases.csv", header = T)
setnames(C34cases10, c("site", "sex", paste0(unique(agegroup.fn(0:85)))))
C34cases10[, year := 2010]
C34cases10 <- melt(C34cases10, c("site", "sex", "year"), variable.name = "agegroup", value.name = "cases")[site == "C34", ]
C34cases10[, sex := ifelse(sex=="1", "Men", "Women")]


C34cases11 <- 
 fread("./Cancer Statistics/2011 cases.csv", header = T)
setnames(C34cases11, c("site", "sex", paste0(unique(agegroup.fn(0:85)))))
C34cases11[, year := 2011]
C34cases11 <- melt(C34cases11, c("site", "sex", "year"), variable.name = "agegroup", value.name = "cases")[site == "C34", ]
C34cases11[, sex := ifelse(sex=="1", "Men", "Women")]

C34cases12 <- 
 fread("./Cancer Statistics/2012 cases.csv", header = T)
setnames(C34cases12, c("site", "sex", paste0(unique(agegroup.fn(0:85)))))
C34cases12[, year := 2012]
C34cases12 <- melt(C34cases12, c("site", "sex", "year"), variable.name = "agegroup", value.name = "cases")[site == "C34", ]
C34cases12[, sex := ifelse(sex=="1", "Men", "Women")]

C34cases <- 
 rbind(C34cases6, C34cases7, C34cases8, C34cases9, C34cases10, C34cases11, C34cases12)

C34cases[is.na(cases), cases := 0]

write.csv(C34cases, "./Cancer Statistics/C34cases.csv", row.names = F)

