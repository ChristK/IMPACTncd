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

C16cases6 <- 
  data.table(year = 2006,
             site = "C16",
             agegroup = unique(agegroup.fn(0:85)),
             sex = rep(c("Men", "Women"), each = 19),
             cases = c(
               0,	0,	0,	0,	1,	0,	6,	12,	24,	58,	88,	134,	261,	351,	512,	693,	808,	638,	449,
               0,	0,	0,	0,	1,	6,	5,	6,	28,	25,	45,	 44,	 88,	142,	208,	289,	371,	435,	488
             )
  )

C16cases7 <- 
  data.table(year = 2007,
             site = "C16",
             agegroup = unique(agegroup.fn(0:85)),
             sex = rep(c("Men", "Women"), each = 19),
             cases = c(
               0,	0,	0,	0,	0,	1,	3,	7,	21,	56,	81,	128,	249,	383,	572,	724,	769,	650,	462,
               0,	0,	0,	1,	0,	0,	8,	9,	18,	35,	44,	67,	108,	138,	206,	270,	379,	445,	496
             )
  )

C16cases8 <- 
  data.table(year = 2008,
             site = "C16",
             agegroup = unique(agegroup.fn(0:85)),
             sex = rep(c("Men", "Women"), each = 19),
             cases = c(
               0,	0,	0,	0,	0,	6,	10,	13,	25,	36,	95,	149,	224,	365,	463,	662,	770,	659,	474,
               0,	0,	0,	1,	2,	1,	7,	10,	11,	41,	44,	 61,	 92,	126,	214,	288,	363,	396,	477
             )
  )

C16cases9 <- 
  fread("./Cancer Statistics/2009 cases.csv", header = T)
setnames(C16cases9, c("site", "sex", paste0(unique(agegroup.fn(0:85)))))
C16cases9[, year := 2009]
C16cases9 <- melt(C16cases9, c("site", "sex", "year"), variable.name = "agegroup", value.name = "cases")[site == "C16", ]
C16cases9[, sex := ifelse(sex=="1", "Men", "Women")]


C16cases10 <- 
  fread("./Cancer Statistics/2010 cases.csv", header = T)
setnames(C16cases10, c("site", "sex", paste0(unique(agegroup.fn(0:85)))))
C16cases10[, year := 2010]
C16cases10 <- melt(C16cases10, c("site", "sex", "year"), variable.name = "agegroup", value.name = "cases")[site == "C16", ]
C16cases10[, sex := ifelse(sex=="1", "Men", "Women")]


C16cases11 <- 
  fread("./Cancer Statistics/2011 cases.csv", header = T)
setnames(C16cases11, c("site", "sex", paste0(unique(agegroup.fn(0:85)))))
C16cases11[, year := 2011]
C16cases11 <- melt(C16cases11, c("site", "sex", "year"), variable.name = "agegroup", value.name = "cases")[site == "C16", ]
C16cases11[, sex := ifelse(sex=="1", "Men", "Women")]

C16cases12 <- 
  fread("./Cancer Statistics/2012 cases.csv", header = T)
setnames(C16cases12, c("site", "sex", paste0(unique(agegroup.fn(0:85)))))
C16cases12[, year := 2012]
C16cases12 <- melt(C16cases12, c("site", "sex", "year"), variable.name = "agegroup", value.name = "cases")[site == "C16", ]
C16cases12[, sex := ifelse(sex=="1", "Men", "Women")]

C16cases <- 
  rbind(C16cases6, C16cases7, C16cases8, C16cases9, C16cases10, C16cases11, C16cases12)

C16cases[is.na(cases), cases := 0]

write.csv(C16cases, "./Cancer Statistics/C16cases.csv", row.names = F)

