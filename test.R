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
