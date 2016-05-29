#cmpfile("./diseases epidemiology.R")
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


# CHD statistics ----------------------------------------------------------
if ("CHD" %in% diseasestoexclude) {
  fatality30chd <- setkey(
    fread(
      "./CVD Statistics/fatality30chd.csv", 
      stringsAsFactors = T, 
      colClasses = c("factor", "factor", 
                     "numeric")
    ),
    agegroup, sex
  )
  
  xx <-  suppressWarnings(
    rbind(fread(paste0("./CVD Statistics/CHD DISMOD Males " ,init.year ,".csv"),
                sep = ",", header = T, stringsAsFactors = F,
                skip = 3, nrows = 101)[, sex := 1],
          fread(paste0("./CVD Statistics/CHD DISMOD Females " ,init.year ,".csv"), 
                sep = ",", header = T, stringsAsFactors = F, 
                skip = 3, nrows = 101)[, sex := 2]
    )[, `:=` (sex = factor(sex), age = as.integer(Age))]
  )
  
  CHDincid <- setnames(copy(xx[, c(15, 14, 6), with = F]), "Incidence (rates)", "incidence")[, incidence := as.numeric(incidence)]
  
  CHDpreval <- setnames(copy(xx[, c(15, 14, 7), with = F]), "Prevalence (rates)", "prevalence")[, prevalence := as.numeric(prevalence)]
  
  CHDsurv <- setnames(copy(xx[, c(15, 14, 9), with = F]), "Case fatality (rates)", "fatality")[, fatality := as.numeric(fatality)]
}

# Stroke statistics -------------------------------------------------------
# Do I have to separate between ischaemic and haemorrhagic? The risk factors seems more ore less the same.
if ("stroke" %in% diseasestoexclude) {
  fatality30stroke <- setkey(
    fread("./CVD Statistics/fatality30stroke.csv", 
          stringsAsFactors = F, 
          colClasses = c("factor", "factor",
                         "numeric")
    ),
    agegroup, sex
  )
  
  xx <-  suppressWarnings(
    rbind(fread(paste0("./CVD Statistics/Stroke DISMOD Males ", init.year ,".csv"), 
                sep = ",", header = T, stringsAsFactors = F,
                skip = 3, nrows = 101)[, sex := 1],
          fread(paste0("./CVD Statistics/Stroke DISMOD Females ", init.year, ".csv"), 
                sep = ",", header = T, stringsAsFactors = F,
                skip = 3, nrows = 101)[, sex := 2]
    )[, `:=` (sex = factor(sex), age = as.integer(Age))]
  )
  
  strokeincid <- setnames(copy(xx[, c(15, 14, 6), with = F]), "Incidence (rates)", "incidence")[, incidence := as.numeric(incidence)]
  
  strokepreval <- setnames(copy(xx[, c(15, 14, 7), with = F]), "Prevalence (rates)", "prevalence")[, prevalence := as.numeric(prevalence)]
  
  strokesurv <- setnames(copy(xx[, c(15, 14, 9), with = F]), "Case fatality (rates)", "fatality")[, fatality := as.numeric(fatality)]
}

# Gastric cancer statistics -----------------------------------------------
if ("C16" %in% diseasestoexclude) {
  xx <-  suppressWarnings(
    rbind(fread(paste0("./Cancer Statistics/C16 DISMOD Males ", init.year, ".csv"), 
                sep = ",", header = T, stringsAsFactors = F,
                skip = 3, nrows = 101)[, sex := 1],
          fread(paste0("./Cancer Statistics/C16 DISMOD Females ", init.year, ".csv"), 
                sep = ",", header = T, stringsAsFactors = F, 
                skip = 3, nrows = 101)[, sex := 2]
    )[, `:=` (sex = factor(sex), age = as.integer(Age))]
  )
  
  
  C16incid  <- setnames(copy(xx[, c(15, 14, 6), with = F]), "Incidence (rates)", "incidence")[, incidence := as.numeric(incidence)]
  
  C16preval <- setnames(copy(xx[, c(15, 14, 7), with = F]), "Prevalence (rates)", "prevalence")[, prevalence := as.numeric(prevalence)]
  
  C16remis  <- setnames(copy(xx[, c(15, 14, 8), with = F]), "Remission (rates)", "remission")[, remission := as.numeric(remission)]
  
  C16fatal  <- setnames(copy(xx[, c(15, 14, 9), with = F]), "Case fatality (rates)", "fatality")[, fatality := as.numeric(fatality)]
}

# Lung cancer statistics --------------------------------------------------
# Estimate P0(incidence if all risks at recommended (optimal) level) and survival
if ("C34" %in% diseasestoexclude) {
  xx <-  suppressWarnings(
    rbind(fread(paste0("./Cancer Statistics/C34 DISMOD Males ", init.year, ".csv"), 
                sep = ",", header = T, stringsAsFactors = F,
                skip = 3, nrows = 101)[, sex := 1],
          fread(paste0("./Cancer Statistics/C34 DISMOD Females ", init.year, ".csv"), 
                sep = ",", header = T, stringsAsFactors = F, 
                skip = 3, nrows = 101)[, sex := 2]
    )[, `:=` (sex = factor(sex), age = as.integer(Age))]
  )
  
  
  C34incid  <- setnames(copy(xx[, c(14, 13, 5), with = F]), "Incidence (rates)", "incidence")[, incidence := as.numeric(incidence)]
  
  C34preval <- setnames(copy(xx[, c(14, 13, 6), with = F]), "Prevalence (rates)", "prevalence")[, prevalence := as.numeric(prevalence)]
  
  C34remis  <- setnames(copy(xx[, c(14, 13, 7), with = F]), "Remission (rates)", "remission")[, remission := as.numeric(remission)]
  
  C34fatal  <- setnames(copy(xx[, c(14, 13, 8), with = F]), "Case fatality (rates)", "fatality")[, fatality := as.numeric(fatality)]
}

if (exists("nam")) rm(nam)
if (exists("xx")) rm(xx)


