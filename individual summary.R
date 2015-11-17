#cmpfile("./individual summary.R")
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


cat("summarise individual trajectories ...\n")
if (i == init.year - 2011) {
  no43 <- POP[between(age, ageL, ageH), sample(id, 1)]
  indiv.traj <- vector("list", yearstoproject)
}

indiv.traj[[i + 2011 - init.year + 1]] <- POP[id == no43, 
                                              ][, year := i + init.year]

if (i == yearstoproject + init.year - 2012) {
  saveRDS(rbindlist(indiv.traj, T, T), 
          file = paste0(output.dir(), "indiv.traj.rds"))
}

cat("summarise individual output (HLE)...\n")
# hle
if (!is.null(diseasestoexclude)) {
  indiv.incid.summ <- rbindlist(indiv.incid, T, T)
  
  output <- vector("list", 3)
  
  if (exists("ind.incid.rds")) output[[1]] <- ind.incid.rds
  
  output[[2]] <- indiv.incid.summ[,
                                  .(
                                    mean = mean(age, na.rm = T),
                                    sd = sd(age, na.rm = T),
                                    n = .N
                                  ),
                                  by=.(
                                    sex,
                                    year,
                                    scenario,
                                    mc
                                  )
                                  ][
                                    ,
                                    group := "S"
                                    ]
  output[[3]] <- indiv.incid.summ[  ,
                      .(
                        mean = mean(age, na.rm = T),
                        sd = sd(age, na.rm = T),
                        n = .N
                      ), 
                      by=.(
                        sex,
                        qimd,
                        year,
                        scenario,
                        mc
                      )
                      ][,
                        group := "SQ"
                        ]
  
  ind.incid.rds <- rbindlist(output, T, T)
  
  if (i == yearstoproject + init.year - 2012) {
    ind.incid.rds[, sex := factor(sex, c("1", "2"), c("Men" ,"Women"))]
    saveRDS(ind.incid.rds, file = paste0(output.dir(), "ind.incid.rds"))
  }
}

# LE
cat("summarise individual output (LE)...\n")
indiv.mort.summ <- rbindlist(indiv.mort, T, T)

output <- vector("list", 3)

if (exists("ind.mortal.rds0")) output[[1]] <- ind.mortal.rds0
output[[2]] <- indiv.mort.summ[,
                               .(
                                 mean = mean(age, na.rm = T),
                                 sd = sd(age, na.rm = T),
                                 n = .N
                               ),
                               by=.(
                                 sex,
                                 year,
                                 scenario,
                                 mc
                               )
                               ][
                                 ,
                                  group := "S"
                                  ]

output[[3]] <- indiv.mort.summ[  ,
                               .(
                                 mean = mean(age, na.rm = T),
                                 sd = sd(age, na.rm = T),
                                 n = .N
                               ), 
                               by=.(
                                 sex,
                                 qimd,
                                 year,
                                 scenario,
                                 mc
                               )
                               ][,
                                 group := "SQ"
                                 ]

ind.mortal.rds0 <- rbindlist(output, T, T)

if (i == yearstoproject + init.year - 2012) {
  ind.mortal.rds0[, sex := factor(sex, c("1", "2"), c("Men" ,"Women"))]
  saveRDS(ind.mortal.rds0, file = paste0(output.dir(), "ind.mortal.rds0"))
}


output <- vector("list", 3)

if (exists("ind.mortal.rds65")) output[[1]] <- ind.mortal.rds65
output[[2]] <- indiv.mort.summ[age > 65,
                               .(
                                 mean = mean(age, na.rm = T),
                                 sd = sd(age, na.rm = T),
                                 n = .N
                               ),
                               by=.(
                                 sex,
                                 year,
                                 scenario,
                                 mc
                               )
                               ][
                                 ,
                                  group := "S"
                                  ]

output[[3]] <- indiv.mort.summ[age > 65 ,
                               .(
                                 mean = mean(age, na.rm = T),
                                 sd = sd(age, na.rm = T),
                                 n = .N
                               ), 
                               by=.(
                                 sex,
                                 qimd,
                                 year,
                                 scenario,
                                 mc
                               )
                               ][,
                                 group := "SQ"
                                 ]

ind.mortal.rds65 <- rbindlist(output, T, T)

if (i == yearstoproject + init.year - 2012) {
  ind.mortal.rds65[, sex := factor(sex, c("1", "2"), c("Men" ,"Women"))]
  saveRDS(ind.mortal.rds65, file = paste0(output.dir(), "ind.mortal.rds65"))
}

cat("Export CVD burden summary...\n\n")
cat(paste0(Sys.time(), "\n\n"))
if (i == init.year-2011) cvd.burden <- vector("list", yearstoproject)

cvd.burden[[(2011 - init.year + i) + 1]] <-
  output.cvd(POP, c("qimd", "sex", "agegroup"))

# cvd.burden[[(2011 - init.year + i) * 5 + 2]] <- 
#   output.cvd(POP, c("sex", "agegroup"))
# 
# cvd.burden[[(2011 - init.year + i) * 5 + 3]] <- 
#   output.cvd(POP, c("qimd", "sex"))
# 
# cvd.burden[[(2011 - init.year + i) * 5 + 4]] <- 
#   output.cvd(POP, c("sex"))
# 
# cvd.burden[[(2011 - init.year + i) * 5 + 5]] <- 
#   output.cvd(POP, c())

if (i == yearstoproject + init.year - 2012) {
  saveRDS(rbindlist(cvd.burden, T, T), file = paste0(output.dir(), "cvd.burden.rds"))
}


