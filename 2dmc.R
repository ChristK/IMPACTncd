#cmpfile("./2dmc.R")
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


cat("Sample RR values for 2d Monte Carlo\n\n")

cvd.lag    <- cvd.lag.l[[counter[[iterations]]]]
cancer.lag <- cancer.lag.l[[counter[[iterations]]]]

atorv.eff        <- atorv.eff.l[[counter[[iterations]]]]
smoking.decr     <- smoking.decr.l[[counter[[iterations]]]]
bmi.rate.decr    <- bmi.rate.decr.l[[counter[[iterations]]]]
sbp.decr         <- sbp.decr.l[[counter[[iterations]]]]
fv.decr          <- fv.decr.l[[counter[[iterations]]]]
persistence      <- persistence.l[[counter[[iterations]]]]
p1               <- p1.l[[counter[[iterations]]]]
p2               <- p2.l[[counter[[iterations]]]]
  
if ("CHD" %in% diseasestoexclude) {
  tobacco.rr.chd <- chd.tobacco.rr.l[.id == counter[[iterations]]]
  chd.ets.rr.mc  <- chd.ets.rr.l[[counter[[iterations]]]]
  sbp.rr.chd     <- chd.sbp.rr.l[.id == counter[[iterations]]]
  chol.rr.chd    <- chd.chol.rr.l[.id == counter[[iterations]]]
  chd.bmi.rr.mc  <- chd.bmi.rr.l[.id == counter[[iterations]]]
  chd.diab.rr.mc <- chd.diab.rr.l[.id == counter[[iterations]]]
  chd.fv.rr.mc   <- chd.fv.rr.l[[counter[[iterations]]]]
  pa.rr.chd      <- chd.pa.rr.l[.id == counter[[iterations]]]
}

if ("stroke" %in% diseasestoexclude) {
  tobacco.rr.stroke <- stroke.tobacco.rr.l[.id == counter[[iterations]]]
  stroke.ets.rr.mc  <- stroke.ets.rr.l[[counter[[iterations]]]]
  sbp.rr.stroke     <- stroke.sbp.rr.l[.id == counter[[iterations]]]
  chol.rr.stroke    <- stroke.chol.rr.l[.id == counter[[iterations]]]
  stroke.bmi.rr.mc  <- stroke.bmi.rr.l[.id == counter[[iterations]]]
  stroke.diab.rr.mc <- stroke.diab.rr.l[.id == counter[[iterations]]]
  stroke.fv.rr.mc   <- stroke.fv.rr.l[[counter[[iterations]]]]
  pa.rr.stroke      <- stroke.pa.rr.l[.id == counter[[iterations]]]
}

if ("C34" %in% diseasestoexclude) {
  c34.ets.rr.mc <- c34.ets.rr.mc.l[[counter[[iterations]]]] 
  c34.fv.rr.mc <- c34.fv.rr.mc.l[[counter[[iterations]]]]
}

if ("C16" %in% diseasestoexclude) {
  c16.salt.optim  <- c16.salt.optim.l[[counter[[iterations]]]]
  c16.salt.mr     <- c16.salt.mr.l[[counter[[iterations]]]]
  c16.tob.rr.mc   <- c16.tob.rr.mc.l[[counter[[iterations]]]]
  c16.extob.rr.mc <- c16.extob.rr.mc.l[[counter[[iterations]]]]
  c16.fv.rr.mc    <- c16.fv.rr.mc.l[.id == counter[[iterations]]]
  c16.salt.rr.mc  <- c16.salt.rr.mc.l[.id == counter[[iterations]]]
  c16.bmi.rr.mc   <- c16.bmi.rr.mc.l[counter[[iterations]]]
}

