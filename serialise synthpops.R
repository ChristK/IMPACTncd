#!/opt/gridware/apps/gcc/R/3.2.0/lib64/R/bin/Rscript
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


cat("Initialising IMPACTncd...\n\n")
options(warn = 1)

if (Sys.info()[1] == "Linux" && system("whoami", T ) == "mdxasck2") {
  setwd("~/IMPACTncd/")
  require(data.table)
  all.files <- list.files("./SynthPop", 
                          pattern = glob2rx("spop2011*.RData"), 
                          full.names = T)
  for (ff in seq_along(all.files)) {
    load(all.files[[ff]])
    SPOP2011[, age := as.integer(as.character(age))]
    SPOP2011[, numsmok := as.numeric(as.character(numsmok))]
    SPOP2011[, cigdyalCat := as.numeric(cigdyalCat)]
    SPOP2011[cigst1 == "2", numsmok:= 0.5]
    SPOP2011[expsmokCat != "0", expsmokCat:= "1"][,expsmokCat := factor(expsmokCat)]
    SPOP2011[, a30to06m := as.integer(as.character(a30to06m))]
    SPOP2011[age<15, cigst1 := "1"]
    SPOP2011[age > 99, age := 99] # for combatibility with lifetables
    saveRDS(SPOP2011, paste0("./SynthPop/spop2011-", ff, ".rds"))
  }
}
  
  
