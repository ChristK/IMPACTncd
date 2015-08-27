#!/opt/gridware/apps/gcc/R/3.2.0/lib64/R/bin/Rscript

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
  
  
