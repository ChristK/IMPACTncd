#cmpfile("./output.R")
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


dir.create(
  path = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/RF/", 
  recursive = T, 
  showWarnings = F
)

dir.create(
  path = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/Other/",
  recursive = T, 
  showWarnings = F
)

dir.create(
  path = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/Tables/", 
  recursive = T, 
  showWarnings = F
)

dir.create(
  path = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/Validation/", 
  recursive = T, 
  showWarnings = F
)


# Risk factors ------------------------------------------
cat("Collecting risk factors output...\n"
)

all.files <- as.list(
  list.files(
    path = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/", 
    pattern = "riskfactors.rds", 
    full.names = T, 
    recursive = T
  )
) 

riskfactors <- rbindlist(
  mclapply(
    all.files,
    readRDS,
    mc.cores = clusternumber
  ),
  T, T
)

riskfactors[
  is.na(qimd) == T & is.na(agegroup) == T & is.na(sex) == F,
  group := "S"
  ]
riskfactors[
  is.na(qimd) == T & is.na(agegroup) == F & is.na(sex) == F,
  group := "SA"
  ]
riskfactors[
  is.na(qimd) == F & is.na(agegroup) == T & is.na(sex) == F,
  group := "SQ"
  ]
riskfactors[
  is.na(qimd) == F & is.na(agegroup) == F & is.na(sex) == F,
  group := "SAQ"
  ]
riskfactors[
  is.na(sex) == T,
  group := "P"
  ]

riskfactors[, sex := factor(sex, c("1", "2"), c("Men" ,"Women"))]

lagtimes.dt <- fread("/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/lagtimes.csv")
lagtimes.dt[, mc := .I]

riskfactors[lagtimes.dt, on="mc", `:=` (year.cvdlag = year - cvd.lag, 
                                        year.calag  = year - cancer.lag)]
save(
  riskfactors,
  file = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/RF/riskfactors.RData"
)

pop.abs <- riskfactors[group=="SAQ",
                       sum(pop),
                       by = .(year,scenario, mc, sex, agegroup, qimd)
                       ][,
                         list(realpop = round(mean(V1) / pop.fraction),
                              pop = round(mean(V1))),
                         by = .(year, scenario, sex, agegroup, qimd, mc)] 
setkey(pop.abs, year, scenario, sex, agegroup, qimd, mc)

save(
  pop.abs,
  file="/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/RF/population.structure.RData"
)

#rm(riskfactors)
#gc()


# Individual trajectories -------------------------------------------------
cat("Collecting individual trajectories...\n"
)
all.files <- as.list(
  list.files(
    path = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output", 
    pattern = "indiv.traj.rds", 
    full.names = T, 
    recursive = T
  )
) 

indiv.traj <- rbindlist(
  mclapply(
    all.files,
    readRDS,
    mc.cores = clusternumber
  ),
  T, T
)

indiv.traj[, sex := factor(sex, c("1", "2"), c("Men" ,"Women"))]

save(
  indiv.traj,
  file="/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/Other/indiv.traj.RData"
)


# High risk ---------------------------------------------------------------
cat("Collecting high risk output...\n"
)

all.files <- as.list(
  list.files(
    path = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output", 
    pattern = "highrisk.rds", 
    full.names = T, 
    recursive = T
  )
) 

highrisk <- rbindlist(
  mclapply(
    all.files,
    readRDS,
    mc.cores = clusternumber
  ),
  T, T
)

if (nrow(highrisk)>0) {
  highrisk [
    is.na(qimd) == F & is.na(agegroup) == F,
    group := "SAQ"
    ]
  
  highrisk[, sex := factor(sex, c("1", "2"), c("Men" ,"Women"))]
  
  save(
    highrisk,
    file="/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/RF/highrisk.RData"
  )
}

#rm(highrisk)
#gc()


# Other cause mortality ---------------------------------------------------
cat("Collecting other causes mortality output...\n"
)

all.files <- as.list(
  list.files(
    path = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output",
    pattern = "other.mortal.rds",
    full.names = T,
    recursive = T
  )
) 

other.mortality <- rbindlist(
  mclapply(
    all.files,
    readRDS,
    mc.cores = clusternumber
  ),
  T, T
)

other.mortality[
  is.na(qimd) == T & is.na(agegroup) == T  & is.na(sex) == F,
  group := "S"
  ]
other.mortality[
  is.na(qimd) == T & is.na(agegroup) == F  & is.na(sex) == F,
  group := "SA"
  ]
other.mortality[
  is.na(qimd) == F & is.na(agegroup) == T  & is.na(sex) == F,
  group := "SQ"
  ]
other.mortality[
  is.na(qimd) == F & is.na(agegroup) == F  & is.na(sex) == F,
  group := "SAQ"
  ]
other.mortality[
  is.na(sex) == T,
  group := "P"
  ]

other.mortality[, sex := factor(sex, c("1", "2"), c("Men" ,"Women"))]

save(
  other.mortality,
  file = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/Other/other.mortality.RData"
)

#rm(other.mortality)
#gc()


# CHD burden --------------------------------------------------------------
if ("CHD" %in% diseasestoexclude) {
  cat("Collecting CHD burden output...\n"
  )
  
  dir.create(
    path = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/CHD/",
    recursive = T,
    showWarnings = F
  )
  
  all.files <- as.list(
    list.files(
      path = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output",
      pattern = "chd.burden.rds",
      full.names = T,
      recursive = T
    )
  ) 
  
  chd.burden <- rbindlist(
    mclapply(
      all.files,
      readRDS,
      mc.cores = clusternumber
    ),
    T, T
  )
  
  chd.burden[
    is.na(qimd) == T & is.na(agegroup) == T & is.na(sex) == F,
    group := "S"
    ]
  chd.burden[
    is.na(qimd) == T & is.na(agegroup) == F & is.na(sex) == F,
    group := "SA"
    ]
  chd.burden[
    is.na(qimd) == F & is.na(agegroup) == T & is.na(sex) == F,
    group := "SQ"
    ]
  chd.burden[
    is.na(qimd) == F & is.na(agegroup) == F & is.na(sex) == F,
    group := "SAQ"
    ]
  chd.burden[
    is.na(sex) == T,
    group := "P"
    ]
  
  chd.burden[, sex := factor(sex, c("1", "2"), c("Men" ,"Women"))]
  
  save(
    chd.burden,
    file="/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/CHD/chd.burden.RData"
  )
  
  #rm(chd.burden)
  #gc()
  
  #write.csv(healthylife.exp, file="/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/CHD/healthylife.exp.csv", row.names = F)
  #save(healthylife.exp.chd, file="/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/CHD/indiv.incid.RData")
}


# Stroke burden -----------------------------------------------------------
if ("stroke" %in% diseasestoexclude) {
  cat("Collecting stroke burden output...\n"
  )
  dir.create(
    path = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/Stroke/",
    recursive = T,
    showWarnings = F
  )
  
  all.files <- as.list(
    list.files(
      path = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output",
      pattern = "stroke.burden.rds",
      full.names = T,
      recursive = T
    )
  )
  
  stroke.burden <- rbindlist(
    mclapply(
      all.files,
      readRDS,
      mc.cores = clusternumber
    ),
    T, T
  )
  
  stroke.burden[
    is.na(qimd) == T & is.na(agegroup) == T & is.na(sex) == F, 
    group := "S"
    ]
  stroke.burden[
    is.na(qimd) == T & is.na(agegroup) == F & is.na(sex) == F, 
    group := "SA"
    ]
  stroke.burden[
    is.na(qimd) == F & is.na(agegroup) == T & is.na(sex) == F,
    group := "SQ"
    ]
  stroke.burden[
    is.na(qimd) == F & is.na(agegroup) == F & is.na(sex) == F, 
    group := "SAQ"
    ]
  stroke.burden[
    is.na(sex) == T, 
    group := "P"
    ]
  
  stroke.burden[, sex := factor(sex, c("1", "2"), c("Men" ,"Women"))]
  
  save(
    stroke.burden, 
    file="/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/Stroke/stroke.burden.RData"
  )
  
  #rm(stroke.burden)
  #gc()
  
  #write.csv(healthylife.exp, file="/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/stroke/healthylife.exp.csv", row.names = F)
  #save(healthylife.exp.stroke, file="/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/Stroke/indiv.incid.RData")
}


# CVD prevalence ----------------------------------------------------------
if ("CHD" %in% diseasestoexclude & "stroke" %in% diseasestoexclude) {
  cat("Collecting CVD prevelence output...\n"
  )
  
  dir.create(
    path = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/CVD/",
    recursive = T,
    showWarnings = F
  )
  
  all.files <- as.list(
    list.files(
      path = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output",
      pattern = "cvd.burden.rds",
      full.names = T,
      recursive = T
    )
  ) 
  
  cvd.burden <- rbindlist(
    mclapply(
      all.files,
      readRDS,
      mc.cores = clusternumber
    ),
    T, T
  )
  
  cvd.burden[,
             group := "SAQ"
             ]
  
  
  cvd.burden[, sex := factor(sex, c("1", "2"), c("Men" ,"Women"))]
  
  save(
    cvd.burden,
    file="/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/CVD/cvd.burden.RData"
  )
}


# Gastric cancer burden ---------------------------------------------------
if ("C16" %in% diseasestoexclude) {
  cat("Collecting gastric cancer burden output...\n"
  )
  
  dir.create(
    path = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/Gastric ca/",
    recursive = T,
    showWarnings = F
  )
  
  all.files <- as.list(
    list.files(
      path = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output",
      pattern = "c16.burden.rds",
      full.names = T,
      recursive = T
    )
  ) 
  
  c16.burden <- rbindlist(
    mclapply(
      all.files,
      readRDS,
      mc.cores = clusternumber
    ),
    T, T
  )
  
  c16.burden[
    is.na(qimd) == T & is.na(agegroup) == T & is.na(sex) == F,
    group := "S"
    ]
  c16.burden[
    is.na(qimd) == T & is.na(agegroup) == F & is.na(sex) == F,
    group := "SA"
    ]
  c16.burden[
    is.na(qimd) == F & is.na(agegroup) == T & is.na(sex) == F,
    group := "SQ"
    ]
  c16.burden[
    is.na(qimd) == F & is.na(agegroup) == F & is.na(sex) == F,
    group := "SAQ"
    ]
  c16.burden[
    is.na(sex) == T,
    group := "P"
    ]
  c16.burden[, sex := factor(sex, c("1", "2"), c("Men" ,"Women"))]
  
  save(
    c16.burden,
    file="/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/Gastric ca/c16.burden.RData"
  )
  
  #rm(c16.burden)
  #gc()
  
  #write.csv(healthylife.exp, file="/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/Gastric ca/healthylife.exp.csv", row.names = F)
  #save(healthylife.exp.c16, file="/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/Gastric ca/indiv.incid.RData")
}

# Lung cancer burden ---------------------------------------------------
if ("C34" %in% diseasestoexclude) {
  cat("Collecting lung cancer burden output...\n"
  )
  
  dir.create(
    path = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/Lung ca/",
    recursive = T,
    showWarnings = F
  )
  
  all.files <- as.list(
    list.files(
      path = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output",
      pattern = "c34.burden.rds",
      full.names = T,
      recursive = T
    )
  ) 
  
  c34.burden <- rbindlist(
    mclapply(
      all.files,
      readRDS,
      mc.cores = clusternumber
    ),
    T, T
  )
  
  c34.burden[
    is.na(qimd) == T & is.na(agegroup) == T & is.na(sex) == F,
    group := "S"
    ]
  c34.burden[
    is.na(qimd) == T & is.na(agegroup) == F & is.na(sex) == F,
    group := "SA"
    ]
  c34.burden[
    is.na(qimd) == F & is.na(agegroup) == T & is.na(sex) == F,
    group := "SQ"
    ]
  c34.burden[
    is.na(qimd) == F & is.na(agegroup) == F & is.na(sex) == F,
    group := "SAQ"
    ]
  c34.burden[
    is.na(sex) == T,
    group := "P"
    ]
  c34.burden[, sex := factor(sex, c("1", "2"), c("Men" ,"Women"))]
  
  save(
    c34.burden,
    file="/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/Lung ca/c34.burden.RData"
  )
  
  #rm(c34.burden)
  #gc()
  
  #write.csv(healthylife.exp, file="/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/Lung ca/healthylife.exp.csv", row.names = F)
  #save(healthylife.exp.c34, file="/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/Lung ca/indiv.incid.RData")
}

# Life expectancy ---------------------------------------------------------
cat("Calculating life expectancy...\n"
)

all.files <- as.list(
  list.files(
    path = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/",
    pattern = "ind.mortal.rds0",
    full.names = T,
    recursive = T
  )
) 

life.exp0 <- rbindlist(
  mclapply(
    all.files,
    readRDS,
    mc.cores = clusternumber
  ), 
  T, T
)

save(life.exp0, file="/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/Other/life.exp0.RData")

#rm(life.exp0)
#gc()


# Life expectancy at 65 ---------------------------------------------------
all.files <- as.list(
  list.files(
    path = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output",
    pattern = "ind.mortal.rds65",
    full.names = T,
    recursive = T
  )
) 

life.exp65 <- rbindlist(
  mclapply(
    all.files,
    readRDS,
    mc.cores = clusternumber
  ), 
  T, T
)

save(life.exp65, file="/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/Other/life.exp65.RData")

#rm(life.exp65)
#gc()


# Healthy life expectancy -------------------------------------------------
cat(
  "Calculating healthy life expectancy...\n"
)
# Gather all objects starting with healthylife.exp.
all.files <- as.list(
  list.files(
    path = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/",
    pattern = "ind.incid.rds",
    full.names = T,
    recursive = T
  )
) 

hlife.exp <- rbindlist(
  mclapply(
    all.files,
    readRDS,
    mc.cores = clusternumber
  ), 
  T, T
)

save(
  hlife.exp, 
  file="/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/Other/hlife.exp.RData"
)

gc()



# Export tables -----------------------------------------------------------
cat("Tables...\n")
Tables <- mclapply(
  Tables.fn, 
  function(f) f(),
  mc.preschedule = F,
  mc.cores = clusternumber) # run all functions in the list

save(
  Tables,
  file="/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/Tables/Tables.rda"
)

# lapply(
#   names(
#     Tables
#   ), 
#   function(x) write.csv(
#     Tables[[x]],
#     file = paste0("/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/Tables/", x,".csv"),
#     quote = T,
#     row.names = F
#   )
# )


# Export graphs from tables -----------------------------------------------
dir.create(
  path = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/Graphs.tbl/", 
  recursive = T, 
  showWarnings = F
)

Graphs <- mclapply(names(Tables), 
                   GraphsfromTables,
                   mc.cores = clusternumber)
names(Graphs) <- names(Tables)

save(Graphs, file="/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/Graphs.tbl/Graphs.tbl.rda")

# Export pdfs
mclapply(names(Graphs), 
         function(x) ggsave(filename=paste0(x,".pdf"),
                            plot=Graphs[[x]], 
                            path = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/Graphs.tbl", 
                            width = 11.69,
                            height = 8.27), mc.cores = clusternumber)


# Export graphs -----------------------------------------------------------
if (export.graphs == T) {
  dir.create(
    path = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/Graphs/", 
    recursive = T, 
    showWarnings = F
  )
  
  Graphs <- mclapply(
    Graphs.fn,
    function(f) f(),
    mc.preschedule = F,
    mc.cores = clusternumber
  ) # run all functions in the list
  
  Graphs[sapply(Graphs, is.null)] <- NULL
  
  # To bypass ggplot bug that facet when operate within a function produce massive objects when saved
  for (uu in grep(glob2rx("*.S"), names(Graphs))) {
    Graphs[[uu]] <- Graphs[[uu]] + facet_grid(sex ~ ., scales="free")
  } 
  
  for (uu in grep(glob2rx("*.SA"), names(Graphs))) {
    Graphs[[uu]] <- Graphs[[uu]] + facet_grid(sex ~ agegroup, scales="free")
  } 
  
  for (uu in grep(glob2rx("*.SA.WHO"), names(Graphs))) {
    Graphs[[uu]] <- Graphs[[uu]] + facet_grid(sex ~ ., scales="free")
  }
  
  for (uu in grep(glob2rx("*.SA.ESP"), names(Graphs))) {
    Graphs[[uu]] <- Graphs[[uu]] + facet_grid(sex ~ ., scales="free")
  }
  
  for (uu in grep(glob2rx("*.SQ"), names(Graphs))) {
    Graphs[[uu]] <- Graphs[[uu]] + facet_grid(sex ~ qimd, scales="free")
  } 
  
  for (uu in grep(glob2rx("*.SAQ"), names(Graphs))) {
    Graphs[[uu]] <- Graphs[[uu]] + facet_grid(sex + qimd ~ agegroup, scales="free")
  }
  
  for (uu in grep(glob2rx("*.SAQ.WHO"), names(Graphs))) {
    Graphs[[uu]] <- Graphs[[uu]] + facet_grid(sex ~ qimd, scales="free")
  }
  
  for (uu in grep(glob2rx("*.SAQ.ESP"), names(Graphs))) {
    Graphs[[uu]] <- Graphs[[uu]] + facet_grid(sex ~ qimd, scales="free")
  }
  
  for (uu in grep(glob2rx("*.P"), names(Graphs))) {
    Graphs[[uu]] <- Graphs[[uu]]
  } 
  
  
  save(Graphs, file="/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/Graphs/Graphs.rda")
  
  # Export pdfs
  mclapply(names(Graphs), 
           function(x) ggsave(filename=paste0(x,".pdf"),
                              plot=Graphs[[x]], 
                              path = "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/Graphs", 
                              width = 11.69,
                              height = 8.27), mc.cores = clusternumber)
  
  # to extract data from graph use
  # ggplot_build(Graphs$smoking.S)$data[[1]]
}


# Calculate DPP/CPP included in the analysis output. current code not dynamic

# Run validation ------------------------------------------------
source(file = "./validation.R")

# Clear intermediate files ------------------------------------------------
if (cleardirectories == T) {
  scenarios.list <- list.files(
    path = "./Scenarios",
    pattern = glob2rx("*.R"),
    full.names = F,
    recursive = F
  )
  
  scenario.dirs <- as.list(
    paste0(
      "/mnt/storage_slow/Model_Outputs/Responsibility_Deal/Output/",
      gsub(".R", "", scenarios.list
      )
    )
  )
  
  lapply(
    scenario.dirs,
    unlink,
    recursive =T,
    force = T
  )
}

rm(
  list = setdiff(
    ls(),
    lsf.str()
  )
) # remove everything but functions


