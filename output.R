dir.create(
  path = "./Output/RF/", 
  recursive = T, 
  showWarnings = F
)

dir.create(
  path = "./Output/Other/",
  recursive = T, 
  showWarnings = F
)

cat(
  "Collecting risk factors output...\n"
)

all.files <- as.list(
  list.files(
    path = "./Output", 
    pattern = "riskfactors.rds", 
    full.names = T, 
    recursive = T
  )
) 

riskfactors <- rbindlist(
  lapply(
    all.files,
    readRDS
  ),
  fill = T
)

riskfactors[
  is.na(qimd) == T &  is.na(agegroup) == T,
  group := "S"
  ]

riskfactors[
  is.na(qimd) == T & is.na(agegroup) == F,
  group := "SA"
  ]

riskfactors[
  is.na(qimd) == F & is.na(agegroup) == T,
  group := "SQ"
  ]

riskfactors[
  is.na(qimd) == F & is.na(agegroup) == F,
  group := "SAQ"
  ]

riskfactors[
  sex == "1", 
  sex := "Men"
  ]

riskfactors[
  sex == "2", 
  sex := "Women"
  ]

save(
  riskfactors,
  file="./Output/RF/riskfactors.RData"
)

cat(
  "Collecting other causes mortality output...\n"
)

all.files <- as.list(
  list.files(
    path = "./Output",
    pattern = "other.mortal.rds",
    full.names = T,
    recursive = T
  )
) 

other.mortality <- rbindlist(
  lapply(
    all.files,
    readRDS
  ),
  fill = T
)

other.mortality[
  is.na(qimd) == T & is.na(agegroup) == T,
  group := "S"
  ]

other.mortality[
  is.na(qimd) == T & is.na(agegroup) == F,
  group := "SA"
  ]

other.mortality[
  is.na(qimd) == F & is.na(agegroup) == T,
  group := "SQ"
  ]

other.mortality[
  is.na(qimd) == F & is.na(agegroup) == F,
  group := "SAQ"
  ]

other.mortality[
  sex == "1", 
  sex := "Men"
  ]

other.mortality[
  sex == "2", 
  sex := "Women"
  ]

save(
  other.mortality,
  file = "./Output/Other/other.mortality.RData"
)

cat(
  "Calculating life expectancy...\n"
)

all.files <- as.list(
  list.files(
    path = "./Output",
    pattern = ".ind.mortal.rds",
    full.names = T,
    recursive = T
  )
) 

life.exp <- rbindlist(
  lapply(
    all.files,
    readRDS
  ), 
  fill = T
)

life.exp[
  sex == "1",
  sex := "Men"
  ]

life.exp[
  sex == "2", 
  sex := "Women"
  ]

# Life expectancy at birth
output <- vector("list", 4)

output[[1]] <- life.exp[
  ,
  .(
    mean = mean(age),
    sd = sd(age)
  ),
  by=.(
    sex,
    year.death,
    scenario,
    mc
  )
  ][
    ,
    group := "S"
    ]

output[[2]] <- life.exp[
  ,
  .(
    mean = mean(age),
    sd = sd(age)
  ),
  by = .(
    sex,
    agegroup,
    year.death,
    scenario,
    mc
  )
  ][
    ,
    group := "SA"
    ]

output[[3]] <- life.exp[
  ,
  .(
    mean = mean(age),
    sd = sd(age)
  ), 
  by=.(
    sex,
    qimd,
    year.death,
    scenario,
    mc)
  ][,
    group := "SQ"
    ]

output[[4]] <- life.exp[
  ,
  .(
    mean = mean(age),
    sd=sd(age)
  ),
  by=.(
    sex,
    agegroup,
    qimd,
    year.death,
    scenario,
    mc)
  ][
    ,
    group := "SAQ"
    ]

life.exp0 <- rbindlist(
  output,
  fill = T
)[
  ,
  year := year.death
]

life.exp0 <- merge(life.exp0, 
                   riskfactors[, list(pop, qimd, sex, agegroup, scenario, mc, group, year)], 
                   by= c("qimd", "sex", "agegroup", "scenario", "mc", "group", "year"), 
                   all.x = T)
save(life.exp0, file="./Output/Other/life.exp0.RData")

# Life expectancy at 65
output <- vector("list", 4)

output[[1]] <- life.exp[age > 65,.(mean=mean(age), sd=sd(age)), by=.(sex, year.death, scenario, mc)][, group := "S"]

output[[2]] <- life.exp[age > 65,.(mean=mean(age), sd=sd(age)), by=.(sex, agegroup, year.death, scenario, mc)][, group := "SA"]

output[[3]] <- life.exp[age > 65,.(mean=mean(age), sd=sd(age)), by=.(sex, qimd, year.death, scenario, mc)][, group := "SQ"]

output[[4]] <- life.exp[age > 65,.(mean=mean(age), sd=sd(age)), by=.(sex, agegroup, qimd, year.death, scenario, mc)][, group := "SAQ"]

life.exp65 <- rbindlist(output, fill = T)[, `:=` (year = year.death, mean = mean - 65)]
life.exp65 <- merge(life.exp65, 
                    riskfactors[, list(pop, qimd, sex, agegroup, scenario, mc, group, year)], 
                    by= c("qimd", "sex", "agegroup", "scenario", "mc", "group", "year"), 
                    all.x = T)
save(life.exp65, file="./Output/Other/life.exp65.RData")


if ("CHD" %in% diseasestoexclude) {
  cat(
    "Collecting CHD burden output...\n"
  )
  
  dir.create(
    path = "./Output/CHD/",
    recursive = T,
    showWarnings = F
  )
  
  all.files <- as.list(
    list.files(
      path = "./Output",
      pattern = "chd.burden.rds",
      full.names = T,
      recursive = T
    )
  ) 
  
  chd.burden <- rbindlist(
    lapply(
      all.files,
      readRDS
    ),
    fill = T
  )
  
  chd.burden[
    is.na(qimd) == T & is.na(agegroup) == T,
    group := "S"
    ]
  
  chd.burden[
    is.na(qimd) == T & is.na(agegroup) == F,
    group := "SA"
    ]
  
  chd.burden[
    is.na(qimd) == F & is.na(agegroup) == T,
    group := "SQ"
    ]
  
  chd.burden[
    is.na(qimd) == F & is.na(agegroup) == F,
    group := "SAQ"
    ]
  
  chd.burden[
    sex == "1",
    sex := "Men"
    ]
  
  chd.burden[
    sex == "2",
    sex := "Women"
    ]
  
  save(
    chd.burden,
    file="./Output/CHD/chd.burden.RData"
  )
  
  all.files <- as.list(
    list.files(
      path = "./Output",
      pattern = "chd.ind.incid.rds",
      full.names = T,
      recursive = T
    )
  ) 
  
  healthylife.exp.chd <- rbindlist(
    lapply(
      all.files,
      readRDS
    ),
    fill = T
  )
  
  healthylife.exp.chd[
    sex == "1",
    sex := "Men"
    ]
  
  healthylife.exp.chd[
    sex == "2",
    sex := "Women"
    ]
  
  healthylife.exp.chd[
    ,
    year := chd.incidence
    ]
  
  #write.csv(healthylife.exp, file="./Output/CHD/healthylife.exp.csv", row.names = F)
  #save(healthylife.exp.chd, file="./Output/CHD/indiv.incid.RData")
}

if ("stroke" %in% diseasestoexclude) {
  cat(
    "Collecting stroke burden output...\n"
  )
  dir.create(
    path = "./Output/Stroke/",
    recursive = T,
    showWarnings = F
  )
  
  all.files <- as.list(
    list.files(
      path = "./Output",
      pattern = "stroke.burden.rds",
      full.names = T,
      recursive = T
    )
  )
  
  stroke.burden <- rbindlist(
    lapply(
      all.files,
      readRDS
    ),
    fill = T
  )
  
  stroke.burden[
    is.na(qimd) == T & is.na(agegroup) == T, 
    group := "S"
    ]
  
  stroke.burden[
    is.na(qimd) == T & is.na(agegroup) == F, 
    group := "SA"
    ]
  
  stroke.burden[
    is.na(qimd) == F & is.na(agegroup) == T,
    group := "SQ"
    ]
  
  stroke.burden[
    is.na(qimd) == F & is.na(agegroup) == F, 
    group := "SAQ"
    ]
  
  stroke.burden[
    sex == "1",
    sex := "Men"
    ]
  
  stroke.burden[
    sex == "2", 
    sex := "Women"
    ]
  
  save(
    stroke.burden, 
    file="./Output/Stroke/stroke.burden.RData"
  )
  
  all.files <- as.list(
    list.files(
      path = "./Output",
      pattern = "stroke.ind.incid.rds",
      full.names = T,
      recursive = T
    )
  )
  
  healthylife.exp.stroke <- rbindlist(
    lapply(
      all.files, 
      readRDS
    ),
    fill = T
  )
  
  healthylife.exp.stroke[
    sex == "1",
    sex := "Men"
    ]
  
  healthylife.exp.stroke[
    sex == "2",
    sex := "Women"
    ]
  
  healthylife.exp.stroke[
    ,
    year := stroke.incidence
    ]
  
  #write.csv(healthylife.exp, file="./Output/stroke/healthylife.exp.csv", row.names = F)
  #save(healthylife.exp.stroke, file="./Output/Stroke/indiv.incid.RData")
}

if ("C34" %in% diseasestoexclude) {
  dir.create(path = "./Output/Lung Cancer/", recursive = T, showWarnings = F)
  
}

# Healthy life expectancy
cat(
  "Calculating healthy life expectancy...\n"
)
# Gather all objects starting with healthylife.exp.
healthylife.exp <- rbindlist(
  lapply(
    as.list(
      apropos(
        "healthylife.exp."
      )
    ),
    get),
  fill = T
)

output <- vector("list", 4)

output[[1]] <- healthylife.exp[
  ,
  .(
    mean = mean(age),
    sd = sd(age)
  )
  , by = .( 
    sex,
    year,
    scenario,
    mc
  )
  ][
    ,
    group := "S"
    ]


output[[2]] <- healthylife.exp[
  ,.(
    mean = mean(age),
    sd = sd(age)
  ), 
  by = .(
    sex,
    agegroup,
    year, 
    scenario,
    mc
  )
  ][
    ,
    group := "SA"
    ]

output[[3]] <- healthylife.exp[
  ,.(
    mean = mean(age),
    sd = sd(age)
  ), 
  by =.(
    sex,
    qimd,
    year,
    scenario,
    mc)
  ][
    ,
    group := "SQ"
    ]

output[[4]] <- healthylife.exp[
  ,.(
    mean = mean(age),
    sd = sd(age)
  ),
  by = .(
    sex,
    agegroup,
    qimd,
    year,
    scenario,
    mc
  )
  ][
    ,
    group := "SAQ"
    ]

hlife.exp <- rbindlist(
  output,
  fill = T
)

hlife.exp <- merge(
  hlife.exp, 
  riskfactors[
    ,
    list(
      pop,
      qimd,
      sex,
      agegroup,
      scenario,
      mc, 
      group,
      year
    )
    ], 
  by = c(
    "qimd",
    "sex",
    "agegroup",
    "scenario",
    "mc",
    "group",
    "year"
  ), 
  all.x = T
)

save(
  hlife.exp, 
  file="./Output/Other/hlife.exp.RData"
)

# Export graphs
dir.create(
  path = "./Output/Graphs/", 
  recursive = T, 
  showWarnings = F
  )

Graphs <- mclapply(
  Graphs.fn,
  function(f) f(),
  mc.preschedule = T,
  mc.cores = clusternumber
  ) # run all functions in the list

# To bypass ggplot bug that facet when operate within a function produce massive objects when saved
for (uu in grep(glob2rx("*.S"), names(Graphs))) {
  Graphs[[uu]] <- Graphs[[uu]] + facet_grid(sex ~ .)
} 

for (uu in grep(glob2rx("*.SA"), names(Graphs))) {
  Graphs[[uu]] <- Graphs[[uu]] + facet_grid(sex ~ agegroup)
} 

for (uu in grep(glob2rx("*.SA.WHO"), names(Graphs))) {
  Graphs[[uu]] <- Graphs[[uu]] + facet_grid(sex ~ .)
}

for (uu in grep(glob2rx("*.SA.ESP"), names(Graphs))) {
  Graphs[[uu]] <- Graphs[[uu]] + facet_grid(sex ~ .)
}

for (uu in grep(glob2rx("*.SQ"), names(Graphs))) {
  Graphs[[uu]] <- Graphs[[uu]] + facet_grid(sex ~ qimd)
} 

for (uu in grep(glob2rx("*.SAQ"), names(Graphs))) {
  Graphs[[uu]] <- Graphs[[uu]] + facet_grid(sex + qimd ~ agegroup)
}

for (uu in grep(glob2rx("*.SAQ.WHO"), names(Graphs))) {
  Graphs[[uu]] <- Graphs[[uu]] + facet_grid(sex ~ qimd)
}

for (uu in grep(glob2rx("*.SAQ.ESP"), names(Graphs))) {
  Graphs[[uu]] <- Graphs[[uu]] + facet_grid(sex ~ qimd)
}
save(Graphs, file="./Output/Graphs/Graphs.rda")

# Export pdfs
Graphs[sapply(Graphs, is.null)] <- NULL

mclapply(names(Graphs), 
         function(x) ggsave(filename=paste0(x,".pdf"),
                            plot=Graphs[[x]], 
                            path = "./Output/Graphs", 
                            width = 11.69,
                            height = 8.27), mc.cores = clusternumber)

# to extract data from graph use
# ggplot_build(Graphs$smoking.S)$data[[1]]

# Export tables
dir.create(
  path = "./Output/Tables/", 
  recursive = T, 
  showWarnings = F
)

Tables <- mclapply(
  Tables.fn, 
  function(f) f(),
  mc.preschedule = T,
  mc.cores = clusternumber) # run all functions in the list

save(
  Tables,
  file="./Output/Tables/Tables.rda"
)

lapply(
  names(
    Tables
  ), 
  function(x) write.csv(
    Tables[[x]],
    file = paste0("./Output/Tables/", x,".csv"),
    quote = T,
    row.names = F
  )
)


if (cleardirectories == T) {
  scenarios.list <- list.files(
    path = "./Scenarios",
    pattern = glob2rx("*.R"),
    full.names = F,
    recursive = F
  )
  
  scenario.dirs <- as.list(
    paste0(
      "./Output/",
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
