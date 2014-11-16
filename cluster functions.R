haha <- paste(sample(c(rep(0:9,each=5),LETTERS,letters),12,replace=T),collapse='')   

# Define function for output dir
output.dir <- function() {
    paste0("./Output/", gsub(".R", "", scenarios.list[[iterations]]), "/", haha , "/")
}



dir.create(path = output.dir(), recursive = T) # create a unique directory for each run of each scenario

# Define list of function to run for each diseases models
diseases <- list(
    chd = function() 
        if ("CHD" %in% get("diseasestoexclude", parent.frame())) sys.source(file = "./chd model.R", my.env),
    stroke = function() 
        if ("stroke" %in% get("diseasestoexclude", parent.frame())) sys.source(file = "./stroke model.R", my.env),
    lung.ca = function() 
        if ("C34" %in% get("diseasestoexclude", parent.frame())) sys.source(file = "./lung cancer model.R", my.env),
    other = function() 
        sys.source(file = "./other model.R", my.env)
)

