require("RGtk2")
# needs sudo apt-get install libgtk2.0-dev before install in linux
require("RGtk2Extras")
input.fn <- function(yearstoproject=10, ageL=30, ageH=84, numberofiterations=1, diseasestoexclude, init.year = 2011, n=1,
                     cvd.lag=5, cancer.lag=10, clusternumber = 4,cleardirectories=T,
                     advanced, alignment, Fertility.Assumption=N, fatality.annual.improvement.chd, fatality.annual.improvement.stroke,
                     fatality.sec.gradient.chd, fatality.sec.gradient.stroke) {
    if (ageL > ageH) {
        tt <- ageL
        ageH <- ageL
        ageH <- tt 
    }
    return(list(yearstoproject = yearstoproject, 
                ageL = ageL, 
                ageH = ageH, 
                init.year = init.year,
                diseasestoexclude = diseasestoexclude,
                cvd.lag = cvd.lag,
                cancer.lag = cancer.lag,
                n = n,
                alignment = alignment,
                Fertility.Assumption = Fertility.Assumption,
                clusternumber = clusternumber,
                cleardirectories = cleardirectories,
                fatality.annual.improvement.chd = fatality.annual.improvement.chd,
                fatality.annual.improvement.stroke = fatality.annual.improvement.stroke,
                fatality.sec.gradient.chd = fatality.sec.gradient.chd,
                fatality.sec.gradient.stroke = fatality.sec.gradient.stroke,
                numberofiterations = numberofiterations))
}

  
.input.fn.dialog = list(
    title = "IMPACTncd by Chris Kypridemos",
    label = "Set Simulation Parameters",
	#long.running = TRUE,
    yearstoproject.rangeItem = c(value=10, from=1, to=50, by=1), 
    label = "Forecast horizon",
    ageL.rangeItem = c(value=30, from=30, to=84, by=1), 
    label = "Define lower age limit for the diseases-model simulation",
    ageH.rangeItem = c(value=84, from=30, to=84, by=1), 
    label = "Define upper age limit for the diseases-model simulation",
    diseasestoexclude.variableSelectorItem =  c("CHD", "stroke", "lung cancer"),
    label = "Define diseases to be included in the simulation",
    n.integerItem = c(value=200000, from=100000, to=2000000, by=100000),
    label = "Define the sample size",
    cvd.lag.rangeItem = c(value=5, from=1, to=10, by=1),
    label = "Define time lag for CHD and stroke (in years)",
    cancer.lag.rangeItem = c(value=10, from=1, to=10, by=1),
    label = "Define time lag for cancers (in years)",
    BREAK = T,
    
    advanced.trueFalseItem = FALSE, 
    label = "     *** Set advanced settings... USE WITH CAUTION!!! ***",
    signal = c("default", "toggle.sensitive", "numberofiterations"),
    signal = c("default", "toggle.sensitive", "alignment"),
    signal = c("default", "toggle.sensitive", "Fertility.Assumption"),
    signal = c("default", "toggle.sensitive", "clusternumber"),
    signal = c("default", "toggle.sensitive", "cleardirectories"),
    signal = c("default", "toggle.sensitive", "fatality.annual.improvement.chd"),
    signal = c("default", "toggle.sensitive", "fatality.annual.improvement.stroke"),
    signal = c("default", "toggle.sensitive", "fatality.sec.gradient.chd"),
    signal = c("default", "toggle.sensitive", "fatality.sec.gradient.stroke"),
    signal = c("default", "toggle.sensitive", "init.year"),
    
    numberofiterations.integerItem = c(value=1, from=0, to=100, by=1),
    label = "Define number of iterations", 
    alignment.trueFalseItem = FALSE,
    label = "Apply correction factor to counterpoise levin's and exposure error)",
    Fertility.Assumption.radiobuttonItem = c(value="N", "H", "L"),
    label = "Select (N)ormal, (H)igh or (L)ow fertility rate asumptions\nbased on ONS scenarios",
    fatality.annual.improvement.chd.rangeItem = c(value=3, from=0, to=10, by=0.1),
    label = "Assumption about annual percentage improvement in CHD fatality",
    fatality.annual.improvement.stroke.rangeItem = c(value=3, from=0, to=10, by=0.1),
    label = "Assumption about annual percentage improvement in stroke fatality",
    fatality.sec.gradient.chd.rangeItem = c(value=40, from=0, to=100, by=10L),
    label = "Assumption about percentage difference in CHD fatality\nbetween QIMD 1 and 5. Positive values mean the\npoorest are experiencing higher fatality",
    fatality.sec.gradient.stroke.rangeItem = c(value=40, from=0, to=100, by=10L),
    label = "Assumption about percentage difference in stroke fatality\nbetween QIMD 1 and 5. Positive values mean the\npoorest are experiencing higher fatality",
    init.year.integerItem = c(value=2011, from=2001, to=2020, by=1),
    label = "Define year to start the simulation",
    tooltip = "Use years other than 2011 with caution",
    clusternumber.integerItem = c(value=4, from=1, to=32, by=1),
    label = "Define number of cores", 
    tooltip = "Each core needs about 3Gb of ram",
    cleardirectories.trueFalseItem = TRUE,
    label = "Delete intermediate output files and logs"
)
run.dialog(input.fn)   

list2env(input_fn_output, envir = .GlobalEnv)  




