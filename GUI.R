#cmpfile("./GUI.R")
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


require("RGtk2")
# needs sudo apt-get install libgtk2.0-dev before install in linux
require("RGtk2Extras")

input.fn <- function(yearstoproject=30L,
                     ageL= 30L, 
                     ageH = 84L,
                     numberofiterations = 1L,
                     diseasestoexclude, 
                     init.year = 2011L, 
                     n = 200000L,
                     cvd.lag = 5L, 
                     cancer.lag = 10L, 
                     clusternumber = 4L,
                     cleardirectories = F,
                     advanced, 
                     alignment, 
                     Fertility.Assumption = "N", 
                     fatality.annual.improvement.chd, 
                     fatality.annual.improvement.stroke,
                     fatality.annual.improvement.c16,
                     fatality.sec.gradient.chd, 
                     fatality.sec.gradient.stroke,
                     fatality.sec.gradient.c16,
                     process.output,
                     export.graphs,
                     qdrisk = T,
                     paired
                     ) {
  if (ageL > ageH) {
    tt <- ageL
    ageH <- ageL
    ageH <- tt 
    rm(tt)
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
              fatality.annual.improvement.c16 = fatality.annual.improvement.c16,
              fatality.sec.gradient.chd = fatality.sec.gradient.chd,
              fatality.sec.gradient.stroke = fatality.sec.gradient.stroke,
              fatality.sec.gradient.c16 = fatality.sec.gradient.c16,
              numberofiterations = numberofiterations,
              process.output = process.output,
              export.graphs = export.graphs,
              qdrisk = qdrisk,
              paired = paired)
  )
}


.input.fn.dialog = list(
  title = "IMPACTncd by Chris Kypridemos",
  label = "Set simulation parameters",
  #long.running = TRUE,
  yearstoproject.rangeItem = c(value=30L, from=1L, to=60L, by=1L), 
  label = "Forecast horizon",
  numberofiterations.integerItem = c(value=30L, from=1L, to=1000L, by=1L),
  label = "Define number of iterations", 
  diseasestoexclude.variableSelectorItem =  c("CHD", "Stroke", "Gastric cancer"),
  label = "Define diseases to be included in the simulation",
  n.integerItem = c(value=200000L, from=100000L, to=2000000L, by=100000L),
  label = "Define the sample size",
  cvd.lag.rangeItem = c(value=5L, from=1L, to=10L, by=1L),
  label = "Define time lag for CHD and stroke (in years)",
  cancer.lag.rangeItem = c(value=8L, from=1L, to=10L, by=1L),
  label = "Define time lag for cancers (in years)",

  
  BREAK = T,
  
  advanced.trueFalseItem = FALSE, 
  label = "         *** Set advanced settings... USE WITH CAUTION!!! ***",
  signal = c("default", "toggle.sensitive", "ageL"),
  signal = c("default", "toggle.sensitive", "ageH"),
  signal = c("default", "toggle.sensitive", "alignment"),
  signal = c("default", "toggle.sensitive", "Fertility.Assumption"),
  signal = c("default", "toggle.sensitive", "clusternumber"),
  signal = c("default", "toggle.sensitive", "cleardirectories"),
  signal = c("default", "toggle.sensitive", "fatality.annual.improvement.chd"),
  signal = c("default", "toggle.sensitive", "fatality.annual.improvement.stroke"),
  signal = c("default", "toggle.sensitive", "fatality.annual.improvement.c16"),
  signal = c("default", "toggle.sensitive", "fatality.sec.gradient.chd"),
  signal = c("default", "toggle.sensitive", "fatality.sec.gradient.stroke"),
  signal = c("default", "toggle.sensitive", "fatality.sec.gradient.c16"),
  signal = c("default", "toggle.sensitive", "init.year"),
  signal = c("default", "toggle.sensitive", "qdrisk"),
  signal = c("default", "toggle.sensitive", "paired"),
  signal = c("default", "toggle.sensitive", "process.output"),
  signal = c("default", "toggle.sensitive", "export.graphs"),
  
  
  fatality.annual.improvement.chd.rangeItem = c(value=3, from=0, to=10, by=0.1),
  label = "Assumption about annual percentage improvement in CHD fatality",
  fatality.annual.improvement.stroke.rangeItem = c(value=3, from=0, to=10, by=0.1),
  label = "Assumption about annual percentage improvement in stroke fatality",
  fatality.annual.improvement.c16.rangeItem = c(value=2, from=0, to=10, by=0.1),
  label = "Assumption about annual percentage improvement in gastric cancer fatality",
  fatality.sec.gradient.chd.rangeItem = c(value=25, from=-100, to=100, by=10L),
  label = "Assumption about percentage difference in CHD fatality\nbetween QIMD 1 and 5. Positive values mean the\npoorest are experiencing higher fatality",
  fatality.sec.gradient.stroke.rangeItem = c(value=10, from=-100, to=100, by=10L),
  label = "Assumption about percentage difference in stroke fatality\nbetween QIMD 1 and 5. Positive values mean the\npoorest are experiencing higher fatality",
  fatality.sec.gradient.c16.rangeItem = c(value=30, from=-100, to=100, by=10L),
  label = "Assumption about percentage difference in gastric cancer fatality\nbetween QIMD 1 and 5. Positive values mean the\npoorest are experiencing higher fatality",
  
  BREAK = T,
  
  ageL.rangeItem = c(value=30L, from=30L, to=39L, by=5L), 
  label = "Define lower age limit for the diseases-model simulation",
  ageH.rangeItem = c(value=84L, from=69L, to=84L, by=5L), 
  label = "Define upper age limit for the diseases-model simulation",
  init.year.choiceItem = c(2006, 2011),
  label = "Define year to start the simulation",
  tooltip = "Use years other than 2011 with caution",
  clusternumber.integerItem = c(value=30L, from=1L, to=80L, by=1L),
  label = "Define number of cores", 
  tooltip = "Each core needs about 2.5Gb of ram",
  Fertility.Assumption.choiceItem = c(value="N", "H", "L"),
  label = "Select (N)ormal, (H)igh or (L)ow fertility rate asumptions\nbased on ONS scenarios",
  alignment.trueFalseItem = FALSE,
  label = "Apply correction factor to balance exposure error)",
  qdrisk.trueFalseItem = TRUE,
  label = "Use QDiabetes score to predict diabetes incidence",
  paired.trueFalseItem = TRUE,
  label = "Do paired experiments",
  process.output.trueFalseItem = TRUE,
  label = "Process the output",
  export.graphs.trueFalseItem = FALSE,
  label = "Produce graphs with old method",
  cleardirectories.trueFalseItem = FALSE,
  label = "Delete intermediate output files and logs"
)

run.dialog(input.fn)   

list2env(input_fn_output, envir = .GlobalEnv)

names(init.year) <- NULL
# Define outersect. Like setdiff but symmetrical. I.e. setdiff(a,b) is not the same as setdiff(b,a). outersect solve this by calculating both
outersect <- function(x, y, ...) {
  big.vec <- c(x, y, ...)
  duplicates <- big.vec[duplicated(big.vec)]
  setdiff(big.vec, unique(duplicates))
}


if ("Gastric cancer" %in% diseasestoexclude) {
  diseasestoexclude <- outersect(diseasestoexclude, "Gastric cancer")
  diseasestoexclude <- c(diseasestoexclude, "C16")
}

if ("Stroke" %in% diseasestoexclude) {
  diseasestoexclude <- outersect(diseasestoexclude, "Stroke")
  diseasestoexclude <- c(diseasestoexclude, "stroke")
}

if ("Lung cancer" %in% diseasestoexclude) {
  diseasestoexclude <- outersect(diseasestoexclude, "Lung cancer")
  diseasestoexclude <- c(diseasestoexclude, "C34")
}

# Function to choose  scenarios     
choose.scenarios <- function(x) {x}

.choose.scenarios.dialog = list(
  title = "IMPACTncd by Chris Kypridemos",
  label = "Choose scenarios to simulate",
  x.variableSelectorItem =  list.files(path = "./Scenarios", pattern = glob2rx("*.Rc"), full.names = F, recursive = F))

run.dialog(choose.scenarios, output.name = "scenarios.list") 
