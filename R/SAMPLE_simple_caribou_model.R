### install neccesarry packages to profile development code
install.packages("devtools")
#   Windows also needs 'Rtools' from
#   http://cran.r-project.org/bin/windows/Rtools/index.html
#   (this is a seperate install of an .exe file)

devtools::install_github("lineprof")
library(lineprof)

devtools::install_github("pryr")
library(pryr)

## simulation code
setwd("~/GitHub/ABM/R")
source("ABM_code_files.R")
source("simulation.R")
dosim(sim.init, do.event, print.results, maxsimtime=10.00, modules=list("habitat", "caribou"))

## profiling of development code
prof <- lineprof(dosim(sim.init, do.event, print.results, maxsimtime=10.00, modules=list("habitat", "caribou")))
shine(prof)
c(address(sim), refs(sim))