### set the working directory
setwd("~/GitHub/")  #setwd("~/Documents/GitHub/")

### development only:
install.packages("devtools", dependencies=TRUE)
#   Windows also needs 'Rtools' from
#   http://cran.r-project.org/bin/windows/Rtools/index.html
#   (this is a seperate install of an .exe file)
devtools::install_github("lineprof")
devtools::install_github("pryr")
library(lineprof)
library(pryr)

### load ABM package
devtools::load_all("ABM") # for development/testing
#devtools::install_github("ABM", username="achubaty")   # local install from GitHub
#library(ABM)   # local installation from CRAN

## simulation code
dosim(sim.init, do.event, print.results, maxsimtime=10.00, modules=list("habitat", "caribou"))
# THIS IS BROKEN RIGHT NOW BECAUSE I MOVED THE SAMPLE MODULES TO A DIFF DIR

## profiling of development code
prof <- lineprof(dosim(sim.init, do.event, print.results, maxsimtime=10.00, modules=list("habitat", "caribou")))
shine(prof)
c(address(sim), refs(sim))
c(address(sim.data), refs(sim.data))
