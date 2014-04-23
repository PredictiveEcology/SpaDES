### set the working directory
setwd("~/GitHub/") 
#setwd("~/Documents/GitHub/")
#setwd("c:/Eliot/GitHub/")

### development only:
install.packages("devtools", dependencies=TRUE)
#   Windows also needs 'Rtools' from
#   http://cran.r-project.org/bin/windows/Rtools/index.html
#   (this is a seperate install of an .exe file)
devtools::install_github("lineprof")
devtools::install_github("pryr")
devtools::install_github("shiny-slickgrid", "wch")
library(lineprof)
library(pryr)
library(shiny)

### load ABM package
#library(ABM)   # local installation from CRAN
#devtools::install_github("ABM", username="achubaty")   # local install from GitHub
devtools::load_all("ABM") # for development/testing

## simulation code
# initialize the simulation
mySim <- sim.init(times=list(start=0.0, stop=100.0),
                   params=list(caribou=list(N=100),
                               fires=list(num=2, spreadprob=0.225, 
                                          persistprob=0.1, its=50)),
                   modules=list("habitat", "fire", "caribou"),
#                  modules=list("habitat", "caribou"),
                  path="ABM/SAMPLE")
mySim <- dosim(mySim)

## profiling of development code
prof <- lineprof(dosim(maxsimtime=10.00, modules=list("habitat", "caribou"), path="ABM/SAMPLE"))
prof <- lineprof(dosim(mySim))
shine(prof)
c(address(sim), refs(sim))
c(address(sim.data), refs(sim.data))
