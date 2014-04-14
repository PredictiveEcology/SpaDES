### set the working directory
setwd("~/GitHub/")  #setwd("~/Documents/GitHub/")
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
devtools::load_all("ABM") # for development/testing
#devtools::install_github("ABM", username="achubaty")   # local install from GitHub
#library(ABM)   # local installation from CRAN

## simulation code
# initialize the simulation
simsim <- sim.init(times=list(start=0.0, stop=10.0),
                   params=list(Ncaribou=100),
                   modules=list("habitat", "caribou"),
                   path="ABM/SAMPLE")
simsim <- dosim(simsim)

## profiling of development code
prof <- lineprof(dosim(maxsimtime=10.00, modules=list("habitat", "caribou"), path="ABM/SAMPLE"))
shine(prof)
c(address(sim), refs(sim))
c(address(sim.data), refs(sim.data))
