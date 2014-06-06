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
x = stack(whatever)
myAgeFunction = function(myStack) {
    ageMax = sim.params(sim)$age$maxAge
    out = pmin(ageMax, ageMap + myStack[["burned"]])
    return(out)
}

mySim <- sim.init(times=list(start=0.0, stop=1.0),
                   params=list(age=list(inputFile="C:/shared/data/shared/age/age.asc",
                                        agingFunction=myAgeFunction,
                                        rasterLayerName="ageMap",
                                        rasterStackName="ageStack",
                                        maxAge=200),
                               succession=list()),
                   modules=list("succession"),
                  path="ABM/SAMPLE")
mySim <- dosim(mySim)

## profiling of development code
#prof <- lineprof(dosim(maxsimtime=10.00, modules=list("habitat", "caribou"), path="ABM/SAMPLE"))
prof <- lineprof(dosim(mySim))
shine(prof)
c(address(sim), refs(sim))
c(address(sim.data), refs(sim.data))
