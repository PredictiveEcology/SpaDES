### set the working directory
OS <- tolower(Sys.info()["sysname"])
hostname <- gsub(Sys.info()["nodename"],pattern="W-VIC-",replace="")

if (OS=="windows") {
    if(pmatch("A105200", hostname, nomatch=FALSE)) {
        setwd("c:/Eliot/GitHub/")
    } else {
        setwd("~/GitHub/")
    }
} else {
    setwd("~/Documents/GitHub/")
}

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

devtools::dev_mode(TRUE)
### load SpaDES package
#library(SpaDES)   # local installation from CRAN
#devtools::install_github("SpaDES", username="achubaty")   # local install from GitHub
devtools::load_all("SpaDES") # for development/testing

## simulation code
# initialize the simulation
mySim <- simInit(times=list(start=0.0, stop=10.1),
                 params=list(.checkpoint=list(interval=5, file="SpaDES/SAMPLE/chkpnt.RData"),
                             caribou=list(N=100),
                             fires=list(num=2, spreadprob=0.215, persistprob=0.1, its=1)
                             ),
#                 modules=list("habitat", "fire", "caribou"),
                 modules=list("habitat", "caribou"),
                 path="SpaDES/SAMPLE")
mySim <- doSim(mySim)

## profiling of development code
#prof <- lineprof(dosim(maxsimtime=10.00, modules=list("habitat", "caribou"), path="ABM/SAMPLE"))
prof <- lineprof(doSim(mySim))
shine(prof)
c(address(sim), refs(sim))
c(address(sim.data), refs(sim.data))
