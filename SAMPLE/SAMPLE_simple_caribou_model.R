### set the working directory
OS <- tolower(Sys.info()["sysname"])
hostname <- gsub(Sys.info()["nodename"], pattern=".-VIC-", replace="")

if (OS=="windows") {
  if(any(pmatch(c("A105200","A105192"), hostname, nomatch=FALSE))) {
    path <- "c:/Eliot/GitHub"
  } else {
    path <- "~/GitHub"
  }
} else {
  path <- "~/Documents/GitHub"
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
devtools::load_all(file.path(path, "SpaDES")) # for development/testing

## simulation code
library(RColorBrewer)
cols = list(
  transparent.red=c("#00000000",paste(brewer.pal(8,"Greys"),"66",sep="")[8:1]),
  grey = brewer.pal(9,"Greys"),
  spectral = brewer.pal(8,"Spectral"),
  terrain = rev(terrain.colors(100)),
  heat = heat.colors(10),
  topo = topo.colors(10)
)

# initialize the simulation
devtools::load_all(file.path(path, "SpaDES")) # for development/testing
dev(2)
mySim <- simInit(times=list(start=0.0, stop=100),
                 params=list(
                   #.checkpoint=list(interval=1000,
                   #                          file=file.path(path, "SpaDES/SAMPLE/chkpnt.RData")),
                             .progress=list(graphical=FALSE,interval = 10),
                             habitat = list(nx=1e3,ny=1e3,toSave=c("habitat"),
                                            savePath = file.path("output","habitat"),
                                            saveFreq=5,
                                            interval = 0, startTime=0),
                             caribou=list(N=1e3,plotFreq=1,toSave=c("caribou"),
                                          savePath = file.path("output","caribou"),
                                          saveFreq = 4,
                                          interval = 1, startTime=0),
                             fire=list(nFires = 1e1, spreadprob=0.225, 
                                        persistprob=0, its=1e6, plotFreq=10,
                                        toSave=c("Fires"),
                                        savePath = file.path("output","fires"),
                                        saveFreq = 5,
                                        interval = 10, startTime=0)
                             ),
                 modules=list("habitat", "fire", "caribou"),
#                 modules=list("habitat", "fire"),
#                  modules=list("habitat"),
                  path=file.path(path, "SpaDES/SAMPLE"))

#simCurrentTime(mySim)<-0
print(system.time(mySim <- doSim(mySim)))
#print(system.time(mySim <- doSim(mySim,timerUpdateFreq=1,graphicalTimer=F)))

## profiling of development code
#prof <- lineprof(dosim(maxsimtime=10.00, modules=list("habitat", "caribou"), path="ABM/SAMPLE"))

#prof <- lineprof(source("doSim.R"))

shine(prof)
c(address(mySim), refs(mySim))
