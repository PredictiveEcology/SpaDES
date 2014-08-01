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


# fileList = list(
#   landCover = raster("C:/shared/data/shared/LandCoverOfCanada2005_V1_4/LCC2005_V1_4a.tif"))#,
#
# setwd(file.path("C","shared","data","shared","rasters"))
# FileListRasters= data.frame(
#     file= dir(),
#     fun = "raster",
#     obj = c("LandCover1","LandCover2"),
#     arg = NA)

#setwd(file.path("C","shared","data","shared","polygons"))
args = rep(list(native=TRUE),6)

fileList= list(
  file = dir(pattern = "asc"),
  funs = NA,
  objs = NA,
  args = args,#I(rep(list(native=TRUE),6)),
  loadTimes = 0,
  intervals = c(rep(NA,length(dir(pattern="asc"))-1),10))

fileList= list(
  file = dir(pattern = "asc"),
  funs = NA,
  objs = NA,
  args = args,#I(rep(list(native=TRUE),6)),
  loadTimes = c(rep(0,5),10))#,
#  intervals = c(rep(NA,length(dir(pattern="asc"))-1),10),
#  stringsAsFactors=FALSE)

#setwd(file.path("C","shared","data","shared","maps"))
#setwd(file.path("C:","Eliot", "ProjectsBig", "Caribou"))
#setwd(file.path("C:","Eliot", "ProjectsBig","Caribou","CoteNord_v2_21nov12"))

#simple one
#fileList= data.frame(files= dir(pattern = "asc"),stringsAsFactors=FALSE)

simInit("sim", times=list(start=0.0, stop=100),
                 params=list(
                   #.checkpoint=list(interval=1000,
                   #                          file=file.path(path, "SpaDES/SAMPLE/chkpnt.RData")),
                              fileList=fileList,
                             .progress=list(graphical=FALSE, interval = 10),
                             habitat = list(nx=1e3, ny=1e3, toSave=c("habitat"),
                                            savePath=file.path("output", "habitat"),
                                            saveFreq=3, plotFreq=10,
                                            interval=0, startTime=0),
                             caribou=list(N=1e3, plotFreq=1, toSave=c("caribou"),
                                          savePath=file.path("output","caribou"),
                                          saveFreq=4, interval=1, startTime=0),
                             fire=list(nFires = 1e1, spreadprob=0.225,
                                        persistprob=0, its=1e6, plotFreq=10,
                                        toSave=c("Fires"),
                                        savePath = file.path("output","fires"),
                                        saveFreq = 5, interval = 10, startTime=0)
                             ),
                 modules=list("habitat", "fire", "caribou"),
#                 modules=list("habitat", "fire"),
#                  modules=list("habitat"),
                 path=file.path(path, "SpaDES/SAMPLE"))

#simCurrentTime(mySim)<-0
#doSim("mySim", debug=FALSE)
print(system.time(doSim("mySim", debug=FALSE)))
#print(system.time(mySim <- doSim(mySim, timerUpdateFreq=1, graphicalTimer=FALSE)))

fls = dir(file.path("output","fires"))
FireMap = list()
for (i in fls)
FireMap[[i]] = readRDS(file.path("output","fires",i))
simPlot(stack(FireMap),col=cols[[1]])

## profiling of development code
#prof <- lineprof(dosim(maxsimtime=10.00, modules=list("habitat", "caribou"), path="ABM/SAMPLE"))

#prof <- lineprof(source("doSim.R"))

shine(prof)
c(address(mySim), refs(mySim))
