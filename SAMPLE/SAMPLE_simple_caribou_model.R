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

### load SpaDES package
#library(SpaDES)   # local installation from CRAN
#devtools::install_github("SpaDES", username="achubaty")   # local install from GitHub
#devtools::load_all(file.path(path, "SpaDES")) # for development/testing

## simulation code
library(RColorBrewer)

# initialize the simulation
#devtools::load_all(file.path(path, "SpaDES")) # for development/testing
#dev(2)


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
args = rep(list(native=TRUE), 6)

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

#setwd(file.path("C:","shared","data","shared","LandCoverOfCanada2005_V1_4"))
#setwd(file.path("C:","Eliot", "ProjectsBig", "Caribou"))

#simple one
setwd(file.path("C:","Eliot", "ProjectsBig","Caribou","CoteNord_v2_21nov12"))
fileList= data.frame(files= dir(pattern = "asc"),stringsAsFactors=FALSE)

setwd("C:/shared/data/shared/LandCoverOfCanada2005_V1_4")
fileList= data.frame(files= dir(pattern = "tif"),stringsAsFactors=FALSE)

# load Pinus Contorta
system.file("DEM.rda",package="SpaDES")
setwd("C:/shared/data/shared/kNN")
fileList = data.frame(files = "NFI_MODIS250m_kNN_Species_Pinu_Con_Lat_v0.tif",stringsAsFactors=FALSE)

mySim <- simInit(times=list(start=0.0, stop=10),

################################################################################
library(devtools)
dev_mode(TRUE)
install(build_vignettes=FALSE) # build_vignette currently fails
library("SpaDES", lib.loc=getOption("devtools.path"))

# set raster options (adjust as needed)
rasterOptions(maxmemory=1e9)

# temporary fix to plot colours (to be changed in plotting.R)
fileList = data.frame(files = dir(file.path(find.package("SpaDES",
                                                         lib.loc=getOption("devtools.path"),
                                                         quiet=FALSE),"maps"),
                                  full.names=TRUE, pattern= "tif"),
                      functions="rasterToMemory",
                      packages="SpaDES",
                      stringsAsFactors=FALSE)

#loadFiles(fileList = fileList)
stackName = "landscape"
outputPath=file.path("~", "tmp", "simOutputs")

mySim <- simInit(times=list(start=0.0, stop=10.0),
                 params=list(
                   #.checkpoint=list(interval=1000,
                   #                 file=file.path(path, "SpaDES/SAMPLE/chkpnt.RData")),
                   #.loadFileList=fileList,
                   .progress=list(.graphical=FALSE, .progressInterval = 10),
                   .globals=list(.stackName=stackName, burnStats="nPixelsBurned",
                                 .outputPath=outputPath),
                   randomLandscapes = list(nx=1e2, ny=1e2, inRAM=TRUE,
                                           #.saveObjects=c(.stackName),
                                           #.savePath=file.path("output", "randomLandscapes"),
                                           #.saveInitialTime=3, .saveInterval=100,
                                           .plotInitialTime=NA, .plotInterval=NA),
                   caribouMovement=list(N=1e2,
                                        #.saveObjects=c("caribou"),
                                        #.savePath=file.path("output","caribouMovement"),
                                        #.saveInitialTime=3, .saveInterval=100,
                                        #.plotInitialTime=NA, .plotInterval=NA,
                                        #.plotInitialTime = 1.01, .plotInterval=1,
                                        moveInterval=1),
                   fireSpread=list(nFires=1e1, spreadprob=0.225,
                                   persistprob=0, its=1e6,
                                   #.plotInitialTime=NA, .plotInterval=NA,
                                   .plotInitialTime = 0.1, .plotInterval=10,
                                   #.saveObjects=c("Fires"), .saveInterval=10,
                                   #.savePath=file.path("output","fireSpread"),
                                   returnInterval=10, startTime=0)
                 ),
                 modules=list("randomLandscapes", "fireSpread", "caribouMovement"),
                 #modules=list("stackFileList"),
                 #modules=list("stackFileList", "fireSpread", "caribouMovement"),
                 #modules=list("caribouMovement", "fireSpread"),
                 path=system.file("sampleModules", package="SpaDES"))

dev(4)
spades(mySim, debug=FALSE)
print(system.time(spades(mySim, debug=false)))
print(system.time(mySim <- spades(mySim, debug=FALSE)))


detach("package:SpaDES", unload=TRUE)

################################################################################
fls = dir(file.path("output","fires"))
FireMap = list()
for (i in fls)
FireMap[[i]] = readRDS(file.path("output", "fires", i))
simPlot(stack(FireMap),col=cols[[1]])

## profiling of development code
#prof <- lineprof(dosim(maxsimtime=10.00, modules=list("habitat", "caribou"), path="ABM/SAMPLE"))
#prof <- lineprof(source("spades.R"))

shine(prof)
c(address(mySim), refs(mySim))

