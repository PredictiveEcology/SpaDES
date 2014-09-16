setwd(file.path("C:","Rwork"))
outputPath=file.path("~", "tmp", "simOutputs")
times <- list(start=0, stop=200)


pathToMaps <- file.path(find.package("SpaDES", quiet=FALSE), "maps")

fileList <- data.frame(files=c("C:/shared/data/shared/LandCoverOfCanada2005_V1_4/LCC2005_V1_4a.tif",
                               "C:/shared/data/shared/age/age.asc"),
                       functions="raster", packages="raster",
                       objectNames=c("lcc05","age"),
#                       .stackName="landscape",
                       stringsAsFactors=FALSE)
loadFiles(fileList=fileList)
ext <- extent(-1073154,-987285,7438423,7512480)
vegMap <- crop(lcc05,ext)
ageMap <- projectRaster(age, to=vegMap, method="ngb")
fireSpreadProb <- reclassify(x=vegMap,
                             rcl=cbind(1:38,
                                   c(0.23,0.2,0.21,0.21, 0.21, 0.23, 0.23, 0.23, 0.23, 0.23,
                                     0.15,0.15,0.15,0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15,
                                     0.15,0.15,0.15,0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15,
                                     0.15,0.15,0.15,0.15, 0, 0, 0, 0)))

setwd("c:/Eliot/GitHub/SpaDES/SAMPLE/")
writeRaster(vegMap, filename="vegMap.tif")
writeRaster(ageMap, filename="ageMap.tif")
writeRaster(fireSpreadProb, filename="fireSpreadProb.tif",overwrite=TRUE)

fileList <- data.frame(files=c("c:/Eliot/GitHub/SpaDES/SAMPLE/vegMap.tif",
                               "c:/Eliot/GitHub/SpaDES/SAMPLE/ageMap.tif",
                               "c:/Eliot/GitHub/SpaDES/SAMPLE/fireSpreadProb.tif"),
                       functions="raster", packages="raster",
                       #.stackName="landscape",
                       stringsAsFactors=FALSE)
loadFiles(fileList=fileList)

times=list(start=0.0, stop=10)


parameters <- list(.globals=list(.stackName="landscape"),
                   .progress=list(.graphical=TRUE, .progressInterval=1),
                   #.loadFileList=fileList,
                   forestSuccession=list(returnInterval=1, startTime=0,
                                   .plotInitialTime=1, .plotInterval=10),
                   forestAge=list(returnInterval=1, startTime=0.5,
                                        .plotInitialTime=1, .plotInterval=10),
                   fireSpread=list(nFires= 1e1, spreadprob=0.225, its=1e6,
                                   persistprob=0, returnInterval=10, startTime=0,
                                   .plotInitialTime=0.1, .plotInterval=10)
#                   caribouMovement=list(N=1e2, moveInterval=1,
#                                        .plotInitialTime=1.01, .plotInterval=1)
)

#path <- system.file("sampleModules", package="SpaDES")


modules <- list("forestSuccession", "forestAge")#, "fireSpreadLcc")
path <- file.path("C:","Eliot","GitHub","SpaDES","SAMPLE")

mySim <- simInit(times=times, params=parameters, modules=modules, path=path)
dev(2)#pdf("test.pdf")
mySim <- spades(mySim, debug=F)
#simStopTime(mySim)<-20
#dev.off()
