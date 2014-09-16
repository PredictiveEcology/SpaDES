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
#loadFiles(fileList=fileList)


parameters <- list(.globals=list(.stackName="landscape"),
                   .progress=list(.graphical=TRUE, .progressInterval=1),
#                   .loadFileList=fileList,
                   forestSuccession=list(returnInterval=1, startTime=0,
                                   .plotInitialTime=1, .plotInterval=10),
                   forestAge=list(returnInterval=1, startTime=0.5,
                                        .plotInitialTime=1, .plotInterval=10)
)
modules <- list("forestSuccession", "forestAge")
path <- file.path("C:","Eliot","GitHub","SpaDES","SAMPLE")

mySim <- simInit(times=times, params=parameters, modules=modules, path=path)
dev(4)#pdf("test.pdf")
mySim <- spades(mySim, debug=F)
#simStopTime(mySim)<-20
#dev.off()
