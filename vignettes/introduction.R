### R code from vignette source 'introduction.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: SpaDES-demo (eval = FALSE)
###################################################
## library("SpaDES")
## 
## # demo: randomLandscapes, fireSpread, caribouMovement
## demo("spades-simulation", package="SpaDES")


###################################################
### code chunk number 2: using-SpaDES
###################################################
library("SpaDES")

outputPath=file.path("~", "tmp", "simOutputs")
times <- list(start=0, stop=10.2)
parameters <- list(.globals=list(.stackName="landscape", .outputPath=outputPath,
                                 burnStats="nPixelsBurned"),
                   .progress=list(NA),
                   randomLandscapes=list(nx=1e2, ny=1e2, inRAM=TRUE),
                   fireSpread=list(nFires= 1e1, spreadprob=0.225, its=1e6,
                                   persistprob=0, returnInterval=10, startTime=0,
                                  .plotInitialTime=0.1, .plotInterval=10),
                   caribouMovement=list(N=1e2, moveInterval=1,
                                        .plotInitialTime=1.01, .plotInterval=1)
                   )
modules <- list("randomLandscapes", "fireSpread", "caribouMovement")
path <- system.file("sampleModules", package="SpaDES")

mySim <- simInit(times=times, params=parameters, modules=modules, path=path)

#dev(4)
spades(mySim)


###################################################
### code chunk number 3: load-landscape-maps
###################################################
### Example: loading habitat maps

# use all built-in maps from the SpaDES package
pathToMaps <- file.path(find.package("SpaDES", quiet=FALSE), "maps")
fileList <- data.frame(files=dir(pathToMaps, full.names=TRUE, pattern= "tif"),
                      functions="rasterToMemory", packages="SpaDES",
                      stringsAsFactors=FALSE)

# this list can be passed to simInit() as an entry in the parameter list
mySim <- simInit(times=list(start=0.0, stop=10),
                 params=list(
                   .loadFileList=fileList,
                   .progress=list(NA),
                   .globals=list(.stackName="landscape", burnStats="nPixelsBurned"),
                   #.globals=list(burnStats="nPixelsBurned"),
                   fireSpread=list(nFires=1e1, spreadprob=0.225, persistprob=0,
                                   its=1e6,returnInterval=10, startTime=0.1,
                                   .plotInitialTime = 0, .plotInterval=10)
                 ),
                 modules=list("fireSpread"),
                 path=system.file("sampleModules", package="SpaDES"))

spades(mySim)


###################################################
### code chunk number 4: fire
###################################################
library(RColorBrewer)
nFires <- 10
landscape[["Fires"]] <-
  spread(landscape[["percentPine"]],
         loci=as.integer(sample(1:ncell(landscape), nFires)),
         spreadProb=landscape[["percentPine"]]/(maxValue(landscape[["percentPine"]])*5)+0.1,
         persistance=0,
         mask=NULL,
         maxSize=1e8,
         directions=8,
         iterations=1e6,
         plot.it=FALSE,
         mapID=TRUE)

setColors(landscape$Fires)<-paste(c("#000000",brewer.pal(8,"Reds")[5:8]),c("00",rep("FF",4)),sep="")

Plot(landscape[["Fires"]], add=FALSE)


###################################################
### code chunk number 5: fire-overlaid
###################################################
# Show the burning more strongly over abundant pine
percentPine<-landscape$percentPine
Plot(percentPine, add=FALSE)
#Plot(landscape[["Fires"]], add=TRUE)
Plot(landscape[["Fires"]], addTo="percentPine", legend=FALSE, title=FALSE)


###################################################
### code chunk number 6: fire-impacts
###################################################
# Show the burning more strongly over abundant pine
fire <- reclassify(landscape[["Fires"]], rcl=cbind(0:1, c(0,ncell(landscape)), 0:1))
pine <- reclassify(landscape[["percentPine"]], rcl=cbind(0:9*10, 1:10*10, 0:9))
PineByFire <- crosstab(fire, pine, long=TRUE)
colnames(PineByFire) <- c("fire", "pine", "freq")
PineByFire$pine <- as.numeric(as.character(PineByFire$pine))
summary(glm(freq ~ fire*pine, data=PineByFire, family="poisson"))


###################################################
### code chunk number 7: fire-impacts-maps
###################################################
landscape[["forestAge"]][landscape[["Fires"]]>0] <- 0
landscape[["forestCover"]][landscape[["Fires"]]>0] <- 0
landscape[["habitatQuality"]][landscape[["Fires"]]>0] <- 0.1
landscape[["percentPine"]][landscape[["Fires"]]>0] <- 0
Plot(landscape, add=FALSE)


###################################################
### code chunk number 8: mobile-point-agent
###################################################
N <- 10 # number of agents

# caribou data vectors
IDs <- letters[1:N]
sex <- sample(c("female", "male"), N, replace=TRUE)
age <- round(rnorm(N, mean=8, sd=3))
x1 <- runif(N, -50, 50) # previous X location
y1 <- runif(N, -50, 50) # previous Y location

# caribou (current) coordinates
x0 <- rnorm(N, x1, 5)
y0 <- rnorm(N, y1, 5)

# create the caribou agent object
# caribou needs to be a named object for plotting, use SpatialPointsDataFrameNamed
caribou <- SpatialPointsDataFrameNamed(coords=cbind(x=x0, y=y0),
                                  data=data.frame(x1, y1, sex, age),name="caribou")
row.names(caribou) <- IDs



###################################################
### code chunk number 9: agent-crw-trajectory
###################################################
#dev(4)
Plot(landscape[["habitatQuality"]], add=FALSE)

for (t in 1:10) {
  #crop any caribou that went off maps
  caribou <<- crop(caribou,landscape)
  drawArrows(from=SpatialPoints(cbind(x=caribou$x1, y=caribou$y1)),
             to=caribou, length=0.04, addTo="habitatQuality")

  # find out what pixels the individuals are on now
  ex <- landscape[["habitatQuality"]][caribou]

  #step length is a function of current cell's landscape quality
  sl <- 0.25/ex

  ln <- rlnorm(length(ex), sl, 0.02) # log normal step length
  sd <- 30 # could be specified globally in params

  caribou <<- crw(caribou, stepLength=ln, stddev=sd, lonlat=FALSE)
}


###################################################
### code chunk number 10: multiple-simulations (eval = FALSE)
###################################################
## ### WARNING this can take a while to run, especially for large mapSizes.
## 
## rasterOptions(maxmemory=1e9)
## 
## # list all parameter values to run sims with
## parameters <- list(mapSize=round(sqrt(c(1e4, 1e5, 1e6, 1e7, 1e8))),
##                    pSpread=seq(0.05, 0.25, 0.05))
## 
## # create data.frame with all parameter combinations
## paramsdf <- expand.grid(parameters)
## 
## # outputs
## nPixelsBurned <- numeric()
## meanPixelsBurned <- cbind(paramsdf, pmean=NA, psd=NA)
## 
## set.seed(42)
## for (i in 1:nrow(paramsdf)) {
##   # initialize each simulation with a param combo from paramsdf
##   mySim <- with(paramsdf,
##                 simInit(times=list(start=0.0, stop=20.0),
##                         params=list(
##                           .progress=list(.graphical=NA, .progressInterval=NA),
##                           .globals=list(.stackName="landscape", burnStats="nPixelsBurned"),
##                           randomLandscapes=list(nx=mapSize[i], ny=mapSize[i],
##                                                 inRAM=TRUE),
##                           fireSpread=list(nFires=1000, spreadprob=pSpread[i],
##                                           persistprob=0, its=1e6,
##                                           returnInterval=10, startTime=0)
##                           ),
##                         modules=list("randomLandscapes", "fireSpread"),
##                         path=system.file("sampleModules", package="SpaDES")))
##   mySim <- spades(mySim)
## 
##   # collect stats for each run
##   proportionBurned <- with(paramsdf, nPixelsBurned / (mapSize[i]^2))
##   meanPixelsBurned[i, "pmean"] <- mean(proportionBurned)
##   meanPixelsBurned[i, "psd"] <- sd(proportionBurned)
## 
##   # cleanup between runs
##   rm(landscape, mySim, nPixelsBurned)
##   for (j in 1:10) gc()
## }
## 
## # overall statistics
## pch <- c(21:25)
## col <- brewer.pal(5, "Set1")
## 
## with(meanPixelsBurned, plot(pmean ~ pSpread, xlab="Spread probability",
##                             ylab="Mean proportion of pixels burned",
##                             ylim=c(0,1), pch=pch, cex=1.5, col=col))
## with(parameters, legend("topleft", legend=formatC(mapSize^2, digits=0),
##                               pch=pch, col=col, cex=1.2))


###################################################
### code chunk number 11: multiple-simulations-outputs (eval = FALSE)
###################################################
## # this is included as the output from the previous chunk,
## # so you don't need to wait for the prev chunk to run
## meanPixelsBurned <- structure(list(mapSize = c(100, 316, 1000, 3162, 10000, 100,
## 316, 1000, 3162, 10000, 100, 316, 1000, 3162, 10000, 100, 316,
## 1000, 3162, 10000, 100, 316, 1000, 3162, 10000), pSpread = c(0.05,
## 0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1, 0.1, 0.1, 0.15, 0.15,
## 0.15, 0.15, 0.15, 0.2, 0.2, 0.2, 0.2, 0.2, 0.25, 0.25, 0.25,
## 0.25, 0.25), pmean = c(0.1494, 0.0160831597500401, 0.00157566666666667,
## 0.000160061440122219, 1.57933333333333e-05, 0.241933333333333,
## 0.0301600972066442, 0.00304333333333333, 0.000307687363234317,
## 3.14933333333333e-05, 0.4268, 0.0709388185654009, 0.00855766666666667,
## 0.000826111731886786, 8.185e-05, 0.673466666666667, 0.271898200074774,
## 0.0522476666666667, 0.0057268723054435, 0.000569923333333333,
## 0.8551, 0.704143967312931, 0.758393333333333, 0.748152608931462,
## 0.755049233333333), psd = c(0.000529150262212918, 0.00034444366932222,
## 2.11266025033211e-05, 5.52032923607854e-06, 2.80416357107309e-07,
## 0.00248461935381122, 0.00117238603350013, 8.3500499000505e-05,
## 5.02117387430545e-06, 6.50640709864774e-08, 0.0103764155660806,
## 0.00292733584237083, 0.000185057648675577, 2.23794372722204e-05,
## 3.33725935462019e-06, 0.00638931399551884, 0.0181866853325732,
## 0.00354059674819561, 6.08251802692677e-05, 3.54532908674686e-05,
## 0.00927739187487513, 0.00349463109959475, 0.000899981296101935,
## 0.00115857888357641, 0.000319090208300621)), .Names = c("mapSize",
## "pSpread", "pmean", "psd"), row.names = c(NA, -25L), class = "data.frame")


