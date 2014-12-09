library(SpaDES)

rasterOptions(maxmemory=2e9)
downloadRequired <- FALSE
interactiveExtent <- FALSE

if (Sys.info()["sysname"]=="Linux") {
  setwd("/mnt/shared/shiny_succession")
} else if (Sys.info()["sysname"]=="Windows") {
  setwd("/shared/shiny_succession")
#  setwd("~/Documents/GitHub/SpaDES/SAMPLE/shiny_succession")
} else if (Sys.info()["sysname"]=="Darwin") {
  setwd("~/Documents/GitHub/SpaDES/SAMPLE/shiny_succession")
}

if(downloadRequired) {
  #46MB file
  fN <- "lcc05.zip"
  download.file("ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip", fN, mode="wb")
  dir.create("data")
  unzip(fN, files="data/LCC2005_V1_4a.tif")

  # 9MB file
  download.file("ftp://ftp.daac.ornl.gov/data/nacp/NA_TreeAge//data/can_age04_1km.tif",
                "data/age.tif", mode="wb")
}

fileList <- data.frame(files=c("data/LCC2005_V1_4a.tif", "data/age.tif"),
                         functions="raster", packages="raster",
                         objectNames=c("lcc05", "age"),
                         stringsAsFactors=FALSE)

loadFiles(fileList=fileList)
#ext <- extent(-1380607, -345446, 7211410, 7971750) # large central BC 12 Million
ext <- extent(-1073154, -987285, 7438423, 7512480) # small central Sask 100 Thousand

if(interactiveExtent) {
  dev(2)
  plot(lcc05)
  ext <- drawExtent()
}

vegMapLcc <- crop(lcc05, ext)
if(ncell(vegMapLcc)>1e6) beginCluster()
# age will not run with projectRaster directly. Instead, project the vegMap to age, then crop, then project back to vegMap
vegMapLcc.crsAge <- projectRaster(vegMapLcc, crs=crs(age))
age.crsAge <- crop(age, vegMapLcc.crsAge)
ageMap <- projectRaster(age.crsAge, to=vegMapLcc, method="ngb")
endCluster()

writeRaster(ageMap, filename="data/ageMap.tif", overwrite=TRUE)

#####################################################

lcc05TrajReclass <- structure(
  list(LCC05.classes=structure(c(2L, 11L, 8L, 6L, 3L, 4L, 9L, 5L, 10L, 7L, 1L),
                               .Label=c("0,30,31,32,33,36,38,39", "1",
                                        "16,35", "17,18,20,21,22,23,24,25",
                                        "19", "2,11,12", "26,27,28,29",
                                        "3,4,5,13,14,15", "34", "37",
                                        "6,7,8,9,10"), class="factor"),
       Trajectory=structure(c(2L, 5L, 7L, 6L, 8L, 9L, 1L, 10L, 11L, 3L, 4L),
                            .Label=c("1,2,3,4,5,6", "1,3,4,5,6", "10",
                                     "11", "2,4", "3,4,5", "3,4,6", "6",
                                     "6", "8", "9"), class="factor"),
       Description=structure(c(2L, 7L, 6L, 4L, 9L, 5L, 1L, 11L, 10L, 3L, 8L),
                             .Label=c("Burned", "Closed coniferous", "Cropland",
                                      "Deciduous", "Herbaceous", "Mixedwood",
                                      "Open coniferous", "Other", "Shrub",
                                      "Water", "Wetland"), class="factor")),
  .Names=c("LCC05.classes", "Trajectory", "Description"),
  class="data.frame", row.names=c(NA, -11L))


lcc05VegReclass <- structure(
  list(LCC05.classes=structure(c(2L, 11L, 8L, 6L, 3L, 4L, 9L, 5L, 10L, 7L, 1L),
                               .Label=c("0,30,31,32,33,36,38,39", "1",
                                        "16,35", "17,18,20,21,22,23,24,25",
                                        "19", "2,11,12", "26,27,28,29",
                                        "3,4,5,13,14,15", "34", "37",
                                        "6,7,8,9,10"), class="factor"),
       VEG.reclass=1:11, Description=structure(
         c(2L, 7L, 6L, 4L, 9L, 5L, 1L, 11L, 10L, 3L, 8L),
         .Label=c("Burned", "Closed coniferous",  "Cropland", "Deciduous",
                    "Herbaceous", "Mixedwood", "Open coniferous", "Other",
                    "Shrub", "Water", "Wetland"), class="factor")),
  .Names=c("LCC05.classes", "VEG.reclass", "Description"),
  class="data.frame", row.names=c(NA, -11L))


lcc05VegLabels <- as.numeric(strsplit(paste(lcc05VegReclass$LCC05.classes, collapse=","), ",")[[1]])
numLccInVeg <- sapply(strsplit(unname(sapply(as.character(lcc05VegReclass$LCC05.classes), function(x) x)), ","), length)
lcc05VegTable <- cbind(lcc05VegLabels, rep(lcc05VegReclass$VEG.reclass, numLccInVeg))
vegMap <- reclassify(vegMapLcc, lcc05VegTable)
vegMapColors <<- getColors(lcc05)[[1]][c(1, lcc05VegTable[,1][match(1:11, lcc05VegTable[,2])]+1)]
setColors(vegMap, n=12 ) <- vegMapColors

# the raster package does not keep colors when writing to a tif file
#writeRaster(vegMap, filename="data/vegMap.tif", overwrite=TRUE)

lcc05TrajLabels <- as.numeric(strsplit(paste(lcc05TrajReclass$LCC05.classes, collapse=","), ",")[[1]])
numLccInTraj <- sapply(strsplit(unname(sapply(as.character(lcc05TrajReclass$LCC05.classes), function(x) x)), ","), length)

lcc05TrajReclass$TrajectoryNum <- lapply(as.character(
  lcc05TrajReclass$Trajectory), function(x) as.numeric(strsplit(x,",")[[1]]))

lcc05TrajTable <- cbind(
  lcc05TrajLabels,
  unlist(
    lapply(1:length(lcc05TrajReclass$TrajectoryNum),
           function(x)
             sample(lcc05TrajReclass$TrajectoryNum[[x]],
                    numLccInTraj[x], replace=TRUE))))

#  lcc05TrajTable <- cbind(lcc05TrajLabels, rep(lcc05TrajReclass$Trajectory, numLccInTraj))
trajMap <<- reclassify(vegMapLcc, lcc05TrajTable)
setColors(trajMap, n=12) <- RColorBrewer::brewer.pal(9, "YlGn")

# trajObj.raw <- read.table(file="clipboard", sep="\t", header=TRUE, stringsAsFactors=FALSE)
# dput(trajObj.raw)
trajObj.raw <- structure(
  list(Veg.Type=c("Closed coniferous", "Open coniferous", "Mixedwood",
                  "Deciduous*", "Deciduous*", "Shrub", "Herbaceous"),
       X0.2=c("Burned", "Burned", "Burned", "Burned", "Burned", "Burned", "Burned"),
       X3.20=c("Closed coniferous", "Open coniferous", "Deciduous",
               "Deciduous", "Deciduous", "Shrub", "Herbaceous"),
       X21.60=c("Closed coniferous", "Open coniferous", "Mixedwood",
                "Mixedwood", "Deciduous", "Shrub", "Herbaceous"),
       X61.80=c("Closed coniferous", "Open coniferous", "Mixedwood",
                "Mixedwood", "Deciduous", "Mixedwood", "Herbaceous"),
       X81.120=c("Closed coniferous", "Open coniferous", "Mixedwood",
                 "Mixedwood", "Deciduous", "Mixedwood", "Herbaceous"),
       X121.160=c("Closed coniferous", "Open coniferous", "Mixedwood",
                  "Open coniferous", "Deciduous", "Closed coniferous",
                  "Herbaceous"),
       X.160=c("Closed coniferous", "Open coniferous", "Closed coniferous",
               "Closed coniferous", "Closed coniferous", "Closed coniferous",
               "Herbaceous")),
  .Names=c("Veg.Type", "X0.2", "X3.20", "X21.60", "X61.80", "X81.120", "X121.160", "X.160"),
  class="data.frame", row.names=c(NA, -7L))

numYearsPer <- na.omit(unlist(lapply(strsplit(substr(colnames(trajObj.raw), 2, 9), "\\."),
                                     function(x) diff(as.numeric(x))))+1)
maxAge <- 200
ages <- 0:maxAge


trajObj1 <- apply(trajObj.raw[-4, -c(1)], 1, function(x) rep(x, times=c(numYearsPer, maxAge+1-sum(numYearsPer))))
trajObj2 <- cbind(trajObj1, matrix(rep(c("Burned", "Wetland", "Water", "Cropland", "Other"), each=maxAge+1), ncol=5))
trajObj <<- matrix(match(trajObj2, as.character(lcc05TrajReclass$Description)), ncol=ncol(trajObj2))

fileList <- data.frame(files=c("data/vegMap.tif", "data/ageMap.tif"),
                       functions="rasterToMemory", packages="SpaDES",
                       stringsAsFactors=FALSE)
loadFiles(fileList=fileList)
ageMapInit <<- ageMap
vegMapInit <<- vegMap
setColors(vegMapInit, n=12 ) <- vegMapColors
setColors(ageMapInit, n=201) <- colorRampPalette(c("LightGreen", "DarkGreen"))(50)


########################################################################
########################################################################
########################################################################
if (Sys.info()["sysname"]=="Linux") {
  setwd("/mnt/shared/shiny_succession")
} else if (Sys.info()["sysname"]=="Windows") {
#  setwd("/shared/shiny_succession")
  setwd("~/Documents/GitHub/SpaDES/SAMPLE/shiny_succession")
} else if (Sys.info()["sysname"]=="Darwin") {
  setwd("~/Documents/GitHub/SpaDES/SAMPLE/shiny_succession")
}

shinyServer(function(input, output) {

    layers <- reactive({
      times=list(start=2005, stop=input$stopTime)
      parameters <- list(.globals=list(burnStats="nPixelsBurned"),
                         #.progress=list(NA),
                         .progress=list(.graphical=TRUE, .progressInterval=1),
                         forestSuccession=list(returnInterval=1, startTime=2005,
                                               .plotInitialTime=NA, .plotInterval=1),
                         forestAge=list(returnInterval=1, startTime=2005.5,
                                        .plotInitialTime=NA, .plotInterval=1),
                         fireSpreadLcc=list(nFires=10,
                                            its=1e6, drought=input$drought,
                                            persistprob=0, returnInterval=1, startTime=2006,
                                            .plotInitialTime=NA, .plotInterval=1),
                         caribouMovementLcc=list(N=1e4, moveInterval=1, startTime=2006,
                                              .plotInitialTime=NA, .plotInterval=1)
      )

      modules <- list(if(input$successionModule) "forestSuccession",
                      "forestAge",
                      if(input$fireModule) "fireSpreadLcc",
                      if(input$caribouModule) "caribouMovementLcc")
      path <- file.path("modules")

      ageMap <- get("ageMapInit", envir=.GlobalEnv)
      assign("ageMap", ageMap, envir=.GlobalEnv)
      vegMap <- get("vegMapInit", envir=.GlobalEnv)
      assign("vegMap", vegMap, envir=.GlobalEnv)

      Fires <<- raster(extent(vegMap), ncol=ncol(vegMap),
                      nrow=nrow(vegMap), vals=0)

      FiresCumul <<- raster(extent(vegMap), ncol=ncol(vegMap),
                      nrow=nrow(vegMap), vals=0)

      mySim <- simInit(times=times, params=parameters, modules=modules, path=path)

      nPixelsBurned <<- numeric(0)
      mySim <- spades(mySim, debug=FALSE)
      return(list(ageMap=get("ageMap", envir=.GlobalEnv),
                  vegMap=get("vegMap", envir=.GlobalEnv),
                  nPixelsBurned=get("nPixelsBurned", envir=.GlobalEnv),
                  caribouRas=get("caribouRas", envir=.GlobalEnv),
                  caribou=get("caribou", envir=.GlobalEnv),
                  FiresCumul=get("FiresCumul", envir=.GlobalEnv)
))
    })

    output$lcc05 <- renderPlot({
      plot(lcc05)
      plot(ext, add=TRUE, lwd=3, col="yellow")
    })

    output$mapsInit <- renderPlot({
      Plot(ageMapInit, vegMapInit, new=TRUE, title=FALSE)
#       seekViewport("top")
#       grid.text(y=0.95, "2005", gp=gpar(cex=2.5))
      seekViewport("ageMapInit.age")
      grid.text(y=1.05, "Forest Age", gp=gpar(cex=1.5))
      seekViewport("vegMapInit")
      grid.text(y=1.05, "Forest Cover", gp=gpar(cex=1.5))
    })

    output$maps <- renderPlot({
      Plot(layers()$ageMap, layers()$vegMap, new=TRUE, title=FALSE)
#       seekViewport("top")
#       grid.text(y=0.95, input$stopTime, gp=gpar(cex=2.5))
      seekViewport("ageMap.age")
      grid.text(y=1.05, "Forest Age", gp=gpar(cex=1.5))
      seekViewport("vegMap")
      grid.text(y=1.05, "Forest Cover", gp=gpar(cex=1.5))
    })

    output$initHists <- renderPlot({
      layout(matrix(c(1,2),byrow=TRUE, ncol=2))
      age <- ageMapInit
      hist(getValues(age), freq=FALSE, axes=FALSE, breaks=seq(0, 200, length.out=21),
           col=colorRampPalette(getColors(age)[[1]])(20),
           main=paste("Forest age in 2005"), ylim=c(0, 6e3/ncell(age)),
           cex.main=1.5, ylab="Hectares", xlab="Forest age")
      axis(side=2,at=c(0,4e3/ncell(age)),labels=round(c(0, 4e3*6.25),0))
      axis(side=1)
      hist(getValues(vegMapInit), freq=FALSE, axes=FALSE, breaks=0:11, col=vegMapColors[1:12],
           main=paste("Vegetation type in 2005"),
           cex.main=1.5, ylab="Hectares", xlab="Vegetation type")
      axis(side=2, at=c(0, 2e4/ncell(age)), labels=c(0, 2e4*6.25))
      axis(side=1)
    })

    output$endHists <- renderPlot({
      layout(matrix(c(1, 2), byrow=TRUE, ncol=2))
      age <- layers()$ageMap
      hist(getValues(age), freq=FALSE, axes=FALSE, breaks=seq(0, 200, length.out=21),
           col=colorRampPalette(getColors(age)[[1]])(20),
           main=paste("Forest age in year", input$stopTime), #ylim=c(0, 6e3/ncell(age)),
           cex.main=1.5, ylab="Hectares", xlab="Forest age")
      axis(side=2, at=c(0, 4e3/ncell(age)), labels=round(c(0, 4e3*6.25), 0))
      axis(side=1)
      hist(getValues(layers()$vegMap), freq=FALSE, axes=FALSE, breaks=0:11, col=vegMapColors[1:12],
           main=paste("Vegetation type in year", input$stopTime),
           cex.main=1.5, ylab="Hectares", xlab="Vegetation type")
      axis(side=2, at=c(0, 2e4/ncell(age)), labels=c(0, 2e4*6.25))
      axis(side=1)
    })

    output$fireHist <- renderPlot({
      if(input$fireModule) {
        hist(layers()$nPixelsBurned*6.25, col="grey", main="Annual area burned (ha)",
             xlab="", ylab="Num. years", cex.main=1.5)
      }
    })

    output$Fire <- renderPlot({
      if(input$fireModule) {
        Plot(layers()$FiresCumul,
             new=TRUE, title=FALSE, pch=".")
        seekViewport("top")
        seekViewport("FiresCumul")
        grid.text(y=1.05, "Cumulative fires burned", gp=gpar(cex=1.4))
      }
    })

    output$caribou <- renderPlot({
      if(input$caribouModule) {
        Plot(layers()$caribouRas, cols=rev(heat.colors(maxValue(layers()$caribouRas))),
             new=TRUE, title=FALSE, pch=".")
        seekViewport("top")
        seekViewport("caribouRas")
        grid.text(y=1.1, paste("Caribou densities between 2005 and", input$stopTime,
                               "\nPopulation size =", length(layers()$caribou)), gp=gpar(cex=1.4))

      }
    })

    output$numPixels <- renderText({
      paste("Cropping, reprojecting and simulating an area of", ncell(vegMap)*6.25,
            "ha, with",ncell(vegMap),"pixels")

    })

    output$endYear <- renderText({
      paste(input$stopTime)

    })

})
