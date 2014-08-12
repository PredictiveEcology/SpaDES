################################################
###
### SUCCESSION MODULE
### - change the composition of cells
###
###############################################

doEvent.succession <- function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    ### check for module dependencies
    # if a required module isn't loaded yet,
    # reschedule this module init for later
    depends <- "age" # list module names here

    if (reloadModulesLater(deparse(sim), depends)) {
        sim <- scheduleEvent(sim, simCurrentTime(sim), "succession", "init")
    } else {
        # do stuff for this event
        sim <- successionInit(sim)

        # schedule the next event
        sim <- scheduleEvent(sim, 0.5, "succession", "succession")
    }
  } else if (eventType=="succession") {
    # do stuff for this event
    sim <- successionSuccession(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim)+1.0, "succession", "succession")
  } else if (eventType=="plot") {
    # do stuff for this event
    sim <- successionPlot(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim)+1.0, "succession", "plot")
  } else {
    warning(paste("Undefined event type: \'", simEvents(sim)[1,"eventType", with=FALSE],
                  "\' in module \'", simEvents(sim)[1, "moduleName", with=FALSE], "\'", sep=""))
  }
  return(sim)
}

successionInit <- function(sim) {
  ### load any required packages
  pkgs <- list("raster", "RColorBrewer") # list required packages here
  loadPackages(pkgs)

  # Reclassify lcc05 to trajMap
  lcc05 <<- if(!exists("lcc05"))
      raster("C:/shared/data/shared/LandCoverOfCanada2005_V1_4/LCC2005_V1_4a.tif")
#    plot(lcc05)
  ext <- extent(-1073154,-987285,7438423,7512480)
  lcc05.cr <<- crop(lcc05,ext)
#    CRS.lcc05 <- crs(lcc05.cr)

  lcc05Labels <- 0:39

  ### From the table 1 in Word file from Steve CUmming & Pierre Vernier, June 6, 2014
  ###  09 A5 MDR ANslysis V4_SL.docx
  #
  # lcc05TrajReclass <- read.table(file="clipboard", header=TRUE, sep="\t")
  # dput(lcc05TrajReclass[,c("LCC05.classes","Trajectory","Description")])
  # dput(lcc05TrajReclass[,c("LCC05.classes","VEG.reclass","Description")])
  #

  lcc05TrajReclass <- structure(
    list(LCC05.classes=structure(c(2L, 11L, 8L, 6L, 3L, 4L, 9L, 5L, 10L, 7L, 1L),
                                 .Label=c("0,30,31,32,33,36,38,39", "1",
                                          "16,35", "17,18,20,21,22,23,24,25",
                                          "19", "2,11,12", "26,27,28,29",
                                          "3,4,5,13,14,15", "34", "37",
                                          "6,7,8,9,10"), class = "factor"),
         Trajectory=structure(c(2L, 5L, 7L, 6L, 8L, 9L, 1L, 10L, 11L, 3L, 4L),
                                .Label=c("1,2,3,4,5,6,7", "1,3,4,5,6", "10",
                                         "11", "2,4", "3,4,5", "3,4,6", "6",
                                         "7", "8", "9"), class = "factor"),
         Description=structure(c(2L, 7L, 6L, 4L, 9L, 5L, 1L, 11L, 10L, 3L, 8L),
                               .Label=c("Burned", "Closed coniferous", "Cropland",
                                        "Deciduous", "Herbaceous", "Mixedwood",
                                        "Open coniferous", "Other", "Shrub",
                                        "Water", "Wetland"), class = "factor")),
    .Names=c("LCC05.classes", "Trajectory", "Description"),
    class="data.frame", row.names = c(NA, -11L))


  lcc05VegReclass <- structure(
    list(LCC05.classes=structure(c(2L, 11L, 8L, 6L, 3L, 4L, 9L, 5L, 10L, 7L, 1L),
                                 .Label=c("0,30,31,32,33,36,38,39", "1",
                                          "16,35", "17,18,20,21,22,23,24,25",
                                          "19", "2,11,12", "26,27,28,29",
                                          "3,4,5,13,14,15", "34", "37",
                                          "6,7,8,9,10"), class = "factor"),
         VEG.reclass=1:11, Description=structure(
           c(2L, 7L, 6L, 4L, 9L, 5L, 1L, 11L, 10L, 3L, 8L),
           .Label = c("Burned", "Closed coniferous",  "Cropland", "Deciduous",
                      "Herbaceous", "Mixedwood", "Open coniferous", "Other",
                      "Shrub", "Water", "Wetland"), class = "factor")),
    .Names = c("LCC05.classes", "VEG.reclass", "Description"),
    class = "data.frame", row.names = c(NA, -11L))

  lcc05Labels <- as.numeric(strsplit(paste(lcc05VegReclass$LCC05.classes, collapse=","),",")[[1]])
  numLccInVeg <- sapply(strsplit(unname(sapply(as.character(lcc05VegReclass$LCC05.classes), function(x) x)), ","), length)
  lcc05VegTable <- cbind(lcc05Labels,rep(lcc05VegReclass$VEG.reclass,numLccInVeg))
  vegMap <- reclassify(lcc05.cr, lcc05VegTable)

  lcc05Labels <- as.numeric(strsplit(paste(lcc05TrajReclass$LCC05.classes, collapse=","), ",")[[1]])
  numLccInTraj <- sapply(strsplit(unname(sapply(as.character(lcc05TrajReclass$LCC05.classes), function(x) x)), ","), length)
  lcc05TrajTable <- cbind(lcc05Labels,rep(lcc05TrajReclass$Trajectory,numLccInTraj))
  trajMap <- reclassify(lcc05.cr, lcc05TrajTable)

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

  numYearsPer <- na.omit(unlist(lapply(strsplit(substr(colnames(trajObj.raw),2,9),"\\."), function(x) diff(as.numeric(x))))+1)
  maxAge <- 200
  ages <- 0:maxAge
#    out = unname( unlist(rep(trajObj.raw[1,-1],times = c(numYearsPer,40))))

  trajObj1 <- apply(trajObj.raw[,-1],1,function(x) rep(x, times=c(numYearsPer, 40)))
  trajObj2 <- cbind(trajObj1,matrix(rep(c("Burned", "Wetland", "Water", "Cropland"), each=201), ncol=4))
  trajObj <- matrix(match(trajObj2, lcc05VegReclass$Description), ncol=11)

  plot(stack(trajMap))
#, speedup=10, add=FALSE, col=list(brewer.pal(9,"YlGnBu"), brewer.pal(10,"Set3")))

  # last thing to do is add module name to the loaded list
  simLoaded(sim) <- append(simLoaded(sim), "succession")

  return(sim)
}

successionSuccession <- function(sim) {
  ageMap.v <- round(getValues(ageMap))
  trajMap.v <- getValues(trajMap)

    vegMap.v <- trajObj[cbind(ageMap.v,trajMap.v)]
    vegMap <- raster(ageMap)
    vegMap <- setValues(vegMap,vegMap.v)

    vegMap[indStatics] <<- valsStatics

    return(sim)
}

successionPlot <- function(sim) {
    simPlot(vegMap, add=FALSE, speedup=20, col=brewer.pal(10,"Set3"))
    return(sim)
}
