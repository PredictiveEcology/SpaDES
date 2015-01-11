
### MODULE: LccToBeaconsReclassify
###
### DESCRIPTION: Takes the LCC05 classification of 39 land cover classes, and
### reclassifies it to the 11 classes of the Beacon's succession model
### Inputs = vegMapLcc
### Outputs to Global Environment and .tif files:
###     2 maps: trajMapBeacons, vegMapBeacons


### load any required packages
### (use `loadPackages`, or `library` directly)
pkgs <- list("SpaDES")
loadPackages(pkgs)
rm(pkgs)

### event functions:
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - module name and this filename must match;
#   - keep event functions short and clean, modularize by calling
#       subroutines from section below.

### template event
doEvent.LccToBeaconsReclassify = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    ### check for module dependencies:
    ### (use or NULL if no dependencies exist)
    depends <- NULL

    ### check for object dependencies:
    ### (use `checkObject` or similar)
    checkObject(name="vegMapLcc") # Lcc map or a clipped extent version


    # if a required module isn't loaded yet,
    # reschedule this module init for later
    if (reloadModuleLater(sim, depends)) {
      sim <- scheduleEvent(sim, simCurrentTime(sim), "LccToBeaconsReclassify", "init")
    } else {
      sim <- LccToBeaconsReclassifyInit(sim)
    }
    sim <- scheduleEvent(sim, simParams(sim)$LccToBeaconsReclassify$.plotInitialTime,
                         "LccToBeaconsReclassify", "plot")
    sim <- scheduleEvent(sim, simParams(sim)$LccToBeaconsReclassify$.saveInitialTime,
                         "LccToBeaconsReclassify", "save")
  } else if (eventType=="plot") {
    Plot(vegMap, trajMap)
    # schedule future event(s)
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$LccToBeaconsReclassify$.plotInterval, "LccToBeaconsReclassify", "plot")
  } else if (eventType=="save") {
    # the raster package does not keep colors when writing to a tif file
    writeRaster(vegMapBeacons, filename="vegMap.tif", overwrite=TRUE)
    writeRaster(trajMapBeacons, filename="trajMap.tif", overwrite=TRUE)

    # schedule future event(s)
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$LccToBeaconsReclassify$.saveInterval, "LccToBeaconsReclassify", "save")
  } else {
      warning(paste("Undefined event type: '", simEvents(sim)[1, "eventType", with=FALSE],
                    "' in module '", simEvents(sim)[1, "moduleName", with=FALSE], "'", sep=""))
  }
  return(invisible(sim))
}

### template initilization
LccToBeaconsReclassifyInit = function(sim) {

  ### From the table 1 in Word file from Steve Cumming & Pierre Vernier, June 6, 2014
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
                              .Label=c("1,2,3,4,5,6", "1,3,4,5,6", "10",
                                       "11", "2,4", "3,4,5", "3,4,6", "6",
                                       "6", "8", "9"), class = "factor"),
         Description=structure(c(2L, 7L, 6L, 4L, 9L, 5L, 1L, 11L, 10L, 3L, 8L),
                               .Label=c("Burned", "Closed coniferous", "Cropland",
                                        "Deciduous", "Herbaceous", "Mixedwood",
                                        "Open coniferous", "Other", "Shrub",
                                        "Water", "Wetland"), class = "factor")),
    .Names=c("LCC05.classes", "Trajectory", "Description"),
    class="data.frame", row.names = c(NA, -11L))


  lcc05VegReclass <<- structure(
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


  lcc05VegLabels <- as.numeric(strsplit(paste(lcc05VegReclass$LCC05.classes, collapse=","),",")[[1]])
  numLccInVeg <- sapply(strsplit(unname(sapply(as.character(lcc05VegReclass$LCC05.classes), function(x) x)), ","), length)
  lcc05VegTable <- cbind(lcc05VegLabels,rep(lcc05VegReclass$VEG.reclass,numLccInVeg))

  # Trajectory
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
                      numLccInTraj[x],replace=T))))

  #  lcc05TrajTable <- cbind(lcc05TrajLabels,rep(lcc05TrajReclass$Trajectory,numLccInTraj))


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


  trajObj1 <- apply(trajObj.raw[-4,-c(1)],1,function(x) rep(x, times=c(numYearsPer, maxAge+1-sum(numYearsPer))))
  trajObj2 <- cbind(trajObj1,matrix(rep(c("Burned", "Wetland", "Water", "Cropland","Other"), each=maxAge+1), ncol=5))
  trajObj <<- matrix(match(trajObj2,
                           as.character(lcc05TrajReclass$Description))
                     , ncol=ncol(trajObj2))

  vegMap <- reclassify(vegMapLcc, lcc05VegTable)
  setColors(vegMap, n=12 ) <- getColors(vegMapLcc)[[1]][c(1,lcc05VegTable[,1][match(1:11,
                                                                         lcc05VegTable[,2])]+1)]
  assign("vegMapBeacons", vegMap, envir = .GlobalEnv)

  trajMap <- reclassify(vegMapLcc, lcc05TrajTable)
  setColors(trajMap,n=12) <- colorRampPalette(RColorBrewer::brewer.pal(8,"Set1"))(12)
  assign("trajMapBeacons", trajMap, envir = .GlobalEnv)

  # last thing to do is add module name to the loaded list
  simModulesLoaded(sim) <- append(simModulesLoaded(sim), "LccToBeaconsReclassify")

  return(invisible(sim))
}
