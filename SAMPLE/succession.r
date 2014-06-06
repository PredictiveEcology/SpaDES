################################################
###
### SUCCESSION MODULE
### - change the composition of cells
###
###############################################



### event functions:
#   - follow the naming convention `moduleName.eventType()`;
#   - `moduleName.init()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling
#       subroutines from section below.
do.event.succession = function(sim, event.time, event.type, debug=FALSE) {
    if (event.type=="init") {
        ### check for module dependencies
        # if a required module isn't loaded yet,
        # reschedule this module init for later
        depends = "age" # list module names here
        
        if (reload.module.later(sim, depends)) {
            sim <- schedule.event(sim, currentTime(sim), "succession", "init")
        } else {
            # do stuff for this event
            sim <- succession.init(sim)
            
            # schedule the next event
            sim <- schedule.event(sim, 0.5, "succession", "succession")
        }
    } else if (event.type=="succession") {
        # do stuff for this event
        sim <- succession(sim)
        
        # schedule the next event
        sim <- schedule.event(sim, currentTime(sim)+1.0, "succession", "succession")
    } else {
        print("polar bears. grr!")
    }
    return(sim)
}

succession.init = function(sim) {
    ### load any required packages
    pkgs = list("raster", "RColorBrewer") # list required packages here
    load.packages(pkgs)

    # Reclassify lcc05 to trajMap
    lcc05 <<- if(!exists("lcc05")) 
        raster("C:/shared/data/shared/LandCoverOfCanada2005_V1_4/LCC2005_V1_4a.tif")
#    plot(lcc05)
    ext <- extent(-1073154,-987285,7438423,7512480)
    lcc05.cr <<- crop(lcc05,ext)
#    CRS.lcc05 <- crs(lcc05.cr)
    
    lcc05Labels <- 0:39
    # From the table 1 in Word file from Steve CUmming & Pierre Vernier, June 6, 2014
    #  09 A5 MDR ANslysis V4_SL.docx
    #lcc05TrajReclass <- read.table(file = "clipboard",header = T, sep="\t")
    #dput(lcc05TrajReclass[,c("LCC05.classes","Trajectory","Description")])
    #dput(lcc05TrajReclass[,c("LCC05.classes","VEG.reclass","Description")])
    
    lcc05TrajReclass <- structure(list(LCC05.classes = structure(c(2L, 11L, 8L, 6L, 3L, 
                                                                   4L, 9L, 5L, 10L, 7L, 1L), .Label = c("0,30,31,32,33,36,38,39", 
                                                                                                        "1", "16,35", "17,18,20,21,22,23,24,25", "19", "2,11,12", "26,27,28,29", 
                                                                                                        "3,4,5,13,14,15", "34", "37", "6,7,8,9,10"), class = "factor"), 
                                       Trajectory = structure(c(2L, 5L, 7L, 6L, 8L, 9L, 1L, 10L, 
                                                                11L, 3L, 4L), .Label = c("1,2,3,4,5,6,7", "1,3,4,5,6", "10", 
                                                                                         "11", "2,4", "3,4,5", "3,4,6", "6", "7", "8", "9"), class = "factor"), 
                                       Description = structure(c(2L, 7L, 6L, 4L, 9L, 5L, 1L, 11L, 
                                                                 10L, 3L, 8L), .Label = c("Burned", "Closed coniferous", "Cropland", 
                                                                                          "Deciduous", "Herbaceous", "Mixedwood", "Open coniferous", 
                                                                                          "Other", "Shrub", "Water", "Wetland"), class = "factor")), .Names = c("LCC05.classes", 
                                                                                                                                                                "Trajectory", "Description"), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                  -11L))

    lcc05VegReclass <- structure(list(LCC05.classes = structure(c(2L, 11L, 8L, 6L, 3L, 
                                                                  4L, 9L, 5L, 10L, 7L, 1L), .Label = c("0,30,31,32,33,36,38,39", 
                                                                                                       "1", "16,35", "17,18,20,21,22,23,24,25", "19", "2,11,12", "26,27,28,29", 
                                                                                                       "3,4,5,13,14,15", "34", "37", "6,7,8,9,10"), class = "factor"), 
                                      VEG.reclass = 1:11, Description = structure(c(2L, 7L, 6L, 
                                                                                    4L, 9L, 5L, 1L, 11L, 10L, 3L, 8L), .Label = c("Burned", "Closed coniferous", 
                                                                                                                                  "Cropland", "Deciduous", "Herbaceous", "Mixedwood", "Open coniferous", 
                                                                                                                                  "Other", "Shrub", "Water", "Wetland"), class = "factor")), .Names = c("LCC05.classes", 
                                                                                                                                                                                                        "VEG.reclass", "Description"), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                                                           -11L))

    lcc05Labels <- as.numeric(strsplit(paste(lcc05VegReclass$LCC05.classes,collapse = ","),",")[[1]])
    numLccInVeg <- sapply(strsplit(unname(sapply(as.character(lcc05VegReclass$LCC05.classes),function(x) x)),","),length)
    lcc05VegTable <- cbind(lcc05Labels,rep(lcc05VegReclass$VEG.reclass,numLccInVeg))
    vegMap <- reclassify(lcc05.cr,lcc05VegTable)
    
    lcc05Labels <- as.numeric(strsplit(paste(lcc05TrajReclass$LCC05.classes,collapse = ","),",")[[1]])
    numLccInTraj <- sapply(strsplit(unname(sapply(as.character(lcc05TrajReclass$LCC05.classes),function(x) x)),","),length)
    lcc05TrajTable <- cbind(lcc05Labels,rep(lcc05TrajReclass$Trajectory,numLccInTraj))
    trajMap <- reclassify(lcc05.cr,lcc05TrajTable)
    
    #trajObj.raw <- read.table(file = "clipboard",se="\t",header = T)
    #dput(trajObj.raw)
    trajObj.raw <- structure(list(Veg.Type = structure(c(1L, 5L, 4L, 2L, 2L, 6L, 3L), .Label = c("Closed coniferous", "Deciduous*", "Herbaceous", 
               "Mixedwood", "Open coniferous", "Shrub"), class = "factor"), 
               X0.2 = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = "Burned", class = "factor"), 
               X3.20 = structure(c(1L, 4L, 2L, 2L, 2L, 5L, 3L), .Label = c("Closed coniferous", 
               "Deciduous", "Herbaceous", "Open coniferous", "Shrub"), class = "factor"), 
               X21.60 = structure(c(1L, 5L, 4L, 4L, 2L, 6L, 3L), .Label = c("Closed coniferous", 
               "Deciduous", "Herbaceous", "Mixedwood", "Open coniferous", 
               "Shrub"), class = "factor"), X61.80 = structure(c(1L, 5L, 
               4L, 4L, 2L, 4L, 3L), .Label = c("Closed coniferous", "Deciduous", 
               "Herbaceous", "Mixedwood", "Open coniferous"), class = "factor"), 
               X81.120 = structure(c(1L, 5L, 4L, 4L, 2L, 4L, 3L), .Label = c("Closed coniferous", 
               "Deciduous", "Herbaceous", "Mixedwood", "Open coniferous"
               ), class = "factor"), X121.160 = structure(c(1L, 5L, 4L, 
               5L, 2L, 1L, 3L), .Label = c("Closed coniferous", "Deciduous", 
               "Herbaceous", "Mixedwood", "Open coniferous"), class = "factor"), 
               X.160 = structure(c(1L, 3L, 1L, 1L, 1L, 1L, 2L), .Label = c("Closed coniferous", 
               "Herbaceous", "Open coniferous"), class = "factor")), .Names = c("Veg.Type", 
               "X0.2", "X3.20", "X21.60", "X61.80", "X81.120", "X121.160", "X.160"
               ), class = "data.frame", row.names = c(NA, -7L))
    numYearsPer <- na.omit(unlist(lapply(strsplit(substr(colnames(trajObj.raw),2,9),"\\."),function(x) diff(as.numeric(x))))+1)
    maxAge <- 200
    ages <- 0:maxAge

    lapply(1:11,function(x) apply(t(trajObj.raw),2,)
           
    
    
     
    

    

    
        
    plot(stack(trajMap))
#, speedup=10,add=F, 
#            col=list(brewer.pal(9,"YlGnBu"),brewer.pal(10,"Set3")))
    
    # last thing to do is add module name to the loaded list
    sim.loaded(sim) <- append(sim.loaded(sim), "succession")
    
    return(sim)
}

succession.succession = function(sim) {

    vegMap <<- trajObj[ageMap,trajMap]
    vegMap[indStatics] <<- valsStatics
    
    return(sim)
}

succession.report = function(sim) {
    

    simplot(vegMap, add=FALSE, speedup=20, 
            col=brewer.pal(10,"Set3"))
    return(sim)
}
