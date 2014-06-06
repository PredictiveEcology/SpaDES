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
    #dput(lcc05TrajReclass[,c("LCC05.classes","Trajectory")])
    #dput(lcc05TrajReclass[,c("LCC05.classes","VEG.reclass")])
    
    lcc05TrajReclass <- structure(list(LCC05.classes = structure(c(2L, 11L, 8L, 6L, 3L, 
                   4L, 9L, 5L, 10L, 7L, 1L), .Label = c("0,30,31,32,33,36,38,39", 
                  "1", "16,35", "17,18,20,21,22,23,24,25", "19", "2,11,12", "26,27,28,29", 
                  "3,4,5,13,14,15", "34", "37", "6,7,8,9,10"), class = "factor"), 
                  Trajectory = structure(c(2L, 5L, 7L, 6L, 8L, 9L, 1L, 10L, 
                  11L, 3L, 4L), .Label = c("1,2,3,4,5,6,7", "1,3,4,5,6", "10", 
                  "11", "2,4", "3,4,5", "3,4,6", "6", "7", "8", "9"), class = "factor")), 
                  .Names = c("LCC05.classes", "Trajectory"), class = "data.frame",
                  row.names = c(NA, -11L))
    lcc05VegReclass <- structure(list(LCC05.classes = structure(c(2L, 11L, 8L, 6L, 3L, 
                  4L, 9L, 5L, 10L, 7L, 1L), .Label = c("0,30,31,32,33,36,38,39", 
                  "1", "16,35", "17,18,20,21,22,23,24,25", "19", "2,11,12", "26,27,28,29", 
                  "3,4,5,13,14,15", "34", "37", "6,7,8,9,10"), class = "factor"), 
                  VEG.reclass = 1:11), .Names = c("LCC05.classes", "VEG.reclass"
                  ), class = "data.frame", row.names = c(NA, -11L))
    lcc05Labels <- as.numeric(strsplit(paste(lcc05VegReclass$LCC05.classes,collapse = ","),",")[[1]])
    numLccInVeg <- sapply(strsplit(unname(sapply(as.character(lcc05VegReclass$LCC05.classes),function(x) x)),","),length)
    lcc05VegTable <- cbind(lcc05Labels,rep(lcc05VegReclass$VEG.reclass,numLccInVeg))
    vegMap <- reclassify(lcc05.cr,lcc05VegTable)
    
    lcc05Labels <- as.numeric(strsplit(paste(lcc05TrajReclass$LCC05.classes,collapse = ","),",")[[1]])
    numLccInTraj <- sapply(strsplit(unname(sapply(as.character(lcc05TrajReclass$LCC05.classes),function(x) x)),","),length)
    lcc05TrajTable <- cbind(lcc05Labels,rep(lcc05TrajReclass$Trajectory,numLccInTraj))
    trajMap <- reclassify(lcc05.cr,lcc05TrajTable)
    
    
    VEG reclass    LCC05 classes	Description	Trajectory
    1	1	Closed coniferous	1,3,4,5,6
    2	6,7,8,9,10	Open coniferous	2,4
    3	3,4,5,13,14,15	Mixedwood	3,4,6
    4	2,11,12	Deciduous	3,4,5
    5	16,35	Shrub	6
    6	17,18,20,21,22,23,24,25	Herbaceous	7
    7	34	Burned	1,2,3,4,5,6,7
    8	19	Wetland	8
    9	37	Water	9
    10	26,27,28,29	Cropland	10
    11	0,30,31,32,33,36,38,39	Other	11
    
    
    trajLabels <- 
    lcc05Labels <- as.numeric(strsplit(paste(lccReclass$LCC05.Labels,collapse = ","),",")[[1]])
    rep(1:7,length.out = 39)
    
    trajTable1 <- read.table(file = "clipboard",sep = "\t",header =T)
    trajTable <- 
        
    lccReclass <- structure(list(BAM.Land.Cover.Class.Name = structure(c(2L, 3L, 
                                                                         5L, 6L, 4L, 9L, 17L, 18L, 16L, 11L, 14L, 12L, 15L, 10L, 13L, 
                                                                         7L, 1L, 8L), .Label = c("Burns ", "Closed Coniferous ", "Closed Deciduous ", 
                                                                                                 "Closed Deciduous Mixed ", "Closed Mature Mixed ", "Closed Young Mixed ", 
                                                                                                 "Mixed Forest/Crop ", "Not Used ", "Open Coniferous ", "Open Herb/Grass ", 
                                                                                                 "Open Mature Deciduous ", "Open Mixed ", "Open Northern ", "Open Young Deciduous ", 
                                                                                                 "Open Young Mixed ", "Poorly Drained ", "Sparse Coniferous ", 
                                                                                                 "Sparse Coniferous Shield "), class = "factor"), BAM.Code = c(1, 
                                                                                                                                                               2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 14, 15, 17, 21, 26, 33, NA), 
                                 LCC05.Labels = structure(c(1L, 8L, 11L, 14L, 15L, 16L, 17L, 
                                                            18L, 2L, 3L, 4L, 5L, 6L, 7L, 9L, 10L, 12L, 13L), .Label = c("1", 
                                                                                                                        "10,19", "11", "12,16", "14", "15", "17,18", "2", "21,22,23,24,25,30,31,32", 
                                                                                                                        "26,27,28,29", "3", "33,34,35", "36,37,38,39", "4", "5", 
                                                                                                                        "6,7", "8,13,20", "9"), class = "factor")), .Names = c("BAM.Land.Cover.Class.Name", 
                                                                                                                                                                               "BAM.Code", "LCC05.Labels"), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                                -18L))
    
    lcc05Labels <- as.numeric(strsplit(paste(lccReclass$LCC05.Labels,collapse = ","),",")[[1]])
    
    numOldInNew <- sapply(strsplit(unname(sapply(as.character(lccReclass$LCC05.Labels),function(x) x)),","),length)
    
    lcc05Reclass <- cbind(lcc05Labels,rep(lccReclass$BAM.Code,numOldInNew))
    
    lcc05BAM <- reclassify(lcc05,lcc05Reclass)

    
    
    simplot(stack(lcc05,trajMap), speedup=10,add=F, 
            col=list(brewer.pal(9,"YlGnBu"),brewer.pal(10,"Set3")))
    
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
