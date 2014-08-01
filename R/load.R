################################################
###
### A LOAD MODULE
###
###############################################

# Just checks for paths, creates them if they do not exist
doEvent.load = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {

    simLoad(sim)

  }
  return(sim)
}


##############################################################
#' Load simulation objects according to fileList
#'
#' This function takes the fileList in the .sim object and loads all the files
#' using the identified functions and arguments
#' 
#' @author Eliot McIntire
#' @author Alex Chubaty
#'
#' @export
#' @docType methods
#' @rdname simLoad
#'
# @examples
# need examples
simLoad = function(sim) {
  fileList <- simParams(sim)$fileList
  curTime <- simCurrentTime(sim)
  
  # fill in columns if they are missing. Assume loadTimes = 0 if missing
  if(is.na(match("loadTimes",names(fileList)))) {
    fileList["loadTimes"] <- 0
  }
    
  # only load those that are to be loaded at their loadTime
  filesCurTime <- fileList[fileList$loadTimes==curTime,]
  
  fl <- filesCurTime[,"files"]
  
  fl.list <- strsplit(fl,".",fixed=TRUE)
  
  # use filenames as object names, unless alternative provided in fileList$obj
  objs <- sapply(fl.list,function(x) paste(x[-length(x)],collapse=".")) 
  if(!is.na(match("objs",names(fileList)))) {
    loadFun[!is.na(fileList$objs)] <- fileList$objs
  }
  
  # extract file extensions, to be used to determine which function to use
  exts <- sapply(fl.list,function(x) x[length(x)])
  
  # determine which function to load with
  loadFun <- as.character(.fileExtensions[exts,funs][,funs])
  # correct those for which a specific function is given in fileList$fun
  if(!is.na(match("funs",names(fileList)))) {
    loadFun[!is.na(fileList[,"funs"])] <- fileList[,"funs"]
  }
  
  # identify arguments
#  args <- filesCurTime$args
  
  
  # raster function sometimes loads file to disk; this will be made explicit
  where <- c("disk","memory")
  
  # load files
  a <- lapply(1:length(fl), function(x) {
    assign(objs[x],get(loadFun[x])(fl[x],native=TRUE,
                                       env=.GlobalEnv))
    if (loadFun[x]=="raster") {
      print(paste(objs[x],"read to",where[inMemory(get(objs[x]))+1],"from",fl[x],"using",loadFun[x]))
    } else {
      print(paste(objs[x],"read to memory from",fl[x],"using",loadFun[x]))
    }
  })
  
  # rasters sometimes don't load with their min and max values set
  israst = sapply(objs, function(x) is(get(x),"Raster"))
  a = lapply(objs[israst],function(x) {
    assign(x, setMinMax(get(x)),env=.GlobalEnv)
  })

  # add new rows of files to load based on fileList$Interval
  if(!is.na(match("intervals",names(fileList)))) {
    if (any(!is.na(fileList$intervals))) {
      keep <- !is.na(fileList$interval) & fileList$interval>curTime
      fileList$loadTime[keep] <- curTime + fileList$interval[keep]
    }
  }
    
  
  # remove files that have been loaded from fileList
  fileList = fileList[fileList$loadTimes!=curTime,]

  simParams(sim)$fileList <- fileList
  if(nrow(fileList)>0)
    sim <- scheduleEvent(sim, min(fileList$loadTime), "load", "init")
  
  # reassign the simList to global environment
  assign(simname, sim, envir=globalenv())
  
}   

   
   # 
#   # extract savePaths from modules
#   modulePaths <- lapply(simParams(sim), function(y) return(y$loadPath) )
# 
#   # extract objects to save from modules
#   toLoad <- lapply(simParams(sim), function(y) return(y$toLoad) )
# 
#   # extract the current module name that called this function
#   moduleName = simEvents(sim)[1,moduleName]
# 
#   # if no savePath is specified, use active working directory
#   if (is.null(toLoad[[moduleName]])) {
#     modulePaths[[moduleName]] <- "."
#   }
# 
#   txtTime = sprintf(paste0("%0", nchar(simStopTime(sim)), "d"), simCurrentTime(sim))
# 
#   # save objects to a filename that has same name as object name, plus current simulation time
#   lapply(toLoad[[moduleName]], function(objectname) {
#     saveRDS(get(objectname),
#             file.path(modulePaths[[moduleName]],
#                       paste(objectname, txtTime, ".rds", sep="")) )})
# 
# }

.fileExtensions = data.table(matrix(ncol=2,byrow=TRUE,c(
  "tif", "raster",
  "png", "raster",
  "csv", "read.csv",
  "shp", "readOGR",
  "txt", "read.table",
  "asc", "raster")))
setnames(.fileExtensions,c("exts","funs"))
setkey(.fileExtensions,exts)

