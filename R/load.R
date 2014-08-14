################################################
###
### A LOAD MODULE
###
###############################################

# Just checks for paths, creates them if they do not exist
doEvent.load = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {

    sim <- simLoad(sim) #, stackName="maps2")

  }
  return(sim)
}


##############################################################
#' Load simulation objects according to fileList
#'
#' This function takes the fileList argument in the simList object and loads all the files
#' using the identified functions and arguments
#'
#' In the fileList object, either a list or a data.frame, there will be minimally a column called "files".
#' All other columns are optional.
#'
#' Other optional columns are:
#'
#' - "objectNames", a character string indicating the name of the object once the
#' file is loaded.
#'
#' - "functions", a character string indicating the function to be used to load the file
#'
#' - "intervals", a numeric indicating the interval between repeated loading of the same
#' file. This should be NA or the column absent if the file is only loaded once.
#'
#' - "loadTime", a numeric indicating when the file should be loaded. Defaults to simTime = 0,
#' but this can be any time. The loading will be scheduled to occur at the "loadTime",
#' whatever that is. If the same file is to loaded many times, but not at a regular interval,
#' then there should be separate line, with a unique loadTime for each.
#'
#' - "arguments" is a list of lists of named arguments, one list for each loading function. For example, if raster
#' is a loading function, arguments = list(native = TRUE). If there is only one list, then it is assumed to apply
#' to all load attempts and will be repeated for each load function.
#'
#'
#' @param sim A \code{simList} object
#'
#' @param stackName String or Null, the default. If a string, then all rasters will be put into
#' a single RasterStack object, with name given.
#'
#' @author Eliot McIntire
#' @author Alex Chubaty
#'
#' @export
#' @docType methods
#' @rdname simLoad
#'
#' @examples
#' #load random maps included with package
#' fileList = data.frame(files = dir(file.path(find.package("SpaDES", quiet = FALSE),"maps"),
#'    full.names=TRUE,pattern= "tif"), functions="rasterToMemory", package="SpaDES",
#'    stringsAsFactors=FALSE)
#' mySim <- simInit(times=list(start=0.0, stop=100),
#'                  params=list(
#'                    #.checkpoint=list(interval=1000,
#'                    #                          file=file.path(path, "SpaDES/SAMPLE/chkpnt.RData")),
#'                    fileList=fileList,
#'                    .progress=list(graphical=FALSE, interval = 1),
#'                    caribouMovement=list(N=1e2, toSave=c("caribou"),
#'                                 savePath=file.path("output","caribouMovement"),
#'                                 saveInitialTime = 3, saveInterval=100,
#'                                 plotInitialTime = 1.01, plotInterval=100,
#'                                 interval=1, startTime=0),
#'                    fireSpread=list(nFires = 1e1, spreadprob=0.225,
#'                              persistprob=0, its=1e6,
#'                              plotInitialTime = 0, plotInterval=100,
#'                              toSave=c("Fires"),
#'                              savePath = file.path("output","firesSpread"),
#'                              saveInterval = 100, interval = 10, startTime=0)
#'                  ),
#'                  modules=list("fireSpread", "caribouMovement"),
#'                  path=system.file("sampleModules", package="SpaDES"))
#'
#' mySim <- simLoad(mySim)
#' simPlot(DEM)
#'
#'# Second, more sophisticated. All maps loaded at time = 0, and the last one is reloaded
#'#  at time = 10 (via "intervals"). Also, pass the single argument as a list to all functions...
#'#  specifically, when add "native = TRUE" as an argument to the raster function
#' arguments = list(native=TRUE)
#' files = dir(file.path(find.package("SpaDES", quiet = FALSE),"maps"),
#'      full.names=TRUE,pattern= "tif")
#' fileList = data.frame(
#'    files = files,
#'    functions="rasterToMemory",
#'    packages="SpaDES",
#'    objectNames = NA,
#'    arguments = arguments,
#'    loadTimes = 0,
#'    intervals = c(rep(NA,length(files)-1),10),
#'    stringsAsFactors=FALSE)
#' mySim <- simInit(times=list(start=0.0, stop=100),
#'   params=list(
#'               fileList=fileList,
#'               .progress=list(graphical=FALSE, interval=10),
#'               randomLandscapes = list(nx=1e2, ny=1e2, toSave=c("habitat"),
#'                              savePath=file.path("output", "habitat"),
#'                              plotInitialTime = 0, plotInterval=1e3,
#'                              saveInitialTime = 3, saveInterval=100,
#'                              interval=0, startTime=0),
#'                    caribouMovement=list(N=1e2, toSave=c("caribou"),
#'                                 savePath=file.path("output","caribou"),
#'                                 saveInitialTime = 3, saveInterval=100,
#'                                 plotInitialTime = 1.01, plotInterval=1,
#'                                 interval=1, startTime=0)),
#'   modules=list("randomLandscapes", "caribouMovement"),
#'   path=system.file("sampleModules", package="SpaDES"))#' sim <- simLoad(sim)
#' print(system.time(mySim <- doSim(mySim, debug=FALSE)))
#'
simLoad = function(sim = NULL, stackName = NULL, fileList = NULL) {

  # check to see if fileList is empty, if it is, skip everything, return nothing
  usedFileList = FALSE
  if(!is.null(fileList)) {
    usedFileList = TRUE
    sim <- simInit(times=list(start=0.0, stop=1),
                   params=list(
                     fileList=fileList
                   ),
                   modules=list(),
                   path="."
    )
  }

  # Pull .fileExtensions into function so that scoping is faster
  .fileExts = .fileExtensions

  fileList <- simParams(sim)$fileList
  curTime <- simCurrentTime(sim)

  arguments <- fileList$arguments
  # Check if arguments is a named list; the name may be concatenated with the "arguments", separated
  #  with a ".". This will extract that
  if ((length(arguments)>0) & (is.null(names(arguments)))) {
    names(arguments) <- sapply(strsplit(names(fileList)[match("arguments",names(fileList))],
                                   ".",fixed=TRUE),function(x)x[-1])
  }

  if(!is.null(arguments)) {
    if(length(arguments)<length(fileList$file)) arguments = rep(arguments, length.out=length(fileList$file))
  }

  if (is(fileList, "list")) {
    fileListdf <- do.call(data.frame, args=list(fileList[-match("arguments",names(fileList))],
                                                stringsAsFactors=FALSE))
  } else {
    fileListdf <- fileList
  }

  # fill in columns if they are missing. Assume loadTime = 0 if missing
  if(is.na(match("loadTime",names(fileListdf)))) {
    fileListdf["loadTime"] <- 0
  }

  # only load those that are to be loaded at their loadTime
  filesCurTime <- fileListdf[fileListdf$loadTime==curTime,]

  fl <- filesCurTime$file

  fl.list <- strsplit(basename(fl),".",fixed=TRUE)

  # extract file extensions, to be used to determine which function to use
  exts <- match(sapply(fl.list,function(x) x[length(x)]),.fileExts[,"exts"])

  # determine which function to load with
  loadFun <- as.character(.fileExts[exts,"functions"])#[,functions])
  loadPackage <- as.character(.fileExts[exts,"package"])#[,functions])

  # correct those for which a specific function is given in fileListdf$functions
  if(!is.na(match("functions",names(fileListdf)))) {
    loadFun[!is.na(fileListdf$functions)] <- fileListdf$functions[!is.na(fileListdf$functions)]
    loadPackage[!is.na(fileListdf$package)] <- fileListdf$package[!is.na(fileListdf$package)]
  }

  # use filenames as object names, unless alternative provided in fileListdf$objectNames
  objectNames <- sapply(fl.list,function(x) paste(x[-length(x)],collapse="."))
  if(!is.na(match("objectNames",names(fileListdf)))) {
    loadFun[!is.na(fileListdf$objectNames)] <- fileListdf$objectNames
  }


  # identify arguments
  #arguments <- filesCurTime$arguments

  # raster function sometimes loads file to disk; this will be made explicit
  where <- c("disk","memory")
  if(is.null(stackName)) {
    environ <- globalenv()
  } else {
    environ <-parent.frame()
  }

  # load files
  for (x in 1:length(fl)) {
    nam = names(arguments[x])
    if(!is.null(nam)) {
      argument <- list(unname(unlist(arguments[x])),fl[x])
      names(argument) <- c(nam,names(formals(get(loadFun[x],envir=.GlobalEnv)))[1])
    } else {
      argument <- list(fl[x])
      names(argument) <- names(formals(get(loadFun[x],envir=.GlobalEnv)))[1]
    }

    # The actual load call
    assign(objectNames[x],do.call(get(loadFun[x]), args = argument),envir=globalenv())

    simObjectsLoaded(sim) <- append(simObjectsLoaded(sim), objectNames[x])

    if (loadFun[x]=="raster") {
      print(paste(objectNames[x],"read to",where[inMemory(get(objectNames[x]))+1],
                  "from",fl[x],"using",loadFun[x]))
    } else {
      print(paste(objectNames[x],"read to memory from",fl[x],"using",loadFun[x]))
    }
  }

  # rasters sometimes don't load with their min and max values set

#    israst = sapply(objectNames, function(x) is(get(x),"Raster"))
#    a = lapply(objectNames[israst],function(x) {
#      assign(x, setMinMax(get(x)),envir=globalenv())
#    })

  if(!is.null(stackName)) {
    all.equal(mget(objectNames))
    extents <- lapply(mget(objectNames[israst], envir=.GlobalEnv), extent)
    extents.equal = logical(length(extents))
    for (i in 1:(length(extents)-1)){
      extents.equal[i] = (extents[[1]] == extents[[i]])
    }

    if (all(extents.equal)) {
      assign(stackName, stack(mget(objectNames,envir=.GlobalEnv)), envir=.GlobalEnv)
      rm(list=objectNames,envir=.GlobalEnv)
      warning(paste(paste(objectNames,collapse=", "),"were deleted; they are in the",stackName,"stack"))
    } else {
      warning("Cannot stack objects because they don't have same extents,
              Returning individual objects to global environment")
    }
  }

  # add new rows of files to load based on fileListdf$Interval
  if(!is.na(match("intervals",names(fileListdf)))) {
    if (any(!is.na(fileListdf$intervals))) {
      keep <- !is.na(fileListdf$interval)
      fileListdf$loadTimes[keep] <- curTime + fileListdf$interval[keep]
    }
  }


  # remove files that have been loaded from fileListdf
  keepOnFileList <- fileListdf$loadTime!=curTime
  fileListdf = fileListdf[keepOnFileList,]

  # If filename had been provided, then no need to return sim object, just report files loaded
  if (!usedFileList) {
    if(is(fileList, "list")) {
      simParams(sim)$fileList <- c(as.list(fileListdf),arguments=arguments[keepOnFileList])
    } else if (is(fileList, "data.frame")) {
      simParams(sim)$fileList <- fileListdf
    } else {
      error("fileList must be either a list or data.frame")
    }
    if(nrow(fileListdf)>0)
      sim <- scheduleEvent(sim, min(fileListdf$loadTimes,na.rm=TRUE),
                           "load", "init")
  }

  return(sim)

}

#' File extensions map
#'
#' How to load various types of files in R.
#'
#' @export
#' @rdname fileextensions
#'
.fileExtensions = data.frame(matrix(ncol=3,byrow=TRUE,c(
  "tif", "raster", "raster" ,
  "png", "raster", "raster" ,
  "csv", "read.csv", "utils" ,
  "shp", "readOGR","rgdal",
  "txt", "read.table","utils",
  "asc", "raster","raster")))
colnames(.fileExtensions) = c("exts","functions","package")


#######################################################
#' Read raster to memory
#'
#' Wrapper to the \code{raster} function, that creates the raster object in
#' memory, even if it was read in from file.
#'
#' @param x An object passed directly to the function raster (e.g., character string of a filename).
#'
#' @return A raster object whose values are stored in memory.
#'
#' @seealso \link{\code{raster}}
#'
#' @name rasterToMemory
#' @importMethodsFrom raster raster
#' @export
#' @docType methods
#' @rdname rasterToMemory-method
#'
#' @author Eliot McIntire
#' @author Alex Chubaty
#'
setGeneric("rasterToMemory", function(x, ...) {
  standardGeneric("rasterToMemory")
})

#' rasterToMemory
#' @rdname rasterToMemory-method
setMethod("rasterToMemory",
          signature=c(x="ANY"),
          definition=function(x, ...) {
  r <- raster(x, ...)
  r <- setValues(r, getValues(r))
  return(r)
})
