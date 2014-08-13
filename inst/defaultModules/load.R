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
#' - "objs", a character string indicating the name of the object once the
#' file is loaded.
#'
#' - "funs", a character string indicating the function to be used to load the file
#'
#' - "intervals", a numeric indicating the interval between repeated loading of the same
#' file. This should be NA or the column absent if the file is only loaded once.
#'
#' - "loadTime", a numeric indicating when the file should be loaded. Defaults to simTime = 0,
#' but this can be any time. The loading will be scheduled to occur at the "loadTime",
#' whatever that is. If the same file is to loaded many times, but not at a regular interval,
#' then there should be separate line, with a unique loadTime for each.
#'
#' - "args" is a list of lists of named arguments, one list for each loading function. For example, if raster
#' is a loading function, args = list(native = TRUE). If there is only one list, then it is assumed to apply
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
#'    full.names=TRUE,pattern= "tif"), fun="rasterToMemory", package="SpaDES",
#'    stringsAsFactors=FALSE)
#' mySim <- simInit(times=list(start=0.0, stop=100),
#'                  params=list(
#'                    #.checkpoint=list(interval=1000,
#'                    #                          file=file.path(path, "SpaDES/SAMPLE/chkpnt.RData")),
#'                    fileList=fileList,
#'                    .progress=list(graphical=FALSE, interval = 1),
#'                    caribouMovement=list(N=1e2, toSave=c("caribou"),
#'                                 savePath=file.path("output","caribou"),
#'                                 saveInitialTime = 3, saveInterval=100,
#'                                 plotInitialTime = 1.01, plotInterval=100,
#'                                 interval=1, startTime=0),
#'                    fireSpread=list(nFires = 1e1, spreadprob=0.225,
#'                              persistprob=0, its=1e6,
#'                              plotInitialTime = 0, plotInterval=100,
#'                              toSave=c("Fires"),
#'                              savePath = file.path("output","fires"),
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
#' args = list(native=TRUE)
#' files = dir(file.path(find.package("SpaDES", quiet = FALSE),"maps"),
#'      full.names=TRUE,pattern= "tif")
#' fileList = data.frame(
#'    files = files,
#'    fun="rasterToMemory",
#'    package="SpaDES",
#'    objs = NA,
#'    args = args,
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
#'                                 plotInitialTime = 1.01, plotInterval=100,
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

  args <- fileList$args
  # Check if args is a named list; the name may be concatenated with the "args", separated
  #  with a ".". This will extract that
  if ((length(args)>0) & (is.null(names(args)))) {
    names(args) <- sapply(strsplit(names(fileList)[pmatch("args",names(fileList))],
                                   ".",fixed=TRUE),function(x)x[-1])
  }

  if(!is.null(args)) {
    if(length(args)<length(fileList$file)) args = rep(args, length.out=length(fileList$file))
  }

  if (is(fileList, "list")) {
    fileListdf <- do.call(data.frame, args=list(fileList[-pmatch("args",names(fileList))],
                                                stringsAsFactors=FALSE))
  } else {
    fileListdf <- fileList
  }

  # fill in columns if they are missing. Assume loadTime = 0 if missing
  if(is.na(pmatch("loadTime",names(fileListdf)))) {
    fileListdf["loadTime"] <- 0
  }

  # only load those that are to be loaded at their loadTime
  filesCurTime <- fileListdf[fileListdf$loadTime==curTime,]

  fl <- filesCurTime$file

  fl.list <- strsplit(basename(fl),".",fixed=TRUE)

  # extract file extensions, to be used to determine which function to use
  exts <- match(sapply(fl.list,function(x) x[length(x)]),.fileExts[,"exts"])

  # determine which function to load with
  loadFun <- as.character(.fileExts[exts,"funs"])#[,funs])
  loadPackage <- as.character(.fileExts[exts,"package"])#[,funs])

  # correct those for which a specific function is given in fileListdf$fun
  if(!is.na(pmatch("fun",names(fileListdf)))) {
    loadFun[!is.na(fileListdf$fun)] <- fileListdf$fun[!is.na(fileListdf$fun)]
    loadPackage[!is.na(fileListdf$package)] <- fileListdf$package[!is.na(fileListdf$package)]
  }

  # use filenames as object names, unless alternative provided in fileListdf$obj
  objs <- sapply(fl.list,function(x) paste(x[-length(x)],collapse="."))
  if(!is.na(pmatch("obj",names(fileListdf)))) {
    loadFun[!is.na(fileListdf$obj)] <- fileListdf$obj
  }


  # identify arguments
  #args <- filesCurTime$args

  # raster function sometimes loads file to disk; this will be made explicit
  where <- c("disk","memory")
  if(is.null(stackName)) {
    environ <- globalenv()
  } else {
    environ <-parent.frame()
  }

  # load files
  a <- lapply(1:length(fl), function(x) {
    nam = names(args[x])
    if(!is.null(nam)) {
      argument <- list(unname(unlist(args[x])),fl[x])
      names(argument) <- c(nam,names(formals(get(loadFun[x],envir=.GlobalEnv)))[1])
    } else {
      argument <- list(fl[x])
      names(argument) <- names(formals(get(loadFun[x],envir=.GlobalEnv)))[1]
    }

    # The actual load call
    assign(objs[x],do.call(get(loadFun[x]), args = argument),envir=globalenv())

    if (loadFun[x]=="raster") {
      print(paste(objs[x],"read to",where[inMemory(get(objs[x]))+1],
                  "from",fl[x],"using",loadFun[x]))
    } else {
      print(paste(objs[x],"read to memory from",fl[x],"using",loadFun[x]))
    }
  })

  # rasters sometimes don't load with their min and max values set

#    israst = sapply(objs, function(x) is(get(x),"Raster"))
#    a = lapply(objs[israst],function(x) {
#      assign(x, setMinMax(get(x)),envir=globalenv())
#    })

  if(!is.null(stackName)) {
    all.equal(mget(objs))
    extents <- lapply(mget(objs[israst], envir=.GlobalEnv), extent)
    extents.equal = logical(length(extents))
    for (i in 1:(length(extents)-1)){
      extents.equal[i] = (extents[[1]] == extents[[i]])
    }

    if (all(extents.equal)) {
      assign(stackName, stack(mget(objs,envir=.GlobalEnv)), envir=.GlobalEnv)
      rm(list=objs,envir=.GlobalEnv)
      warning(paste(paste(objs,collapse=", "),"were deleted; they are in the",stackName,"stack"))
    } else {
      warning("Cannot stack objects because they don't have same extents,
              Returning individual objects to global environment")
    }
  }

  # add new rows of files to load based on fileListdf$Interval
  if(!is.na(pmatch("interval",names(fileListdf)))) {
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
      simParams(sim)$fileList <- c(as.list(fileListdf),args=args[keepOnFileList])
    } else if (is(fileList, "data.frame")) {
      simParams(sim)$fileList <- fileListdf
    } else {
      error("fileList must be either a list or data.frame")
    }
    if(nrow(fileListdf)>0)
      sim <- scheduleEvent(sim, min(fileListdf$loadTimes,na.rm=TRUE),
                           "load", "init")
  }
#   } else {
#       error("Need to specify either a sim object or a filename")
#   }
  return(sim)

}

.fileExtensions = data.frame(matrix(ncol=3,byrow=TRUE,c(
  "tif", "raster", "raster" ,
  "png", "raster", "raster" ,
  "csv", "read.csv", "utils" ,
  "shp", "readOGR","rgdal",
  "txt", "read.table","utils",
  "asc", "raster","raster")))
colnames(.fileExtensions) = c("exts","funs","package")
#setkey(.fileExtensions,exts)

#' Read raster to memory
#'
#' Calls the raster function, and then the getValues function to force the file to memory.
#'
#' @param x an object passed directly to the function raster, like a string of a filename.
#' @export
rasterToMemory <- function(x, ...){
  rast <- raster(x, ...)
  rast[] <- getValues(rast)
  return(rast)
}
