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
#' #simple one - be sure to setwd first!
#' fileList= data.frame(files= dir(pattern = "asc"),stringsAsFactors=FALSE)
#' simInit("sim", times=list(start=0.0, stop=100),
#'   params=list(
#'               fileList=fileList,
#'               .progress=list(graphical=FALSE, interval = 10),
#'               habitat = list(nx=1e3, ny=1e3, toSave=c("habitat"),
#'                                savePath=file.path("output", "habitat"),
#'                                saveFreq=3, plotFreq=10,
#'                                interval=0, startTime=0),
#'               caribou=list(N=1e3, plotFreq=1, toSave=c("caribou"),
#'                              savePath=file.path("output","caribou"),
#'                              saveFreq=4, interval=1, startTime=0)),
#'   modules=list("habitat", "caribou"),
#'   path=file.path(path, "SpaDES/SAMPLE"))
#' sim <- simLoad(sim)
#' simPlot(RoadCostSurface_uaf_3)
#'
#'# Second, more sophisticated. All maps loaded at time = 0, and the last one is reloaded
#'#  at time = 10 (via "intervals"). Also, pass the single argument as a list to all functions...
#'#  specifically, when add "native = TRUE" as an argument to the raster function
#'args = list(native=TRUE)
#'fileList= list(
#'   file = c(dir(pattern = "asc"),"C:/Rwork/Maps/LCC2005_V1_4a.tif"),
#'   funs = NA,
#'   objs = NA,
#'   args = args,
#'   loadTimes = 0,
#'   intervals = c(rep(NA,length(dir(pattern="asc"))),10))
#' simInit("sim", times=list(start=0.0, stop=100),
#'   params=list(
#'               fileList=fileList,
#'               .progress=list(graphical=FALSE, interval = 10),
#'               habitat = list(nx=1e3, ny=1e3, toSave=c("habitat"),
#'                                savePath=file.path("output", "habitat"),
#'                                saveFreq=3, plotFreq=10,
#'                                interval=0, startTime=0),
#'               caribou=list(N=1e3, plotFreq=1, toSave=c("caribou"),
#'                              savePath=file.path("output","caribou"),
#'                              saveFreq=4, interval=1, startTime=0)),
#'   modules=list("habitat", "caribou"),
#'   path=file.path(path, "SpaDES/SAMPLE"))
#' sim <- simLoad(sim)
#'
simLoad = function(sim, stackName = NULL) {
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
  exts <- sapply(fl.list,function(x) x[length(x)])

  # determine which function to load with
  loadFun <- as.character(.fileExtensions[exts,funs][,funs])
  # correct those for which a specific function is given in fileListdf$fun
  if(!is.na(pmatch("fun",names(fileListdf)))) {
    loadFun[!is.na(fileListdf$fun)] <- fileListdf$fun[!is.na(fileListdf$fun)]
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
      names(argument) <- c(nam,names(formals(loadFun[x]))[1])
    } else {
      argument <- list(fl[x])
      names(argument) <- names(formals(loadFun[x]))[1]
    }

    assign(objs[x],do.call(get(loadFun[x]), args = argument),envir=globalenv())
    if (loadFun[x]=="raster") {
      print(paste(objs[x],"read to",where[inMemory(get(objs[x]))+1],
                  "from",fl[x],"using",loadFun[x]))
    } else {
      print(paste(objs[x],"read to memory from",fl[x],"using",loadFun[x]))
    }
  })

  # rasters sometimes don't load with their min and max values set
   israst = sapply(objs, function(x) is(get(x),"Raster"))
   a = lapply(objs[israst],function(x) {
     assign(x, setMinMax(get(x)),envir=globalenv())
   })

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
  if(!is.na(match("interval",names(fileListdf)))) {
    if (any(!is.na(fileListdf$intervals))) {
      keep <- !is.na(fileListdf$interval) & fileListdf$interval>curTime
      fileListdf$loadTime[keep] <- curTime + fileListdf$interval[keep]
    }
  }


  # remove files that have been loaded from fileListdf
  keepOnFileList <- fileListdf$loadTime!=curTime
  fileListdf = fileListdf[keepOnFileList,]

  if(is(fileList, "list")) {
    simParams(sim)$fileList <- c(as.list(fileListdf),args=args[keepOnFileList])
  } else if (is(fileList, "data.frame")) {
    simParams(sim)$fileList <- fileListdf
  } else {
    error("fileList must be either a list or data.frame")
  }
  if(nrow(fileListdf)>0)
    sim <- scheduleEvent(sim, min(fileList$loadTime), "load", "init")

  # reassign the simList to global environment
  #assign(simname, sim, envir=globalenv())
  return(sim)
}

.fileExtensions = data.table(matrix(ncol=2,byrow=TRUE,c(
  "tif", "raster",
  "png", "raster",
  "csv", "read.csv",
  "shp", "readOGR",
  "txt", "read.table",
  "asc", "raster",
  "rds", "readRDS")))
setnames(.fileExtensions,c("exts","funs"))
setkey(.fileExtensions,exts)

