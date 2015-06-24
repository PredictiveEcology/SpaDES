if (getRversion() >= "3.1.0") utils::globalVariables(c("loadTime", "objectNames", "package"))

# extract filename (without extension) of a file
# - will accept list or charcter vector
# - outputs character vector
fileName = function (x) {
  return(unlist(strsplit(basename(unlist(x)), "\\..*$")))
}

# extract the file extension of a file
# - will accept list or charcter vector
# - outputs character vector
#' @importFrom magrittr '%>%'
fileExt = function (x) {
  f = strsplit(basename(unlist(x)), "^.*\\.") %>%
      sapply(., function(y) { y[[length(y)]] })
}

# Just checks for paths, creates them if they do not exist
doEvent.load = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="later") {
    sim <- loadFiles(sim)
  }
  return(invisible(sim))
}

###############################################################################
#' Load simulation objects according to \code{fileList}
#'
#' This function takes the fileList argument in the \code{simList} object and
#' loads all the files using the identified functions and arguments.
#'
#' In the \code{fileList} object, either a \code{list} or a \code{data.frame},
#' there will be minimally a column called "files".
#' All other columns are optional.
#'
#' Other optional columns are:
#'
#' - \code{objectNames}: a character string indicating the name of the object once the
#' file is loaded. Default is to use the file names, with file extension removed.
#'
#' - \code{package}: a character string indicating the package that the function is found in.
#' There is no default.
#'
#' - \code{functions}: a character string indicating the function to be used to load the file.
#' Default is to use the mapping between file extensions in the \code{.fileExtensions} function
#' and the actual file extensions.
#'
#' - \code{intervals}: a numeric indicating the interval between repeated loading of the same
#' file. This should be NA or the column absent if the file is only loaded once. Default is
#' absent, so files are loaded only at \code{start} in the \code{simList}.
#'
#' - \code{loadTime}: a numeric indicating when the file should be loaded. Defaults to
#' \code{simTime=0},but this can be any time. The loading will be scheduled to occur
#' at the "loadTime", whatever that is. If the same file is to loaded many times,
#' but not at a regular interval, then there should be separate line, with a unique
#' loadTime for each.
#'
#' - \code{arguments}: is a list of lists of named arguments, one list for each loading function.
#' For example, if raster is a loading function, \code{arguments = list(native = TRUE)}.
#' If there is only one list, then it is assumed to apply to all load attempts
#' and will be repeated for each load function.
#'
#' @param sim      \code{simList} object.
#'
#' @param fileList \code{list} or \code{data.frame} to call \code{loadFiles} directly from the
#'                  \code{fileList} as described in Details
#'
#' @param ...      Additional arguments.
#'
#' @author Eliot McIntire
#' @author Alex Chubaty
#'
#' @name loadFiles
#' @include simulation.R
#' @importFrom methods is
#' @import rgdal
#' @import raster
#' @import sp
#' @export
#' @docType methods
#' @rdname loadFiles
#'
#' @examples
#' \dontrun{
#' # Load random maps included with package
#' fileList = data.table(files=dir(file.path(find.package("SpaDES", quiet=FALSE), "maps"),
#'    full.names=TRUE, pattern="tif"), functions="rasterToMemory", package="SpaDES")
#'
#' times <- list(start=0, stop=3)
#' parameters <- list(.globals=list(stackName="landscape"),
#'                    .load=list(fileList=fileList))
#' modules <- list("randomLandscapes", "caribouMovement")
#' path <- system.file("sampleModules", package="SpaDES")
#' mySim <- simInit(times=times, params=parameters, modules=modules, path=path)
#' ls(mySim)
#'
#' sim1 <- loadFiles(fileList=fileList)
#' clearPlot()
#' Plot(sim1$DEM)
#' }
#'
#' \dontrun{
#' # Second, more sophisticated. All maps loaded at time = 0, and the last one is reloaded
#' #  at time = 10 and 20 (via "intervals").
#' # Also, pass the single argument as a list to all functions...
#' #  specifically, when add "native = TRUE" as an argument to the raster function
#' arguments = list(native=TRUE)
#' files = dir(file.path(find.package("SpaDES", quiet = FALSE), "maps"),
#'      full.names=TRUE, pattern= "tif")
#' fileList = data.table(
#'    files = files,
#'    functions="raster::raster",
#'    objectNames = NA,
#'    arguments = arguments,
#'    loadTime = 0,
#'    intervals = c(rep(NA, length(files)-1), 10))
#'
#' sim2 <- loadFiles(fileList=fileList)
#' end(sim2) <- 20
#' sim2 <- spades(sim2)
#' }
setGeneric("loadFiles", function(sim, fileList, ...)  {
  standardGeneric("loadFiles")
})

#' @rdname loadFiles
setMethod("loadFiles",
          signature(sim="simList", fileList="missing"),
          definition = function(sim, ...) {

            # Pull .fileExtensions() into function so that scoping is faster
            .fileExts = .fileExtensions()
            if(!is.null(simFileList(sim))) {
              fileList <- simFileList(sim)
              curTime <- time(sim, "seconds")
              arguments <- fileList$arguments

              # Check if arguments is a named list; the name may be concatenated
              # with the "arguments", separated by a ".". This will extract that.
              if ((length(arguments)>0) & (is.null(names(arguments)))) {
                names(arguments) <- sapply(strsplit(names(fileList)[match("arguments", names(fileList))],
                                                    ".", fixed=TRUE), function(x) { x[-1] } )
              }

              if (!is.null(arguments)) {
                if (length(arguments) < length(fileList$file)) {
                  arguments <-  rep(arguments, length.out=length(fileList$file))
                }
              }

              if (is(fileList, "list")) {
                fileListDT <- do.call(data.table,
                                      args=list(fileList[-match("arguments", names(fileList))]))
              } else {
                fileListDT <- data.table(fileList)
              }

              # Fill in columns if they are missing:
              if (!("package" %in% names(fileListDT))) {
                fileListDT[,package:=NA]
              }

              #  assume loadTime = start(sim) if missing
              if(!("loadTime" %in% names(fileListDT))) {
                fileListDT[,loadTime:=start(sim)]
              }

              # only load those that are to be loaded at their loadTime
              filesCurTime <- fileListDT[loadTime==curTime]

              fl <- filesCurTime$file
              # extract file extensions, to be used to determine which function to use
              exts <- match(fileExt(fl), .fileExts[,"exts"])

              # determine which function to load with
              loadFun <- as.character(.fileExts[exts, "functions"])#[,functions])
              loadPackage <- as.character(.fileExts[exts, "package"])#[,functions])

              # correct those for which a specific function is given in fileListDT$functions
              if("functions" %in% names(fileListDT)) {
                loadFun[!is.na(fileListDT$functions)] <- fileListDT$functions[!is.na(fileListDT$functions)]
                loadPackage[!is.na(fileListDT[,package])] <- fileListDT$package[!is.na(fileListDT$package)]
                loadPackage[grepl("::",loadFun)] <- sapply(strsplit(split="::",loadFun), function(x) x[1])
                loadFun[grepl("::",loadFun)] <- sapply(strsplit(split="::",loadFun), function(x) x[2])
              }

              # use filenames as object names, unless alternative provided in fileListDT$objectNames
              objectNames <- fileName(fl)#sapply(fl.list, function(x) paste(x[-length(x)], collapse="."))
               if(!is.na(match("objectNames", names(fileListDT)))) {
                 objectNames[!is.na(fileListDT$objectNames)] <- fileListDT$objectNames
               }

              # load files
              for (x in 1:length(fl)) {
                nam = names(arguments[x])
                if(!is.null(nam)) {
                  argument <- list(unname(unlist(arguments[x])), fl[x])
                  names(argument) <- c(nam, names(formals(get(loadFun[x], envir=.GlobalEnv)))[1])
                } else {
                  argument <- list(fl[x])
                  names(argument) <- names(formals(get(loadFun[x], envir=.GlobalEnv)))[1]
                }

                # The actual load call
                sim[[objectNames[x]]] <- do.call(get(loadFun[x]), args=argument)

                simObjectsLoaded(sim) <- append(simObjectsLoaded(sim), objectNames[x])


                if (loadFun[x]=="raster") {
                  message(paste0(objectNames[x]," read from ",fl[x]," using ", loadFun[x],
                                "(inMemory=",inMemory(sim[[objectNames[x]]]),")"))
                  } else {
                    message(paste0(objectNames[x]," read from ",fl[x]," using ", loadFun[x]))
                 }

              } # end x
              # add new rows of files to load based on fileListDT$Interval
              if(!is.na(match("intervals", names(fileListDT)))) {
                if (any(!is.na(fileListDT$intervals))) {
                  keep <- !is.na(fileListDT$interval)
                  fileListDT$loadTime[keep] <- curTime + fileListDT$interval[keep]
                }
              }

              # remove files that have been loaded from fileListDT
              keepOnFileList <- fileListDT$loadTime!=curTime
              fileListDT = fileListDT[keepOnFileList,]

              if(!exists("usedFileList")) usedFileList <- FALSE

              # If filename had been provided, then no need to return sim object,
              #   just report files loaded
              if (!usedFileList) {
                if(is(fileList, "list")) {
                  simFileList(sim) <- c(as.list(fileListDT), arguments=arguments[keepOnFileList])
                } else if (is(fileList, "data.frame")) {
                  simFileList(sim) <- fileListDT
                } else {
                  stop("fileList must be either a list or data.frame")
                }

                if(nrow(fileListDT)>0) {
                  sim <- scheduleEvent(sim, min(fileListDT$loadTime, na.rm=TRUE), "load", "later")
                }
              }
            }
            message("") ## print empty message to add linebreak to console message output
            return(invisible(sim))
})

#' @rdname loadFiles
setMethod("loadFiles",
          signature(sim="missing", fileList="ANY"),
          definition = function(fileList, ...) {

            sim <- simInit(times=list(start=0.0, stop=1),
                           params=list(.load=list(fileList=fileList)),
                           modules=list(), path=".")
            return(invisible(sim))
})

#' @rdname loadFiles
setMethod("loadFiles",
          signature(sim="missing", fileList="missing"),
          definition = function(...) {
            message("no files loaded because sim and fileList are empty")
})

#' File extensions map
#'
#' How to load various types of files in R.
#'
#' @export
#' @rdname loadFiles
.fileExtensions = function() {
  .fE <- data.frame(matrix(ncol=3, byrow=TRUE,c(
    "tif", "raster", "raster" ,
    "png", "raster", "raster" ,
    "csv", "read.csv", "utils" ,
    "shp", "readOGR", "rgdal",
    "txt", "read.table", "utils",
    "asc", "raster", "raster")))
  colnames(.fE) = c("exts", "functions", "package")
  return(.fE)
}


#######################################################
#' Read raster to memory
#'
#' Wrapper to the \code{raster} function, that creates the raster object in
#' memory, even if it was read in from file.
#'
#' @param x An object passed directly to the function raster (e.g., character string of a filename).
#'
#' @param ... Additional arguments to \code{raster}.
#' @return A raster object whose values are stored in memory.
#'
#' @seealso \code{\link{raster}}.
#'
#' @name rasterToMemory
#' @importMethodsFrom raster raster
#' @export
#' @docType methods
#' @rdname rasterToMemory
#'
#' @author Eliot McIntire and Alex Chubaty
#'
setGeneric("rasterToMemory", function(x, ...) {
  standardGeneric("rasterToMemory")
})

#' @rdname rasterToMemory
setMethod("rasterToMemory",
          signature=c(x="ANY"),
          definition=function(x, ...) {
            r <- raster(x, ...)
            r <- setValues(r, getValues(r))
            return(r)
})
