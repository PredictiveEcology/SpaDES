################################################
###
### A LOAD MODULE
###
###############################################

# extract filename (without extension) of a file
# - will accept list or charcter vector
# - outputs character vector
fileName = function (x) {
  return(unlist(strsplit(basename(unlist(x)), "\\..*$")))
}

# extract the file extension of a file
# - will accept list or charcter vector
# - outputs character vector
fileExt = function (x) {
  f = strsplit(basename(unlist(x)), "^.*\\.")
  sapply(f, function(y) { y[[length(y)]] })
}

# Just checks for paths, creates them if they do not exist
doEvent.load = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="later") {
    sim <- loadFiles(sim)
  }
  return(invisible(sim))
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
#' - \code{objectNames}: a character string indicating the name of the object once the
#' file is loaded. Default is to use the file names, with file extension removed.
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
#' simTime=0,but this can be any time. The loading will be scheduled to occur
#' at the "loadTime", whatever that is. If the same file is to loaded many times,
#' but not at a regular interval, then there should be separate line, with a unique
#' loadTime for each.
#'
#' - \code{.stackName}: a character (vector) indicating the name(s) of the RasterStack(s)
#' to stack all the RasterLayers into. If left blank, then the individual RasterLayers will
#' not be automatically stacked. If this is specified, the individual RasterLayers loaded
#' will not actually be created as individual R objects; they will be loaded directly
#' into the stack. Finally, the behaviour here is
#'
#' - \code{arguments}: is a list of lists of named arguments, one list for each loading function.
#' For example, if raster is a loading function, arguments = list(native = TRUE). If there is only
#' one list, then it is assumed to apply to all load attempts and will be repeated
#' for each load function.
#'
#' @param sim A \code{simList} object
#'
#' @param fileList List or data.frame to call loadFiles directly from the fileList as
#' described in Details
#'
#' @param ... Additional arguments.
#'
#' @author Eliot McIntire
#' @author Alex Chubaty
#'
#' @name loadFiles
#' @include simulation.R
#' @import rgdal
#' @import raster
#' @import sp
#' @export
#' @docType methods
#' @rdname loadFiles-method
#'
#' @examples
#' #load random maps included with package
#' fileList = data.frame(files = dir(file.path(find.package("SpaDES", quiet = FALSE), "maps"),
#'    full.names=TRUE, pattern= "tif"), functions="rasterToMemory", package="SpaDES",
#'    stringsAsFactors=FALSE)
#'
#' loadFiles(fileList=fileList)
#' Plot(DEM)
#'
#' # Second, more sophisticated. All maps loaded at time = 0, and the last one is reloaded
#' #  at time = 10 (via "intervals"). Also, pass the single argument as a list to all functions...
#' #  specifically, when add "native = TRUE" as an argument to the raster function
#' arguments = list(native=TRUE)
#' files = dir(file.path(find.package("SpaDES", quiet = FALSE), "maps"),
#'      full.names=TRUE, pattern= "tif")
#' fileList = data.frame(
#'    files = files,
#'    functions="rasterToMemory",
#'    packages="SpaDES",
#'    objectNames = NA,
#'    arguments = arguments,
#'    loadTimes = 0,
#'    intervals = c(rep(NA, length(files)-1), 10),
#'    stringsAsFactors=FALSE)
#'
#' loadFiles(fileList=fileList)
#'
setGeneric("loadFiles", function(sim, fileList, ...)  {
  standardGeneric("loadFiles")
})

#' @rdname loadFiles-method
setMethod("loadFiles",
          signature(sim="simList", fileList="missing"),
          definition = function(sim, fileList, ...) {

            # Pull .fileExtensions() into function so that scoping is faster
            .fileExts = .fileExtensions()
            if(!is.null(simFileList(sim))) {
              fileList <- simFileList(sim)
              #            if(!is.null(fileList) & length(fileList$file)>0) {
              curTime <- simCurrentTime(sim)
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
                fileListdf <- do.call(data.frame, args=list(fileList[-match("arguments", names(fileList))],
                                                            stringsAsFactors=FALSE))
              } else {
                fileListdf <- fileList
              }

              # fill in columns if they are missing. Assume loadTime = 0 if missing
              if(is.na(match("loadTime", names(fileListdf)))) {
                fileListdf["loadTime"] <- 0
              }

              # only load those that are to be loaded at their loadTime
              filesCurTime <- fileListdf[fileListdf$loadTime==curTime,]

              fl <- filesCurTime$file

              #fl.list <- strsplit(basename(fl), ".", fixed=TRUE)

              # extract file extensions, to be used to determine which function to use
              exts <- match(fileExt(fl), .fileExts[,"exts"])

              # determine which function to load with
              loadFun <- as.character(.fileExts[exts, "functions"])#[,functions])
              loadPackage <- as.character(.fileExts[exts, "package"])#[,functions])

              # correct those for which a specific function is given in fileListdf$functions
              if(!is.na(match("functions",names(fileListdf)))) {
                loadFun[!is.na(fileListdf$functions)] <- fileListdf$functions[!is.na(fileListdf$functions)]
                loadPackage[!is.na(fileListdf$package)] <- fileListdf$package[!is.na(fileListdf$package)]
              }

              # use filenames as object names, unless alternative provided in fileListdf$objectNames
              objectNames <- fileName(fl)#sapply(fl.list, function(x) paste(x[-length(x)], collapse="."))
               if(!is.na(match("objectNames", names(fileListdf)))) {
                 objectNames[!is.na(fileListdf$objectNames)] <- fileListdf$objectNames
               }

              # identify arguments
              #arguments <- filesCurTime$arguments

              # raster function sometimes loads file to disk; this will be made explicit
              where <- c("disk", "memory")
              if(length(simGlobals(sim)$.stackName)==1) {
                stackName=rep(simGlobals(sim)$.stackName,length(objectNames))
              } else if (length(simGlobals(sim)$.stackName)==length(objectNames)){
                stackName=simGlobals(sim)$.stackName
              } else if (is.null(simGlobals(sim)$.stackName)) {
                stackName=rep(NA, length(objectNames))
                simGlobals(sim)$.stackName <- NA_character_
              } else {
                stop(".stackName must be same length as fileList or length=1")
              }

              if(any(unique(simGlobals(sim)$.stackName) %in% ls(envir=.GlobalEnv)))
                rm(list=unique(simGlobals(sim)$.stackName), envir=.GlobalEnv)

              # create empty stack
              localStacks = list()
              if(!is.na(unique(stackName))) {
                for(uniqueStacki in unique(stackName)) {
                  if(exists(uniqueStacki, envir=.GlobalEnv)) {
                    localStacks[uniqueStacki] <- get(uniqueStacki)
                  } else {
                    localStacks[uniqueStacki] <- stack()
                  }
                }
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
                if(is.na(stackName[x])) {
                  assign(objectNames[x], do.call(get(loadFun[x]), args=argument), envir=.GlobalEnv)
                  assign(objectNames[x], RasterLayerNamed(get(objectNames[x], envir=.GlobalEnv),name=objectNames[x]),
                         , envir=.GlobalEnv)
                } else {
                  whLayer <- which(names(localStacks[[stackName[x]]])==objectNames[x])
                  if (length(whLayer)>0) {
                    localStacks[[stackName[x]]][[whLayer]] <- do.call(get(loadFun[x]),
                                                                      args=argument)
                  } else {
                    localStacks[[stackName[x]]] <- stack(localStacks[[stackName[x]]],
                                                         do.call(get(loadFun[x]), args=argument))
                  }
                }
                simObjectsLoaded(sim) <- append(simObjectsLoaded(sim), objectNames[x])

                if (loadFun[x]=="raster") {
                  message(paste0(objectNames[x]," read from ",fl[x]," using ", loadFun[x],
                                "(inMemory=",inMemory(get(objectNames[x])),")"))
#                   message(paste(objectNames[x], "read to", where[inMemory(get(objectNames[x]))+1],
#                                 "from", fl[x], "using", loadFun[x]))
                  } else {
                    message(paste0(objectNames[x]," read from ",fl[x]," using ", loadFun[x]))
                 }
              } # end x

              if(!is.na(unique(stackName))) {
                for(uniqueStacki in unique(stackName)) {
                  name(localStacks[[uniqueStacki]]) <- uniqueStacki
                  assign(uniqueStacki, localStacks[[uniqueStacki]],
                         envir=.GlobalEnv)
                  message(paste("individual files have been stacked into",uniqueStacki,
                                "and are not individual RasterLayers"))
                }
              }


              # rasters sometimes don't load with their min and max values set

              #    israst = sapply(objectNames, function(x) is(get(x), "Raster"))
              #    a = lapply(objectNames[israst], function(x) {
              #      assign(x, setMinMax(get(x)), envir=.GlobalEnv)
              #    })

              #               if(!all(is.na(stackName))) {
              #                 for(uniqueStacki in unique(stackName)) {
              #                   whichUniqueStacki <- which(!is.na(match(stackName, uniqueStacki)))
              #                   extents <- lapply(mget(objectNames[whUniqueStacki],
              #                                          envir=.GlobalEnv), extent)
              #                   extents.equal = logical(length(extents)-1)
              #                   for (i in 1:(length(extents)-1)){
              #                     extents.equal[i] = (extents[[1]] == extents[[i]])
              #                   }
              #                   if (all(extents.equal)) {
              #                     newStack <- stack(mget(objectNames[whichUniqueStacki], envir=.GlobalEnv))
              #                     name(newStack) <- uniqueStacki
              #                     assign(uniqueStacki, newStack, envir=.GlobalEnv)
              #                   } else {
              #                     warning("Cannot stack objects because they don't have same extents,
              #                             Returning individual objects to global environment")
              #                   }
              #                   rm(list=objectNames, envir=.GlobalEnv)
              #                   warning(paste(paste(objectNames, collapse=", "),
              #                                 "were deleted; they are in the", stackName, "stack"))
              #                 }
              #               }

              # add new rows of files to load based on fileListdf$Interval
              if(!is.na(match("intervals", names(fileListdf)))) {
                if (any(!is.na(fileListdf$intervals))) {
                  keep <- !is.na(fileListdf$interval)
                  fileListdf$loadTimes[keep] <- curTime + fileListdf$interval[keep]
                }
              }

              # remove files that have been loaded from fileListdf
              keepOnFileList <- fileListdf$loadTime!=curTime
              fileListdf = fileListdf[keepOnFileList,]

              if(!exists("usedFileList")) usedFileList <- FALSE

              # If filename had been provided, then no need to return sim object, just report files loaded
              if (!usedFileList) {
                if(is(fileList, "list")) {
                  simFileList(sim) <- c(as.list(fileListdf), arguments=arguments[keepOnFileList])
                } else if (is(fileList, "data.frame")) {
                  simFileList(sim) <- fileListdf
                } else {
                  error("fileList must be either a list or data.frame")
                }

                if(nrow(fileListdf)>0) {
                  sim <- scheduleEvent(sim, min(fileListdf$loadTimes, na.rm=TRUE), "load", "later")
                }
              }
            } else {
              message("No files loaded, because no fileList (or empty fileList) provided.")
            }
            message("") ## print empty message to add linebreak to console message output
            return(invisible(sim))
          })

#' @rdname loadFiles-method
setMethod("loadFiles",
          signature(sim="missing", fileList="ANY"),
          definition = function(sim, fileList, ...) {
            if(any(names(fileList)==".stackName")) {
              stackName = fileList$.stackName
            } else {
              stackName = NA
            }

            sim <- simInit(times=list(start=0.0, stop=1),
                           params=list(.globals=list(.stackName=stackName),
                                       .loadFileList=fileList),
                           modules=list(), path="."
            )
            return(invisible(sim))
          })

#' @rdname loadFiles-method
setMethod("loadFiles",
          signature(sim="missing", fileList="missing"),
          definition = function(sim, fileList, ...) {
            message("no files loaded because sim and fileList are empty")
          })

#' File extensions map
#'
#' How to load various types of files in R.
#'
#' @export
#' @rdname loadFiles-method
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
