if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("loadTime", "objectName", "package"))
}

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
#' Load simulation objects according to \code{filelist}
#'
#' This function takes the filelist argument in the \code{simList} object and
#' loads all the files using the identified functions and arguments.
#'
#' In the \code{filelist} object, either a \code{list} or a \code{data.frame},
#' there will be minimally a column called "files".
#' All other columns are optional.
#'
#' Other optional columns are:
#'
#' - \code{objectName}: a character string indicating the name of the object once the
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
#' @param filelist \code{list} or \code{data.frame} to call \code{loadFiles} directly from the
#'                  \code{filelist} as described in Details
#'
#' @param ...      Additional arguments.
#'
#' @author Eliot McIntire
#' @author Alex Chubaty
#'
#' @name loadFiles
#' @include simulation.R
#' @importFrom stringi stri_detect_fixed
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
#' filelist = data.table(files=dir(file.path(find.package("SpaDES", quiet=FALSE), "maps"),
#'    full.names=TRUE, pattern="tif"), functions="rasterToMemory", package="SpaDES")
#'
#' times <- list(start=0, stop=3)
#' parameters <- list(.globals=list(stackName="landscape"),
#'                    .load=list(filelist=filelist))
#' modules <- list("randomLandscapes", "caribouMovement")
#' path <- system.file("sampleModules", package="SpaDES")
#' mySim <- simInit(times=times, params=parameters, modules=modules, path=path)
#' ls(mySim)
#'
#' sim1 <- loadFiles(filelist=filelist)
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
#' filelist = data.table(
#'    files = files,
#'    functions="raster::raster",
#'    objectName = NA,
#'    arguments = arguments,
#'    loadTime = 0,
#'    intervals = c(rep(NA, length(files)-1), 10))
#'
#' sim2 <- loadFiles(filelist=filelist)
#' end(sim2) <- 20
#' sim2 <- spades(sim2)
#' }
setGeneric("loadFiles", function(sim, filelist, ...)  {
  standardGeneric("loadFiles")
})

#' @rdname loadFiles
setMethod(
  "loadFiles",
  signature(sim="simList", filelist="missing"),
  definition = function(sim, ...) {

    # Pull .fileExtensions() into function so that scoping is faster
    .fileExts = .fileExtensions()

    if(NROW(inputs(sim))!=0) {
      filelist <- inputs(sim) # does not create a copy - because data.table ... this is a pointer
      curTime <- time(sim, "seconds")
      arguments <- inputArgs(sim)
      # Check if arguments is a named list; the name may be concatenated
      # with the "arguments", separated by a ".". This will extract that.
      if ((length(arguments)>0) & (is.null(names(arguments)))) {
        names(arguments) <- sapply(strsplit(
          names(filelist)[match("arguments", names(filelist))], ".", fixed=TRUE),
          function(x) { x[-1] }
        )
      }

      if (!is.null(arguments)) {
        if (length(arguments) < length(filelist$file)) {
          arguments <-  rep(arguments, length.out=length(filelist$file))
        }
      }

      if (is(filelist, "list")) {
        filelistDT <- do.call(
          data.table,
          args=list(filelist[-match("arguments", names(filelist))])
        )
      } else if(!is(filelist, "data.table")) {
        filelistDT <- data.table(filelist)
      } else {
        filelistDT <- filelist
      }

#       # Fill in columns if they are missing:
#       if (!("package" %in% names(filelistDT))) {
#         filelistDT[,package:=NA]
#       }

      #  assume loadTime = start(sim) if missing
      if(any(is.na(filelistDT[,loadTime]))) {
        filelistDT[is.na(loadTime),loadTime:=start(sim, "second")]
      #  filelistDT[,loadTime:=start(sim, "second")]
      }

      # only load those that are to be loaded at their loadTime
      cur <- filelistDT$loadTime==curTime

      if(any(cur)) {

        fl <- filelistDT[cur,file]
        # extract file extensions, to be used to determine which function to use
        exts <- match(fileExt(fl), .fileExts[,"exts"])

        # determine which function to load with
        loadFun <- as.character(.fileExts[exts, "fun"])
        loadPackage <- as.character(.fileExts[exts, "package"])

  #       # correct those for which a specific function is given in filelistDT$fun
  #
  #       if(any(!is.na(filelistDT[,fun]))) {
  #         loadFun[!is.na(filelistDT$fun)] <- filelistDT$fun[!is.na(filelistDT$fun)]
  #         loadPackage[!is.na(filelistDT[,package])] <- filelistDT$package[!is.na(filelistDT$package)]
  #         loadPackage[grepl("::",loadFun)] <- sapply(strsplit(split="::",loadFun), function(x) x[1])
  #         loadFun[grepl("::",loadFun)] <- sapply(strsplit(split="::",loadFun), function(x) x[2])
  #       }

        # use filenames as object names, unless alternative provided in filelistDT$objectName
        objectName <- fileName(fl)
        if(any(!is.na(filelistDT[cur,objectName]))) {
          objectName[!is.na(filelistDT[cur,objectName])] <- filelistDT[cur,objectName][!is.na(filelistDT[cur,objectName])]
        }

        # correct those for which a specific function is given in filelistDT$fun
        if(any(!is.na(filelistDT[cur,fun]))) {
          loadFun[!is.na(filelistDT[cur,fun])] <- filelistDT[cur,fun][!is.na(filelistDT[cur,fun])]
          loadPackage[!is.na(filelistDT[cur,package])] <- filelistDT[cur,package][!is.na(filelistDT[cur,package])]
          loadPackage[stri_detect_fixed(loadFun,"::")] <- sapply(strsplit(split="::",loadFun), function(x) x[1])
          loadFun[stri_detect_fixed(loadFun,"::")] <- sapply(strsplit(split="::",loadFun), function(x) x[2])
        }
        # load files
        for (x in 1:length(fl)) {
          y <- which(cur)[x]
          nam = names(arguments[y])

          if(!is.null(nam)) {
            argument <- list(unname(unlist(arguments[y])), filelistDT[y,file])
            names(argument) <- c(nam, names(formals(get(loadFun[x], envir=.GlobalEnv)))[1])
          } else {
            argument <- list(filelistDT[y,file])
            names(argument) <- names(formals(get(loadFun[x], envir=.GlobalEnv)))[1]
          }

          # The actual load call
          sim[[objectName[x]]] <- do.call(get(loadFun[x]), args=argument)
          filelistDT[y,loaded:=TRUE]

          #simObjectsLoaded(sim) <- append(simObjectsLoaded(sim), objectName[x])


          if (loadFun[x]=="raster") {
            message(paste0(
              objectName[x], " read from ", fl[x], " using ", loadFun[x],
              "(inMemory=", inMemory(sim[[objectName[x]]]), ")",ifelse(filelistDT[y,loadTime!=start(sim,"seconds")],
                                                                       paste("\n  at time",
                                                                       filelistDT[y,loadTime]),"")
            ))
          } else {
              message(paste0(objectName[x]," read from ",fl[x]," using ", loadFun[x],
                             ifelse(filelistDT[y,loadTime!=start(sim,"seconds")],
                                    paste("\n   at time",
                                    filelistDT[y,loadTime]),"")))
          }

        } # end x
        # add new rows of files to load based on filelistDT$Interval
        if(!is.na(match("intervals", names(filelistDT)))) {
          if (any(!is.na(filelistDT[loaded==TRUE,intervals]))) {
            filelistDT <- filelistDT[loaded==TRUE & !is.na(intervals),] %>%
              .[,`:=`(loadTime=curTime+intervals, loaded=NA, intervals=NA)] %>%
              list(filelistDT, .) %>%
              rbindlist

          }
        }

  #       # remove files that have been loaded from filelistDT
  #       keepOnFileList <- filelistDT$loadTime!=curTime
  #       filelistDT = filelistDT[keepOnFileList,]

      } # if there are no files to load at curTime, then nothing

      if(!exists("usedFileList")) usedFileList <- FALSE

      # If filename had been provided, then no need to return sim object,
      #   just report files loaded
      if (!usedFileList) {
        if(is(filelist, "list")) {
          inputs(sim) <- c(as.list(filelistDT), arguments=arguments[keepOnFileList])
        } else if (is(filelist, "data.table")) {
          inputs(sim) <- filelistDT
        } else {
          stop("filelist must be either a list or data.frame")
        }

        if(any(is.na(filelistDT[,loaded]))) {
          sim <- scheduleEvent(sim, filelistDT[is.na(loaded),min(loadTime,na.rm=TRUE)], "load", "later")
        }
      }
    }
    message("") ## print empty message to add linebreak to console message output
    return(invisible(sim))
})

#' @rdname loadFiles
setMethod("loadFiles",
          signature(sim="missing", filelist="ANY"),
          definition = function(filelist, ...) {

            sim <- simInit(times=list(start=0.0, stop=1),
                           params=list(),
                           inputs=list(filelist=filelist),
                           modules=list(), path=".")
            return(invisible(sim))
})

#' @rdname loadFiles
setMethod("loadFiles",
          signature(sim="missing", filelist="missing"),
          definition = function(...) {
            message("no files loaded because sim and filelist are empty")
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
  colnames(.fE) = c("exts", "fun", "package")
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
#' @importFrom raster raster
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

