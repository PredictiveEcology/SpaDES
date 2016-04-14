if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("fun", "intervals", "keepOnFileList", "inMemory",
                           "loaded", "loadTime", "objectName", "package"))
}

#' File extensions map
#'
#' How to load various types of files in R.
#'
#' @export
#' @rdname loadFiles
.fileExtensions = function() {
  .fE <- data.frame(matrix(ncol = 3, byrow = TRUE, c(
    "Rdata", "load", "base",
    "rdata", "load", "base",
    "RData", "load", "base",
    "rds", "readRDS", "base",
    "RDS", "readRDS", "base",
    "tif", "raster", "raster",
    "png", "raster", "raster",
    "csv", "read.csv", "utils",
    "shp", "readOGR", "rgdal",
    "txt", "read.table", "utils",
    "asc", "raster", "raster")),
    stringsAsFactors = FALSE)
  colnames(.fE) = c("exts", "fun", "package")
  return(.fE)
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
#
# igraph exports %>% from magrittr
fileExt = function (x) {
  strsplit(basename(unlist(x)), "^.*\\.") %>%
    sapply(., function(y) { y[[length(y)]] })
}

# The load doEvent
doEvent.load = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "inputs") {
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
#' - \code{packages}: a character string indicating the package that the function is found in.
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
#' @importFrom data.table data.table rbindlist ':='
#' @importFrom stringi stri_detect_fixed
# @importFrom utils getFromNamespace
#' @export
#' @docType methods
#' @rdname loadFiles
#'
#' @examples
#' \dontrun{
#' # Load random maps included with package
#' filelist <- data.frame(
#'     files = dir(file.path(find.package("SpaDES", quiet = FALSE), "maps"),
#'     full.names = TRUE, pattern = "tif"), functions = "rasterToMemory", package = "SpaDES"
#' )
#'
#' times <- list(start = 0, end = 3)
#' parameters <- list(.globals = list(stackName = "landscape"))
#' modules <- list("randomLandscapes", "caribouMovement")
#' paths <- list(moduleName = system.file("sampleModules", package = "SpaDES"))
#' mySim <- simInit(times = times, params = parameters, modules = modules,
#' paths = paths, inputs = filelist)
#' ls(mySim)
#'
#' sim1 <- loadFiles(filelist = filelist)
#' clearPlot()
#' Plot(sim1$DEM)
#'
#' # Second, more sophisticated. All maps loaded at time = 0, and the last one is reloaded
#' #  at time = 10 and 20 (via "intervals").
#' # Also, pass the single argument as a list to all functions...
#' #  specifically, when add "native = TRUE" as an argument to the raster function
#' files = dir(system.file("maps", package = "SpaDES"),
#'             full.names = TRUE, pattern = "tif")
#' arguments = I(rep(list(native = TRUE), length(files)))
#' filelist = data.frame(
#'    files = files,
#'    functions = "raster::raster",
#'    objectName = NA,
#'    arguments = arguments,
#'    loadTime = 0,
#'    intervals = c(rep(NA, length(files)-1), 10)
#' )
#'
#' sim2 <- loadFiles(filelist = filelist)
#' end(sim2) <- 20
#' sim2 <- spades(sim2)
#' }
setGeneric("loadFiles", function(sim, filelist, ...)  {
  standardGeneric("loadFiles")
})

#' @rdname loadFiles
setMethod(
  "loadFiles",
  signature(sim = "simList", filelist = "missing"),
  definition = function(sim, ...) {

    # Pull .fileExtensions() into function so that scoping is faster
    .fileExts = .fileExtensions()
    #usedIntervals <- FALSE # This is for a speed reason later on.
                           #Whether or not intervals for loading files are defined

    if (NROW(inputs(sim)) != 0) {
      inputs(sim) <- .fillInputRows(inputs(sim))
      filelist <- inputs(sim) # does not create a copy - because data.table ... this is a pointer

      curTime <- time(sim, "seconds")
      arguments <- inputArgs(sim)
      # Check if arguments is a named list; the name may be concatenated
      # with the "arguments", separated by a ".". This will extract that.
      if ((length(arguments)>0) & (!is.null(names(arguments)))) {
        names(arguments) <- sapply(strsplit(
          names(filelist)[pmatch("arguments", names(filelist))], ".", fixed = TRUE),
          function(x) { x[-1] }
        )
      }

      # check if arguments should be, i.e,. recycled
      if (!is.null(arguments)) {
        if (length(arguments) < length(filelist$file)) {
          arguments <- rep(arguments, length.out = length(filelist$file))
        }
      }

      # if(!is(filelist, "data.table") & is(filelist, "data.frame")) {
      #   filelistDT <- data.table(filelist)
      # } else if (is(filelist, "list")) {
      #   filelistDT <- do.call(
      #       data.table,
      #       args = list(filelist[!(names(filelist) %in% "arguments" )])
      #    )
      #
      # } else {
      #   filelistDT <- filelist
      # }

      # only load those that are to be loaded at their loadTime
      cur <- filelist$loadTime == curTime

      if (any(cur)) {
        # load files
        loadPackage <- filelist$package
        loadFun <- filelist$fun

        for (y in which(cur)) {
          #y <- which(cur)[x]
          nam = names(arguments[y])

          if(!is.null(nam)) {
            argument <- list(unname(unlist(arguments[y])), filelist[y,"file"])
            names(argument) <- c(nam, names(formals(getFromNamespace(loadFun[y], loadPackage[y])))[1])
          } else {
            argument <- list(filelist[y,"file"])
            names(argument) <- names(formals(getFromNamespace(loadFun[y], loadPackage[y])))[1]
          }

          # The actual load call
          if (identical(loadFun[y], "load")) {
            do.call(getFromNamespace(loadFun[y], loadPackage[y]),
                    args = argument, envir = envir(sim))

          } else {
            sim[[filelist[y, "objectName"]]] <- do.call(getFromNamespace(loadFun[y], loadPackage[y]),
                                            args = argument)
          }
          filelist[y, "loaded"] <- TRUE

          if (loadFun[y] == "raster") {
            message(paste0(
              filelist[y, "objectName"], " read from ", filelist[y, "file"], " using ", loadFun[y],
              "(inMemory=", inMemory(sim[[filelist[y, "objectName"]]]), ")",
              ifelse(filelist[y, "loadTime"] != start(sim, "seconds"),
                     paste("\n  at time", filelist[y, "loadTime"]),"")
            ))
          } else {
            message(paste0(
              filelist[y, "objectName"], " read from ", filelist[y, "file"], " using ", loadFun[y],
              ifelse(filelist[y, "loadTime"] != start(sim, "seconds"),
                     paste("\n   at time", filelist[y, "loadTime"]), "")
            ))
          }
        } # end y
        # add new rows of files to load based on filelistDT$Interval
        if (!is.na(match("intervals", names(filelist)))) {
          if (any(!is.na(filelist[filelist$loaded, "intervals"]))) {

            newFilelist <- filelist[(filelist$loaded & !is.na(filelist$intervals)),]
            newFilelist[,c("loadTime", "loaded", "intervals")] = data.frame(curTime+newFilelist$intervals, NA, NA_real_)
            filelist <- rbind(filelist, newFilelist)
          }
        }
      } # if there are no files to load at curTime, then nothing

      if (is(filelist, "data.frame")) {
        inputs(sim) <- filelist # this is required if intervals is used
      } else if (is(filelist, "list")) {
        inputs(sim) <- c(as.list(filelist), arguments = arguments)
      } else {
        stop("filelist must be either a list or data.frame")
      }
    }
    message("") ## print empty message to add linebreak to console message output
    return(invisible(sim))
})

#' @rdname loadFiles
setMethod("loadFiles",
          signature(sim = "missing", filelist = "ANY"),
          definition = function(filelist, ...) {
            sim <- simInit(times = list(start = 0.0, end = 1),
                           params = list(),
                           inputs = filelist,
                           modules = list(), ...)
            return(invisible(sim))
})

#' @rdname loadFiles
setMethod("loadFiles",
          signature(sim = "missing", filelist = "missing"),
          definition = function(...) {
            message("no files loaded because sim and filelist are empty")
})

#######################################################
#' Read raster to memory
#'
#' Wrapper to the \code{raster} function, that creates the raster object in
#' memory, even if it was read in from file.
#'
#' @param x An object passed directly to the function raster (e.g., character string of a filename).
#'
#' @param ... Additional arguments to \code{raster}.
#'
#' @return A raster object whose values are stored in memory.
#'
#' @seealso \code{\link{raster}}.
#'
#' @name rasterToMemory
#' @importFrom raster getValues raster setValues
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
          signature = c(x = "ANY"),
          definition = function(x, ...) {
            r <- raster(x, ...)
            r <- setValues(r, getValues(r))
            return(r)
})
