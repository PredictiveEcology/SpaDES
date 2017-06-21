if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("saved", "saveTime", "fun", "package"))
}

# Just checks for paths, creates them if they do not exist
doEvent.save <- function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    if (NROW(outputs(sim)) > 0) {
      firstSave <- min(outputs(sim)[, "saveTime"], na.rm = TRUE)
      attributes(firstSave)$unit <- sim@simtimes[["timeunit"]]
      sim <- scheduleEvent(sim, firstSave, "save", "spades", .last())
      #sim <- scheduleEvent(sim, end(sim, sim@simtimes[["timeunit"]]), "save", "end", .last())
    }
    checkPath(sim@paths$outputPath, create = TRUE)

  } else if (eventType == "spades") {
    sim <- saveFiles(sim)
  } else if (eventType == "later") {
    sim <- saveFiles(sim)
  } else if (eventType == "end") {
    message(paste0("Files saved. Use outputs(your simList) for details"))
  }

  return(invisible(sim))
}

##############################################################
#' Save objects using \code{.saveObjects} in \code{params} slot of \code{simInit}
#'
#' In the \code{\link{simInit}} call, a parameter called \code{.saveObjects} can be provided in
#' each module.
#' This must be a character string vector of all object names to save. These objects will
#' then be saved whenever a call to \code{saveFiles} is made.
#'
#' The file names will be equal to the object name plus \code{time(sim)} is
#' appended at the end.
#' The files are saved as \code{.rds} files, meaning, only one object gets
#' saved per file.
#'
#'
#' For objects saved using this function, the module developer must create save
#' events that schedule a call to \code{saveFiles}.
#'
#' If this function is used outside of a module, it will save all files in the
#' outputs(sim) that are scheduled to be saved at the current time in the simList.
#'
#' There are 3 ways to save objects using \code{SpaDES}.
#'
#' @section 1. Model-level saving:
#'
#' Using the \code{outputs} slot in the \code{\link{simInit}} call.
#' See example in \code{\link{simInit}}.
#' This can be convenient because it gives overall control of many modules at a
#' time, and it gets automatically scheduled during the
#' \code{\link{simInit}} call.
#'
#' @section 2. Module-level saving:
#'
#' Using the \code{saveFiles} function inside a module.
#' This must be accompanied by a \code{.saveObjects} list element in the
#' \code{params} slot in the \code{\link{simList}}.
#' Usually a module developer will create this method for future users of
#' their module.
#'
#' @section 3. Custom saving:
#'
#' A module developer can save any object at any time inside their module, using
#' standard R functions for saving R objects (e.g., \code{save} or \code{saveRDS}).
#' This is the least modular approach, as it will happen whether a module user
#' wants it or not.
#'
#' @author Eliot McIntire
#' @author Alex Chubaty
#' @note It is not possible to schedule separate saving events for each object
#' that is listed in the \code{.saveObjects}.
#'
#' @param sim A \code{simList} simulation object.
#'
#' @importFrom dplyr bind_rows
#' @importFrom data.table data.table
#' @export
#' @docType methods
#' @rdname saveFiles
#'
#' @examples
#' \dontrun{
#'
#' # This will save the "caribou" object at the save interval of 1 unit of time
#' #  in the outputPath location
#' outputPath <- file.path(tempdir(), "test_save")
#' times <- list(start = 0, end = 6, "month")
#' parameters <- list(
#'   .globals = list(stackName = "landscape"),
#'   caribouMovement = list(
#'     .saveObjects = "caribou",
#'     .saveInitialTime = 1, .saveInterval = 1
#'   ),
#'   randomLandscapes = list(.plotInitialTime = NA, nx = 20, ny = 20))
#'
#' modules <- list("randomLandscapes", "caribouMovement")
#' paths <- list(
#'   modulePath = system.file("sampleModules", package = "SpaDES"),
#'   outputPath = savePath
#' )
#' mySim <- simInit(times = times, params = parameters, modules = modules,
#'                  paths = paths)
#'
#' # The caribou module has a saveFiles(sim) call, so it will save caribou
#' spades(mySim)
#' dir(outputPath)
#'
#' # remove the files
#' file.remove(dir(savePath, full.names = TRUE))
#'
#' }
saveFiles <- function(sim) {
  curTime <- time(sim, sim@simtimes[["timeunit"]])
  # extract the current module name that called this function
  moduleName <- sim@current[["moduleName"]]
  if (length(moduleName) == 0) {
    moduleName <- "save"
    if (NROW(outputs(sim)[outputs(sim)$saveTime == curTime, ])) {
      outputs(sim)[outputs(sim)$saveTime == curTime, "saved"] <- NA
    }
  }

  if (moduleName != "save") {
    # i.e., a module driven save event
    toSave <- lapply(params(sim), function(y) return(y$.saveObjects))[[moduleName]] %>%
      data.frame(objectName = ., saveTime = curTime, file = ., stringsAsFactors = FALSE)
    toSave <- .fillOutputRows(toSave)
    outputs(sim) <- rbind(outputs(sim), toSave)

    # don't need to save exactly same thing more than once - use data.table here because distinct
    # from dplyr does not do as expected
    outputs(sim) <- data.table(outputs(sim)) %>%
      unique(., by = c("objectName", "saveTime", "file", "fun", "package", "arguments")) %>%
      data.frame()
  }

  if (NROW(outputs(sim)[outputs(sim)$saveTime == curTime & is.na(outputs(sim)$saved), "saved"]) > 0) {
    wh <- which(outputs(sim)$saveTime == curTime & is.na(outputs(sim)$saved))
    for (i in wh) {
      if (exists(outputs(sim)[i, "objectName"], envir = sim@.envir)) {
        args <- append(list(get(outputs(sim)[i, "objectName"], envir = sim@.envir),
                            file = outputs(sim)[i, "file"]),
                       outputArgs(sim)[[i]])
        args <- args[!sapply(args, is.null)]
        args <- suppressWarnings(args[!unlist(lapply(args, function(j) {
          isTRUE(tryCatch(is.na(j), error = function(e) FALSE))
        }))])

        # The actual save line
        do.call(outputs(sim)[i, "fun"], args = args,
                envir = getNamespace(outputs(sim)[i, "package"]))

        outputs(sim)[i, "saved"] <- TRUE
      } else {
        warning(paste(outputs(sim)$obj[i], "is not an object in the simList. Cannot save."))
        outputs(sim)[i, "saved"] <- FALSE
      }
    }
  }

  # Schedule an event for the next time in the saveTime column
  if (any(is.na(outputs(sim)[outputs(sim)$saveTime > curTime, "saved"]))) {
    nextTime <- min(outputs(sim)[is.na(outputs(sim)$saved), "saveTime"], na.rm = TRUE)
    attributes(nextTime)$unit <- sim@simtimes[["timeunit"]]
    if (time(sim) == end(sim)) {
      sim <- scheduleEvent(sim, nextTime, "save", "end", .last())
    } else {
      sim <- scheduleEvent(sim, nextTime, "save", "later", .last())
    }
  }
  return(invisible(sim))
}

#' File extensions map
#'
#' How to load various types of files in R.
#'
#' @export
#' @rdname loadFiles
.saveFileExtensions <- function() {
  .sFE <- data.frame(matrix(ncol = 3, byrow = TRUE, c(
    "rds", "saveRDS", "base",
    "txt", "write.table", "utils",
    "csv", "write.csv", "utils",
    "grd", "writeRaster", "raster"
  )), stringsAsFactors = FALSE)
  setnames(.sFE, new = c("exts", "fun", "package"), old = paste0("X", 1:3))
  .sFE <- .sFE[order(.sFE$package, .sFE$fun), ]
  return(.sFE)
}


#' Save a whole \code{simList} object to disk
#'
#' Because of the environment slot, this is not quite as straightforward as
#' just saving the object. This also has option for file-backed Rasters.
#'
#' @export
#' @inheritParams spades
#' @param filename Character string with the path for saving \code{simList}
#' @param keepFileBackedAsIs Logical. If there are file-backed \code{Raster}
#'        objects, should they be kept in their file-backed format (TRUE and default),
#'        or loaded into RAM and saved within the \code{.Rdata} file (FALSE).
#'        If TRUE, then the files will be copied to
#'        \code{file.path(dirname(filename), "rasters")}.
#' @inheritParams base::save
#' @return A saved .Rdata file in \code{filename} location.
#' @rdname loadFiles
saveSimList <- function(sim, filename, keepFileBackedAsIs, envir=parent.frame()) {
  simName <- sim
  sim <- get(sim, envir=envir)

  isRaster <- unlist(lapply(sim@.envir, function(x) is(x, "Raster")))
  if(any(isRaster)) {
    if(keepFileBackedAsIs) {
      for(x in names(isRaster)[isRaster])
        sim[[x]] <- prepareFileBackedRaster(sim[[x]], repoDir = dirname(filename))
    } else {
      for(x in names(isRaster)[isRaster])
        sim[[x]][] <- sim[[x]][]
    }
  }
  assign(simName, sim, envir=envir)
  save(list=simName, file=filename, envir=envir)
}
