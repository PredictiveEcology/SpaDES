if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("saved", "saveTime", "fun", "package"))
}

# Just checks for paths, creates them if they do not exist
doEvent.save <- function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    # check that output directory exists, make it if not
    #pathsToCheck <- checkPath(outputPath(sim), create = TRUE)

    if (NROW(outputs(sim)) > 0) {
      firstSave <- min(outputs(sim)[, "saveTime"], na.rm = TRUE)
      attributes(firstSave)$unit <- timeunit(sim)
      sim <- scheduleEvent(sim, firstSave, "save", "spades", .last())
      sim <- scheduleEvent(sim, end(sim, timeunit(sim)), "save", "end", .last())
    }

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
#' For objects saved using this function, the module developer must create save
#' events that schedule a call to \code{saveFiles}.
#'
#' There are 3 ways to save objects using \code{SpaDES}.
#'
#' @section 1. Model-level saving:
#'
#' Using the \code{outputs} slot in the \code{\link{simInit}} call.
#' See 2nd example in \code{\link{simInit}}.
#' This can be convenient because it gives overall control of many modules at a
#' time, and there is an implicit scheduling that gets created during the
#' \code{\link{simInit}} call.
#'
#' @section 2. Module-level saving:
#'
#' Using the \code{saveFiles} function inside a module.
#' This must be accompanied by a \code{.saveObjects} list element in the
#' \code{params} slot in the \code{\link{simInit}} call.
#' Usually a module developer will create this method for future users of
#' their module.
#'
#' @section 3. User saving:
#'
#' A user can save any object at any time inside their module.
#' This is the least modular approach.
#'
#' @author Eliot McIntire
#' @author Alex Chubaty
#' @note It is not possible to schedule separate saving events for each object
#' that is listed in the \code{.saveObjects}.
#'
#' @param sim A \code{simList} simulation object.
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr distinct
#' @export
#' @docType methods
#' @rdname saveFiles
#'
#' @examples
#' \dontrun{
#'  sim <- saveFiles(mySim)
#' }
saveFiles = function(sim) {
  curTime <- time(sim, timeunit(sim))
  # extract the current module name that called this function
  moduleName <- currentModule(sim)

  if (moduleName != "save") { # i.e., .a module driven save event
    toSave <- lapply(params(sim), function(y) return(y$.saveObjects))[[moduleName]] %>%
      data.frame(objectName = ., saveTime = curTime, file = ., stringsAsFactors = FALSE)
    toSave <- .fillOutputRows(toSave)
    outputs(sim) <- rbind(outputs(sim), toSave)

    # don't need to save exactly same thing more than once
    outputs(sim) <- distinct(outputs(sim), objectName, saveTime, file, fun, package)
  }

  if (NROW(outputs(sim)[outputs(sim)$saveTime == curTime & is.na(outputs(sim)$saved), "saved"]) > 0) {

    wh <- which(outputs(sim)$saveTime == curTime & is.na(outputs(sim)$saved))
    for (i in wh) {
      if (exists(outputs(sim)[i,"objectName"], envir = envir(sim))) {
        args <- append(list(get(outputs(sim)[i, "objectName"], envir = envir(sim)),
                     file = outputs(sim)[i, "file"]),
                     outputArgs(sim)[[i]])
        args <- args[!sapply(args, is.null)]

        # The actual save line
        do.call(outputs(sim)[i,"fun"], args = args,
                envir=getNamespace(outputs(sim)[i,"package"]))

        outputs(sim)[i,"saved"] <- TRUE
      } else {
        warning(paste(outputs(sim)$obj[i],
                      "is not an object in the simList. Cannot save."))
        outputs(sim)[i, "saved"] <- FALSE
      }
    }
  }

  # Schedule an event for the next time in the saveTime column
  if (any(is.na(outputs(sim)[outputs(sim)$saveTime > curTime,"saved"]))) {
    nextTime <- min(outputs(sim)[is.na(outputs(sim)$saved),"saveTime"], na.rm = TRUE)
    attributes(nextTime)$unit <- timeunit(sim)
    sim <- scheduleEvent(sim, nextTime, "save", "later", .last())
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
    "rds", "saveRDS", "base" ,
    "txt", "write.table", "utils" ,
    "csv", "write.csv", "utils" ,
    "grd", "writeRaster", "raster"
  )), stringsAsFactors = FALSE)
  setnames(.sFE, new = c("exts", "fun", "package"), old = paste0("X", 1:3))
  .sFE <- .sFE[order(.sFE$package, .sFE$fun),]
  return(.sFE)
}
