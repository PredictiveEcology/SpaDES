if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("saved", "saveTime"))
}

# Just checks for paths, creates them if they do not exist
doEvent.save = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    # check that output directory exists, make it if not

    pathsToCheck <- checkPath(outputPath(sim), create=TRUE)

    # The load doEvent

    if (NROW(outputs(sim))>0) {
      firstSave <- min(outputs(sim)[,"saveTime"], na.rm=TRUE)
      attributes(firstSave)$unit <- timeunit(sim)
      sim <- scheduleEvent(sim, firstSave, "save", "spades")
      sim <- scheduleEvent(sim, end(sim, timeunit(sim)), "save", "end")
    }

  } else if (eventType=="spades") {
    sim <- saveFiles(sim)
  } else if (eventType=="later") {
    sim <- saveFiles(sim)
  } else if (eventType=="end") {
    message(paste0("Files saved. Use outputs(your simList) for details"))
  }


#     # make paths if they don't exist
#     lapply(pathsToCheck, function(x) {
#       if (is.null(outputPath(sim))){
#         outputPath <- x
#       } else {
#         outputPath <- file.path(outputPath(sim), x)
#       }
#       outputPath <- checkPath(outputPath, create=TRUE)
#     })

    # no scheduling of new event. Saving will be called by other events,
    #   in an event-specific manner.

  return(invisible(sim))
}

##############################################################
#' Save simulation objects according to params
#'
#' If there is a list entry with \code{.saveObjects} as a character string vector of
#' object names to save, then these objects will be saved with a call to saveFiles.
#' The file names will be equal to the object name plus
#' \code{time(sim)} is appended at the end. The files are saved as \code{.rds} files,
#' meaning, only one object gets saved per file.
#'
#' @author Eliot McIntire
#' @author Alex Chubaty
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
  moduleName <- events(sim)[1L,moduleName]

  if(moduleName!="save") { # i.e., .a module driven save event

    toSave <- lapply(params(sim), function(y) return(y$.saveObjects))[[moduleName]] %>%
      data.frame(objectName=., saveTime=curTime,
                 file=., stringsAsFactors=FALSE)
    outputs(sim) <- bind_rows(list(outputs(sim), toSave))

    # don't need to save exactly same thing more than once

    outputs(sim) <- distinct(outputs(sim), objectName, saveTime, file, fun, package)

  }

  if(NROW(outputs(sim)[outputs(sim)$saveTime==curTime & is.na(outputs(sim)$saved),"saved"])>0) {

    wh <- which(outputs(sim)$saveTime==curTime & is.na(outputs(sim)$saved))
    for (i in wh) {
      if(exists(outputs(sim)[i,"objectName"], envir=envir(sim))) {
        args <- append(list(get(outputs(sim)[i,"objectName"], envir=envir(sim)),
                     file=outputs(sim)[i,"file"]),
                     outputArgs(sim)[[i]])
        args <- args[!sapply(args, is.null)]

        # The actual save line
        do.call(outputs(sim)[i,"fun"], args = args)

        outputs(sim)[i,"saved"] <- TRUE
      } else {
        warning(paste(outputs(sim)[i,"objectName"],
                      "is not an object in the simList. Cannot save."))
        outputs(sim)[i,saved] <- FALSE
      }
    }
  }

  # Schedule an event for the next time in the saveTime column
  if(any(is.na(outputs(sim)[outputs(sim)$saveTime>curTime,"saved"]))) {
    nextTime <- min(outputs(sim)[is.na(outputs(sim)$saved),"saveTime"],na.rm=TRUE)
    attributes(nextTime)$unit <- timeunit(sim)
    sim <- scheduleEvent(sim, nextTime, "save", "later")
  }

  return(invisible(sim))

}

#' File extensions map
#'
#' How to load various types of files in R.
#'
#' @export
#' @rdname loadFiles
.saveFileExtensions = function() {
  .sFE <- data.table(matrix(ncol=3, byrow=TRUE,c(
    "rds", "saveRDS", "base" ,
    "txt", "write.table", "utils" ,
    "csv", "write.csv", "utils" ,
    "", "writeRaster", "raster"
  )))
  setnames(.sFE, new = c("exts", "fun", "package"), old=paste0("V",1:3))
  setkey(.sFE, package, fun)
  return(.sFE)
}
