################################################
###
### A SAVE MODULE
###
###############################################

# Just checks for paths, creates them if they do not exist
doEvent.save = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    # check that output directory exists, make it if not
    pathsToCheck <- unname(unlist(lapply(simParams(sim), function(y) return(y$.savePath))))

    # make paths if they don't exist
    lapply(pathsToCheck, function(x) {
      if (is.null(simGlobalsOutputPath(sim))){
        outputPath <- x
      } else {
        outputPath <- file.path(simGlobalsOutputPath(sim), x)
      }
      outputPath <- checkPath(outputPath, create=TRUE)
    })

    # no scheduling of new event. Saving will be called by other events,
    #   in an event-specific manner.
  }
  return(invisible(sim))
}


##############################################################
#' Save simulation objects according to simParams
#'
#' If there is a list entry with \code{.saveObjects} as a character string vector of
#' object names to save, then these objects will be saved with a call to saveFiles.
#' The file names will be equal to the object name plus
#' \code{simCurrentTime(sim)} is appended at the end. The files are saved as \code{.rds} files,
#' meaning, only one object gets saved per file.
#'
#' @author Eliot McIntire
#' @author Alex Chubaty
#'
#' @param sim A \code{simList} simulation object.
#'
#' @export
#' @docType methods
#' @rdname saveFiles
#'
#' @examples
#' \dontrun{
#'   saveFiles(mySim)
#' }
saveFiles = function(sim) {
  # extract savePaths from modules
  modulePaths <- sapply(simParams(sim), function(y) {
    if (is.null(simGlobalsOutputPath(sim))){
      outputPath <- y$.savePath
    } else {
      outputPath <- file.path(simGlobalsOutputPath(sim), y$.savePath)
    }
    return(outputPath)
    })

  # extract objects to save from modules
  toSave <- lapply(simParams(sim), function(y) return(y$.saveObjects) )

  # extract the current module name that called this function
  moduleName = simEvents(sim)[1, moduleName]

  # if no savePath is specified, use active working directory
  if (is.null(toSave[[moduleName]])) {
    modulePaths[[moduleName]] <- "."
  }

  txtTime = paddedFloatToChar(simCurrentTime(sim), ceiling(log10(simStopTime(sim)+1)))

  # save objects to a filename that has same name as object name, plus current simulation time
  lapply(toSave[[moduleName]], function(objectname) {
    saveRDS(get(objectname),
            file.path(modulePaths[[moduleName]],
                      paste(objectname, txtTime, ".rds", sep="")) )})

}
