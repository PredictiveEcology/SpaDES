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
      if (is.null(simParams(sim)$.globals$.outputPath)){
        outputPath <- x
      } else {
        outputPath <- file.path(simParams(sim)$.globals$.outputPath, x)
      }
      outputPath <- checkPath(outputPath, create=TRUE)
    })

    # no scheduling of new event. Saving will be called by other events,
    #   in an event-specific manner.
  }
  return(sim)
}


##############################################################
#' Save simulation objects according to simParams
#'
#' @author Eliot McIntire
#' @author Alex Chubaty
#'
#' @export
#' @docType methods
#' @rdname simSave
#'
# @examples
# need examples
simSave = function(sim) {
  # extract savePaths from modules
  modulePaths <- sapply(simParams(sim), function(y) {
    if (is.null(simParams(sim)$.globals$.outputPath)){
      outputPath <- y$.savePath
    } else {
      outputPath <- file.path(simParams(sim)$.globals$.outputPath, y$.savePath)
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

  txtTime = sprintf(paste0("%0", nchar(simStopTime(sim)), "d"), simCurrentTime(sim))

  # save objects to a filename that has same name as object name, plus current simulation time
  lapply(toSave[[moduleName]], function(objectname) {
    saveRDS(get(objectname),
            file.path(modulePaths[[moduleName]],
                      paste(objectname, txtTime, ".rds", sep="")) )})

}
