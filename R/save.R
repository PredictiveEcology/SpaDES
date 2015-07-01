################################################
###
### A SAVE MODULE
###
###############################################

# Just checks for paths, creates them if they do not exist
doEvent.save = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    # check that output directory exists, make it if not
    pathsToCheck <- checkPath(outputPath(sim), create=TRUE)

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
  }
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
  # extract objects to save from modules
  toSave <- lapply(params(sim), function(y) return(y$.saveObjects) )

  # extract the current module name that called this function
  moduleName = events(sim)[1, moduleName]

  txtTime = paste0(attr(time(sim),"unit"),
                   paddedFloatToChar(time(sim), ceiling(log10(end(sim)+1))))

  # save objects to a filename that has same name as object name, plus current simulation time
  lapply(toSave[[moduleName]], function(objectname) {
    saveRDS(sim[[objectname]],
            file.path(outputPath(sim),
                      paste(objectname, txtTime, ".rds", sep="")) )})

}
