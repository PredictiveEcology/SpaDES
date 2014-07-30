##############################################################
#' Save simulation objects at specified frequencies.
#'
#' @param sim           A \code{SimList} simulation object.
#' 
#' @param eventTime    A numeric specifying the time of the next event.
#' 
#' @param eventType    A character string specifying the type of event:
#'                      one of either \code{"init"}, \code{"load"}, or \code{"save"}.
#' 
#' @param debug         Optional logical flag determines whether sim debug info
#'                      will be printed (default \code{debug=FALSE}.
#'
#' @return Returns the modified \code{SimList} object.
#' 
#' @seealso \code{\link{.Random.seed}}.
#' 
#' @author Alex Chubaty
#' 
#' @export
#' @docType methods
#' @rdname checkpoint
#'
# @examples
# need examples
doEvent.save = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    # check that output directory exists, make it if not
    pathsToCheck <- unname(unlist(lapply(simParams(sim),function(y) {        
        return(y$savePath)
      }
      )))
        
      lapply(pathsToCheck, checkPath, create = TRUE)
  } 
  return(sim)
}


simSave = function(sim) {

  # extract savePaths from modules 
  modulePaths <- lapply(simParams(sim), function(y) return(y$savePath) )
  
  # extract objects to save from modules
  toSave <- lapply(simParams(sim), function(y) return(y$toSave) )
  
  # extract the current module name that called this function
  moduleName = simEvents(sim)[1,moduleName]

  # if no savePath is specified, use active working directory
  if (is.null(toSave[[moduleName]])) {
    modulePaths[[moduleName]] <- "."
  } 
  # save objects to a filename that has same name as object name, plus current simulation time
  lapply(toSave[[moduleName]], function(objectname) {
    saveRDS(get(objectname),
            file.path(modulePaths[[moduleName]],
                      paste(objectname,simCurrentTime(sim), ".rds", sep="")) )})
    
#   print(simCurrentTime(sim))
#   print(paste("modulePath:",modulePaths[[moduleName]]))
#   print(paste("toSave: ",toSave[[moduleName]]))
      
}       
