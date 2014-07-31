################################################
###
### A LOAD MODULE
###
###############################################

# Just checks for paths, creates them if they do not exist
doEvent.load = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {

    # Different types of data to load in or connect to
    #library(raster)
    #raster("filename)
    
    #library(RSQLite)
    #con <- dbConnect(SQLite(), ":memory:")         ## in-memory, replace with file
    #data(USArrests)
    #dbWriteTable(con, "arrests", USArrests)
    
    #csv
    #read.csv("filename")
    
    #
    #sim <- scheduleEvent(sim, simParams(sim)$load$loadTime, "load", "load")
  }
  return(sim)
}


# ##############################################################
# #' Save simulation objects according to simParams
# #'
# #' @author Eliot McIntire
# #' @author Alex Chubaty
# #'
# #' @export
# #' @docType methods
# #' @rdname simSave
# #'
# # @examples
# # need examples
# simLoad = function(sim) {
# 
#   # extract savePaths from modules
#   modulePaths <- lapply(simParams(sim), function(y) return(y$loadPath) )
# 
#   # extract objects to save from modules
#   toLoad <- lapply(simParams(sim), function(y) return(y$toLoad) )
# 
#   # extract the current module name that called this function
#   moduleName = simEvents(sim)[1,moduleName]
# 
#   # if no savePath is specified, use active working directory
#   if (is.null(toLoad[[moduleName]])) {
#     modulePaths[[moduleName]] <- "."
#   }
# 
#   txtTime = sprintf(paste0("%0", nchar(simStopTime(sim)), "d"), simCurrentTime(sim))
# 
#   # save objects to a filename that has same name as object name, plus current simulation time
#   lapply(toLoad[[moduleName]], function(objectname) {
#     saveRDS(get(objectname),
#             file.path(modulePaths[[moduleName]],
#                       paste(objectname, txtTime, ".rds", sep="")) )})
# 
# }
