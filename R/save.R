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

    # The load doEvent

    if (NROW(outputs(sim))>0) {
      firstSave <- outputs(sim)[,min(saveTime, na.rm=TRUE)]
      attributes(firstSave)$unit <- timeunit(sim)
      sim <- scheduleEvent(sim, firstSave, "save", "spades")
      sim <- scheduleEvent(sim, end(sim), "save", "end")
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
#' @export
#' @docType methods
#' @rdname saveFiles
#'
#' @examples
#' \dontrun{
#'  sim <- saveFiles(mySim)
#' }
saveFiles = function(sim) {
  # extract savePaths from modules
  # extract objects to save from modules

  curTime <- time(sim, timeunit(sim))

  # extract the current module name that called this function
  moduleName <- events(sim)[1L,moduleName]
  if(moduleName!="save") { # i.e., .a module driven save event
#     txtTime = paste0(attr(time(sim, timeunit(sim)),"unit"),
#                      paddedFloatToChar(time(sim, timeunit(sim)), ceiling(log10(end(sim, timeunit(sim))+1))))

    toSave <- lapply(params(sim), function(y) return(y$.saveObjects))[[moduleName]] %>%
      data.table(objectName=., saveTime=curTime,
                 file=paste0(.,".rds"))
    outputs(sim) <- rbindlist(list(outputs(sim), toSave), fill = TRUE)

    # don't need to save exactly same thing more than once
    outputs(sim) <- unique(outputs(sim))

  }

  if(NROW(outputs(sim)[saveTime==curTime & is.na(saved)])>0) {

    wh <- which(outputs(sim)$saveTime==curTime & is.na(outputs(sim)$saved))
    for (i in wh) {
      if(exists(outputs(sim)[i,objectName], envir=envir(sim))) {
        args <- append(list(get(outputs(sim)[i,objectName], envir=envir(sim)),
                     file=outputs(sim)[i,file]),
                     outputArgs(sim)[[i]])
        args <- args[!sapply(args, is.null)]

        #get(outputs(sim)[i,fun])(get(outputs(sim)[i,objectName], envir=envir(sim)),
        #                     file=outputs(sim)[i,file])
        do.call(outputs(sim)[i,fun], args = args)
        outputs(sim)[i,saved:=TRUE]
      } else {
        browser()
        warning(paste(outputs(sim)[i,objectName], "is not an object in the simList. Cannot save."))
        outputs(sim)[i,saved:=FALSE]
      }
    }
  }

  # Schedule an event for the next time in the saveTime column
  if(any(is.na(outputs(sim)[saveTime>curTime,saved]))) {
    nextTime <- outputs(sim)[is.na(saved),min(saveTime,na.rm=TRUE)]
    attributes(nextTime)$unit <- timeunit(sim)
    sim <- scheduleEvent(sim, nextTime, "save", "later")
  }

  return(invisible(sim))

}
