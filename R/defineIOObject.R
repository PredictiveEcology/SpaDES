#' This function is to define the input and output object
#'
#'
#' @param IO character, specify whether the objects are inputs or outputs,
#'                      two options "input" or "output"
#'
#' @param objectName character vector, specify the object name
#'
#' @param objectClass character, specify the object class, default is NA
#'
#' @param sourceURL character, specify the URL to reach the object, default is NA
#'
#' @param other character,  notes or descriptions something like those.
#'                          default is NA
#'
#'
#'
#' @return a data frame
#'
#'
#'
#' @export
#' @docType methods
#' @rdname defineIOObject
#'
#' @author Yong Luo
#'
setGeneric("defineIOObject",
           function(IO,
                    objectName,
                    objectClass,
                    sourceURL,
                    other) {
             standardGeneric("defineIOObject")
           })
#' @export
#' @rdname defineIOObject
setMethod("defineIOObject",
          signature = signature(IO = "character",
                                objectName = "character",
                                objectClass = "character",
                                sourceURL = "character",
                                other = "character"),
          definition = function(IO,
                                objectName,
                                objectClass,
                                sourceURL,
                                other){
            if(IO == "input"){
              returnDataframe <- data.frame(cbind(objectName,
                                                  objectClass,
                                                  sourceURL,
                                                  other),
                                            stringsAsFactors = FALSE)
              return(returnDataframe)
            } else if(IO == "output"){
              returnDataframe <- data.frame(cbind(objectName,
                                                  objectClass,
                                                  other),
                                            stringsAsFactors = FALSE)
              return(returnDataframe)
            }
          })

#' @export
#' @rdname defineIOObject
setMethod("defineIOObject",
          signature = signature(IO = "character",
                                objectName = "character",
                                objectClass = "missing",
                                sourceURL = "character",
                                other = "character"),
          definition = function(IO,
                                objectName,
                                sourceURL,
                                other){
            return(defineIOObject(IO, objectName, objectClass = NA_character_,
                                   sourceURL, other))
          })



#' @export
#' @rdname defineIOObject
setMethod("defineIOObject",
          signature = signature(IO = "character",
                                objectName = "character",
                                objectClass = "character",
                                sourceURL = "missing",
                                other = "character"),
          definition = function(IO,
                                objectName,
                                objectClass,
                                other){
            return(defineIOObject(IO, objectName, objectClass,
                                   sourceURL = NA_character_, other))
          })


#' @export
#' @rdname defineIOObject
setMethod("defineIOObject",
          signature = signature(IO = "character",
                                objectName = "character",
                                objectClass = "character",
                                sourceURL = "character",
                                other = "missing"),
          definition = function(IO,
                                objectName,
                                objectClass,
                                sourceURL){
            other <- NA_character_
            return(defineIOObject(IO, objectName, objectClass,
                                   sourceURL, other = NA_character_))
          })



#' @export
#' @rdname defineIOObject
setMethod("defineIOObject",
          signature = signature(IO = "character",
                                objectName = "character",
                                objectClass = "missing",
                                sourceURL = "missing",
                                other = "character"),
          definition = function(IO,
                                objectName,
                                other){
            return(defineIOObject(IO, objectName, objectClass = NA_character_,
                                   sourceURL = NA_character_, other))
          })

#' @export
#' @rdname defineIOObject
setMethod("defineIOObject",
          signature = signature(IO = "character",
                                objectName = "character",
                                objectClass = "character",
                                sourceURL = "missing",
                                other = "missing"),
          definition = function(IO,
                                objectName,
                                objectClass){

            return(defineIOObject(IO, objectName, objectClass,
                                   sourceURL = NA_character_, other = NA_character_))
          })


#' @export
#' @rdname defineIOObject
setMethod("defineIOObject",
          signature = signature(IO = "character",
                                objectName = "character",
                                objectClass = "missing",
                                sourceURL = "missing",
                                other = "missing"),
          definition = function(IO,
                                objectName){
            return(defineIOObject(IO, objectName, objectClass = NA_character_,
                                   sourceURL = NA_character_, other = NA_character_))
          })


