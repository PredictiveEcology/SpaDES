# register the S3 `person` class for use with S4 methods.
setClass("person4",
         slots=list(given="character", family="character", middle="character",
                    email="character", role="character", comment="character",
                    first="character", last="character")
)
setOldClass("person", S4Class="person4")
selectMethod("show", "person")
removeClass("person4")



#' The \code{moduleDeps} class
#'
#' Descriptor object for specifying SpaDES module dependecies.
#'
#' @slot name           Name of the module as a character string.
#'
#' @slot description    Description of the module as a character string.
#'
#' @slot keywords       Character vector containing a module's keywords.
#'
#' @slot authors        The author(s) of the module as a \code{\link{person}} object.
#'
#' @slot spatialExtent  Specifies the module's spatial extent as an \code{\link{Extent}} object.
#'                      Defaults to \code{NA}.
#'
#' @slot timeframe      Specifies the valid timeframe for which the module was designed to simulate.
#'                      Must be a \code{\link{POSIXt}} object of length 2, specifying the start and end times
#'                      (e.g., \code{as.POSIXlt(c("1990-01-01 00:00:00", "2100-12-31 11:59:59"))}).
#'                      Can be specified as \code{NA} using \code{as.POSIXlt(c(NA, NA))}.
#'
#' @slot translators    Character vector describing the "translators" available for this module.
#'                      Defaults to \code{NA_character_}.
#'
#' @slot citation       A citation for the module, as a character string. Defaults to \code{NA_character_}.
#'
#' @slot reqdPkgs       Character vector of R package names to be loaded. Defaults to \code{NA_character_}.
#'
#' @slot inputObjects   A \code{data.frame} specifying the object dependecies of the module,
#'                      with columns \code{objectName} and \code{objectClass}.
#'                      For objects that are used within the module as both an input and an output,
#'                      add the object to each of these \code{data.frame}s.
#'
#' @slot outputObjects  A \code{data.frame} specifying the objects output by the module,
#'                      following the format of \code{inputObjects}.
#'
#' @rdname moduleDeps-class
#' @importFrom raster extent
#' @exportClass moduleDeps
#'
#' @seealso simDeps
#'
#' @author Alex Chubaty
#'
setClass("moduleDeps",
         slots=list(name="character", description="character", keywords="character",
                    authors="person", spatialExtent="Extent", timeframe="POSIXt",
                    translators="list", citation="list", reqdPkgs="list",
                    inputObjects="data.frame", outputObjects="data.frame"),
         prototype=list(name=character(), description=character(),
                        keywords=character(), authors=person(),
                        spatialExtent=extent(as.numeric(c(NA,NA,NA,NA))),
                        timeframe=as.POSIXlt(c(NA, NA)), translators=list(),
                        citation=list(), reqdPkgs=list(),
                        inputObjects=data.frame(name=character(), class=character(), stringsAsFactors=FALSE),
                        outputObjects=data.frame(name=character(), class=character()), stringsAsFactors=FALSE),
         validity=function(object) {
           if (length(object@name)!=1) stop("name must be a single character string.")
           if (length(object@description)!=1) stop("description must be a single character string.")
           if (length(object@keywords)<1) stop("keywords must be supplied.")
           if (length(object@authors)<1) stop("authors must be specified.")
           if (length(object@timeframe)!=2) stop("timeframe must be specified using two date-times.")
           if (!any(unlist(lapply(object@reqdPkgs, is.character)))) stop("reqdPkgs must be specified as a list of package names.")

           object@inputObjects <- as.data.frame(object@inputObjects, stringsAsFactors=FALSE)
           object@outputObjects <- as.data.frame(object@outputObjects, stringsAsFactors=FALSE)
           if (length(object@inputObjects)<1) stop("input object name and class must be specified, or NA.")
           if (length(object@outputObjects)<1) stop("output object name and class must be specified, or NA.")
           if ( !("name" %in% colnames(object@inputObjects)) ||
                  !("class" %in% colnames(object@inputObjects)) ) {
             stop("input object data.frame must use colnames name and class.")
           }
           if ( !("name" %in% colnames(object@outputObjects)) ||
                  !("class" %in% colnames(object@outputObjects)) ) {
             stop("output object data.frame must use colnames name and class.")
           }
           if (!is.character(object@inputObjects$name)) stop("input object name must be a character string.")
           if (!is.character(object@inputObjects$class)) stop("input object class must be a character string.")
           if (!is.character(object@outputObjects$name)) stop("output object name must be a character string.")
           if (!is.character(object@outputObjects$class)) stop("output object class must be a character string.")
         }
)

#' The \code{simDeps} class
#'
#' Defines all simulation dependencies for all modules within a SpaDES simulation.
#'
#' @slot dependencies   List of \code{\link{moduleDeps}} dependency objects.
#'
#' @rdname simDeps-class
#' @exportClass simDeps
#'
#' @author Alex Chubaty
#'
setClass("simDeps",
         slots=list(dependencies="list"),
         prototype=list(dependencies=list(NULL)),
         validity=function(object) {
           # remove empty (NULL) elements
           object@dependencies <- object@dependencies[lapply(object@dependencies, length)>0]

           # ensure list contains only moduleDeps objects
           lapply(object@dependencies, is, class2="moduleDeps")

           if ( (lower(object@objectDeps$objectType)!="input") ||
                  (lower(object@objectDeps$objectType)!="output") ) {
             stop("objectType must be one of \'input\' or \'output\'.")
           }
         }
)



#' Define a new module
#'
#' Specify a new module's metadata as well as object and package dependecies.
#' This is simply a constructor method for the \code{\link{moduleDepends}} class.
#'
#' @param name         Name of the module as a character string.
#'
#' @param description  Description of the module as a character string.
#'
#' @param keywords     Character vector containing a module's keywords.
#'
#' @param authors      The author(s) of the module as a \code{\link{person}} object.
#'
#' @param spatialExtent Specifies the module's spatial extent as an \code{\link{Extent}} object.
#'
#' @param timeframe    Specifies the valid timeframe for which the module was designed to simulate.
#'                     Must be a \code{\link{POSIXt}} object of length 2, specifying the start and end times
#'                     (e.g., \code{as.POSIXlt(c("1990-01-01 00:00:00", "2100-12-31 11:59:59"))}).
#'                     Defaults to \code{as.POSIXlt(c(NA,NA))}.
#'
#' @param translators  List of "translators" available for this module, which should include
#'                     the name of the "translator" as well as where to find it, e.g., a url.
#'
#' @param citation     A list giving the citation for the module.
#'
#' @param reqdPkgs     A list giving the names of packages required for the module.
#'
#' @param objectDeps   A \code{data.frame} specifying the object dependecies of the module:
#'                     each row specifies a directional dependency, with columns
#'                     \code{objectName}, \code{objectClass}, and \code{objectType}.
#'                     These first two are self-explanatory, and \code{objectType}
#'                     should be one of \code{"input"} or \code{"output"}.
#'                     For objects that are used within the module as both an input and an output,
#'                     each of these should be added as separate entries in the data.frame.
#'
#' @export
#' @docType methods
#' @rdname defineModule-method
#'
#' @author Alex Chubaty
#'
#' @examples
#' \dontrun{
#'   defineModule(...)
#' }
#'
setGeneric("defineModule", function(name, description, keywords, authors,
                                    spatialExtent, timeframe, translators,
                                    citation, reqdPkgs, inputObjects, outputObjects) {
  standardGeneric("defineModule")
})

#' @rdname defineModule-method
#'
setMethod("defineModule",
          signature(name="character", description="character", keywords="character",
                    authors="person", spatialExtent="Extent", timeframe="POSIXt",
                    translators="list", citation="list", reqdPkgs="list",
                    inputObjects="data.frame", outputObjects="data.frame"),
          definition=function(name, description, keywords, authors, spatialExtent,
                              timeframe, translators, citation, reqdPkgs,
                              inputObjects, outputObjects) {
            x <- new("moduleDeps", name=name, description=description,
                     keywords=keywords, authors=authors,
                     spatialExtent=spatialExtent, timeframe=timeframe,
                     translators=translators, citation=citation,
                     reqdPkgs=reqdPkgs, inputObjects=inputObjects,
                     outputObjects=outputObjects)

            return(x)
})
