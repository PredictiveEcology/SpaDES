#' Persons (S4)
#'
#' An S4 implementation of the \code{\link{person}} class.
#'
#' @inheritParams person
#'
#' @rdname person4-class
#' @exportClass person4
#'
#' @seealso person
#'
#' @author Alex Chubaty
#'
setClass("person4",
         prototype=list(given=as.character(NULL), family=as.character(NULL),
                        email=as.character(NULL), role=as.character(NULL),
                        comment=as.character(NULL))
)
setOldClass("person", S4Class="person4")
selectMethod("show", "person")
removeClass("person4")



#' The \code{moduleDeps} class
#'
#' Descriptor object for specifying SpaDES module dependecies.
#'
#' Each \code{moduleDeps} describes a module's dependency on another object for a simulation.
#' Multiple \code{moduleDeps} objects (i.e., describing multiple dependencies) are typically supplied using a \code{\link{dependsList}}.
#'
#' @slot name         Name of the module as a character string.
#'
#' @slot description  Description of the module as a character string.
#'
#' @slot keywords     Character vector containing a module's keywords.
#'
#' @slot authors      The author(s) of the module as a \code{\link{person}} object.
#'
#' @slot spatialExtent Specifies the module's spatial extent as an \code{\link{Extent}} object.
#'
#' @slot timeframe    Specifies the valid timeframe for which the module was designed to simulate.
#'                    Must be a \code{\link{POSIXlt}} object of length 2, specifying the start and end times
#'                    (e.g., \code{as.POSIXlt(c("1990-01-01 00:00:00", "2100-12-31 11:59:59"))}).
#'
#' @slot translators  Character vector describing the "translators" available for this module.
#'
#' @slot citation     A citation for the module, as a character string.
#'
#' @slot objectDeps   A \code{data.frame} specifying the object dependecies of the module:
#'                    each row specifies a directional dependency, with columns
#'                    \code{objectName}, \code{objectClass}, and \code{objectType}.
#'                    These first two are self-explanatory, and \code{objectType}
#'                    should be one of \code{"input"} or \code{"output"}.
#'                    For objects that are used within the module as both an input and an output,
#'                    each of these should be added as separate entries in the data.frame.
#'
#' @rdname moduleDeps-class
#' @importFrom raster extent
#' @exportClass moduleDeps
#'
#' @seealso dependsList
#'
#' @author Alex Chubaty
#'
setClass("moduleDeps",
         slots=list(name="character", description="character", keywords="character",
                    authors="person", spatialExtent="Extent", timeframe="POSIXlt",
                    translators="list", citation="character", objectDeps="data.frame"),
         prototype=list(name=as.character(NULL), description=as.character(NULL),
                        keywords=as.character(NULL), authors=person(NULL),
                        spatialExtent=extent(as.numeric(c(NA,NA,NA,NA))),
                        timeframe=as.POSIXlt(c(NA, NA)),
                        translators=list(NULL), citation=as.character(NULL),
                        objectDeps=data.frame(objectName=as.character(NULL),
                                              objectClass=as.character(NULL),
                                              objectTYPE=as.character(NULL))),
         validity=function(object) {
           if (length(object@name)!=1) stop("name must be a single character string.")
           if (length(object@description)!=1) stop("description must be a single character string.")
           if (length(object@keywords)<1) stop("keywords must be supplied.")
           if (length(object@authors)<1) stop("authors must be specified.")
           if (length(object@timframe)!=2) stop("timeframe must be specified using two date-times.")
           if (length(object@translators)<1) stop("translators must be specified, or NA.")

           if (length(object@objectDeps$objName)!=1) stop("objName must be a single character string.")
           if (length(object@objectDeps$objClass)<1) stop("objClass must be specified.")
           if ( (lower(object@objectDeps$objectType)!="input") ||
                  (lower(object@objectDeps$objectType)!="output") ) {
             stop("objectType must be one of \'input\' or \'output\'.")
           }
         }
)

#' The \code{dependsList} class
#'
#' List of module object dependecies within a SpaDES simulation.
#'
#' @slot dependencies   List of \code{\link{moduleDeps}} dependency objects.
#'
#' @rdname dependsList-class
#' @exportClass dependsList
#'
#' @author Alex Chubaty
#'
setClass("dependsList",
         slots=list(dependencies="list"),
         prototype=list(dependencies=list(NULL)),
         validity=function(object) {
           # remove empty (NULL) elements
           object@dependencies <- object@dependencies[lapply(object@dependencies, length)>0]

           # ensure list contains only moduleDeps objects
           lapply(object@dependencies, is, class2="moduleDeps")
         }
)



### playing around with `igraph`

#df <- data.frame(module=c("A", "A", "B", "B", "C"),
#                 depends=c("B", "C", "C", "D", "D"))
#gdf <- graph.data.frame(df, directed=TRUE)
#plot(gdf)





















