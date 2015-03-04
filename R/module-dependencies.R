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
#' @slot timestep       Describes the length of time corresponding to 1.0 simulation time units.
#'                      Possible values: \code{"second"}, \code{"minute"}, \code{"hour"}, \code{"day"},
#'                      \code{"week"}, \code{"month"}, \code{"year"}, or \code{NA} (default).
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
                    timestep="character", translators="list", citation="list", reqdPkgs="list",
                    inputObjects="data.frame", outputObjects="data.frame"),
         prototype=list(name=character(), description=character(),
                        keywords=character(), authors=person(),
                        spatialExtent=extent(rep(NA_real_, 4L)),
                        timeframe=as.POSIXlt(c(NA, NA)), timestep=NA_character_,
                        translators=list(), citation=list(), reqdPkgs=list(),
                        inputObjects=data.frame(name=character(), class=character(), stringsAsFactors=FALSE),
                        outputObjects=data.frame(name=character(), class=character(), stringsAsFactors=FALSE)),
         validity=function(object) {
           if (length(object@name)!=1L) stop("name must be a single character string.")
           if (length(object@description)!=1L) stop("description must be a single character string.")
           if (length(object@keywords)<1L) stop("keywords must be supplied.")
           if (length(object@authors)<1L) stop("authors must be specified.")
           if (length(object@timeframe)!=2L) stop("timeframe must be specified using two date-times.")
           if (length(object@timestep)<1L) stop("timestep must be specified.")
           if (!any(unlist(lapply(object@reqdPkgs, is.character)))) stop("reqdPkgs must be specified as a list of package names.")

           object@inputObjects <- as.data.frame(object@inputObjects, stringsAsFactors=FALSE)
           object@outputObjects <- as.data.frame(object@outputObjects, stringsAsFactors=FALSE)
           if (length(object@inputObjects)<1L) stop("input object name and class must be specified, or NA.")
           if (length(object@outputObjects)<1L) stop("output object name and class must be specified, or NA.")
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
#' @importFrom methods is
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
           if (!all(unlist(lapply(object@dependencies, is, class2="moduleDeps")))) stop("invalid type: non-moduleDeps object")
         }
)

################################################################################
#' Define a new module.
#'
#' Specify a new module's metadata as well as object and package dependecies.
#' Packages are loaded during this call.
#'
#' @param x   A named list containing the parameters used to construct a new
#'            \code{moduleDeps} object.
#'
#' @inheritParams moduleDeps-class
#'
#' @return This is a closure that serves as a wrapper for adding simulation
#'          dependencies via \code{simDepends(sim, add=TRUE)<-}.
#'
#' @export
#' @docType methods
#' @rdname defineModule-method
#'
#' @author Alex Chubaty
#'
#' @examples
#' \dontrun{
#'   moduleInfo <- list(...)
#'   defineModule(moduleInfo)
#' }
#'
setGeneric("defineModule", function(x) {
  standardGeneric("defineModule")
})

#' @rdname defineModule-method
#'
setMethod("defineModule",
          signature(x="list"),
          definition=function(x) {
            function(sim) {
              loadPackages(x$reqdPkgs)
              m <- do.call(new, c("moduleDeps", x))
              simDepends(sim) <- addSimDep(sim, m))
              return(sim)
            }
})

#########

# igraph::topological.sort should sort deps, which can be used to get a module load order
#

# getSpaDES(".simDeps")
