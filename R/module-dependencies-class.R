# register the S3 `numeric_version` class for use with S4 methods.
setOldClass("numeric_version")
selectMethod("show", "numeric_version")

# register the S3 `person` class for use with S4 methods.
setClass(
  "person4",
  slots = list(given = "character", family = "character", middle = "character",
               email = "character", role = "character", comment = "character",
               first = "character", last = "character")
)
setOldClass("person", S4Class = "person4")
selectMethod("show", "person")
removeClass("person4")

################################################################################
#' Create an empty (template) inputObjects and outputObjects data.frames
#'
#' Internal function.
#'
#' @param x Not used. Should be missing.
#'
#' @return An empty inputObjects or outputObjects data.frame.
#'
#' @docType methods
#' @keywords internal
#' @rdname inputObjects
#'
#' @author Alex Chubaty
#'
setGeneric(".inputObjects", function(x) {
  standardGeneric(".inputObjects")
})

#' @rdname inputObjects
setMethod(".inputObjects",
          signature(x = "missing"),
          definition = function() {
            in.df <- data.frame(
              objectName = character(0), objectClass = character(0),
              desc = character(0), sourceURL = character(0),
              stringsAsFactors = FALSE
            )
            return(in.df)
})

#' @rdname inputObjects
setGeneric(".outputObjects", function(x) {
  standardGeneric(".outputObjects")
})

#' @rdname inputObjects
setMethod(".outputObjects",
          signature(x = "missing"),
          definition = function() {
            out.df <- data.frame(
              objectName = character(0), objectClass = character(0),
              desc = character(0), stringsAsFactors = FALSE
            )
            return(out.df)
})

################################################################################
#' The \code{.moduleDeps} class
#'
#' Descriptor object for specifying SpaDES module dependencies.
#'
#' @slot name           Name of the module as a character string.
#'
#' @slot description    Description of the module as a character string.
#'
#' @slot keywords       Character vector containing a module's keywords.
#'
#' @slot authors        The author(s) of the module as a \code{\link{person}} object.
#'
#' @slot childModules   A character vector of child module names.
#'                      Modules listed here will be loaded with this module.
#'
#' @slot version        The module version as a \code{numeric_version}.
#'                      Semantic versioning is assumed \url{http://semver.org/}.
#'
#' @slot spatialExtent  Specifies the module's spatial extent as an
#'                      \code{\link{Extent}} object. Default is \code{NA}.
#'
#' @slot timeframe      Specifies the valid timeframe for which the module was
#'                      designed to simulate. Must be a \code{\link{POSIXt}}
#'                      object of length 2, specifying the start and end times
#'                      (e.g., \code{as.POSIXlt(c("1990-01-01 00:00:00", "2100-12-31 11:59:59"))}).
#'                      Can be specified as \code{NA} using \code{as.POSIXlt(c(NA, NA))}.
#'
#' @slot timeunit       Describes the time (in seconds) corresponding to 1.0
#'                      simulation time units. Default is \code{NA}.
#'
#' @slot citation       A list of citations for the module, each as character strings.
#'                      Alternatively, list of filenames of \code{.bib} or similar files.
#'                      Defaults to \code{NA_character_}.
#'
#' @slot documentation  List of filenames referring to module documentation sources.
#'
#' @slot reqdPkgs       Character vector of R package names to be loaded.
#'                      Defaults to \code{NA_character_}.
#'
#' @slot parameters     A \code{data.frame} specifying the object dependencies
#'                      of the module, with columns \code{paramName},
#'                      \code{paramClass}, and \code{default}, whose values are
#'                      of type \code{character}, \code{character}, and
#'                      \code{ANY}, respectively. Default values may be
#'                      overridden by the user by passing a list of parameters
#'                      to \code{\link{simInit}}.
#'
#' @slot inputObjects   A \code{data.frame} specifying the object dependencies of
#'                      the module, with columns \code{objectName},
#'                      \code{objectClass}, and \code{other}.
#'                      For objects that are used within the module as both an
#'                      input and an output, add the object to each of these
#'                      \code{data.frame}s.
#'
#' @slot outputObjects  A \code{data.frame} specifying the objects output by the
#'                      module, following the format of \code{inputObjects}.
#'
#' @aliases .moduleDeps
#' @rdname moduleDeps-class
#' @importFrom raster extent
#' @keywords internal
#'
#' @seealso \code{.simDeps}, \code{\link{spadesClasses}}
#'
#' @author Alex Chubaty
#'
setClass(
  ".moduleDeps",
  slots = list(
    name = "character", description = "character", keywords = "character",
    childModules = "character", authors = "person", version = "numeric_version",
    spatialExtent = "Extent", timeframe = "POSIXt", timeunit = "ANY",
    citation = "list", documentation = "list", reqdPkgs = "list",
    parameters = "data.frame", inputObjects = "data.frame", outputObjects = "data.frame"
  ),
  prototype = list(
    name = character(0), description = character(0), keywords = character(0),
    childModules = character(0), authors = person(), version = numeric_version("0.0.0"),
    spatialExtent = extent(rep(NA_real_, 4L)), timeframe = as.POSIXlt(c(NA, NA)),
    timeunit = NA_real_, citation = list(), documentation = list(), reqdPkgs = list(),
    parameters = data.frame(
      paramName = character(0), paramClass = character(0),
      default = I(list()), min = I(list()), max = I(list()),
      paramDesc = character(0), stringsAsFactors = FALSE
    ),
    inputObjects = .inputObjects(),
    outputObjects = .outputObjects()
  ),
  validity = function(object) {
    if (length(object@name) != 1L) stop("name must be a single character string.")
    if (length(object@description) != 1L) stop("description must be a single character string.")
    if (length(object@keywords) < 1L) stop("keywords must be supplied.")
    if (length(object@authors) < 1L) stop("authors must be specified.")
    if (length(object@timeframe) != 2L) stop("timeframe must be specified using two date-times.")
    if (length(object@timeunit) < 1L) stop("timeunit must be specified.")
    if (length(object@reqdPkgs)) {
      if (!any(unlist(lapply(object@reqdPkgs, is.character)))) {
        stop("reqdPkgs must be specified as a list of package names.")
      }
    }

    # data.frame checking
    if (length(object@inputObjects) < 1L) stop("input object name and class must be specified, or NA.")
    if (length(object@outputObjects) < 1L) stop("output object name and class must be specified, or NA.")
    if ( !all(names(.inputObjects()) %in% colnames(object@inputObjects)) ) {
      stop(paste("input object data.frame must use colnames", paste(collapse = ", ", colnames(.inputObjects()) )))
    }
    # if ( !("sourceURL" %in% colnames(object@inputObjects)) ) {
    #   warning("input object data.frame should use colnames sourceURL.")
    # }
    if ( !all(colnames(.outputObjects()) %in% colnames(object@outputObjects)) ) {
     stop(paste("output object data.frame must use colnames", paste(collapse = ", ", colnames(.outputObjects()))))
    }
    # try coercing to character because if data.frame was created without specifying
    # `stringsAsFactors=FALSE`, or used `NA` (logical) there will be problems...
    if (!is.character(object@inputObjects$objectName)) {
     object@inputObjects$objectName <- as.character(object@inputObjects$objectName)
    }
    if (!is.character(object@inputObjects$objectClass)) {
     object@inputObjects$objectClass <- as.character(object@inputObjects$objectClass)
    }
    if (!is.character(object@inputObjects$sourceURL)) {
      object@inputObjects$sourceURL <- as.character(object@inputObjects$sourceURL)
    }
    if (!is.character(object@inputObjects$other)) {
     object@inputObjects$desc <- as.character(object@inputObjects$desc)
    }
    if (!is.character(object@outputObjects$objectName)) {
     object@outputObjects$objectName <- as.character(object@outputObjects$objectName)
    }
    if (!is.character(object@outputObjects$objectClass)) {
     object@outputObjects$objectClass <- as.character(object@outputObjects$objectClass)
    }
    if (!is.character(object@outputObjects$desc)) {
     object@outputObjects$desc <- as.character(object@outputObjects$desc)
    }
})

#' The \code{.simDeps} class
#'
#' Defines all simulation dependencies for all modules within a SpaDES simulation.
#'
#' @slot dependencies   List of \code{\link{.moduleDeps}} dependency objects.
#'
#' @seealso \code{\link{.moduleDeps}}, \code{\link{spadesClasses}}
#'
#' @aliases .simDeps
#' @keywords internal
#' @rdname simDeps-class
#'
#' @author Alex Chubaty
#'
setClass(
  ".simDeps",
  slots = list(dependencies = "list"),
  prototype = list(dependencies = list(NULL)),
  validity = function(object) {
    # remove empty (NULL) elements
    object@dependencies <- object@dependencies[lapply(object@dependencies, length) > 0]

    # ensure list contains only .moduleDeps objects
    if (!all(unlist(lapply(object@dependencies, is, class2 = ".moduleDeps")))) {
      stop("invalid type: not a .moduleDeps object")
    }
})
