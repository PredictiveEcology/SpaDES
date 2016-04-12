################################################################################
#' Blank (template) event list
#'
#' Internal function called from \code{spades}, returning an empty event list.
#'
#' Event lists are sorted (keyed) first by time, second by priority.
#' Each event is represented by a \code{\link{data.table}} row consisting of:
#' \tabular{ll}{
#'   \code{eventTime} \tab The time the event is to occur.\cr
#'   \code{moduleName} \tab The module from which the event is taken.\cr
#'   \code{eventType} \tab A character string for the programmer-defined event type.\cr
#'   \code{eventPriority} \tab The priority given to the event. \cr
#' }
#'
#' @param eventTime      The time the event is to occur.
#' @param moduleName     The module from which the event is taken.
#' @param eventType      A character string for the programmer-defined event type.
#' @param eventPriority  The priority given to the event.
#'
#' @return Returns an empty event list.
#'
#' @importFrom data.table data.table
#' @keywords internal
#' @docType methods
#' @rdname emptyEventList
#'
#' @author Alex Chubaty
setGeneric(".emptyEventList", function(eventTime, moduleName, eventType, eventPriority) {
  standardGeneric(".emptyEventList")
})

#' @rdname emptyEventList
setMethod(
  ".emptyEventList",
  signature(eventTime = "numeric", moduleName = "character",
            eventType = "character", eventPriority = "numeric"),
  definition = function(eventTime, moduleName, eventType, eventPriority) {
    data.table(eventTime = eventTime, moduleName = moduleName,
               eventType = eventType, eventPriority = eventPriority)
    # don't set key because it is set later when used
})

#' @rdname emptyEventList
setMethod(
  ".emptyEventList",
  signature(eventTime = "missing", moduleName = "missing",
            eventType = "missing", eventPriority = "missing"),
  definition = function() {
    data.table(eventTime = numeric(0L), moduleName = character(0L),
               eventType = character(0L), eventPriority = numeric(0L))
    # don't set key because it is set later when used
})

#' @rdname emptyEventList
.emptyEventListCols <- colnames(.emptyEventList())

#' @rdname emptyEventList
.emptyEventListObj <- .emptyEventList()

#' @rdname emptyEventList
.emptyEventListNA <- .emptyEventList(NA_integer_, NA_character_, NA_character_, NA_integer_)

################################################################################
#' Default (empty) metadata
#'
#' Internal use only.
#' Default values to use for metadata elements when not otherwise supplied.
#'
#' @param x  Not used. Should be missing.
#'
#' @importFrom raster extent
#' @keywords internal
#' @include simList-class.R
#' @docType methods
#' @rdname emptyMetadata
#' @author Alex Chubaty
#'
setGeneric(".emptyMetadata", function(x) {
  standardGeneric(".emptyMetadata")
})

#' @rdname emptyMetadata
setMethod(
  ".emptyMetadata",
  signature(x = "missing"),
  definition = function() {
    out <- list(
      name = character(0),
      description = character(0),
      keywords = character(0),
      childModules = character(0),
      authors = person("unknown"),
      version = numeric_version(NULL),
      spatialExtent = raster::extent(rep(NA_real_, 4)),
      timeframe = as.POSIXlt(c(NA, NA)),
      timeunit = NA_character_,
      citation = list(),
      documentation = list(),
      reqdPkgs = list(),
      parameters = defineParameter(),
      inputObjects = .inputObjects(),
      outputObjects = .outputObjects()
    )
    return(out)
})

