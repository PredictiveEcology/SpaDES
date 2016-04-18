if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(".", "moduleName"))
}

################################################################################
#' ganttStatus
#'
#' Internal function assign the "status" of each event to be passed to
#' \code{\link[DiagrammeR]{mermaid}} to make a Gantt chart representing the
#' events in a completed simulation.
#' 'init' events are set as "done"; 'plot' events as "critical"; and all others
#' as "active".
#'
#' @param eventType Character vector of events.
#'
#' @return A character vector.
#'
#' @include simList-accessors.R
#' @docType methods
#' @rdname ganttStatus
#'
#' @author Alex Chubaty
#'
setGeneric("ganttStatus", function(eventType) {
  standardGeneric("ganttStatus")
})

#' @rdname ganttStatus
setMethod("ganttStatus",
          signature(eventType = "character"),
          definition = function(eventType) {
            status <- lapply(eventType, function(x) {
              if (x == "init") {
                "done"
              } else if (x == "plot") {
                "crit"
              } else {
                "active"
              }
            })
            return(unlist(status))
})

################################################################################
#' sim2gantt
#'
#' Internal function to convert the completed events list of a \code{simList}
#' object to a list of \code{data.frame}s suitable to pass to a call to
#' \code{\link[DiagrammeR]{mermaid}} to make a Gannt chart representing the
#' events in a completed simulation.
#'
#' @param sim  A \code{simList} object (typically corresponding to a
#'             completed simulation).
#'
#' @param n    The number of most recently completed events to plot.
#'
#' @param startDate  A character representation of date in YYYY-MM-DD format.
#'
#' @param width  Numeric. Passed to determine scale of vertical bars.
#'
#' @return A list of data.frames
#'
#' @include simList-accessors.R
# @importFrom utils tail
#' @docType methods
#' @rdname sim2gantt
#'
#' @author Alex Chubaty
#'
# igraph exports %>% from magrittr
setGeneric(".sim2gantt", function(sim, n, startDate, width) {
  standardGeneric(".sim2gantt")
})

#' @rdname sim2gantt
setMethod(
  ".sim2gantt",
  signature(sim = "simList", n = "numeric", startDate = "character", width = "numeric"),
  definition = function(sim, n, startDate, width) {
    DT <- tail(completed(sim), n)
    modules <- unique(DT$moduleName)
    width <- 4500 / as.numeric(width) # fixed at 3 days

    # simulation timestep in 'days'
    ts <- timeunit(sim) %>%
      inSeconds(envir = envir(sim)) %>%
      convertTimeunit("day", envir = envir(sim)) %>%
      as.numeric()

    out <- lapply(modules, function(x) {
      data.frame(
        task = DT[moduleName == x]$eventType,
        status = ganttStatus(DT[moduleName == x]$eventType),
        pos = paste0(x, 1:nrow(DT[moduleName == x])),
        start = as.Date(
          DT[moduleName == x]$eventTime * ts, origin = startDate
        ),
        end = as.Date(
          DT[moduleName == x]$eventTime * ts + width, origin = startDate
        )
      )
    })
    names(out) <- modules
    return(out)
})

################################################################################
#' Simulation event diagram
#'
#' Create a Gantt Chart representing the events in a completed simulation.
#' This event diagram is constructed using the completed event list
#' To change the number of events shown, provide an \code{n} argument.
#'
#' Simulation time is presented on the x-axis, starting at date 'startDate'.
#' Each module appears in a color-coded row, within which each event for that
#' module is displayed corresponding to the sequence of events for that module.
#' Note that only the start time of the event is meaningful is these figures:
#' the width of the bar associated with a particular module's event DOES NOT
#' correspond to an event's "duration".
#'
#' Based on this StackOverflow answer: \url{http://stackoverflow.com/a/29999300/1380598}.
#'
#' @note
#' A red vertical line corresponding to the current date may appear on the figure.
#' This is useful for Gantt Charts generally but can be considered a 'bug' here.
#'
#' @param sim  A \code{simList} object (typically corresponding to a
#'             completed simulation).
#'
#' @param n    The number of most recently completed events to plot.
#'
#' @param startDate  A character representation of date in YYYY-MM-DD format.
#'
#' @param ...  Additional arguments passed to \code{mermaid}.
#'             Useful for specifying \code{height} and \code{width}.
#'
#' @return Plots an event diagram as Gantt Chart, invisibly returning a \code{mermaid} object.
#'
#' @seealso \code{\link{mermaid}}.
#'
#' @include simList-accessors.R
#' @importFrom DiagrammeR mermaid
#' @export
#' @docType methods
#' @rdname eventDiagram
#'
#' @author Alex Chubaty
#'
setGeneric("eventDiagram", function(sim, n, startDate, ...) {
  standardGeneric("eventDiagram")
})

#' @export
#' @rdname eventDiagram
setMethod(
  "eventDiagram",
  signature(sim = "simList", n = "numeric", startDate = "character"),
  definition = function(sim, n, startDate, ...) {
    # get automatic scaling of vertical bars in Gantt chart
    dots <- list(...)
    dots$width <- if (any(grepl(pattern = "width", names(dots)))) {
      as.numeric(dots$width)
    } else {
      1000
    }
    ll <- .sim2gantt(sim, n, startDate, dots$width)

    # remove progress bar events
    ll <- ll[names(ll) != "progress"]

    if (length(ll)) {
      # estimate the height of the diagram
      dots$height <- if (any(grepl(pattern = "height", names(dots)))) {
        as.numeric(dots$height)
      } else {
        sapply(ll, NROW) %>% sum %>% `*`(., 26L)
      }

      diagram <- paste0(
        # mermaid "header"
        "gantt", "\n",
        "dateFormat  YYYY-MM-DD", "\n",
        "title SpaDES event diagram", "\n",

        # mermaid "body"
        paste("section ", names(ll), "\n", lapply(ll, function(df) {
          paste0(df$task, ":", df$status, ",", df$pos, ",",
                 df$start, ",", df$end, collapse = "\n")
        }), collapse = "\n"), "\n"
      )
      do.call(mermaid, args = append(diagram, dots))
    } else {
      stop("Unable to create eventDiagram for a simulation that hasn't been run.\n",
           "Run your simulation using `mySim <- spades(mySim)` and try again.")
    }
})

#' @export
#' @rdname eventDiagram
setMethod(
  "eventDiagram",
  signature(sim = "simList", n = "missing", startDate = "character"),
  definition = function(sim, startDate, ...) {
    eventDiagram(sim = sim, n = NROW(completed(sim)), startDate = startDate, ...)
})

################################################################################
#' Simulation object dependency diagram
#'
#' Create a sequence diagram illustrating the data object dependencies of a
#' simulation. Offers a more detailed view of specific objects than does
#' plotting the \code{depsEdgeList} directly with \code{\link{moduleDiagram}}.
#'
#' @param sim  A \code{simList} object (typically corresponding to a
#'             completed simulation).
#'
#' @param ...  Additional arguments passed to \code{mermaid}.
#'             Useful for specifying \code{height} and \code{width}.
#'
#' @return Plots a sequence diagram, invisibly returning a \code{mermaid} object.
#'
#' @seealso \code{\link{mermaid}}.
#'
#' @include simList-accessors.R
#' @importFrom DiagrammeR mermaid
#' @export
#' @docType methods
#' @rdname objectDiagram
#'
#' @author Alex Chubaty
#'
setGeneric("objectDiagram", function(sim, ...) {
  standardGeneric("objectDiagram")
})

#' @export
#' @rdname objectDiagram
setMethod(
  "objectDiagram",
  signature(sim = "simList"),
  definition = function(sim, ...) {
    dt <- depsEdgeList(sim, FALSE)
    DiagrammeR::mermaid(...,
      paste0(
        # mermaid "header"
        "sequenceDiagram", "\n",

        # mermaid "body"
        paste(dt$from, "->>", dt$to, ":", dt$objName, collapse = "\n"),
        "\n"
      )
    )
})

################################################################################
#' Simulation module dependency diagram
#'
#' Create a network diagram illustrating the simplified module dependencies of a
#' simulation. Offers a less detailed view of specific objects than does
#' plotting the \code{depsEdgeList} directly with \code{\link{objectDiagram}}.
#'
#' @param sim  A \code{simList} object (typically corresponding to a
#'             completed simulation).
#'
#' @param type  Character string, either \code{"rgl"} for \code{igraph::rglplot}
#' or \code{"tk"} for \code{igraph::tkplot}. Default missing, which uses regular
#' \code{plot}.
#'
#' @param ...  Additional arguments passed to plotting function specfied by \code{type}.
#'
#' @return Plots module dependency diagram.
#'
#' @seealso \code{\link{igraph}}.
#'
#' @include simList-accessors.R
#' @export
#' @docType methods
#' @rdname moduleDiagram
#'
#' @author Alex Chubaty
# igraph is being imported in spades-package.R
setGeneric("moduleDiagram", function(sim, type, ...) {
  standardGeneric("moduleDiagram")
})

#' @export
#' @rdname moduleDiagram
setMethod("moduleDiagram",
          signature = c(sim = "simList", type = "character"),
          definition = function(sim, type, ...) {
            if (type == "rgl") {
              rglplot(depsGraph(sim, TRUE), ...)
            } else if (type == "tk") {
              tkplot(depsGraph(sim, TRUE), ...)
            } else {
              moduleDiagram(sim)
            }
})

#' @export
#' @rdname moduleDiagram
setMethod("moduleDiagram",
          signature = c(sim = "simList", type = "missing"),
          definition = function(sim, type, ...) {
              plot(depsGraph(sim, TRUE), ...)
})
