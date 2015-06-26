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
          signature(eventType="character"),
          definition=function(eventType) {
            status <- lapply(eventType, function(x) {
              if (x=="init") {
                "done"
              } else if (x=="plot") {
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
#' @param startDate  A character representation of date in YYYY-MM-DD format.
#'
#' @return A list of data.frames
#'
#' @include simList-accessors.R
#' @docType methods
#' @rdname sim2gantt
#'
#' @author Alex Chubaty
#'
setGeneric("sim2gantt", function(sim, startDate) {
  standardGeneric("sim2gantt")
})

#' @rdname sim2gantt
setMethod("sim2gantt",
          signature(sim="simList", startDate="character"),
          definition=function(sim, startDate) {
            dt <- completed(sim)
            ts <- convertTimeunit(inSeconds(timeunit(sim)), "day") # simulation timestep

            mts <- lapply(timeunits(sim), function(x) {
              t <- convertTimeunit(inSeconds(x), unit="day") # module timesteps
              if ( is.null(t) || (t==0) ) {
                t <- 1
              }
              return(as.numeric(t))
            })

            modules <- unique(dt$moduleName)
            out <- lapply(modules, function(x) {
              data.frame(task = dt[moduleName==x]$eventType,
                         status = ganttStatus(dt[moduleName==x]$eventType),
                         pos = paste0(x, 1:nrow(dt[moduleName==x])),
                         start = as.Date(dt[moduleName==x]$eventTime * ts, origin=startDate),
                         end = as.Date(dt[moduleName==x]$eventTime * ts + mts[[x]],
                                       origin=startDate))
            })
            names(out) <- modules
            return(out)
})

################################################################################
#' Simulation event diagram
#'
#' Create a Gantt Chart representing the events in a completed simulation.
#' This event diagram is constructed using the completed event list, which by
#' default only stores the 10 most recently completed events (unless
#' \code{spades(debug=TRUE)} is used, in which case all events are retained).
#' To change the number of events stored, users may override this option using
#' \code{options(spades.nCompleted = value)}.
#'
#' Simulation time is presented on the x-axis, starting at date 'startDate'.
#' Each module appears in a color-coded row, within which each event for that
#' module is displayed corresponding to the sequence of events for that module.
#' Note that only the start time of the event is meaningful is these figures:
#' the width of the bar associated with a particular module's event corresponds
#' to the module's timestepUnit, not the event's "duration".
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
#' @param startDate  A character representation of date in YYYY-MM-DD format.
#'
#' @return Plots an event diagram as Gantt Chart.
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
setGeneric("eventDiagram", function(sim, startDate) {
  standardGeneric("eventDiagram")
})

#' @export
#' @rdname eventDiagram
setMethod("eventDiagram",
          signature(sim="simList", startDate="character"),
          definition=function(sim, startDate) {
            ll <- sim2gantt(sim, startDate)

            DiagrammeR::mermaid(
              paste0(
                # mermaid "header"
                "gantt", "\n",
                "dateFormat  YYYY-MM-DD", "\n",
                "title SPaDES event diagram", "\n",

                # mermaid "body"
                paste("section ", names(ll), "\n", lapply(ll, function(df) {
                  paste0(df$task, ":", df$status, ",",
                         df$pos, ",", df$start, ",", df$end,
                         collapse = "\n")
                }), collapse = "\n"), "\n"
              )
            )
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
#' @return Plots a sequence diagram.
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
setGeneric("objectDiagram", function(sim) {
  standardGeneric("objectDiagram")
})

#' @export
#' @rdname objectDiagram
setMethod("objectDiagram",
          signature(sim="simList"),
          definition=function(sim) {
            dt <- depsEdgeList(sim, FALSE)
            DiagrammeR::mermaid(
              paste0(
                # mermaid "header"
                "sequenceDiagram", "\n",

                # mermaid "body"
                paste(dt$from, "->>", dt$to, ":", dt$objName, collapse="\n"),
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
#' @return Plots module dependency diagram.
#'
#' @seealso \code{\link{igraph}}.
#'
#' @include simList-accessors.R
#' @import igraph
#' @export
#' @docType methods
#' @rdname moduleDiagram
#'
#' @author Alex Chubaty
#'
setGeneric("moduleDiagram", function(sim) {
  standardGeneric("moduleDiagram")
})

#' @export
#' @rdname moduleDiagram
setMethod("moduleDiagram",
          signature(sim="simList"),
          definition=function(sim) {
            plot(depsGraph(sim, TRUE))
})
