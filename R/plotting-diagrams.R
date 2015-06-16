if (getRversion() >= "3.1.0") utils::globalVariables(".")

timestep <- function(x) {
  stopifnot(is.character(x))
  return(as.numeric(eval(parse(text=paste0("d", x, "(1)")))))
}

################################################################################
#' ganntStatus
#'
#' Internal function assign the "status" of each event to be passed to
#' \code{\link[DiagrammeR]{mermaid}} to make a Gannt chart representing the
#' events in a completed simulation.
#' 'init' events are set as "done"; 'plot' events as "critical"; and all others
#' as "active".
#'
#' @param sim  A \code{simList} object (typically corresponding to a
#'             completed simulation).
#'
#' @param startDate  A character representation of date in YYYY-MM-DD format.
#'
#' @return A character vector.
#'
#' @include simList.R
#' @docType methods
#' @rdname ganntStatus
#'
#' @author Alex Chubaty
#'
setGeneric("ganntStatus", function(eventType) {
  standardGeneric("ganntStatus")
})

#' @rdname ganntStatus
setMethod("ganntStatus",
          signature(eventType="character"),
          definition=function(eventType) {
            status <- lapply(eventType, function(x) {
              if (x=="init") {
                "done"
              } else if (x=="plot") {
                "critical"
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
#' @include simList.R
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
            dt <- simCompleted(sim)
            ts <- timestep(simTimestepUnit(sim)) / timestep("day")

            out <- lapply(unique(dt$moduleName), function(x) {
              data.frame(task = dt[moduleName==x]$eventType,
                         status = ganttStatus(dt[moduleName==x]$eventType),
                         pos = paste0(x, 1:nrow(dt[moduleName==x])),
                         start = as.Date(dt[moduleName==x]$eventTime * ts, origin=startDate),
                         end = as.Date(dt[moduleName==x]$eventTime * ts + (2*ts), origin=startDate))
            })
            names(out) <- unique(dt$moduleName)
            return(out)
})

################################################################################
#' Simulation event diagram
#'
#' Create a Gantt Chart representing the events in a completed simulation.
#'
#' Simulation time is presented on the x-axis, starting at date 'startDate'.
#' Each module appears in a color-coded row, within which each event for that module
#' is displayed corresponding to the sequence of events for that module.
#'
#' Based on this StackOverflow answer: \url{http://stackoverflow.com/a/29999300/1380598}.
#'
#' @note A red vertical line corresponding to the current date may appear on the
#' figure. This is useful for Gantt Charts generally but can be considered a 'bug' here.
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
#' @include simList.R
#' @importFrom Diagrammer mermaid
#' @importFrom tidyr unite
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
                # mermaid "header", each component separated with "\n" (line break)
                "gantt", "\n",
                "dateFormat  YYYY-MM-DD", "\n",
                "title SPaDES event diagram", "\n",
                # unite the first two columns (task & status) and separate them with ":"
                # then, unite the other columns and separate them with ","
                # this will create the required mermaid "body"
                paste("section ", names(ll), "\n", lapply(ll, function(df) {
                  paste(df %>%
                          tidyr::unite(i, task, status, sep = ":") %>%
                          tidyr::unite(j, i, pos, start, end, sep = ",") %>%
                          .$j,
                        collapse = "\n")
                }), collapse = "\n"), "\n"
              )
            )
})
