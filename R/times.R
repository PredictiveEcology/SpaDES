################################################################################
#' SpaDES time units
#'
#' \code{SpaDES} modules commonly use approximate durations that divide with no
#' remainder among themselves.
#' For example, models that simulate based on a "week" timestep, will likely
#' want to fall in lock step with a second module that is a "year" timestep.
#' Since, weeks, months, years don't really have this behaviour because of:
#' leap years, leap seconds, not quite 52 weeks in a year, months that are of
#' different duration, etc.
#' We have generated a set of units that work well together that are based on
#' the astronomical or "Julian" year.
#' In an astronomical year, leap years are added within each year with an extra
#' 1/4 day, (i.e., 1 year == 365.25 days); months are defined as year/12, and
#' weeks as year/52.
#'
#' When these units are not correct, a module developer can create their own
#' time unit using, and create a function to calculate the number of seconds
#' in that unit using the "d" prefix (for duration), following the
#' \code{lubridate} package standard:
#' \code{dfortNight <- function(x) lubridate::new_duration(dday(14))}.
#' Then the module developer can use "fortNight" as the module's time unit.
#'
#' @param x numeric. Number of the desired units
#'
#' @return Number of seconds within each unit
#'
#' @export
#' @docType methods
#' @rdname spadesTime
#'
#' @author Eliot McIntire
#'
setGeneric("dyears", function(x) {
  standardGeneric("dyears")
})

#' @importFrom lubridate new_duration
#' @export
#' @docType methods
#' @rdname spadesTime
setMethod("dyears",
          signature(x="numeric"),
          definition=function(x){
            lubridate::new_duration(x * 60 * 60 * 24 * 365.25)
          })

#' @inheritParams dyears
#' @export
#' @docType methods
#' @rdname spadesTime
setGeneric("dmonths", function(x) {
  standardGeneric("dmonths")
})

#' @importFrom lubridate new_duration
#' @rdname spadesTime
setMethod("dmonths",
          signature(x="numeric"),
          definition=function(x){
            lubridate::new_duration(x * as.numeric(SpaDES::dyears(1))/12)
          })

#' @export
#' @rdname spadesTime
setGeneric("dweeks", function(x) {
  standardGeneric("dweeks")
})

#' @export
#' @importFrom lubridate new_duration
#' @rdname spadesTime
setMethod("dweeks",
          signature(x="numeric"),
          definition=function(x){
            lubridate::new_duration(x * as.numeric(SpaDES::dyears(1))/52)
})

#' @export
#' @rdname spadesTime
dweek <- function(x) {
  dweeks(x)
}

#' @export
#' @rdname spadesTime
dmonth <- function(x) {
  dmonths(x)
}

#' @export
#' @rdname spadesTime
dyear <- function(x) {
  dyears(x)
}

#' @export
#' @rdname spadesTime
#' @importFrom lubridate dseconds
dsecond <- function(x) {
  lubridate::dseconds(x)
}

#' @export
#' @rdname spadesTime
#' @importFrom lubridate ddays
dday <- function(x) {
  lubridate::ddays(x)
}

#' @export
#' @rdname spadesTime
#' @importFrom lubridate dhours
dhour <- function(x) {
  lubridate::dhours(x)
}

#' @export
#' @rdname spadesTime
setGeneric("dNA", function(x) {
  standardGeneric("dNA")
})

#' @export
#' @importFrom lubridate new_duration
#' @rdname spadesTime
setMethod("dNA",
          signature(x="ANY"),
          definition=function(x){
            lubridate::new_duration(0)
})

#' @export
#' @rdname spadesTime
setGeneric("d", function(x) {
  standardGeneric("d")
})

#' @export
#' @importFrom lubridate new_duration
#' @rdname spadesTime
setMethod("d",
          signature(x="ANY"),
          definition=function(x){
            lubridate::new_duration(0)
})

################################################################################
#' Convert time units
#'
#' In addition to using the \code{lubridate} package, some additional functions
#' and to work with times are provided.
#'
#' @param unit Character string indicating time units to be returned as seconds.
#' @export
#' @author Alex Chubaty & Eliot McIntire
#' @docType methods
#' @rdname timeConversion
#'
setGeneric("inSecs", function(unit) {
  standardGeneric("inSecs")
})

#' @export
#' @docType methods
#' @rdname timeConversion
setMethod("inSecs",
          signature=c("character"),
          definition <- function(unit) {

            if(!is.na(unit)) {
              out <- switch(unit,
                            second =  as.numeric(dsecond(1)),
                            seconds =  as.numeric(dsecond(1)),
                            hour = as.numeric(dhour(1)),
                            hours = as.numeric(dhour(1)),
                            day = as.numeric(dday(1)),
                            days = as.numeric(dday(1)),
                            week = as.numeric(dweek(1)),
                            weeks = as.numeric(dweek(1)),
                            month = as.numeric(dmonth(1)),
                            months = as.numeric(dmonth(1)),
                            year = as.numeric(dyear(1)),
                            years = as.numeric(dyear(1)))
            } else {
              out <- 0
            }
            return(out)
})

#' @export
#' @docType methods
#' @rdname timeConversion
setMethod("inSecs",
          signature=c("NULL"),
          definition <- function(unit) {
            return(0)
})
