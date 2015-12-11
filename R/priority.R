################################################################################
#' Event priority
#'
#' Preset envent priorities: 1 = first (highest); 5 = normal; 10 = last (lowest).
#'
#'
#'
#' @return A numeric.
#'
#' @export
#' @docType methods
#' @aliases priority
#' @rdname priority
#'
#' @author Alex Chubaty
#'
.first <- function() {
  .highest()
}

#' @rdname priority
.highest <- function() {
  return(1)
}

#' @rdname priority
.last <- function() {
  .lowest()
}

#' @rdname priority
.lowest <- function() {
  return(10)
}

#' @rdname priority
.normal <- function() {
  5
}
