################################################################################
#' Event priority
#'
#' Preset event priorities: 1 = first (highest); 5 = normal; 10 = last (lowest).
#'
#' @return A numeric.
#'
#' @export
#' @docType methods
#' @name priority
#' @rdname priority
#'
#' @author Alex Chubaty
#'
.first <- function() {
  .highest()
}

#' @rdname priority
#' @export
.highest <- function() {
  return(1)
}

#' @rdname priority
#' @export
.last <- function() {
  .lowest()
}

#' @rdname priority
#' @export
.lowest <- function() {
  return(10)
}

#' @rdname priority
#' @export
.normal <- function() {
  5
}
