#' Classes defined in SpaDES
#'
#' These S4 classes are defined within \code{SpaDES}. "dot" classes are not exported and
#' are therefore intended for internal use only.
#'
#' @section Simulation classes:
#' \tabular{ll}{
#'   \code{\link{simList}} \tab The 'simList' class\cr
#'   \code{\link{.moduleDeps}} \tab Descriptor object for specifying \code{SpaDES} module dependencies\cr
#'   \code{\link{.simDeps}} \tab Defines all simulation dependencies for all modules within a \code{SpaDES} simulation\cr
#'   --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
#' }
#'
#' @section Plotting classes - used within \code{Plot}:
#'
#' \tabular{ll}{
#'   New classes\tab \cr
#'   \code{\link{.spadesPlot}} \tab Main class for \code{Plot} - contains \code{.spadesGrob} and \code{.arrangement} objects\cr
#'   \code{\link{.spadesGrob}} \tab GRaphical OBject used by SpaDES - smallest unit\cr
#'   \code{\link{.arrangement}} \tab The layout or "arrangement" of plot objects\cr
#'   --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
#' }
#'
#' \tabular{ll}{
#'   Unions of existing classes\tab \cr
#'   \code{\link{.spadesPlottables}} \tab The union of all object classes Plot can accept\cr
#'   \code{\link{.spadesPlotObjects}} \tab The union of spatialObjects and several others\cr
#'   \code{\link{spatialObjects}} \tab The union of several spatial classes\cr
#'   --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
#' }
#'
#' @seealso \code{\link{Plot}}, \code{\link{simInit}}
#' @name spadesClasses
#' @rdname spades-classes
#' @author Eliot McIntire and Alex Chubaty
NULL
