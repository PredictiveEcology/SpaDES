options(spades.moduleRepo = "https://github.com/PredictiveEcology/SpaDES-modules")

################################################################################
#' Download a module from a SpaDES module repository
#'
#' THIS IS STILL INCOMPLETE
#'
#' @param name  Character string giving the module name.
#'
#' @param path  Character string giving the location in which to save the downloaded module(s).
#'
#' @param version The module version to download.
#'                (optional. if not specified, the most recent version will be retrieved.)
#'
#' @param url   Base url containing the module subdirectory.
#'              Default is \url{https://github.com/PredictiveEcology/SpaDES-modules/modules}.
#'
#' @author Alex Chubaty
#'
#' @export
#' @rdname downloadModules-method
#'
setGeneric("downloadModules", function(name, path, version, url) {
  standardGeneric("zipModule")
})

#' @rdname downloadModules-method
setMethod("downloadModules",
          signature=c(name="character", path="character", version="character", url="character"),
          definition = function(name, url, path, version) {
            path <- checkPath(path, create=TRUE)

            # try to download a zip
            zip <- paste0(url, "/", name, "_", version, ".zip")
            dl <- lapply(zip, download.file, destfile=path)
            return(invisible(all(dl)))
})

setMethod("downloadModules",
          signature=c(name="character", path="character", version="character", url="missing"),
          definition = function(name, path, version) {
            downloadModules(name, path, version, url=getOption("spades.moduleRepo"))
})
