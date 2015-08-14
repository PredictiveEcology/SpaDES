################################################################################
#' Parse and extract module metadata
#'
#' This function will return the metadata from either a module file or a
#' \code{simList} object. If a \code{simList} object is provided, then the
#' \code{path} is ignored.
#'
#' @param module Character vector. The names of modules from which metadata should
#' be returned.
#'
#' @param path   Character string specifying the file path to modules directory.
#'               Default is the current working directory.
#'
#' @param simList a \code{simList} object. If this is provided, module metadata
#' will be returned from modules contained within the \code{simList}, not the \code{path}
#'
#' @return A list of module metadata, matching the structure in
#'         \code{\link{defineModule}}.
#'
#' @export
#' @docType methods
#' @rdname moduleMetadata
#'
#' @seealso \code{\link{defineModule}}
#'
#' @author Alex Chubaty
#'
#' @examples
#'   path <- system.file(package="SpaDES", "sampleModules")
#'   sampleModules <- dir(path)
#'   (x <- moduleMetadata(sampleModules, path))
#'
setGeneric("moduleMetadata", function(module, path, simList) {
  standardGeneric("moduleMetadata")
})

#' @export
#' @rdname moduleMetadata
setMethod(
  "moduleMetadata",
  signature = c(module="character", path="character", simList="missing"),
  definition = function(module, path) {

    metadata <- lapply(module, function(mod) {
      filename <- paste(path, "/", mod, "/", mod, ".R", sep="")
      stopifnot(file.exists(filename))

      parsedFile <- parse(filename)
      defineModuleItem <- grepl(pattern="defineModule", parsedFile)

      # pull out the list portion from "defineModule"
      x <- parsedFile[defineModuleItem] %>%
        as.character %>%
        gsub("[[:space:]]*\\n[[:space:]]*", " ", .) %>%
        sub("^defineModule[[:space:]]*\\([[:space:]]*", "", .) %>%
        sub("^sim[[:space:]]*,[[:space:]]*", "", .) %>%
        sub("\\)$", "", .) %>%
        gsub("[[:space:]]*=[[:space:]]*", " = ", .)

      # ensure variables in params are kept as strings
      x <- gsub("(globals\\(sim\\)\\$[^\\),]*)", "\"\\1\"", x, perl=TRUE) %>%
        gsub("(params\\(sim\\)\\$[^,]*)", "\"\\1\"", ., perl=TRUE)

      # check input types
      x <- gsub("extent\\(rep\\(NA, 4\\)\\)", "extent\\(rep\\(NA_real_, 4\\)\\)", x) %>%
        gsub("extent\\(c\\(NA, NA, NA, NA\\)\\)", "extent\\(rep\\(NA_real_, 4\\)\\)", .)

      # store metadata as list
      metadata <- eval(parse(text = x))
      return(metadata)
    })

    names(metadata) <- sapply(metadata, function(x) x$name)
    return(metadata)
})

#' @export
#' @rdname moduleMetadata
setMethod(
  "moduleMetadata",
  signature = c(module="character", path="missing", simList="missing"),
  definition = function(module) {
    moduleMetadata(module, getwd())
})

#' @export
#' @rdname moduleMetadata
setMethod(
  "moduleMetadata",
  signature = c(module="character", path="missing", simList="simList"),
  definition = function(module, simList) {
    metadata <- match(module, sapply(simList@depends@dependencies, function(x) x@name)) %>%
      lapply(., function(mod) {
        metadata <- slotNames(simList@depends@dependencies[[mod]]) %>%
          lapply(., function(x) slot(simList@depends@dependencies[[mod]],
                                 x))
        names(metadata) <- slotNames(simList@depends@dependencies[[mod]])
        metadata
      })

    names(metadata) <- sapply(metadata, function(x) x$name)
    return(metadata)
  })

#' @export
#' @rdname moduleMetadata
setMethod(
  "moduleMetadata",
  signature = c(module="missing", path="missing", simList="simList"),
  definition = function(simList) {
    modNames <- sapply(1:length(sapply(simList@depends@dependencies, function(x) x@name)),
           function(mod) {
      simList@depends@dependencies[[mod]]@name
    })
    moduleMetadata(modNames, simList=simList)
  })

#' rdname moduleMetadata
#' @export
moduleKeywords <- function(moduleMetadata) {
  keywords <- lapply(moduleMetadata, function(x) x$keywords)
  names(keywords) <- sapply(moduleMetadata, function(x) x$name)
  return(keywords)
}

#' rdname moduleMetadata
#' @export
moduleParameters <- function(moduleMetadata) {
  keywords <- lapply(moduleMetadata, function(x) x$parameters)
  names(keywords) <- sapply(moduleMetadata, function(x) x$name)
  return(keywords)
}

