################################################################################
#' Parse and extract module metadata
#'
#' @param module Character string. Your module's name.
#'
#' @param path   Character string specifying the file path to modules directory.
#'               Default is the current working directory.
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
#'   path <- system.file(package = "SpaDES", "sampleModules")
#'   sampleModules <- dir(path)
#'   x <- moduleMetadata(sampleModules[1], path)
#'
setGeneric("moduleMetadata", function(module, path) {
  standardGeneric("moduleMetadata")
})

#' @export
#' @rdname moduleMetadata
setMethod(
  "moduleMetadata",
  signature = c(module = "character", path = "character"),
  definition = function(module, path) {
    filename <- paste(path, "/", module, "/", module, ".R", sep = "")
    if (!file.exists(filename)) {
      stop(paste(filename, "does not exist. This was created by putting",
                 "modulePath with the module name as a folder and filename. ",
                 "Please correct the modulePath or module name in",
                 "the simInit call."))
    }

    parsedFile <- parse(filename)
    defineModuleItem <- grepl(pattern = "defineModule", parsedFile)

    # pull out the list portion from "defineModule"
    x <- parsedFile[defineModuleItem] %>%
      as.character %>%
      gsub("[[:space:]]*\\n[[:space:]]*", " ", .) %>%
      sub("^defineModule[[:space:]]*\\([[:space:]]*", "", .) %>%
      sub("^sim[[:space:]]*,[[:space:]]*", "", .) %>%
      sub("\\)$", "", .) %>%
      gsub("[[:space:]]*=[[:space:]]*", " = ", .)

    # ensure variables in params are kept as strings
    x <- gsub("(globals\\(sim\\)\\$[^\\),]*)", "\"\\1\"", x, perl = TRUE) %>%
      gsub("(params\\(sim\\)\\$[^,]*)", "\"\\1\"", ., perl = TRUE)

    # check input types
    x <- gsub("extent\\(rep\\(NA, 4\\)\\)", "extent\\(rep\\(NA_real_, 4\\)\\)", x) %>%
      gsub("extent\\(c\\(NA, NA, NA, NA\\)\\)", "extent\\(rep\\(NA_real_, 4\\)\\)", .)

    # store metadata as list
    metadata <- eval(parse(text = x))

    return(metadata)
})

#' @export
#' @rdname moduleMetadata
setMethod(
  "moduleMetadata",
  signature = c(module = "character", path = "missing"),
  definition = function(module) {
    moduleMetadata(module, getwd())
})
