################################################################################
#' Parse and extract module metadata
#'
#' @param module Character string. Your module's name.
#'
#' @param path   Character string specifying the file path to modules directory.
#'               Default is to use the \code{spades.modulesPath} option.
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
#' path <- system.file(package = "SpaDES", "sampleModules")
#' sampleModules <- dir(path)
#' x <- moduleMetadata(sampleModules[3], path)
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

    # parsedFile <- parse(filename)
    # defineModuleItem <- grepl(pattern = "defineModule", parsedFile)
    #
    # # pull out the list portion from "defineModule"
    # x <- parsedFile[defineModuleItem] %>%
    #   as.character %>%
    #   gsub("[[:space:]]*\\n[[:space:]]*", " ", .) %>%
    #   sub("^defineModule[[:space:]]*\\([[:space:]]*", "", .) %>%
    #   sub("^sim[[:space:]]*,[[:space:]]*", "", .) %>%
    #   sub("\\)$", "", .) %>%
    #   gsub("[[:space:]]*=[[:space:]]*", " = ", .)
    #
    # # ensure variables in params are kept as strings
    # x <- gsub("(globals\\(sim\\)\\$[^\\),]*)", "\"\\1\"", x, perl = TRUE)
    # x <- gsub( "(params\\(sim\\)\\$[^\\),]*)", "\"\\1\"", x, perl = TRUE)
    #
    # # check input types
    # x <- gsub("extent\\(rep\\(NA, 4\\)\\)", "extent\\(rep\\(NA_real_, 4\\)\\)", x) %>%
    #   gsub("extent\\(c\\(NA, NA, NA, NA\\)\\)", "extent\\(rep\\(NA_real_, 4\\)\\)", .)

    # store metadata as list
    defineModuleListItems <- c("name", "description", "keywords", "childModules", "authors",
      "version", "spatialExtent", "timeframe", "timeunit", "citation",
      "documentation", "reqdPkgs", "parameters", "inputObjects", "outputObjects")
    metadata <- lapply(defineModuleListItems,
           function(xx) {
             pmp <- .parseModulePartial(filename = file.path(path,module,paste0(module,".R")),
                                 defineModuleElement = xx)
             out2 <- try(eval(pmp), silent = TRUE)
             if(is(out2, "try-error")) {
               inner2 <- lapply(pmp, function(yyy) { # pmp is whole rbind statement
                 out4 <- try(eval(yyy), silent = TRUE)
                 if(is(out4, "try-error")) {
                   yyy <- lapply(yyy, function(yyyyy) { # yyy is whole defineParameter statement
                     out5 <- try(eval(yyyyy), silent = TRUE)
                     if(is(out5, "try-error")) yyyyy <- deparse(yyyyy)
                     return(yyyyy)
                    })
                  }
                 if(is.list(yyy)) yyy <- as.call(yyy)
                 return(yyy)
               })
               out2 <- as.call(inner2)
             }
             return(eval(out2))
          })

    names(metadata) <- defineModuleListItems

    #metadata <- eval(parse(text = x)) # can't be used because can't evaluate start(sim)

    return(metadata)
})

#' @export
#' @rdname moduleMetadata
setMethod(
  "moduleMetadata",
  signature = c(module = "character", path = "missing"),
  definition = function(module) {
    moduleMetadata(module, getOption("spades.modulesPath"))
})
