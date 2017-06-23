################################################################################
#' Parse and extract module metadata
#'
#' @param module Character string. Your module's name.
#'
#' @param path   Character string specifying the file path to modules directory.
#'               Default is to use the \code{spades.modulePath} option.
#'
#' @inheritParams spades
#'
#' @return A list of module metadata, matching the structure in
#'         \code{\link{defineModule}}.
#'
#' @docType methods
#' @export
#' @include simulation-simInit.R
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
#' # using simList
#' mySim <- simInit(
#'    times = list(start = 2000.0, end = 2002.0, timeunit = "year"),
#'    params = list(
#'      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned")
#'    ),
#'    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'    paths = list(modulePath = system.file("sampleModules", package = "SpaDES"))
#' )
#' moduleMetadata(sim = mySim)
#'
setGeneric("moduleMetadata", function(module, path, sim) {
  standardGeneric("moduleMetadata")
})

#' @export
#' @rdname moduleMetadata
setMethod(
  "moduleMetadata",
  signature = c(module = "character", path = "character", sim = "missing"),
  definition = function(module, path) {
    filename <- paste(path, "/", module, "/", module, ".R", sep = "")
    if (!file.exists(filename)) {
      stop(paste(filename, "does not exist. This was created by putting",
                 "modulePath with the module name as a folder and filename. ",
                 "Please correct the modulePath or module name in",
                 "the simInit() call."))
    }

    ## store metadata as list
    defineModuleListItems <- c("name", "description", "keywords", "childModules", "authors",
      "version", "spatialExtent", "timeframe", "timeunit", "citation",
      "documentation", "reqdPkgs", "parameters", "inputObjects", "outputObjects")
    metadata <- lapply(defineModuleListItems,
           function(xx) {
             pmp <- .parseModulePartial(filename = file.path(path, module, paste0(module, ".R")),
                                        defineModuleElement = xx)
             out2 <- try(eval(pmp), silent = TRUE)
             if (is(out2, "try-error")) {
               inner2 <- lapply(pmp, function(yyy) {
                 # pmp is whole rbind statement
                 out4 <- try(eval(yyy), silent = TRUE)
                 if (is(out4, "try-error")) {
                   yyy <- lapply(yyy, function(yyyyy) {
                     # yyy is whole defineParameter statement
                     out5 <- try(eval(yyyyy), silent = TRUE)
                     if (is(out5, "try-error")) yyyyy <- deparse(yyyyy)
                     return(yyyyy)
                    })
                  }
                 if (is.list(yyy)) yyy <- as.call(yyy)
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
  signature = c(module = "character", path = "missing", sim = "missing"),
  definition = function(module) {
    moduleMetadata(module, getOption("spades.modulePath"))
})

#' @export
#' @rdname moduleMetadata
setMethod(
  "moduleMetadata",
  signature = c(module = "ANY", path = "missing", sim = "simList"),
  definition = function(module, sim) {
    if (missing(module)) module <- modules(sim)

    metadata <- lapply(module, function(mod)
      moduleMetadata(mod, path = modulePath(sim)))
    if (length(module) == 1) {
      metadata <- unlist(metadata, recursive = FALSE)
    } else {
      names(metadata) <- module
    }
    return(metadata)
})

################################################################################
#' Parse and extract a module's version
#'
#' @param module Character string. Your module's name.
#'
#' @param path   Character string specifying the file path to modules directory.
#'               Default is to use the \code{spades.modulePath} option.
#'
#' @inheritParams spades
#'
#' @return \code{numeric_version} indicating the module's version.
#'
#' @docType methods
#' @export
#' @include simulation-simInit.R
#' @rdname moduleVersion
#'
#' @seealso \code{\link{moduleMetadata}}
#'
#' @author Alex Chubaty
#'
#' @examples
#' path <- system.file(package = "SpaDES", "sampleModules")
#'
#' # using filepath
#' moduleVersion("caribouMovement", path)
#'
#' # using simList
#' mySim <- simInit(
#'    times = list(start = 2000.0, end = 2002.0, timeunit = "year"),
#'    params = list(
#'      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned")
#'    ),
#'    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'    paths = list(modulePath = path)
#' )
#' moduleVersion("caribouMovement", sim = mySim)
#'
setGeneric("moduleVersion", function(module, path, sim) {
  standardGeneric("moduleVersion")
})

#' @export
#' @rdname moduleVersion
setMethod(
  "moduleVersion",
  signature = c(module = "character", path = "character", sim = "missing"),
  definition = function(module, path) {
  v <- .parseModulePartial(filename = file.path(path, module, paste0(module, ".R")),
                           defineModuleElement = "version")
  if (is.null(names(v))) {
    as.numeric_version(v) ## SpaDES < 1.3.1.9044
  } else {
    as.numeric_version(v[[module]]) ## SpaDES >= 1.3.1.9044
  }
})

#' @export
#' @rdname moduleVersion
setMethod(
  "moduleVersion",
  signature = c(module = "character", path = "missing", sim = "missing"),
  definition = function(module) {
    moduleVersion(module = module, path = getOption("spades.modulePath"))
})

#' @export
#' @rdname moduleVersion
setMethod(
  "moduleVersion",
  signature = c(module = "character", path = "missing", sim = "simList"),
  definition = function(module, sim) {
    v <- .parseModulePartial(sim = sim, modules = list(module), defineModuleElement = "version") %>%
      `[[`(module)

    if (is.null(names(v))) {
      as.numeric_version(v) ## SpaDES < 1.3.1.9044
    } else {
      as.numeric_version(v[[module]])  ## SpaDES >= 1.3.1.9044
    }
})
