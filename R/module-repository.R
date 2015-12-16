### deal with spurious httr warnings
if(getRversion() >= "3.1.0") {
  utils::globalVariables(c("content"))
}

################################################################################
#' Find the latest module version from a SpaDES module repository
#'
#' Modified from \url{http://stackoverflow.com/a/25485782/1380598}.
#'
#' @param name  Character string giving the module name.
#'
#' @param repo  GitHub repository name.
#'              Default is \code{"PredictiveEcology/SpaDES-modules"}, which is
#'              specified by the global option \code{spades.modulesRepo}.
#'
#' @importFrom httr content GET stop_for_status
#' @export
#' @rdname getModuleVersion
#'
#' @author Alex Chubaty
#'
# igraph exports %>% from magrittr
setGeneric("getModuleVersion", function(name, repo) {
  standardGeneric("getModuleVersion")
})

#' @rdname getModuleVersion
setMethod(
  "getModuleVersion",
  signature = c(name = "character", repo = "character"),
  definition = function(name, repo) {
    if (length(name)>1) {
      warning("name contains more than one module. Only the first will be used.")
      name = name[1]
    }
    apiurl <- paste0("https://api.github.com/repos/", repo, "/git/trees/master?recursive=1")
    request <- GET(apiurl)
    stop_for_status(request)
    allFiles <- unlist(lapply(content(request)$tree, "[", "path"), use.names = FALSE)
    moduleFiles <- grep(paste0("^modules/", name), allFiles, value = TRUE)
    zipFiles <- grep("[.]zip$", moduleFiles, value = TRUE)
    versions <- strsplit(zipFiles, "_") %>%
                unlist %>%
                grep("[.]zip$", ., value = TRUE) %>%
                strsplit(., "[.]zip$") %>%
                unlist %>%
                as.numeric_version
    current <- sort(versions, decreasing = TRUE)[1]

    return(current)
})

#' @rdname getModuleVersion
setMethod("getModuleVersion",
          signature = c(name = "character", repo = "missing"),
          definition = function(name) {
            v <- getModuleVersion(name, getOption("spades.modulesRepo"))
            return(v)
})

################################################################################
#' Download a module from a SpaDES module GitHub repository
#'
#' Download a .zip file of the module and extract (unzip) it to a user-specified location.
#'
#' Currently only works with a public GitHub repository, where modules are in
#' a \code{modules} directory in the root tree on the \code{master} branch.
#'
#' NOTE: the default is to overwrite any existing files in the case of a conflict.
#'
#' @inheritParams getModuleVersion
#'
#' @param path  Character string giving the location in which to save the downloaded module.
#'
#' @param version The module version to download.
#'                (If not specified, or \code{NA}, the most recent version will be retrieved.)
#'
#' @return Invisibly, a character vector containing a list of extracted files.
#'
# @importFrom utils unzip download.file
#' @export
#' @rdname downloadModule
#'
#' @author Alex Chubaty
#'
setGeneric("downloadModule", function(name, path, version, repo) {
  standardGeneric("downloadModule")
})

#' @rdname downloadModule
setMethod(
  "downloadModule",
  signature = c(name = "character", path = "character", version = "character",
                repo = "character"),
  definition = function(name, path, version, repo) {
    path <- checkPath(path, create = TRUE)
    if (is.na(version)) version <- getModuleVersion(name, repo)
    zip <- paste0("https://raw.githubusercontent.com/", repo,
                  "/master/modules/", name, "/", name, "_", version, ".zip")
    localzip <- file.path(path, basename(zip))
    download.file(zip, destfile = localzip, quiet = TRUE)
    files <- unzip(localzip, exdir = file.path(path), overwrite = TRUE)

    # after download, check for childModules that also require downloading
    children <- moduleMetadata(name, path)$childModules
    if (!is.null(children)) {
      if ( all( nzchar(children) & !is.na(children) ) ) {
        files2 <- lapply(children, downloadModule, path = path)
      }
    }
    return(invisible(c(files, files2)))
})

#' @rdname downloadModule
setMethod(
  "downloadModule",
  signature = c(name = "character", path = "character", version = "character",
                repo = "missing"),
  definition = function(name, path, version) {
    files <- downloadModule(name, path, version,
                            repo = getOption("spades.modulesRepo"))
    return(invisible(files))
})

#' @rdname downloadModule
setMethod(
  "downloadModule",
  signature = c(name = "character", path = "character", version = "missing",
                repo = "missing"),
  definition = function(name, path) {
    files <- downloadModule(name, path, version = NA_character_,
                            repo = getOption("spades.modulesRepo"))
    return(invisible(files))
})

#' @rdname downloadModule
setMethod(
  "downloadModule",
  signature = c(name = "character", path = "character", version = "missing",
                repo = "character"),
  definition = function(name, path, repo) {
    files <- downloadModule(name, path, version = NA_character_, repo = repo)
    return(invisible(files))
})

################################################################################
#' Download module data
#'
#' Download external data for a module if not already present in the module
#' directory or if there is a checksum mismatch indicating that the file is not
#' the correct one.
#'
#' @param module  Character string giving the name of the module.
#'
#' @param path    Character string giving the path to the module directory.
#'
#' @return Invisibly, a character vector containing a list of downloaded files.
#'
#' @include moduleMetadata.R
# @importFrom utils download.file
#' @export
#' @rdname downloadData
#'
#' @author Alex Chubaty
#'
setGeneric("downloadData", function(module, path) {
  standardGeneric("downloadData")
})

#' @rdname downloadData
setMethod(
  "downloadData",
  signature = c(module = "character", path = "character"),
  definition = function(module, path) {
    cwd <- getwd()
    path <- checkPath(path, create = FALSE)
    urls <- moduleMetadata(module, path)$inputObjects$sourceURL
    ids <- which( urls == "" || is.na(urls) )
    to.dl <- if (length(ids)) { urls[-ids] } else { urls }

    if (length(to.dl)) {
      setwd(path); on.exit(setwd(cwd))
      files <- lapply(to.dl, function(x) {
        destfile <- file.path(path, module, "data", basename(x))
        id <- which(checksums(module, path)$file == basename(x))
        chksum <- suppressMessages(checksums(module, path)[id, "results"])
        if ( !file.exists(destfile) || chksum != "OK" ) {
          download.file(x, destfile = destfile, quiet = TRUE, mode = "wb")
        }
        basename(x)
      })
    } else {
      files <- list()
    }

    # will print warning if checksums don't match
    checksum <- suppressMessages(checksums(module, path))

    return(invisible(cbind(files, checksum$result)))
})

################################################################################
#' Calculate checksums for a module's data files
#'
#' Verify (and optionally write) checksums for data files in a module's
#' \code{data/} subdirectory. The file \code{data/CHECKSUMS.txt} contains the
#' expected checksums for each data file.
#' Checksums are computed using \code{digest::digest(..., algo = "md5")}.
#'
#' Modules may require data that for various reasons cannot be distributed with
#' the module source code. In these cases, the module developer should ensure
#' that the module downloads and extracts the data required. It is useful to not
#' only check that the data files exist locally but that their checksums match
#' those expected. See also \code{\link{downloadData}}.
#'
#' @param module  Character string giving the name of the module.
#'
#' @param path    Character string giving the path to the module directory.
#'
#' @param write   Logical indicating whether to overwrite \code{CHECKSUMS.txt}.
#'                Default is \code{FALSE}, as users should not change this file.
#'                Module developers should write this file prior to distributing
#'                their module code, and update accordingly when the data change.
#'
#' @return A data.frame of filenames, checksums, and results.
#'
#' @include moduleMetadata.R
#' @importFrom digest digest
#' @export
#' @rdname checksums
#'
#' @author Alex Chubaty
#'
setGeneric("checksums", function(module, path, write) {
  standardGeneric("checksums")
})

#' @rdname checksums
setMethod(
  "checksums",
  signature = c(module = "character", path = "character", write = "logical"),
  definition = function(module, path, write) {
    path <- checkPath(path, create = FALSE) %>% file.path(., module, "data")
    if (!write) stopifnot(file.exists(file.path(path, "CHECKSUMS.txt")))

    files <- list.files(path, full.names = TRUE) %>%
      grep("CHECKSUMS.txt", ., value = TRUE, invert = TRUE)

    checksums <- sapply(files, function(x) {
      digest(file = x, algo = "md5") # use sha1?
    }) %>% unname()

    out <- data.frame(file = basename(files), checksum = checksums,
                      stringsAsFactors = FALSE)

    if (write) {
      write.table(out, file.path(path, "CHECKSUMS.txt"), eol = "\n",
                  col.names = TRUE, row.names = FALSE)
      return(out)
    } else {
      txt <- read.table(file.path(path, "CHECKSUMS.txt"), header = TRUE,
                        stringsAsFactors = FALSE)
      results <- apply(out, 1, function(x) {
        which(txt[, "file"] == x["file"]) %>%
          txt[., "checksum"] %in% x["checksum"] %>%
          any()
        }) %>%
        as.character() %>%
        gsub("TRUE", "OK", .) %>%
        gsub("FALSE", "FAIL", .)

      if (all(results == "OK")) {
        message("All file checksums match.")
      } else {
        warning("All file checksums do not match!")
      }
      return(cbind(out, results, stringsAsFactors = FALSE))
    }
})

#' @rdname checksums
setMethod(
  "checksums",
  signature = c(module = "character", path = "character", write = "missing"),
  definition = function(module, path) {
    checksums(module, path, write = FALSE)
})
