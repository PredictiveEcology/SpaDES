### deal with spurious httr warnings
if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("actualFile", "content", "result"))
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
    if (length(name) > 1) {
      warning("name contains more than one module. Only the first will be used.")
      name = name[1]
    }
    moduleFiles <- checkModule(name, repo)
    zipFiles <- grep(paste0(name,"_+.+.zip"), moduleFiles, value = TRUE) # moduleName_....zip only
    zipFiles <- grep(file.path(name,"data"), zipFiles, invert = TRUE, value = TRUE) # remove any zip in data folder
    # all zip files is not correct behaviour, only
    versions <- strsplit(zipFiles, "_") %>%
      unlist() %>%
      grep("[.]zip$", ., value = TRUE) %>%
      strsplit(., "[.]zip$") %>%
      unlist() %>%
      as.numeric_version()
    currentVersion <- sort(versions, decreasing = TRUE)[1]

    return(currentVersion)
})

#' @rdname getModuleVersion
setMethod("getModuleVersion",
          signature = c(name = "character", repo = "missing"),
          definition = function(name) {
            v <- getModuleVersion(name, getOption("spades.modulesRepo"))
            return(v)
})

################################################################################
#' Check for the existence of a remote module
#'
#' Looks in the remote \code{repo} for a module named \code{name}.
#'
#' @param name  Character string giving the module name.
#'
#' @param repo  GitHub repository name.
#'              Default is \code{"PredictiveEcology/SpaDES-modules"}, which is
#'              specified by the global option \code{spades.modulesRepo}.
#'
#' @importFrom httr content GET stop_for_status
#' @export
#' @rdname checkModule
#'
#' @author Eliot McIntire
#'
# igraph exports %>% from magrittr
setGeneric("checkModule", function(name, repo) {
  standardGeneric("checkModule")
})

#' @rdname checkModule
setMethod(
  "checkModule",
  signature = c(name = "character", repo = "character"),
  definition = function(name, repo) {
    if (length(name) > 1) {
      warning("name contains more than one module. Only the first will be used.")
      name = name[1]
    }
    apiurl <- paste0("https://api.github.com/repos/", repo,
                     "/git/trees/master?recursive=1")
    request <- GET(apiurl)
    stop_for_status(request)
    allFiles <- unlist(lapply(content(request)$tree, "[", "path"), use.names = FALSE)
    moduleFiles <- grep(paste0("^modules/", name), allFiles, value = TRUE)
    if (length(moduleFiles) == 0) {
      agrep(name, allFiles, max.distance = 0.25, value = TRUE,
            ignore.case = FALSE) %>%
        strsplit(., split="/") %>%
        lapply(., function(x) x[2]) %>%
        unique() %>%
        unlist() %>%
        paste(., collapse = ", ") %>%
        stop("Module ", name, " does not exist in the repository. ",
             "Did you mean: ", ., "?")
    }
    return(invisible(moduleFiles))
})

#' @rdname checkModule
setMethod("checkModule",
          signature = c(name = "character", repo = "missing"),
          definition = function(name) {
            v <- checkModule(name, getOption("spades.modulesRepo"))
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
#' @param path    Character string giving the location in which to save the
#'                downloaded module.
#'
#' @param version The module version to download. (If not specified, or \code{NA},
#'                the most recent version will be retrieved.)
#'
#' @param data    Logical. If TRUE, then the data that is identified in the module
#'                metadata will be downloaded, if possible. Default if FALSE.
#'
#' @param quiet   Logical. This is passed to \code{download.file}. Default is FALSE.
#'
#' @return A list of length 2. The first elemet is a character vector containing
#'    a character vector of extracted files for the module. The second element is
#'    a tbl with details about the data that is relevant for the function, including
#'    whether it was downloaded or not, whether it was renamed (because there
#'    was a local copy that had the wrong file name).
#'
# @importFrom utils unzip download.file
#' @export
#' @rdname downloadModule
#'
#' @author Alex Chubaty
#'
setGeneric("downloadModule", function(name, path, version, repo, data = FALSE,
                                      quiet = FALSE) {
  standardGeneric("downloadModule")
})

#' @rdname downloadModule
setMethod(
  "downloadModule",
  signature = c(name = "character", path = "character", version = "character",
                repo = "character", data = "logical", quiet = "logical"),
  definition = function(name, path, version, repo, data, quiet) {
    path <- checkPath(path, create = TRUE)
    checkModule(name, repo)
    if (is.na(version)) version <- getModuleVersion(name, repo)

    versionWarning(name, version)

    zip <- paste0("https://raw.githubusercontent.com/", repo,
                  "/master/modules/", name, "/", name, "_", version, ".zip")
    localzip <- file.path(path, basename(zip))
    download.file(zip, destfile = localzip, mode = "wb", quiet = quiet)
    files <- unzip(localzip, exdir = file.path(path), overwrite = TRUE)

    # after download, check for childModules that also require downloading
    files2 <- list()
    children <- .parseModulePartial(filename = file.path(path, name, paste0(name, ".R")),
                                    defineModuleElement = "childModules")
    #children <- moduleMetadata(name, path)$childModules
    dataList2 <- data.frame(result = character(0), expectedFile = character(0),
                            actualFile = character(0), checksum = character(0),
                            stringsAsFactors = FALSE)
    if (!is.null(children)) {
      if ( all( nzchar(children) & !is.na(children) ) ) {
        tmp <- lapply(children, function(x) {
          f <- downloadModule(x, path = path, data = data)
          files2 <<- append(files2, f[[1]])
          dataList2 <<- bind_rows(dataList2, f[[2]])
        })
      }
    }

    if (data) {
      dataList <- downloadData(module = name, path = path, quiet = quiet)
    } else {
      dataList <- checksums(module = name, path = path)
    }
    message("Download complete for module ", name, ".")

    return(list(c(files, files2), bind_rows(dataList, dataList2)))
})

#' @rdname downloadModule
setMethod(
  "downloadModule",
  signature = c(name = "character", path = "missing", version = "ANY",
                repo = "ANY", data = "ANY", quiet = "ANY"),
  definition = function(name, version, data, quiet) {
    files <- downloadModule(name, path = getOption("spades.modulesPath"),
                            version, repo = getOption("spades.modulesRepo"),
                            data = data, quiet = quiet)
    return(invisible(files))
})

#' @rdname downloadModule
setMethod(
  "downloadModule",
  signature = c(name = "character", path = "character", version = "missing",
                repo = "missing", data = "ANY", quiet = "ANY"),
  definition = function(name, path, data, quiet) {
    files <- downloadModule(name, path, version = NA_character_,
                            repo = getOption("spades.modulesRepo"),
                            data = data,
                            quiet = quiet)
    return(invisible(files))
})

#' @rdname downloadModule
setMethod(
  "downloadModule",
  signature = c(name = "character", path = "character", version = "character",
                repo = "missing", data = "ANY", quiet = "ANY"),
  definition = function(name, path, version, data, quiet) {
    files <- downloadModule(name, path, version,
                            repo = getOption("spades.modulesRepo"),
                            data = data, quiet = quiet)
    return(invisible(files))
})

#' @rdname downloadModule
setMethod(
  "downloadModule",
  signature = c(name = "character", path = "character", version = "missing",
                repo = "character", data = "ANY", quiet = "ANY"),
  definition = function(name, path, repo, data, quiet) {
    files <- downloadModule(name, path, version = NA_character_, repo = repo,
                            data = data, quiet)
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
#' @param quiet   Logical. This is passed to \code{download.file}. Default is FALSE.
#'
#' @return Invisibly, a list of downloaded files.
#'
#' @include moduleMetadata.R
# @importFrom utils download.file
#' @importFrom dplyr mutate_
#' @export
#' @rdname downloadData
#'
#' @author Alex Chubaty
#'
setGeneric("downloadData", function(module, path, quiet) {
  standardGeneric("downloadData")
})

#' @rdname downloadData
setMethod(
  "downloadData",
  signature = c(module = "character", path = "character", quiet = "logical"),
  definition = function(module, path, quiet) {
    cwd <- getwd()
    path <- checkPath(path, create = FALSE)
    urls <- .parseModulePartial(filename = file.path(path, module, paste0(module,".R")),
                                defineModuleElement = "inputObjects")$sourceURL
    if (is.call(urls)) {
      # This is the case where it can't evaluate the .parseModulePartial because of a reference
      #  to the sim object that isn't available. Because sourceURL is unlikely to use
      #  a call to sim object, then try to evaluate again here, just the one column
      urls <- eval(urls)
      #urls <- moduleMetadata(module, path)$inputObjects$sourceURL
    }

    ids <- which( urls == "" | is.na(urls) )
    to.dl <- if (length(ids)) urls[-ids] else urls
    chksums <- checksums(module, path) %>%
      mutate(renamed = NA, module = module)
    dataDir <- file.path(path, module, "data" )

    if (any(chksums$result == "FAIL")) {
      setwd(path); on.exit(setwd(cwd), add = TRUE)

      files <- sapply(to.dl, function(x) {
        destfile <- file.path(dataDir, basename(x))
        id <- which(chksums$expectedFile == basename(x))
        if ( is.na(chksums$actualFile[id]) ) {
          tmpFile <- file.path(tempdir(), "SpaDES_module_data") %>%
            checkPath(create = TRUE) %>%
            file.path(., basename(x))
          message("Downloading data for module ", module, " ...")
          download.file(x, destfile = tmpFile, mode = "wb", quiet = quiet)
          copied <- file.copy(from = tmpFile, to = destfile, overwrite = TRUE)
          destfile
        } else {
          message("  Download data step skipped for module ", module, ". Local copy exists.")
        }
      })

      chksums <- checksums(module, path) %>%
        mutate(renamed = NA, module = module)
    } else if (NROW(chksums) > 0) {
      message("  Download data step skipped for module ", module, ". Local copy exists.")
    } else {
      message("  No data to download for module ", module)
    }

    wh <- match(chksums$actualFile, chksums$expectedFile) %>% is.na() %>% which()
    if (length(wh)) {
      chksums[wh, "renamed"] <- sapply(wh, function(id) {
        renamed <- file.rename(
          from = file.path(dataDir, chksums$actualFile[id]),
          to = file.path(dataDir, chksums$expectedFile[id])
        )
      })
    }

    # if (any(!chksums$renamed %>% na.omit())) {
    #   message("Unable to automatically give proper name to downloaded files. ",
    #           "Check for existence of \n",
    #           paste(file.path(path, module, "data",
    #                     chksums$expectedFile[!chksums$renamed] %>% na.omit()),
    #                           collapse = "\n"),
    #           "\nfollowing downloads.")
    # }

    # after download, check for childModules that also require downloading
    children <- .parseModulePartial(filename = file.path(path, module, paste0(module, ".R")),
                                   defineModuleElement = "childModules")
    #children <- moduleMetadata(module, path)$childModules
    if (!is.null(children)) {
      if ( all( nzchar(children) & !is.na(children) ) ) {
        chksums2 <- lapply(children, downloadData, path = path, quiet = quiet) %>%
          bind_rows()
      }
    }

    return(bind_rows(chksums, chksums2))
})

#' @rdname downloadData
setMethod(
  "downloadData",
  signature = c(module = "character", path = "missing", quiet = "missing"),
  definition = function(module) {
    downloadData(module = module, path = getOption("spades.modulesPath"), quiet = FALSE)
})

#' @rdname downloadData
setMethod(
  "downloadData",
  signature = c(module = "character", path = "missing", quiet = "logical"),
  definition = function(module, quiet) {
    downloadData(module = module, path = getOption("spades.modulesPath"), quiet = quiet)
})

#' @rdname downloadData
setMethod(
  "downloadData",
  signature = c(module = "character", path = "character", quiet = "missing"),
  definition = function(module, path) {
    downloadData(module = module, path = path, quiet = FALSE)
})

################################################################################
#' Calculate the hashes of multiple files
#'
#' Internal function. Wrapper for \code{\link[digest]{digest}} using md5sum.
#'
#' @param file  Character vector of file paths.
#' @param ...   Additional arguments to \code{digest::digest}.
#'
#' @return A character vector of hashes.
#'
#' @importFrom digest digest
#' @keywords internal
#' @rdname digest
#'
#' @author Alex Chubaty
#'
setGeneric("digest", function(file, ...) {
  standardGeneric("digest")
})

#' @rdname digest
setMethod(
  "digest",
  signature = c(file = "character"),
  definition = function(file, ...) {
    sapply(file, function(f) {
      digest::digest(object = f, file = TRUE, algo = "xxhash64", ...)
    }) %>% unname() %>% as.character() # need as.character for empty case
})

################################################################################
#' Calculate checksums for a module's data files
#'
#' Verify (and optionally write) checksums for data files in a module's
#' \code{data/} subdirectory. The file \code{data/CHECKSUMS.txt} contains the
#' expected checksums for each data file.
#' Checksums are computed using \code{SpaDES:::digest}, which is simply a
#' wrapper around \code{digest::digest}.
#'
#' Modules may require data that for various reasons cannot be distributed with
#' the module source code. In these cases, the module developer should ensure
#' that the module downloads and extracts the data required. It is useful to not
#' only check that the data files exist locally but that their checksums match
#' those expected. See also \code{\link{downloadData}}.
#'
#' @note In version 1.2.0 and earlier, two checksums per file were required
#' because of differences in the checksum hash values on Windows and Unix-like
#' platforms. Recent versions use a different (faster) algorithm and only require
#' one checksum value per file.
#' To update your \file{CHECKSUMS.txt} files using the new algorithm, see
#' \url{https://github.com/PredictiveEcology/SpaDES/issues/295#issuecomment-246513405}.
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
#' @return A data.frame with columns: result, expectedFile, actualFile, and checksum.
#'
#' @include moduleMetadata.R
#' @importFrom dplyr arrange desc filter group_by_ left_join mutate rename_ row_number select_
#' @export
#' @rdname checksums
#'
#' @author Alex Chubaty
#'
#' @examples
#' \dontrun{
#' moduleName <- "my_module"
#' modulePath <- file.path("path", "to", "modules")
#'
#' ## verify checksums of all data files
#' checksums(monudleName, modulePath)
#'
#' ## write new CHECKSUMS.txt file
#'
#' # 1. verify that all data files are present (and no extra files are present)
#' list.files(file.path(modulePath, moduleName, "data")
#'
#' # 2. calculate file checksums and write to file (this will overwrite CHECKSUMS.txt)
#' checksums(monudleName, modulePath, write = TRUE)
#' }
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

    checksums <- digest(files) # uses SpaDES:::digest()

    out <- data.frame(file = basename(files), checksum = checksums,
                      stringsAsFactors = FALSE)

    checksumFile <- file.path(path, "CHECKSUMS.txt")

    txt <- if (file.info(checksumFile)$size > 0) {
      read.table(checksumFile, header = TRUE, stringsAsFactors = FALSE)
    } else {
      data.frame(file = character(0), checksum = character(0),
                 stringsAsFactors = FALSE)
    }

    if (write) {
      write.table(out, checksumFile, eol = "\n", col.names = TRUE, row.names = FALSE)
      return(out)
    } else {
      results.df <- out %>%
        rename_(actualFile = "file") %>%
        left_join(txt, ., by = "checksum") %>%
        rename_(expectedFile = "file") %>%
        dplyr::group_by_("expectedFile") %>%
        mutate(result = ifelse(is.na(actualFile), "FAIL", "OK")) %>%
        dplyr::arrange(desc(result)) %>%
        select_("result", "expectedFile", "actualFile", "checksum") %>%
        filter(row_number() == 1L)

      return(results.df)
    }
})

#' @rdname checksums
setMethod(
  "checksums",
  signature = c(module = "character", path = "character", write = "missing"),
  definition = function(module, path) {
    checksums(module, path, write = FALSE)
})
