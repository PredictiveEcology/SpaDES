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
#' @param repo   GitHub repository name.
#'                Default is \code{"PredictiveEcology/SpaDES-modules"},
#'                which is specified by the global option \code{spades.modulesRepo}.
#'
#' @importFrom httr content
#' @importFrom httr GET
#' @importFrom httr stop_for_status
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
setMethod("getModuleVersion",
          signature=c(name="character", repo="character"),
          definition = function(name, repo) {
            if (length(name)>1) {
              warning("name contains more than one module. Only the first will be used.")
              name = name[1]
            }
            apiurl <- paste0("https://api.github.com/repos/", repo, "/git/trees/master?recursive=1")
            request <- GET(apiurl)
            stop_for_status(request)
            allFiles <- unlist(lapply(content(request)$tree, "[", "path"), use.names=FALSE)
            moduleFiles <- grep(paste0("^modules/", name), allFiles, value=TRUE)
            zipFiles <- grep("[.]zip$", moduleFiles, value=TRUE)
            versions <- strsplit(zipFiles, "_") %>%
                        unlist %>%
                        grep("[.]zip$", ., value=TRUE) %>%
                        strsplit(., "[.]zip$") %>%
                        unlist %>%
                        as.numeric_version
            current <- sort(versions, decreasing=TRUE)[1]

            return(current)
})

#' @rdname getModuleVersion
setMethod("getModuleVersion",
          signature=c(name="character", repo="missing"),
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
#' @importFrom downloader download
# @importFrom utils unzip
#' @export
#' @rdname downloadModule
#'
#' @author Alex Chubaty
#'
setGeneric("downloadModule", function(name, path, version, repo) {
  standardGeneric("downloadModule")
})

#' @rdname downloadModule
setMethod("downloadModule",
          signature=c(name="character", path="character", version="character", repo="character"),
          definition = function(name, path, version, repo) {
            path <- checkPath(path, create=TRUE)
            if (is.na(version)) version <- getModuleVersion(name, repo)
            zip <- paste0("https://raw.githubusercontent.com/", repo,
                          "/master/modules/", name, "/", name, "_", version, ".zip")
            localzip <- file.path(path, basename(zip))
            download(zip, destfile=localzip, quiet=TRUE)
            files <- unzip(localzip, exdir=file.path(path), overwrite=TRUE)
            return(invisible(files))
})

#' @rdname downloadModule
setMethod("downloadModule",
          signature=c(name="character", path="character", version="character", repo="missing"),
          definition = function(name, path, version) {
            files <- downloadModule(name, path, version, repo=getOption("spades.modulesRepo"))
            return(invisible(files))
})

#' @rdname downloadModule
setMethod("downloadModule",
          signature=c(name="character", path="character", version="missing", repo="missing"),
          definition = function(name, path) {
            files <- downloadModule(name, path, version=NA_character_, repo=getOption("spades.modulesRepo"))
            return(invisible(files))
})

#' @rdname downloadModule
setMethod("downloadModule",
          signature=c(name="character", path="character", version="missing", repo="character"),
          definition = function(name, path, repo) {
            files <- downloadModule(name, path, version=NA_character_, repo=repo)
            return(invisible(files))
})


################################################################################
#' List modules from local or on github repository
#'
#' Modified from
#' \url{http://stackoverflow.com/questions/25485216/how-to-get-list-files-from-a-github-repository-folder-using-r}.
#' If repo is unspecified, then modulesDir
#'
#' @param repo   GitHub repository name.
#'                Default is \code{"PredictiveEcology/SpaDES-modules"},
#'                which is specified by the global option \code{spades.modulesRepo}.
#'
#'  @param path  Character string of length 1. The base directory (relative)
#'  within which there are only module subdirectories. Defaults to "modules".
#'
#'
#' @importFrom httr content
#' @importFrom httr GET
#' @importFrom httr stop_for_status
#' @export
#' @rdname listModules
#'
#' @author Eliot McIntire
#'
# igraph exports %>% from magrittr
setGeneric("listModules", function(repo,path) {
  standardGeneric("listModules")
})

#' @rdname listModules
setMethod("listModules",
          signature=c(repo="character", path="character"),
          definition = function(repo,path) {

            #remove trailing slashes
            repo <- gsub("/$", "", repo)

            #remove trailing slashes, then add filepath on github to relative path,
            #  including adding back trailing slash
            fullPath <- gsub("/$", "", path) %>%
              paste0(repo,"/tree/master/",.,"/")
            gitFullPath <- paste0("https://github.com/",fullPath)

            req <- GET(gitFullPath)
            stop_for_status(req)
            rawTxt = sapply(content(req, "text"), function(x) strsplit(x, split="<"))[[1]]

            modules <- rawTxt[grep(rawTxt, pattern=fullPath)] %>%
              strsplit(split=">") %>% sapply(., function(x) x[[2]])
            return(modules)
})

#' @rdname listModules
setMethod("listModules",
          signature=c(repo="missing",path="missing"),
          definition = function() {
            repo <- getOption("spades.modulesRepo")
            listModules(repo=repo, path="modules")
          })

#' @rdname listModules
setMethod("listModules",
          signature=c(repo="character",path="missing"),
          definition = function(repo) {
            listModules(repo=repo, path="modules")
          })

#' @rdname listModules
setMethod("listModules",
          signature=c(repo="missing",path="character"),
          definition = function(path) {
            dir(path=path, pattern="[\\.][rR]$",recursive = TRUE) %>%
              strsplit(.,split="/") %>%
              sapply(., function(x) x[1])
          })
