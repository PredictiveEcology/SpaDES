if (getRversion() >= "3.1.0") utils::globalVariables(".")

#' Get the name of a source file
#'
#' This will only work for files that are \code{source}-ed.
#' Based on this: \url{http://stackoverflow.com/a/1816487/1380598}.
#'
#' @param fullname Logical (default \code{FALSE}) indicating whether the full path should be returned.
#'
#' @return Character string representing the filename.
#'
#' @importFrom magrittr '%>%'
#' @export
#' @docType methods
#' @rdname getFileName
#'
#' @author Alex Chubaty
#'
setGeneric("getFileName", function(fullname) {
  standardGeneric("getFileName")
})

#' @rdname getFileName
setMethod("getFileName",
          signature="logical",
          definition=function(fullname) {
            f <- lapply(sys.frames(), function(i) i$filename) %>%
              Filter(Negate(is.null), .) %>%
              unlist
            if (fullname) {
              f <- normalizePath(file.path(getwd(), f), winslash="/")
            } else {
              f <- basename(f)
            }
            return(f)
})

################################################################################
#' Update elements of a named list with elements of a second named list
#'
#' Merge two named list based on their named entries. Where
#' any element matches in both lists, the value from the
#' second list is used in the updated list.
#' Subelements are not examined and are simply replaced.
#'
#' @param x   a named list
#' @param y   a named list
#'
#' @return A named list, with elements sorted by name.
#'          The values of matching elements in list \code{y}
#'          replace the values in list \code{x}.
#'
#' @export
#' @docType methods
#' @rdname updateList
#'
#' @author Alex Chubaty
#'
#' @examples
#' L1 <- list(a="hst", b=NA_character_, c=43)
#' L2 <- list(a="gst", c=42, d=list(letters))
#' updateList(L1, L2)
#'
setGeneric("updateList", function(x, y) {
  standardGeneric("updateList")
})

#' @rdname updateList
setMethod("updateList",
          signature=c("list", "list"),
          definition=function(x, y) {
            if (any(is.null(names(x)), is.null(names(y)))) {
              stop("All elements in lists x,y must be named.")
            } else {
              i <- which(names(x) %in% names(y))
              z <- append(x[-i], y)
              return(z[order(names(z))])
            }
})

#' @rdname updateList
setMethod("updateList",
          signature=c("NULL", "list"),
          definition=function(x, y) {
            if (is.null(names(y))) {
              stop("All elements in list y must be named.")
            }
            return(y[order(names(y))])
})

#' @rdname updateList
setMethod("updateList",
          signature=c("list", "NULL"),
          definition=function(x, y) {
            if (is.null(names(x))) {
              stop("All elements in list x must be named.")
            }
            return(x[order(names(x))])
})

#' @rdname updateList
setMethod("updateList",
          signature=c("NULL", "NULL"),
          definition=function(x, y) {
            return(list())
})

################################################################################
#' Load packages.
#'
#' Load and optionally install additional packages.
#'
#' @param packageList A list of character strings specifying
#' the names of packages to be loaded.
#'
#' @param install Logical flag. If required packages are not
#' already installed, should they be installed?
#'
#' @param quiet Logical flag. Should the final "packages loaded"
#' message be suppressed?
#'
#' @return Nothing is returned. Specified packages are loaded and attached using \code{library()}.
#'
#' @seealso \code{\link{library}}.
#'
#' @export
#' @docType methods
#' @rdname loadPackages
#'
#' @author Alex Chubaty
#'
#' @examples
#' \dontrun{pkgs <- list("ggplot2", "lme4")}
#' \dontrun{loadPackages(pkgs) # loads packages if installed}
#' \dontrun{loadPackages(pkgs, install=TRUE) # loads packages after installation (if needed)}
#'
setGeneric("loadPackages", function(packageList, install=FALSE, quiet=TRUE) {
  standardGeneric("loadPackages")
})

#' @rdname loadPackages
setMethod("loadPackages",
           signature="list",
            definition=function(packageList, install, quiet) {
              load <- function(name, install) {
                if (!require(name, character.only=TRUE)) {
                  if (install) {
                    cran <- if ( is.null(getOption("repos")) | getOption("repos")=="") {
                      "http://cran.rstudio.com"
                    } else {
                      getOption("repos")[[1]]
                    }
                    install.packages(name, repos=cran)
                    library(name, character.only=TRUE)
                    } else {
                      message(paste("NOTE: unable to load package ", name, ". Is it installed?", sep=""))
                    }
                  }
                }
              lapply(packageList, load, install)
              if (!quiet) message(paste("Loaded", length(packageList), "packages.", sep=" "))
})

#' @rdname loadPackages
setMethod("loadPackages",
          signature="character",
          definition=function(packageList, install, quiet) {
            loadPackages(as.list(packageList), install, quiet)
})

################################################################################
#' Check filepath.
#'
#' Checks the specified filepath for formatting consistencies,
#' such as trailing slashes, etc.
#'
#' @param path A character string corresponding to a filepath.
#'
#' @param create A logical indicating whether the path should
#' be created if it doesn't exist. Default is \code{FALSE}.
#'
#' @return Character string denoting the cleaned up filepath.
#'
#' @seealso \code{\link{file.exists}}, \code{\link{dir.create}}.
#'
#' @importFrom magrittr '%>%'
#' @export
#' @docType methods
#' @rdname checkPath
#'
setGeneric("checkPath", function(path, create) {
  standardGeneric("checkPath")
})

#' @rdname checkPath
setMethod("checkPath",
          signature(path="character", create="logical"),
          definition=function(path, create) {
            if (!is.na(path)) {
              if (length(path)>0) {
                # use slash instead of backslash
                # do tilde etc. expansion
                # remove trailing slash
                path = gsub("\\\\", "/", path) %>%
                       normalizePath(., winslash="/", mustWork=FALSE) %>%
                       gsub("/$", "", .)

                if (!file.exists(path)) {
                  if (create==TRUE) {
                    dir.create(file.path(path), recursive=TRUE, showWarnings=FALSE)
                  } else {
                    stop(paste("Specified path", normalizePath(path, winslash="/"),
                               "doesn't exist. Create it and try again."))
                  }
                }
              return(path)
            } else {
              stop("Invalid path: cannot be empty.")
            }
          } else {
            stop("Invalid path: cannot be NA.")
          }
})

#' @rdname checkPath
setMethod("checkPath",
          signature(path="character", create="missing"),
          definition=function(path) {
            return(checkPath(path, create=FALSE))
})

#' @rdname checkPath
setMethod("checkPath",
          signature(path="NULL", create="ANY"),
          definition=function(path) {
            stop("Invalid path: cannot be NULL.")
})

###############################################################
#' Convert numeric to character with padding
#'
#' @param x numeric. Number to be converted to character with padding
#'
#' @param padL numeric. Desired number of digits on left side of decimal.
#'              If not enough, \code{pad} will be used to pad.
#'
#' @param padR numeric. Desired number of digits on right side of decimal.
#'              If not enough, \code{pad} will be used to pad.
#'
#' @param pad character to use as padding (\code{nchar(pad)==1} must be \code{TRUE}).
#'            Passed to \code{\link[stringr]{str_pad}}
#'
#' @return Character string representing the filename.
#'
#' @importFrom fpCompare '%==%'
#' @importFrom magrittr '%>%'
#' @importFrom stringr str_pad
#' @export
#' @docType methods
#' @rdname paddedFloatToChar
#'
#' @author Eliot McIntire and Alex Chubaty
paddedFloatToChar <- function(x, padL=ceiling(log10(x+1)), padR=3, pad="0") {
  xIC <- x %/% 1 %>%
    format(., trim=TRUE, digits=5,scientific=FALSE) %>%
    str_pad(., pad=pad, width=padL, side="left")
  xf <- x %% 1
  xFC <- if(xf %==% 0) { "" } else {
    strsplit(format(xf, digits=padR, scientific=FALSE), split="\\.")[[1]][2] %>%
      str_pad(., width=padR, side="right", pad=pad) %>%
      paste0(".", .)
  }
  return(paste0(xIC, xFC))
}

###############################################################################
#' Generate random strings
#'
#' Generate a vector of random alphanumeric strings each of an arbitrary length.
#'
#' @param n   Number of strings to generate (default 1).
#'            Will attempt to coerce to integer value.
#'
#' @param len Length of strings to generate (default 8).
#'            Will attempt to coerce to integer value.
#'
#' @param characterFirst Logical, if \code{TRUE}, then a letter will be the
#'        first character of the string (useful if being used for object names).
#'
#' @return Character vector of random strings.
#'
#' @export
#' @docType methods
#' @rdname rndstr
#'
#' @author Alex Chubaty and Eliot McIntire
setGeneric("rndstr", function(n, len, characterFirst) {
  standardGeneric("rndstr")
})

#' @rdname rndstr
setMethod("rndstr",
          signature(n="numeric", len="numeric", characterFirst="logical"),
          definition=function(n, len, characterFirst) {
            stopifnot(n>0, len>0)
            unlist(lapply(character(as.integer(n)), function(x) {
              i <- as.integer(characterFirst)
              x <- paste0(c(sample(c(letters, LETTERS), size=i),
                            sample(c((0:9), letters, LETTERS),
                                   size=as.integer(len)-i, replace=TRUE)),
                          collapse="")
              }))
})

#' @rdname rndstr
setMethod("rndstr",
          signature(n="numeric", len="numeric", characterFirst="missing"),
          definition=function(n, len) {
            rndstr(n=n, len=len, characterFirst=TRUE)
})

#' @rdname rndstr
setMethod("rndstr",
          signature(n="numeric", len="missing", characterFirst="logical"),
          definition=function(n, characterFirst) {
            rndstr(n=n, len=8, characterFirst=characterFirst)
})

#' @rdname rndstr
setMethod("rndstr",
          signature(n="missing", len="numeric", characterFirst="logical"),
          definition=function(len, characterFirst) {
            rndstr(n=1, len=len, characterFirst=characterFirst)
})

#' @rdname rndstr
setMethod("rndstr",
          signature(n="numeric", len="missing", characterFirst="missing"),
          definition=function(n) {
            rndstr(n=n, len=8, characterFirst=TRUE)
})

#' @rdname rndstr
setMethod("rndstr",
          signature(n="missing", len="numeric", characterFirst="missing"),
          definition=function(len) {
            rndstr(n=1, len=len, characterFirst=TRUE)
})

#' @rdname rndstr
setMethod("rndstr",
          signature(n="missing", len="missing", characterFirst="logical"),
          definition=function(characterFirst) {
            rndstr(n=1, len=8, characterFirst=characterFirst)
})

#' @rdname rndstr
setMethod("rndstr",
          signature(n="missing", len="missing", characterFirst="missing"),
          definition=function(n, len, characterFirst) {
            rndstr(n=1, len=8, characterFirst=TRUE)
})
