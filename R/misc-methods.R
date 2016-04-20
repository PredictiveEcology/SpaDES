if (getRversion() >= "3.1.0") {
  utils::globalVariables(".")
}

#' Get the name of a \code{source}-ed file
#'
#' Use \code{getFileName} in a file that is \code{source}-ed.
#' Based on \url{http://stackoverflow.com/a/1816487/1380598}.
#'
#' @param fullname   Logical (default \code{FALSE}) indicating whether the full
#'                   path should be returned.
#'
#' @return Character string representing the filename.
#'
#' @export
#' @docType methods
#' @rdname getFileName
#'
#' @author Alex Chubaty
#'
# igraph exports %>% from magrittr
setGeneric("getFileName", function(fullname) {
  standardGeneric("getFileName")
})

#' @rdname getFileName
setMethod("getFileName",
          signature = "logical",
          definition = function(fullname) {
            f <- lapply(sys.frames(), function(i) i$filename) %>%
              Filter(Negate(is.null), .) %>%
              unlist
            if (fullname) {
              f <- normalizePath(file.path(getwd(), f), winslash = "/")
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
#' Subelements are not examined and are simply replaced. If one list is empty, then
#' it returns the other one, unchanged.
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
#' L1 <- list(a = "hst", b = NA_character_, c = 43)
#' L2 <- list(a = "gst", c = 42, d = list(letters))
#' updateList(L1, L2)
#'
#' updateList(L1, NULL)
#' updateList(NULL, L2)
#' updateList(NULL, NULL) # should return empty list
#'
setGeneric("updateList", function(x, y) {
  standardGeneric("updateList")
})

#' @rdname updateList
setMethod("updateList",
          signature = c("list", "list"),
          definition = function(x, y) {
            if (any(is.null(names(x)), is.null(names(y)))) {
              # If one of the lists is empty, then just return the other, unchanged
              if (length(y)==0) return(x)
              if (length(x)==0) return(y)
              stop("All elements in lists x,y must be named.")
            } else {
              i <- which(names(x) %in% names(y))
              z <- append(x[-i], y)
              return(z[order(names(z))])
            }
})

#' @rdname updateList
setMethod("updateList",
          signature = c("NULL", "list"),
          definition = function(x, y) {
            if (is.null(names(y))) {
              if (length(y) == 0) return(x)
              stop("All elements in list y must be named.")
            }
            return(y[order(names(y))])
})

#' @rdname updateList
setMethod("updateList",
          signature = c("list", "NULL"),
          definition = function(x, y) {
            if (is.null(names(x))) {
              if (length(x) == 0) return(x)
              stop("All elements in list x must be named.")
            }
            return(x[order(names(x))])
})

#' @rdname updateList
setMethod("updateList",
          signature = c("NULL", "NULL"),
          definition = function(x, y) {
            return(list())
})

################################################################################
#' Add a module to a \code{moduleList}
#'
#' Ordinary base lists and vectors do not retain their attributes when subsetted
#' or appended. This function appends items to a list while preserving the
#' attributes of items in the list (but not of the list itself).
#'
#' Similar to \code{updateList} but does not require named lists.
#'
#' @param x  A \code{list} of items with optional attributes.
#'
#' @param y  See \code{x}.
#'
#' @return An updated \code{list} with attributes.
#'
#' @export
#' @docType methods
#' @rdname append_attr
#'
#' @author Alex Chubaty and Eliot McIntire
#'
#' @examples
#' library(igraph) # igraph exports magrittr's pipe operator
#' tmp1 <- list("apple", "banana") %>% lapply(., `attributes<-`, list(type = "fruit"))
#' tmp2 <- list("carrot") %>% lapply(., `attributes<-`, list(type = "vegetable"))
#' append_attr(tmp1, tmp2)
#' rm(tmp1, tmp2)
setGeneric("append_attr", function(x, y) {
  standardGeneric("append_attr")
})

#' @export
#' @rdname append_attr
setMethod("append_attr",
          signature = c(x = "list", y = "list"),
          definition = function(x, y) {
            attrs <- c(lapply(x, attributes), lapply(y, attributes))
            out <- append(x, y)
            if (length(out)) {
              for (i in length(out)) {
                attributes(out[i]) <- attrs[[i]]
              }
            }
            return(unique(out))
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
#' @return Specified packages are loaded and attached using \code{require()},
#'         invisibly returning a logical vector of successes.
#'
#' @seealso \code{\link{require}}.
#'
#' @export
#' @docType methods
#' @rdname loadPackages
# @importFrom igraph '%>%'
# @importFrom utils install.packages
#'
#' @author Alex Chubaty
#'
#' @examples
#' \dontrun{
#'   pkgs <- list("ggplot2", "lme4")
#'   loadPackages(pkgs) # loads packages if installed
#'   loadPackages(pkgs, install = TRUE) # loads packages after installation (if needed)
#' }
#'
setGeneric("loadPackages", function(packageList, install = FALSE, quiet = TRUE) {
  standardGeneric("loadPackages")
})

#' @rdname loadPackages
setMethod(
  "loadPackages",
  signature = "character",
  definition = function(packageList, install, quiet) {
    packageList <- na.omit(packageList) %>% as.character
    if (length(packageList)) {
      if (install) {
        repos <- getOption("repos")
        if ( is.null(repos) | any(repos == "") ) {
          repos <- "https://cran.rstudio.com"
        }
        installed <- unname(installed.packages()[,"Package"])
        toInstall <- packageList[packageList %in% installed]
        install.packages(toInstall, repos = repos)
      }

      loaded <- sapply(packageList, require, character.only = TRUE)

      if (!quiet) {
        message(paste("Loaded", length(which(loaded == TRUE)), "of",
                      length(packageList), "packages.", sep = " "))
      }
    } else {
      loaded <- character(0)
    }
    return(invisible(loaded))
})

#' @rdname loadPackages
setMethod("loadPackages",
          signature = "list",
          definition = function(packageList, install, quiet) {
            loadPackages(unlist(packageList), install, quiet)
})

#' @rdname loadPackages
setMethod("loadPackages",
          signature = "NULL",
          definition = function(packageList, install, quiet) {
            return(invisible(character(0)))
})

################################################################################
#' Normalize filepath
#'
#' Checks the specified filepath for formatting consistencies:
#'  1) use slash instead of backslash;
#'  2) do tilde etc. expansion;
#'  3) remove trailing slash.
#'
#' @param path A character vector of filepaths.
#'
#' @return Character vector of cleaned up filepaths.
#'
#' @export
#' @docType methods
#' @rdname normPath
#'
# igraph exports %>% from magrittr
setGeneric("normPath", function(path) {
  standardGeneric("normPath")
})

#' @export
#' @rdname normPath
setMethod("normPath",
          signature(path = "character"),
          definition = function(path) {
            lapply(path, function(x) {
                if (is.na(x)) {
                  NA_character_
                } else {
                  normalizePath(x, winslash = "/", mustWork = FALSE)
                }
              }) %>%
              unlist() %>%
              gsub("^[.]", paste0(getwd()), .) %>%
              gsub("\\\\", "//", .) %>%
              gsub("//", "/", .) %>%
              gsub("/$", "", .)
})

#' @export
#' @rdname normPath
setMethod("normPath",
          signature(path = "list"),
          definition = function(path) {
            return(normPath(unlist(path)))
})

#' @export
#' @rdname normPath
setMethod("normPath",
          signature(path = "NULL"),
          definition = function(path) {
            return(character(0))
})

#' @export
#' @rdname normPath
setMethod("normPath",
          signature(path = "missing"),
          definition = function() {
            return(character(0))
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
#' @export
#' @docType methods
#' @rdname checkPath
#'
# igraph exports %>% from magrittr
setGeneric("checkPath", function(path, create) {
  standardGeneric("checkPath")
})

#' @export
#' @rdname checkPath
setMethod(
  "checkPath",
  signature(path = "character", create = "logical"),
  definition = function(path, create) {
    if (length(path) != 1) {
      stop("path must be a character vector of length 1.")
    } else {
      if (is.na(path)) {
        stop("Invalid path: cannot be NA.")
      } else {
        path = normPath(path)

        if (!file.exists(path)) {
          if (create == TRUE) {
            dir.create(file.path(path), recursive = TRUE, showWarnings = FALSE)
          } else {
            stop(paste("Specified path", path, "doesn't exist.",
                       "Create it and try again."))
          }
        }
        return(path)
      }
    }
})

#' @export
#' @rdname checkPath
setMethod("checkPath",
          signature(path = "character", create = "missing"),
          definition = function(path) {
            return(checkPath(path, create = FALSE))
})

#' @export
#' @rdname checkPath
setMethod("checkPath",
          signature(path = "NULL", create = "ANY"),
          definition = function(path) {
            stop("Invalid path: cannot be NULL.")
})

#' @export
#' @rdname checkPath
setMethod("checkPath",
          signature(path = "missing", create = "ANY"),
          definition = function() {
            stop("Invalid path: no path specified.")
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
#' @importFrom stringr str_pad
#' @export
#' @docType methods
#' @rdname paddedFloatToChar
#'
#' @author Eliot McIntire and Alex Chubaty
#'
#' @examples
#' paddedFloatToChar(1.25)
#' paddedFloatToChar(1.25, padL = 3, padR = 5)
#'
# igraph exports %>% from magrittr
paddedFloatToChar <- function(x, padL = ceiling(log10(x+1)), padR = 3, pad = "0") {
  xIC <- x %/% 1 %>%
    format(., trim = TRUE, digits = 5,scientific = FALSE) %>%
    str_pad(., pad = pad, width = padL, side = "left")
  xf <- x %% 1
  xFC <- ifelse(xf %==% 0 , "" ,
    strsplit(format(xf, digits = padR, scientific = FALSE), split = "\\.")[[1]][2] %>%
      str_pad(., width = padR, side = "right", pad = pad) %>%
      paste0(".", .))

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
#' @examples
#' set.seed(11)
#' rndstr()
#' rndstr(len = 10)
#' rndstr(characterFirst = FALSE)
#' rndstr(n = 5, len = 10)
#' rndstr(n = 5)
#' rndstr(n = 5, characterFirst = TRUE)
#' rndstr(len = 10, characterFirst = TRUE)
#' rndstr(n = 5, len = 10, characterFirst = TRUE)
#'
setGeneric("rndstr", function(n, len, characterFirst) {
  standardGeneric("rndstr")
})

#' @rdname rndstr
setMethod(
  "rndstr",
  signature(n = "numeric", len = "numeric", characterFirst = "logical"),
  definition = function(n, len, characterFirst) {
    if (!((n > 0) & (len > 0))) {
      stop("rndstr requires n > 0 and len > 0")
    }

    unlist(lapply(character(as.integer(n)), function(x) {
      i <- as.integer(characterFirst)
      x <- paste0(c(sample(c(letters, LETTERS), size = i),
                    sample(c((0:9), letters, LETTERS),
                           size = as.integer(len) - i, replace = TRUE)),
                  collapse = "")
      }))
})

#' @rdname rndstr
setMethod("rndstr",
          signature(n = "numeric", len = "numeric", characterFirst = "missing"),
          definition = function(n, len) {
            rndstr(n=n, len=len, characterFirst=TRUE)
})

#' @rdname rndstr
setMethod("rndstr",
          signature(n = "numeric", len = "missing", characterFirst = "logical"),
          definition = function(n, characterFirst) {
            rndstr(n=n, len=8, characterFirst=characterFirst)
})

#' @rdname rndstr
setMethod("rndstr",
          signature(n = "missing", len = "numeric", characterFirst = "logical"),
          definition = function(len, characterFirst) {
            rndstr(n=1, len=len, characterFirst=characterFirst)
})

#' @rdname rndstr
setMethod("rndstr",
          signature(n = "numeric", len = "missing", characterFirst = "missing"),
          definition = function(n) {
            rndstr(n=n, len=8, characterFirst=TRUE)
})

#' @rdname rndstr
setMethod("rndstr",
          signature(n = "missing", len = "numeric", characterFirst = "missing"),
          definition = function(len) {
            rndstr(n=1, len=len, characterFirst=TRUE)
})

#' @rdname rndstr
setMethod("rndstr",
          signature(n = "missing", len = "missing", characterFirst = "logical"),
          definition = function(characterFirst) {
            rndstr(n=1, len=8, characterFirst=characterFirst)
})

#' @rdname rndstr
setMethod("rndstr",
          signature(n = "missing", len = "missing", characterFirst = "missing"),
          definition = function(n, len, characterFirst) {
            rndstr(n=1, len=8, characterFirst=TRUE)
})

################################################################################
#' Filter objects by class
#'
#' Based on \url{http://stackoverflow.com/a/5158978/1380598}.
#'
#' @param x Character vector of object names to filter, possibly from \code{ls}.
#'
#' @param include   Class(es) to include, as a character vector.
#'
#' @param exclude   Optional class(es) to exclude, as a character vector.
#'
#' @param envir     The environment ins which to search for objects.
#'                  Default is the calling environment.
#'
#' @return Vector of object names matching the class filter.
#'
#' @note \code{\link{inherits}} is used internally to check the object class,
#' which can, in some cases, return results inconsistent with \code{is}.
#' See \url{http://stackoverflow.com/a/27923346/1380598}.
#' These (known) cases are checked manually and corrected.
#'
#' @export
#' @docType methods
#' @rdname classFilter
#'
#' @author Alex Chubaty
#'
#' @examples
#' \dontrun{
#'   ## from global environment
#'   a <- list(1:10)     # class `list`
#'   b <- letters        # class `character`
#'   d <- stats::runif(10)      # class `numeric`
#'   f <- sample(1L:10L) # class `numeric`, `integer`
#'   g <- lm( jitter(d) ~ d ) # class `lm`
#'   h <- glm( jitter(d) ~ d ) # class `lm`, `glm`
#'   classFilter(ls(), include=c("character", "list"))
#'   classFilter(ls(), include = "numeric")
#'   classFilter(ls(), include = "numeric", exclude = "integer")
#'   classFilter(ls(), include = "lm")
#'   classFilter(ls(), include = "lm", exclude = "glm")
#'   rm(a, b, d, f, g, h)
#' }
#'
#' ## from local (e.g., function) environment
#' local({
#'   e <- environment()
#'   a <- list(1:10)     # class `list`
#'   b <- letters        # class `character`
#'   d <- stats::runif(10)      # class `numeric`
#'   f <- sample(1L:10L) # class `numeric`, `integer`
#'   g <- lm( jitter(d) ~ d ) # class `lm`
#'   h <- glm( jitter(d) ~ d ) # class `lm`, `glm`
#'   classFilter(ls(), include=c("character", "list"), envir = e)
#'   classFilter(ls(), include = "numeric", envir = e)
#'   classFilter(ls(), include = "numeric", exclude = "integer", envir = e)
#'   classFilter(ls(), include = "lm", envir = e)
#'   classFilter(ls(), include = "lm", exclude = "glm", envir = e)
#'   rm(a, b, d, e, f, g, h)
#' })
#'
#' ## from another environment
#' e = new.env(parent = emptyenv())
#' e$a <- list(1:10)     # class `list`
#' e$b <- letters        # class `character`
#' e$d <- stats::runif(10)      # class `numeric`
#' e$f <- sample(1L:10L) # class `numeric`, `integer`
#' e$g <- lm( jitter(e$d) ~ e$d ) # class `lm`
#' e$h <- glm( jitter(e$d) ~ e$d ) # class `lm`, `glm`
#' classFilter(ls(e), include=c("character", "list"), envir = e)
#' classFilter(ls(e), include = "numeric", envir = e)
#' classFilter(ls(e), include = "numeric", exclude = "integer", envir = e)
#' classFilter(ls(e), include = "lm", envir = e)
#' classFilter(ls(e), include = "lm", exclude = "glm", envir = e)
#' rm(a, b, d, f, g, h, envir = e)
#' rm(e)
#'
setGeneric("classFilter", function(x, include, exclude, envir) {
  standardGeneric("classFilter")
})

#' @rdname classFilter
setMethod(
  "classFilter",
  signature(x = "character", include = "character", exclude = "character",
            envir = "environment"),
  definition = function(x, include, exclude, envir) {
    f <- function(w) {
      # -------------------- #
      # using `inherits` doesn't work as expected in some cases,
      #  so we tweak the 'include' to work with those cases:
      if ( ("numeric" %in% include) &
           (inherits(get(w, envir = envir), "integer")) ) {
             include <- c(include, "integer")
      }
      # --- end tweaking --- #

      if (is.na(exclude)) {
        inherits(get(w, envir = envir), include)
      } else {
        inherits(get(w, envir = envir), include) &
          !inherits(get(w, envir = envir), exclude)
      }
    }
    return(Filter(f, x))
})

#' @rdname classFilter
setMethod(
  "classFilter",
  signature(x = "character", include = "character", exclude = "character",
            envir = "missing"),
  definition = function(x, include, exclude) {
    return(classFilter(x, include, exclude, envir = sys.frame(-1)))
})

#' @rdname classFilter
setMethod(
  "classFilter",
  signature(x = "character", include = "character", exclude = "missing",
            envir = "environment"),
  definition = function(x, include, envir) {
    return(classFilter(x, include, exclude = NA_character_, envir = envir))
})

#' @rdname classFilter
setMethod(
  "classFilter",
  signature(x = "character", include = "character", exclude = "missing",
            envir = "missing"),
  definition = function(x, include) {
    return(classFilter(x, include, exclude = NA_character_, envir = sys.frame(-1)))
})

################################################################################
#' Sort a any named object with dotted names first
#'
#' Internal use only. This exists so Windows and Linux machines can have
#' the same order after a sort.
#'
#' @param obj  An arbitrary R object for which a \code{names} function
#'              returns a character vector.
#'
#' @return The same object as \code{obj}, but sorted with .objects first.
#'
#' @include simList-class.R
#' @docType methods
#' @rdname sortDotsFirst
#' @author Eliot McIntire
sortDotsFirst <- function(obj) {
  dotObjs <- grep("^\\.", names(obj))
  append(obj[dotObjs][order(names(obj[dotObjs]))],
         obj[-dotObjs][order(names(obj[-dotObjs]))])
}

#' Compare module version against SpaDES package version and warn if incompatible
#'
#' Performs a basic check to ensure the module version is compatible with the
#' SpaDES package version.
#' Compatibility is best assured when both versions are equal.
#' If module version < spades version, there is likely no problem, as SpaDES
#' should be backwards compatible.
#' However, if module version > spades version, the user needs to update their
#' version of SpaDES because module compatibility cannot be assured.
#'
#' @param moduleName     Character string providing the module name.
#' @param moduleVersion  The module version as either a character, numeric, or
#'                       numeric version (e.g., extracted from module metadata).
#'                       Is coerced to \code{numeric_version}.
#' @return Logical (invisibly) indicating whether the module is compatible with
#' the version of the SpaDES package.
#' Will also produce a warning if not compatible.
#'
#' @author Alex Chubaty
#'
setGeneric("versionWarning", function(moduleName, moduleVersion) {
  standardGeneric("versionWarning")
})

#' @rdname versionWarning
setMethod(
  "versionWarning",
  signature(moduleName = "character", moduleVersion = "ANY"),
  definition = function(moduleName, moduleVersion) {
    moduleVersion <- as.numeric_version(moduleVersion)
    pkgVersion <- packageVersion("SpaDES")

    isOK <- (pkgVersion >= moduleVersion)

    if (!isOK) {
      warning("Module version (", moduleVersion, ") does not match ",
              "SpaDES package version (", pkgVersion, ").\n")
    }
    return(invisible(isOK))
})
