#' SpatialPointsNamed class
#'
#' This new class extends \code{SpatialPoints} by adding a slot
#' called \code{name}.
#'
#' Using the simulation visualization features in \code{Plot} requires
#' objects to be plotted to have a name set.
#'
#' @slot name The name of the object.
#'
#' @seealso \code{\link{SpatialPoints}}
#'
#' @rdname SpatialPointsNamed-class
# @importClassesFrom sp SpatialPoints
#' @import sp
#' @exportClass SpatialPointsNamed
#'
setClass("SpatialPointsNamed",
         slots=list(name="character"),
         prototype=list(name=NA_character_),
         contains="SpatialPoints",
         validity=function(object) {
           # check for valid sim times and make default list
           if (is.na(object@name)) {
             stop("name must be provided")
           }
})

##############################################################
#' Create a new \code{SpatialPointsNamed} object.
#'
#' @param ... Additional arguments to \code{SpatialPoints}
#'
#' @param name  The name of the object.
#'
#' @return Returns an object of class \code{SpatialPointsNamed}.
#'
#' @seealso \code{\link{SpatialPoints}}
#'
#' @import sp
#' @export
#' @docType methods
#' @rdname SpatialPointsNamed-method
#'
#' @author Eliot McIntire
#' @author Alex Chubaty
setGeneric("SpatialPointsNamed", function(..., name) {
             standardGeneric("SpatialPointsNamed")
})


#' @rdname SpatialPointsNamed-method
#' @export
setMethod("SpatialPointsNamed",
          signature="character",
          definition=function(..., name) {
            obj <- SpatialPoints(...)
            name(obj) <- name
            return(obj)
})

### show is already defined in the methods package
#' show SpatialPointsNamed
#' @export
setMethod("show",
          signature="SpatialPointsNamed",
          definition=function(object) {
            out = list()
            out[[1]] = capture.output(print(object,
                                            quote=FALSE, row.names=FALSE))
            out[[2]] = capture.output(cat(paste("name        :",object@name)))

            ### print result
            cat(unlist(out), fill=FALSE, sep="\n")
})

#' SpatialPointsDataFrameNamed class
#'
#' This new class extends \code{SpatialPointsDataFrame} by adding a slot
#' called \code{name}.
#'
#' Using the simulation visualization features in \code{Plot} requires
#' objects to be plotted to have a name set.
#'
#' @slot name The name of the object.
#'
#' @seealso \code{\link{SpatialPointsDataFrame}}
#'
#' @rdname SpatialPointsDataFrameNamed-class
# @importClassesFrom sp SpatialPointsDataFrame
#' @import sp
#' @exportClass SpatialPointsDataFrameNamed
#'
setClass("SpatialPointsDataFrameNamed",
         slots=list(name="character"),
         prototype=list(name=NA_character_),
         contains="SpatialPointsDataFrame",
         validity=function(object) {
           # check for valid sim times and make default list
           if (is.na(object@name)) {
             stop("name must be provided")
           }
})

##############################################################
#' Create a new \code{SpatialPointsDataFrameNamed} object.
#'
#' @param ... Additional arguments to \code{SpatialPointsDataFrame}
#'
#' @param name  The name of the object.
#'
#' @return Returns an object of class \code{SpatialPointsDataFrameNamed}.
#'
#' @seealso \code{\link{SpatialPointsDataFrame}}
#'
#' @import sp
#' @docType methods
#' @rdname SpatialPointsDataFrameNamed-method
#'
#' @author Eliot McIntire
#' @author Alex Chubaty
#'
#' @export
setGeneric("SpatialPointsDataFrameNamed", function(..., name) {
             standardGeneric("SpatialPointsDataFrameNamed")
 })

#' @rdname SpatialPointsDataFrameNamed-method
#' @export
setMethod("SpatialPointsDataFrameNamed",
          signature="character",
          definition=function(..., name) {
            obj <- SpatialPointsDataFrame(...)
            name(obj) <- name
            return(obj)
})

### show is already defined in the methods package
#' show SpatialPointsDataFrameNamed
#' @export
setMethod("show",
          signature="SpatialPointsDataFrameNamed",
          definition=function(object) {
            out = list()
            out[[1]] = capture.output(print(object, quote=FALSE, row.names=FALSE))
            out[[2]] = capture.output(cat(paste("name        :", object@name)))

            ### print result
            cat(unlist(out), fill=FALSE, sep="\n")
})

#' RasterStackNamed class
#'
#' This new class extends \code{RasterStack} by adding a slot
#' called \code{name}.
#'
#' Using the simulation visualization features in \code{Plot} requires
#' objects to be plotted to have a name set.
#'
#' @slot name The name of the object.
#'
#' @seealso \code{\link{RasterStack}}
#'
#' @rdname RasterStackNamed-class
# @importClassesFrom raster RasterStack
#' @import raster
#' @exportClass RasterStackNamed
#'
setClass("RasterStackNamed",
         slots=list(name="character"),
         prototype=list(name=NA_character_),
         contains="RasterStack",
         validity=function(object) {
           # check for valid sim times and make default list
           if (is.na(object@name)) {
             stop("name must be provided")
           }
})


##############################################################
#' Create a new \code{RasterStackNamed} object.
#'
#' @param ... Additional arguments to \code{RasterStack}
#'
#' @param name  The name of the object.
#'
#' @return Returns an object of class \code{RasterStackNamed}.
#'
#' @seealso \code{\link{RasterStackNamed}}
#'
#' @export
#' @docType methods
#' @rdname RasterStackNamed-method
#'
#' @author Eliot McIntire
#' @author Alex Chubaty
#'
setGeneric("RasterStackNamed", signature=c("..."), function(..., name) {
             standardGeneric("RasterStackNamed")
})

#' @rdname RasterStackNamed-method
#' @export
setMethod("RasterStackNamed",
          signature="RasterStack",
          definition= function(..., name) {
            new("RasterStackNamed", ..., name=name)
})

### show is already defined in the methods package
#' show RasterStackNamed
#' @export
setMethod("show",
          signature="RasterStackNamed",
          definition=function(object) {
            cat("class       :", class(object), "\n")
            if (rotated(object)) {
              cat("rotated     : TRUE\n")
            }
            mnr <- 15
            if (filename(object) != "") {
              cat("filename    :", filename(object), "\n")
            }
            nl <- nlayers(object)
            if (nl == 0) {
              cat("nlayers     :", nl, "\n")
            }
            else {
              cat("dimensions  : ", nrow(object), ", ", ncol(object),
                  ", ", ncell(object), ", ", nl, "  (nrow, ncol, ncell, nlayers)\n",
                  sep = "")
              cat("resolution  : ", xres(object), ", ", yres(object),
                  "  (x, y)\n", sep = "")
              cat("extent      : ", object@extent@xmin, ", ", object@extent@xmax,
                  ", ", object@extent@ymin, ", ", object@extent@ymax,
                  "  (xmin, xmax, ymin, ymax)\n", sep = "")
              cat("name        :", name(object), "\n")
              cat("coord. ref. :", projection(object, TRUE), "\n")
              ln <- names(object)
              if (nl > mnr) {
                ln <- c(ln[1:mnr], "...")
              }
              n <- nchar(ln)
              if (nl > 5) {
                b <- n > 26
                if (any(b)) {
                  ln[b] <- paste(substr(ln[b], 1, 9), "//", substr(ln[b],
                                                                   nchar(ln[b]) - 9, nchar(ln[b])), sep = "")
                }
              }
              minv <- format(minValue(object))
              maxv <- format(maxValue(object))
              minv <- gsub("Inf", "?", minv)
              maxv <- gsub("-Inf", "?", maxv)
              if (nl > mnr) {
                minv <- c(minv[1:mnr], "...")
                maxv <- c(maxv[1:mnr], "...")
              }
              w <- pmax(nchar(ln), nchar(minv), nchar(maxv))
              m <- rbind(ln, minv, maxv)
              for (i in 1:ncol(m)) {
                m[, i] <- format(m[, i], width = w[i], justify = "right")
              }
              cat("names       :", paste(m[1, ], collapse = ", "),
                  "\n")
              cat("min values  :", paste(m[2, ], collapse = ", "),
                  "\n")
              cat("max values  :", paste(m[3, ], collapse = ", "),
                  "\n")
            }
            z <- getZ(object)
            if (length(z) > 0) {
              name <- names(object@z)
              if (is.null(name))
                name <- "z-value"
              if (name == "")
                name <- "z-value"
              name <- paste(sprintf("%-12s", name), ":", sep = "")
              if (length(z) < mnr) {
                cat(name, paste(as.character(z), collapse = ", "),
                    "\n")
              }
              else {
                z <- range(z)
                cat(name, paste(as.character(z), collapse = " - "),
                    "(range)\n")
              }
            }
            cat("\n")
            #             out = list()
            #             out[[1]] = capture.output(show(object))
            #             out[[2]] = capture.output(cat(paste("name        :",object@name)))
            #
            #             ### print result
            #             cat(unlist(out), fill=FALSE, sep="\n")
})

#' RasterLayerNamed class
#'
#' This new class extends \code{RasterLayer} by adding a slot
#' called \code{name}.
#'
#' Using the simulation visualization features in \code{Plot} requires
#' objects to be plotted to have a name set.
#'
#' @slot name The name of the object.
#'
#' @seealso \code{\link{RasterLayer}}
#'
#' @rdname RasterLayerNamed-class
# @importClassesFrom raster RasterLayer
#' @import raster
#' @exportClass RasterLayerNamed
#'
setClass("RasterLayerNamed",
         slots=list(name="character"),
         prototype=list(name=NA_character_),
         contains="RasterLayer",
         validity=function(object) {
           # check for valid sim times and make default list
           if (is.na(object@name)) {
             stop("name must be provided")
           }
         })


##############################################################
#' Create a new \code{RasterLayerNamed} object.
#'
#' @param ... Additional arguments to \code{RasterLayer}
#'
#' @param name  The name of the object.
#'
#' @return Returns an object of class \code{RasterLayerNamed}.
#'
#' @seealso \code{\link{RasterLayerNamed}}
#'
#' @export
#' @docType methods
#' @rdname RasterLayerNamed-method
#'
#' @author Eliot McIntire
#' @author Alex Chubaty
#'
setGeneric("RasterLayerNamed", signature=c("..."), function(..., name) {
  standardGeneric("RasterLayerNamed")
})

#' @rdname RasterLayerNamed-method
#' @export
setMethod("RasterLayerNamed",
          signature="RasterLayer",
          definition= function(..., name) {
            .Object <- new("RasterLayerNamed", ..., name=name)
            #.Object <- new("RasterLayerNamed", object, name=value)
            names(.Object) <- name
            return(.Object)
          })

### show is already defined in the methods package
#' show RasterLayerNamed
#' @export
setMethod("show",
          signature="RasterLayerNamed",
          definition=function(object) {
            cat("class       :", class(object), "\n")
            if (rotated(object)) {
              cat("rotated     : TRUE\n")
            }
            mnr <- 15
            if (filename(object) != "") {
              cat("filename    :", filename(object), "\n")
            }
            nl <- nlayers(object)
            if (nl == 0) {
              cat("nlayers     :", nl, "\n")
            }
            else {
              cat("dimensions  : ", nrow(object), ", ", ncol(object),
                  ", ", ncell(object), ", ", nl, "  (nrow, ncol, ncell, nlayers)\n",
                  sep = "")
              cat("resolution  : ", xres(object), ", ", yres(object),
                  "  (x, y)\n", sep = "")
              cat("extent      : ", object@extent@xmin, ", ", object@extent@xmax,
                  ", ", object@extent@ymin, ", ", object@extent@ymax,
                  "  (xmin, xmax, ymin, ymax)\n", sep = "")
              cat("name        :", name(object), "\n")
              cat("coord. ref. :", projection(object, TRUE), "\n")
              ln <- names(object)
              if (nl > mnr) {
                ln <- c(ln[1:mnr], "...")
              }
              n <- nchar(ln)
              if (nl > 5) {
                b <- n > 26
                if (any(b)) {
                  ln[b] <- paste(substr(ln[b], 1, 9), "//", substr(ln[b],
                                                                   nchar(ln[b]) - 9, nchar(ln[b])), sep = "")
                }
              }
              minv <- format(minValue(object))
              maxv <- format(maxValue(object))
              minv <- gsub("Inf", "?", minv)
              maxv <- gsub("-Inf", "?", maxv)
              if (nl > mnr) {
                minv <- c(minv[1:mnr], "...")
                maxv <- c(maxv[1:mnr], "...")
              }
              w <- pmax(nchar(ln), nchar(minv), nchar(maxv))
              m <- rbind(ln, minv, maxv)
              for (i in 1:ncol(m)) {
                m[, i] <- format(m[, i], width = w[i], justify = "right")
              }
              cat("names       :", paste(m[1, ], collapse = ", "),
                  "\n")
              cat("min values  :", paste(m[2, ], collapse = ", "),
                  "\n")
              cat("max values  :", paste(m[3, ], collapse = ", "),
                  "\n")
            }
            z <- getZ(object)
            if (length(z) > 0) {
              name <- names(object@z)
              if (is.null(name))
                name <- "z-value"
              if (name == "")
                name <- "z-value"
              name <- paste(sprintf("%-12s", name), ":", sep = "")
              if (length(z) < mnr) {
                cat(name, paste(as.character(z), collapse = ", "),
                    "\n")
              }
              else {
                z <- range(z)
                cat(name, paste(as.character(z), collapse = " - "),
                    "(range)\n")
              }
            }
            cat("\n")
            #             out = list()
            #             out[[1]] = capture.output(show(object))
            #             out[[2]] = capture.output(cat(paste("name        :",object@name)))
            #
            #             ### print result
            #             cat(unlist(out), fill=FALSE, sep="\n")
          })



#' @exportClass namedSpatialPoints
setClassUnion("namedSpatialPoints", c("SpatialPointsNamed", "SpatialPointsDataFrameNamed"))

#' @exportClass spatialObjects
setClassUnion("spatialObjects", c("SpatialPointsNamed","SpatialPointsDataFrameNamed",
                                  "RasterLayer", "RasterStack"))

################################################################################
#' Get and set the name of an object.
#'
#' Using the simulation visualization features in \code{Plot} requires
#' objects to be plotted to have a name set. Currently, required of:
#' \code{SpatialPoints}, \code{SpatialPointsDataFrame}, and \code{RasterStack}
#' objects. Adding a name to an object of these classes converts these objects
#' to a \code{*Named} class.
#'
#' @param ... Additional arguments.
#'
#' @author Eliot McIntire
#' @author Alex Chubaty
#'
#' @name name
#' @export
#' @docType methods
#' @rdname name-methods
#'
#' @seealso \code{\link{SpatialPointsNamed}},
#'          \code{\link{SpatialPointsDataFrameNamed}},
#'          \code{\link{RasterStackNamed}}.
#'          \code{\link{RasterLayerNamed}}.
#'
setGeneric("name", function(object) {
  standardGeneric("name")
})

#' @export
#' @rdname name-methods
setMethod("name",
          signature="SpatialPointsNamed",
          definition=function(object) {
            return(object@name)
})

#' @export
#' @rdname name-methods
setMethod("name",
          signature="SpatialPointsDataFrameNamed",
          definition=function(object) {
            return(object@name)
})

#' @export
#' @rdname name-methods
setMethod("name",
          signature="RasterStackNamed",
          definition=function(object) {
            return(object@name)
})

#' @export
#' @rdname name-methods
setMethod("name",
          signature="RasterLayerNamed",
          definition=function(object) {
            return(object@name)
          })

#' @export
#' @name name<-
#' @rdname name-methods
setGeneric("name<-", function(object, value) {
             standardGeneric("name<-")
 })

#' @export
#' @name name<-
#' @rdname name-methods
setReplaceMethod("name",
                 signature="SpatialPointsNamed",
                 definition=function(object, value) {
                   object@name <- value
                   validObject(object)
                   return(object)
})

#' @export
#' @name name<-
#' @rdname name-methods
setReplaceMethod("name",
                 signature="SpatialPoints",
                 definition=function(object, value) {
                   new("SpatialPointsNamed", object, name=value)
})

#' @export
#' @name name<-
#' @rdname name-methods
setReplaceMethod("name",
                 signature="SpatialPointsDataFrameNamed",
                 definition=function(object, value) {
                   object@name <- value
                   validObject(object)
                   return(object)
})

#' @export
#' @name name<-
#' @rdname name-methods
setReplaceMethod("name",
                 signature="SpatialPointsDataFrame",
                 definition=function(object, value) {
                   new("SpatialPointsDataFrameNamed", object, name=value)
})

#' @export
#' @name name<-
#' @rdname name-methods
setReplaceMethod("name",
                 signature="RasterStackNamed",
                 definition=function(object, value) {
                   object@name <- value
                   validObject(object)
                   return(object)
})

#' @export
#' @name name<-
#' @rdname name-methods
setReplaceMethod("name",
                 signature="RasterStack",
                 definition=function(object, value) {
                   new("RasterStackNamed", object, name=value)
})

#' @export
#' @name name<-
#' @rdname name-methods
setReplaceMethod("name",
                 signature="RasterLayerNamed",
                 definition=function(object, value) {
                   object@name <- value
                   names(object) <- value
                   validObject(object)
                   return(object)
                 })

#' @export
#' @name name<-
#' @rdname name-methods
setReplaceMethod("name",
                 signature="RasterLayer",
                 definition=function(object, value) {
                   .Object <- new("RasterLayerNamed", object, name=value)
                   names(.Object) <- value
                   return(.Object)
                 })
