##############################################################
#' Get colours for plotting Raster* objects.
#'
#' @param object     A \code{Raster*} object.
#'
#' @return Returns a named list of colors.
#'
#' @export
#' @docType methods
#' @rdname getColors-method
#' @aliases getColours
#' @aliases getColors
#'
#' @seealso \code{\link{setColors<-}}, \code{\link[RColorBrewer]{brewer.pal}}
#'
#' @author Alex Chubaty
#'
setGeneric("getColors", function(object) {
  standardGeneric("getColors")
})

#' @rdname getColors-method
setMethod("getColors",
          signature="Raster",
          definition=function(object) {
            cols <- lapply(names(object), function(x) {
              as.character(object[[x]]@legend@colortable)
            })
            names(cols) <- names(object)
            return(cols)
})

##############################################################
#' Set colours for plotting Raster* objects.
#'
#' @param object     A \code{Raster*} object.
#'
#' @param ...   Additional arguments to \code{colorRampPalette}.
#'
#' @param n     An optional vector of values specifiying the number
#'              of levels from which to interpolate the color palette.
#'
#' @param value  Named list of hex color codes (e.g., from
#'               \code{RColorBrewer::brewer.pal}), corresponding to the names
#'               of RasterLayers in \code{x}.
#'
#' @return Returns a Raster with the colortable slot set to values.
#'
#' @export
#' @docType methods
#' @rdname setColors-method
#'
#' @seealso \code{\link[RColorBrewer]{brewer.pal}},
#'          \code{\link[grDevices]{colorRampPalette}}.
#'
#' @author Alex Chubaty
#'
setGeneric("setColors<-",
           function(object, ..., n, value) {
             standardGeneric("setColors<-")
})

#' @export
#' @rdname setColors-method
#' @docType methods
setReplaceMethod("setColors",
                 signature("RasterLayer", "numeric", "character"),
                 function(object, ..., n, value) {
                   pal <- colorRampPalette(value, alpha=TRUE, ...)
                   object@legend@colortable <- pal(n)
                   validObject(object)
                   return(object)
})

#' @export
#' @rdname setColors-method
#' @docType methods
setReplaceMethod("setColors",
                 signature("RasterLayer", "missing", "character"),
                 function(object, ..., value) {
                   n <- round((maxValue(object)-minValue(object)))+1
                   pal <- colorRampPalette(value, alpha=TRUE, ...)
                   object@legend@colortable <- pal(n)
                   validObject(object)
                   return(object)
})

#' @export
#' @rdname setColors-method
#' @docType methods
setReplaceMethod("setColors",
                 signature("Raster", "numeric", "list"),
                 function(object, ..., n, value) {
                   i <- which(names(object) %in% names(value))
                   for(x in names(object)[i]) {
                     setColors(object[[x]], ..., n=n) <- value[[x]]
                   }
                   validObject(object)
                   return(object)
})

#' @export
#' @rdname setColors-method
#' @docType methods
setReplaceMethod("setColors",
                 signature("Raster", "missing", "list"),
                 function(object, ..., value) {
                   i <- which(names(object) %in% names(value))
                   for(x in names(object)[i]) {
                     setColors(object[[x]], ...) <- value[[x]]
                   }
                   validObject(object)
                   return(object)
})
