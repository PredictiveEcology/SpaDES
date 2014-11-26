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
#' @seealso \code{\link{setColors}}, \code{\link{brewer.pal}}
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
#' @param value  Named list of hex color codes (e.g., from \code{RColorBrewer::brewer.pal}),
#'              corresponding to the names of RasterLayers in \code{x}.
#'
#' @return Returns a Raster with the colortable slot set to values.
#'
#' @import RColorBrewer
#' @export
# @name setColors<-
#' @docType methods
#' @rdname setColors-method
#' @aliases setColors
#' @aliases setColours
#'
#' @seealso \code{\link{brewer.pal}}, \code{\link{colorRampPalette}}.
#'
#' @author Alex Chubaty
#'
setGeneric("setColors<-",
           function(object, ..., n, value) {
             standardGeneric("setColors<-")
})

#' @name setColors<-
#' @export
#' @aliases setColors<-RasterLayer,numeric,character-method
#' @rdname setColors-method
#' @docType methods
setReplaceMethod("setColors",
                 signature("RasterLayer", "numeric", "character"),
                 function(object, ..., n, value) {
                   pal <- colorRampPalette(value, alpha=TRUE)
                   object@legend@colortable <- pal(n)
                   validObject(object)
                   return(object)
})

#' @name setColors<-
#' @export
#' @aliases setColors<-RasterLayer,missing,character-method
#' @rdname setColors-method
#' @docType methods
setReplaceMethod("setColors",
                 signature("RasterLayer", "missing", "character"),
                 function(object, ..., value) {
                   n <- round((maxValue(object)-minValue(object)))+1
                   pal <- colorRampPalette(value, alpha=TRUE)
                   object@legend@colortable <- pal(n)
                   validObject(object)
                   return(object)
})

#' @name setColors<-
#' @export
#' @aliases setColors<-Raster,numeric,list-method
#' @rdname setColors-method
#' @docType methods
setReplaceMethod("setColors",
                 signature("Raster", "numeric", "list"),
                 function(object, ..., n, value) {
                   for(x in names(object)) {
                     setColors(object[[x]], ..., n=n) <- value[[x]]
                   }
                   validObject(object)
                   return(object)
})

#' @name setColors<-
#' @export
#' @aliases setColors<-Raster,missing,list-method
#' @rdname setColors-method
#' @docType methods
setReplaceMethod("setColors",
                 signature("Raster", "missing", "list"),
                 function(object, ..., value) {
                   for(x in names(object)) {
                     setColors(object[[x]], ...) <- value[[x]]
                   }
                   validObject(object)
                   return(object)
})


