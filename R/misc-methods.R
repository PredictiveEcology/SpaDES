##############################################################
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
#' @rdname checkpath
#'
# @examples
# need examples
checkPath = function(path, create=FALSE) {
    if (is.character(path)) {
        # check if path has a trailing slash and remove it
        strlets <- strsplit(path, "")[[1]]
        strlen <- length(strlets)
        if (strlets[strlen]=="/") {
            path <- cat("\"",strlets[-strlen], "\"", sep="")
        } else {}
        
        if (file.exists(path)) {
            exists = TRUE # basically, do nothing if it exists
        } else {
            print("Warning: the specified path doesn't exist...")
            if (create==TRUE) {
                print(paste("...creating directory structure:", path))
                dir.create(file.path(path), recursive=TRUE, showWarnings=FALSE)
            } else {
                print("...please create it and try again.")
            }
        }
    } else {
        stop("Error: `path` should be specified as a character string.")
    }
    return(path)
}
