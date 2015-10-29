### TEMPORARY FIX FOR ISSUE #209 -- FIXES RSTUDIO BUG
### https://github.com/achubaty/rstudio/commit/378ae10a29a3c6a0eb7680ff7f99d495b44aed9c
### pull request to come, pending copyright transfer agreement form

options(editor = function(name, file, title) {

  # use internal editor for files and functions, otherwise
  # delegate to the default editor
  if (missing(name) || is.null(name) || is.function(name)) {

    # if no name then use file
    if (missing(name) || is.null(name)) {
      if (!is.null(file) && nzchar(file))
        targetFile <- file
      else
        targetFile <- scratchFile
    }
    # otherwise it's a function, write it to a file for editing
    else {
      functionSrc <- .rs.deparseFunction(name, TRUE, FALSE)
      targetFile <- scratchFile
      writeLines(functionSrc, targetFile)
    }

    # invoke the RStudio editor on the file
    if (.Call("rs_editFile", targetFile)) {

      # try to parse it back in
      newFunc <- try(eval.parent(parse(targetFile)),
                     silent = TRUE)
      if (inherits(newFunc, "try-error")) {
        stop(newFunc, "You can attempt to correct the error using ",
             title, " = edit()")
      }

      return(newFunc)
    }
    else {
      stop("Error occurred while editing function '", name, "'")
    }
  }
  else
    edit(name, file, title, editor=defaultEditor)
})
