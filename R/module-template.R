################################################################################
#' Create new module from template.
#'
#' Autogenerate a skeleton for a new SpaDES module, a template for a
#' documentation file, a citation file, a license file, and a readme.txt file.
#' The \code{newModuleDocumentation} will not generate the module file, but will
#' create the other 4 files.
#'
#' All 5 (or 4, if using \code{newModuleDocumentation}) files will be created
#' within a subfolder named \code{name} within the \code{path}.
#'
#' @param name  Character string. Your module's name.
#'
#' @param path  Character string. Subdirectory in which to place the new module code file.
#'              The default is the current working directory.
#'
#' @param open  Logical. Should the new module file be opened after creation?
#'              Default \code{TRUE}.
#'
#' @return Nothing is returned. The new module file is created at \code{path/name.R}, as
#' well as anciliary files for documentation, citation, license, and readme.
#'
#' @export
#' @docType methods
#' @rdname newModule
# @importFrom utils file.edit
#' @author Alex Chubaty and Eliot McIntire
#'
#' @examples
#' \dontrun{
#'   ## create a "myModule" module in the "modules" subdirectory.
#'   newModule("myModule", "modules")
#' }
setGeneric("newModule", function(name, path, open) {
  standardGeneric("newModule")
})

#' @export
#' @rdname newModule
setMethod("newModule",
          signature=c(name="character", path="character", open="logical"),
          definition = function(name, path, open) {
            path <- checkPath(path, create=TRUE)
            nestedPath <- file.path(path, name)
            checkPath(nestedPath, create=TRUE)
            filenameR <- file.path(nestedPath, paste0(name, ".R"))

            cat("
defineModule(sim, list(
  name=\"", name, "\",
  description=\"insert module description here\",
  keywords=c(\"insert key words here\"),
  authors=c(person(c(\"First\", \"Middle\"), \"Last\", email=\"email@example.com\", role=c(\"aut\", \"cre\"))),
  childModules=character(),
  version=numeric_version(\"0.0.0\"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c(NA, NA)),
  timeunit=NA_character_, # e.g., \"year\"
  citation=list(),
  reqdPkgs=list(),
  parameters=rbind(
    defineParameter(\".plotInitialTime\", \"numeric\", NA, NA, NA, \"This describes the simulation time at which the first plot event should occur\"),
    defineParameter(\".saveInitialTime\", \"numeric\", NA, NA, NA, \"This describes the simulation time at which the first save event should occur\")),
    #defineParameter(\"paramName\", \"paramClass\", value, min, max, \"parameter description\")),
  inputObjects=data.frame(objectName=NA_character_, objectClass=NA_character_, other=NA_character_, stringsAsFactors=FALSE),
  outputObjects=data.frame(objectName=NA_character_, objectClass=NA_character_, other=NA_character_, stringsAsFactors=FALSE)
))

## event types
#   - type `init` is required for initiliazation

doEvent.", name, " = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType==\"init\") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)

    # do stuff for this event
    sim$", name, "Init(sim)

    # schedule future event(s)
    scheduleEvent(sim, params(sim)$", name, "$.plotInitialTime, \"", name, "\", \"plot\")
    scheduleEvent(sim, params(sim)$", name, "$.saveInitialTime, \"", name, "\", \"save\")
  } else if (eventType==\"templateEvent\") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # scheduleEvent(sim, time(sim) + increment, \"", name, "\", \"templateEvent\")

    # ! ----- STOP EDITING ----- ! #
    } else {
      warning(paste(\"Undefined event type: \'\", events(sim)[1, \"eventType\", with=FALSE],
                    \"\' in module \'\", events(sim)[1, \"moduleName\", with=FALSE], \"\'\", sep=\"\"))
    }
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initilization
sim$", name, "Init = function(sim) {

  # # ! ----- EDIT BELOW ----- ! #



  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for save events
sim$", name, "Save = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
sim$", name, "Plot = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot(\"object\")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
sim$", name, "Event1 = function(sim) {
  # ! ----- EDIT BELOW ----- ! #



  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
sim$", name, "Event2 = function(sim) {
  # ! ----- EDIT BELOW ----- ! #



  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above\n",
            file=filenameR, fill=FALSE, sep="")
            if(open) file.edit(filenameR)

            ### Make Rmarkdown file for module documentation
            newModuleDocumentation(name=name, path=path, open=open)

})

#' @export
#' @rdname newModule
setMethod("newModule",
          signature=c(name="character", path="missing", open="logical"),
          definition = function(name, open) {
            newModule(name=name, path=".", open=open)
})

#' @export
#' @rdname newModule
setMethod("newModule",
          signature=c(name="character", path="character", open="missing"),
          definition = function(name, path) {
            newModule(name=name, path=path, open=TRUE)
})

#' @export
#' @rdname newModule
setMethod("newModule",
          signature=c(name="character", path="missing", open="missing"),
          definition = function(name) {
            newModule(name=name, path=".", open=TRUE)
})

###########################################################################
#' @export
#' @docType methods
#' @rdname newModule
# @importFrom utils file.edit
#' @author Eliot McIntire
#'
setGeneric("newModuleDocumentation", function(name, path, open) {
  standardGeneric("newModuleDocumentation")
})

#' @export
#' @rdname newModule
setMethod("newModuleDocumentation",
          signature=c(name="character", path="character", open="logical"),
          definition = function(name, path, open) {
            path <- checkPath(path, create=TRUE)
            nestedPath <- file.path(path, name)
            checkPath(nestedPath, create=TRUE)
            filenameRmd <- file.path(nestedPath, paste0(name, ".Rmd"))
            filenameCitation <- file.path(nestedPath, "citation.bib")
            filenameLICENSE <- file.path(nestedPath, "LICENSE")
            filenameREADME <- file.path(nestedPath, "README.txt")

### Make Rmarkdown file for module documentation
cat(
"---
title: \"",name,"\"
author: \"Module Author\"
date: \"", format(Sys.Date(), "%d %B %Y"), "\"
output: pdf_document
---

Module documentation should be written so that others can use your module.
This is a template for module documentation, and should be changed.

## Plotting

Write what is plotted.

## Saving

Write what is saved.

## Input data

## Output data

## Anticipated linkages to other modules

## Other Markdown help
For help writing in RMarkdown, see http://rmarkdown.rstudio.com/. We have also included
The `html_vignette` template includes a basic CSS theme. To override this theme you can specify your own CSS in the document metadata as follows:

output:
rmarkdown::html_vignette:
css: mystyles.css

## Figures

The figure sizes have been customised so that you can easily put two images side-by-side.

```{r, fig.show='hold'}
plot(1:10)
plot(10:1)
```

You can enable figure captions by `fig_caption: yes` in YAML:

output:
rmarkdown::html_vignette:
fig_caption: yes

Then you can use the chunk option `fig.cap = \"Your figure caption.\"` in **knitr**.

## More Examples

You can write math expressions, e.g. $Y = X\\beta + \\epsilon$, footnotes^[A footnote here.], and tables, e.g. using `knitr::kable()`.

```{r, echo=FALSE, results='asis'}
knitr::kable(head(mtcars, 10))
```

Also a quote using `>`:

> \"He who gives up [code] safety for [code] speed deserves neither.\"
([via](https://twitter.com/hadleywickham/status/504368538874703872))
",
file=filenameRmd, fill=FALSE, sep="")

### Make citation.bib file
cat("
@Manual{,
title = {",name,"},
author = {{Authors}},
organization = {Organization},
address = {Somewhere, Someplace},
year = {2015},
url = {},
}
",
file=filenameCitation, fill=FALSE, sep="")

### Make LICENSE file
cat("
    # Provide explicit details of the license for this module.
    # See http://choosealicense.com for help selecting one.",
    file=filenameLICENSE, fill=FALSE, sep="")

### Make README file
cat("
Any other details that a user may need to know, like where to get more information,
 where to download data etc.",
    file=filenameREADME, fill=FALSE, sep="")

# If we choose to have the pdf of the documentation file made at this stage, uncomment this.
#  Requires pandoc to be installed and working
# knit(filenameRmd, output=filenameMd)
# system(paste("pandoc",filenameMd,
#              "--to latex --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash-implicit_figures",
#              "-o", filenamePdf,
#              "--template \"C:\\Eliot\\R\\win-library\\3.1\\rmarkdown\\rmd\\latex\\default.tex\"",
#              "--highlight-style tango",
#              "--latex-engine pdflatex",
#              "--variable \"geometry:margin=1in\"
#              "))
# file.remove(filenameMd)

if(open) file.edit(filenameRmd)

})

#' @export
#' @rdname newModule
setMethod("newModuleDocumentation",
          signature=c(name="character", path="missing", open="logical"),
          definition = function(name, open) {
            newModuleDocumentation(name=name, path=".", open=open)
})

#' @export
#' @rdname newModule
setMethod("newModuleDocumentation",
          signature=c(name="character", path="character", open="missing"),
          definition = function(name, path) {
            newModuleDocumentation(name=name, path=path, open=TRUE)
})

#' @export
#' @rdname newModule
setMethod("newModuleDocumentation",
          signature=c(name="character", path="missing", open="missing"),
          definition = function(name) {
            newModuleDocumentation(name=name, path=".", open=TRUE)
})

################################################################################
#' Open all modules nested within a base directory
#'
#' This is just a convenience wrapper for openning several modules at once, recursively.
#' A module is defined as any file that ends in \code{.R} or \code{.r} and has a
#' directory name identical to its filename. Thus, this must be case sensitive.
#'
#' @param basedir  Character string of length 1. The base directory within which there are only
#' module subdirectories.
#'
#' @param names Character vector with names of modules to open. If missing, then all modules
#' will be opened within the basedir.
#'
#' @return Nothing is returned. All file are open via \code{file.edit}.
#'
#' @export
#' @docType methods
#' @rdname openModules
# @importFrom utils file.edit
#'
#' @author Eliot McIntire
#'
#' @examples
#' \dontrun{openModules("~\SpaDESModules")}
#'
setGeneric("openModules", function(basedir, names) {
  standardGeneric("openModules")
})

#' @export
#' @rdname openModules
setMethod("openModules",
          signature=c(basedir="character", names="character"),
          definition = function(basedir, names) {
            basedir <- checkPath(basedir, create=FALSE)
            origDir <- getwd()
            setwd(basedir)
            if(any(names=="all")) {
              Rfiles <- dir(pattern="[\\.][rR]$",recursive = TRUE)
            } else {
              Rfiles <- dir(pattern="[\\.][rR]$",recursive = TRUE)
              Rfiles <- Rfiles[pmatch(names,Rfiles)]
            }
            Rfiles <- Rfiles[grep(pattern="[/\\\\]",Rfiles)]
            Rfiles <- Rfiles[sapply(strsplit(Rfiles,"[/\\\\\\.]"),
                                    function(x) any(duplicated(x)))]
            lapply(Rfiles, file.edit)
            setwd(origDir)
})

#' @export
#' @rdname openModules
setMethod("openModules",
          signature=c(basedir="missing", names="missing"),
          definition = function() {
            openModules(basedir=".", names="all")
})

#' @export
#' @rdname openModules
setMethod("openModules",
          signature=c(basedir="character", names="missing"),
          definition = function(basedir) {
            openModules(basedir=basedir, names="all")
})

################################################################################
#' Create a zip archive of a module subdirectory
#'
#' The most common use of this would be from a "modules" directory, rather than
#' inside a given module.
#'
#' @param name    Character string giving the module name.
#' @param path    A file path to a directory containing the module subdirectory.
#' @param version The module version.
#' @param ...     Additional arguments to \code{\link{zip}}:
#'                e.g., add \code{"-q"} using \code{flags="-q -r9X"}
#'                (the default flags are \code{"-r9X"}).
#'
#' @author Eliot McIntire and Alex Chubaty
#'
#' @export
#' @rdname zipModule
#'
setGeneric("zipModule", function(name, path, version, ...) {
  standardGeneric("zipModule")
})

#' @export
# @importFrom utils zip
#' @rdname zipModule
setMethod("zipModule",
signature=c(name="character", path="character", version="character"),
definition = function(name, path, version, ...) {
  # If we choose to have the pdf of the documentation file made at this stage, uncomment this.
  #  Requires pandoc to be installed and working

  path <- checkPath(path, create=FALSE)

  callingWd <- getwd()
  on.exit(setwd(callingWd))
  setwd(path)
  zipFileName=paste0(name, "_", version, ".zip")
  print(paste("Zipping module into zip file"))
  zip(zipFileName, files=file.path(name), extras=c("-x","*.zip"), ...)
  file.copy(zipFileName, to=paste0(name, "/", zipFileName), overwrite=TRUE)
  file.remove(zipFileName)
})

#' @rdname zipModule
#' @export
setMethod("zipModule",
          signature=c(name="character", path="missing", version="character"),
          definition = function(name, version, ...) {
            zipModule(name=name, path=".", version=version, ...)
})

#' @export
#' @rdname zipModule
setMethod("zipModule",
          signature=c(name="character", path="missing", version="missing"),
          definition = function(name, ...) {
            zipModule(name=name, path=".", version="0.0.0", ...)
})
