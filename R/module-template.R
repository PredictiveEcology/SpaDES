##############################################################
#' Create new module from template.
#'
#' Autogenerate a skeleton for a new SpaDES module.
#'
#' @param name  Character string. Your module's name.
#'
#' @param path  Character string. Subdirectory in which to place the new module code file.
#'              The default is the current working directory.
#'
#' @param open  Logical. Should the new module file be opened after creation?
#'              Default \code{TRUE}.
#'
#' @return Nothing is returned. The new module is created at \code{path/name.R}.
#'
#' @export
#' @docType methods
#' @rdname newModule-method
#'
#' @author Alex Chubaty
#'
#' @examples
#' \dontrun{## create a "fastfood" module in the "modules" subdirectory.}
#' \dontrun{newModule("fastfood", "modules")}
#'
setGeneric("newModule", function(name, path, open) {
  standardGeneric("newModule")
})

#' @rdname newModule-method
setMethod("newModule",
          signature=c(name="character", path="character", open="logical"),
          definition = function(name, path, open) {
            path <- checkPath(path, create=TRUE)
            nestedPath <- file.path(path, name)
            checkPath(nestedPath, create=TRUE)
            filenameR <- file.path(nestedPath, paste0(name, ".R"))
            filenameRmd <- file.path(nestedPath, paste0(name, ".Rmd"))
            filenameCitation <- file.path(nestedPath, paste0(name, ".citation.bib"))
            filenameLICENSE <- file.path(nestedPath, "LICENSE")
            filenameREADME <- file.path(nestedPath, "README.txt")


            cat("
### Specify module (and dependencies) definitions:
###
### name:         ", name, "
###
### description:  <provide module description>
###
### keywords:     <provide module keywords>
###
### authors:      <author name(s) and email address(es)>
###
### version:      0.0.0
###
### spatialExtent: NA
###
### timeframe:    NA
###
### timestep:     NA
###
### translators:  NA
###
### citation:     NA
###
### reqdPkgs:     NA
###
### inputObjects: NA
###
### outputObjects: objectName: NA
###                objectClass: NA
###
defineModule(list(
  name=\"", name, "\",
  description=\"insert module description here\",
  keywords=c(\"insert key words here\"),
  authors=c(person(c(\"First\", \"Middle\"), \"Last\", email=\"email@example.com\", role=c(\"aut\", \"cre\"))),
  version=numeric_version(\"0.0.0\"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c(NA, NA)),
  translators=list(),
  timestep=NA_character_,
  citation=list(),
  reqdPkgs=list(),
  inputObjects=data.frame(name=NA_character_, class=NA_character_),
  outputObjects=data.frame(name=NA_character_, class=NA_character_)
))

### event functions:
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - module name and this filename must match;
#   - keep event functions short and clean, modularize by calling
#       subroutines from section below.

### template event
doEvent.", name, " = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType==\"init\") {
    ### check for object dependencies:
    ### (use `checkObject` or similar)


    # if a required module isn't loaded yet,
    # reschedule this module init for later
    if (reloadModuleLater(sim, depends)) {
      sim <- scheduleEvent(sim, simCurrentTime(sim), \"", name, "\", \"init\")
    } else {
      sim <- ", name, "Init(sim)
    }
    sim <- scheduleEvent(sim, simParams(sim)$", name, "$.plotInitialTime, \"", name, "\", \"plot\")
    sim <- scheduleEvent(sim, simParams(sim)$", name, "$.saveInitialTime, \"", name, "\", \"save\")
  } else if (eventType==\"templateEvent\") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, simCurrentTime(sim) + increment, \"", name, "\", \"templateEvent\")

    # ! ----- STOP EDITING ----- ! #
    } else {
      warning(paste(\"Undefined event type: \'\", simEvents(sim)[1, \"eventType\", with=FALSE],
                    \"\' in module \'\", simEvents(sim)[1, \"moduleName\", with=FALSE], \"\'\", sep=\"\"))
    }
  return(invisible(sim))
}

### template initilization
", name, "Init = function(sim) {

  # # ! ----- EDIT BELOW ----- ! #


  # ! ----- STOP EDITING ----- ! #

  # last thing to do is add module name to the loaded list
  simModulesLoaded(sim) <- append(simModulesLoaded(sim), \"", name, "\")

  return(invisible(sim))
}

### template for save events
", name, "Save = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  saveFiles(sim)

  # schedule future event(s)
  sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$", name, "$.saveInterval, \"", name, "\", \"save\")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
", name, "Plot = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  Plot()

  # schedule future event(s)
  sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$", name, "$.plotInterval, \"", name, "\", \"plot\")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
", name, "Event1 = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event

  # schedule future event(s)
  sim <- scheduleEvent(sim, simCurrentTime(sim), \"", name, "\", \"event1\")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
", name, "Event2 = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event

  # schedule future event(s)
  sim <- scheduleEvent(sim, simCurrentTime(sim), \"", name, "\", \"event2\")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above\n",
            file=filenameR, fill=FALSE, sep="")
            if(open) file.edit(filenameR)

### Make Rmarkdown file for module documentation
             cat("
---
title: \"",name,"\"
author: \"Module Author\"
date: \"`r Sys.Date()`\"
output: pdf_document
---

Module documentation should be written so that others can use your module. This is a default module documentation, and should be changed.

## Module Info

## Styles

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
# Provide explicit details of the license for this module
# A default could be GPL http://www.gnu.org/copyleft/gpl.html",
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

# If we choose to have the pdf of the documentation file made at this stage, uncomment this.
#  Requires pandoc to be installed and working
# callingWd <- getwd()
# setwd(path)
# zip(paste0(name,".zip"), files=name)
# setwd(callingWd)

})

#' @rdname newModule-method
setMethod("newModule",
          signature=c(name="character", path="missing", open="logical"),
          definition = function(name, open) {
            newModule(name=name, path=".", open=open)
})

#' @rdname newModule-method
setMethod("newModule",
          signature=c(name="character", path="character", open="missing"),
          definition = function(name, path) {
            newModule(name=name, path=path, open=TRUE)
})

#' @rdname newModule-method
setMethod("newModule",
          signature=c(name="character", path="missing", open="missing"),
          definition = function(name) {
            newModule(name=name, path=".", open=TRUE)
})
