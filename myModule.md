---
title: "myModule"
author: "Module Author"
date: "02 February 2016"
output: pdf_document
---

# Overview

Provide an overview of what the module does / how to use the module.

Module documentation should be written so that others can use your module.
This is a template for module documentation, and should be changed to reflect your module.

## RMarkdown

RMarkdown syntax allows R code, outputs, and figures to be rendered in the documentation.

For help writing in RMarkdown, see http://rmarkdown.rstudio.com/.

# Usage


```r
library(SpaDES)
library(magrittr)

inputDir <- file.path(tempdir(), "inputs") %>% checkPath(create = TRUE)
outputDir <- file.path(tempdir(), "outputs")
times <- list(start = 0, end = 10)
parameters <- list(
  .globals = list(burnStats = "nPixelsBurned"),
  #.progress = list(type = "text", interval = 1), # for a progress bar
  ## If there are further modules, each can have its own set of parameters:
  #module1 = list(param1 = value1, param2 = value2),
  #module2 = list(param1 = value1, param2 = value2)
)
```

```
## Error in list(.globals = list(burnStats = "nPixelsBurned"), ): argument 2 is empty
```

```r
modules <- list("myModule")
objects <- list()
paths <- list(
  cachePath = file.path(outputDir, "cache"),
  modulePath = file.path(".."),
  inputPath = inputDir,
  outputPath = outputDir
)

mySim <- simInit(times = times, params = parameters, modules = modules,
                 objects = objects, paths = paths)
```

```
## Error in simInit(times = times, params = parameters, modules = modules, : error in evaluating the argument 'params' in selecting a method for function 'simInit': Error: object 'parameters' not found
```

```r
spades(mySim)
```

```
## Error in spades(mySim): error in evaluating the argument 'sim' in selecting a method for function 'spades': Error: object 'mySim' not found
```

# Events

Describe what happens for each event type.

## Plotting

Write what is plotted.

## Saving

Write what is saved.

# Data dependencies

## Input data

How to obtain input data, and a description of the data required by the module.
If `sourceURL` is specified, `downloadData("myModule", "path/to/modules/dir")` may be sufficient.

## Output data

Description of the module outputs.

# Links to other modules

Describe any anticipated linkages to other modules.

