---
title: "Plotting with `SpaDES`"
author: "Eliot J. B. McIntire"
date: "January 09 2015"
output:
  pdf_document:
    number_sections: yes
    toc: yes
  html_document:
    self_contained: no
    toc: yes
  word_document: default
vignette: >
  %\VignetteEngine{knitr::knitr}
  %\VignetteIndexEntry{Plotting with SpaDES}
  %\VignetteDepends{SpaDES}
  %\VignetteKeyword{visualization, plotting, mapping}
  %\usepackage[utf8]{inputenc}
---

# Plotting in `SpaDES`

One of the major features of the `SpaDES` package is that can take advantage of the numerous visualization tools available natively or through user built packages (*e.g.*, `RgoogleVis`, `ggplot2`, `rgl`). Nevertheless, none of these was built to be fast, modular, and replottable hundreds, thousands or more times during a simulation model. We therefore built a plotting function to fulfill this need. The main plotting function, `Plot` (*i.e.*, with a capital `P`), is built using the grid package. We have specifically built a plotting system that allows for relatively fast plotting of rasters, points, and polygons with the ability to make multi-frame plots without the module (or user) knowing which plots are already plotted. In other words, the main plotting function can handle SpaDES modules, each of which can add plots, without each knowing what the current state of the active plotting device is. This means that plotting can be treated as modular.  Importantly, conventional R plotting (*e.g.*, plot, hist, etc.) still works fine, so you can use the features provided in this package or you can use base plotting functions without having to relearn a new set of plotting commands. The `Plot` function is therefore intended to be used as a way to interact visually during model development. If fine tuning and customization are desired, other plotting tools may be more suited (*e.g.*, `ggplot2`, or a dedicated GIS program).

To demonstrate plotting, we first load some maps. These maps are randomly generated maps that come with the `SpaDES` package. In the code snippet below, we create the list of files to load, which is every file in the "maps" subdirectory of the package. Then we load that list of files. Because we specified .stackName in the fileList, the loadFiles function will automatically put the individual layers into a RasterStack; the individual layers will, therefore, not be available as individual objects within the R environment. If `.stackNames` did not exist, then the individual files would be individual objects.


























