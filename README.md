# Spatial Discrete Event Simulation (SpaDES)

[![Build Status](https://travis-ci.org/PredictiveEcology/SpaDES.svg?branch=master)](https://travis-ci.org/PredictiveEcology/SpaDES)
[![Appveyor Build status](https://ci.appveyor.com/api/projects/status/2fxqhgk6miv2fytd/branch/master?svg=true)](https://ci.appveyor.com/project/achubaty/spades/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/PredictiveEcology/SpaDES/badge.svg?branch=master)](https://coveralls.io/github/PredictiveEcology/SpaDES?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/SpaDES)](https://cran.r-project.org/package=SpaDES)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/SpaDES)](https://cran.r-project.org/package=SpaDES)
[![DOI](https://zenodo.org/badge/17892/PredictiveEcology/SpaDES.svg)](https://zenodo.org/badge/latestdoi/17892/PredictiveEcology/SpaDES)

## Develop and run spatially explicit discrete event simulation models

Metapackage for implementing a variety of event-based models, with a focus on spatially explicit models.
These include raster-based, event-based, and agent-based models.
The core simulation components (provided by [`SpaDES.core`](http://spades-core.predictiveecology.org/)) are built upon a discrete event simulation (DES) framework that facilitates modularity, and easily enables the user to include additional functionality by running user-built simulation modules (see also [`SpaDES.tools`](http://spades-tools.predictiveecology.org/)).
Included are numerous tools to visualize rasters and other maps (via [`quickPlot`](http://quickplot.predictiveecology.org/)), and caching methods for reproducible simulations (via [`reproducible`](http://reproducible.predictiveecology.org/)).
Additional functionality is provided by the [`SpaDES.addins`](http://spades-addins.predictiveecology.org/) and [`SpaDES.shiny`](http://spades-shiny.predictiveecology.org/) packages.

![](images/lcc05.png)
![](images/MapsSmall.gif)

### Websites

**`SpaDES` metapackage:** [http://SpaDES.PredictiveEcology.org](http://SpaDES.PredictiveEcology.org)

**Other `SpaDES` ecosystem packages:**

- `quickPlot`: http://quickplot.predictiveecology.org/
- `reproducible`: http://reproducible.predictiveecology.org/
- `SpaDES.addins`: http://spades-addins.predictiveecology.org
- `SpaDES.core`: http://spades-core.predictiveecology.org/
- `SpaDES.shiny`: http://spades-shiny.predictiveecology.org/
- `SpaDES.tools`: http://spades-tools.predictiveecology.org/

**Predictive Ecology Blog:** [http://predictiveecology.org/](http://predictiveecology.org/)

**Canadian Forest Service Adaptation Toolkit:** [http://www.nrcan.gc.ca/forests/climate-change/tools-resources/17770](http://www.nrcan.gc.ca/forests/climate-change/tools-resources/17770)

**LandWeb Demonstration App:** [http://landweb.predictiveecology.org/Demo/](http://landweb.predictiveecology.org/Demo/)

**Examples for R-savvy users:**

*Copy the linked `.Rmd` file to your computer.*
*Open it with a text editor or in RStudio, and run all chunks in R.*
*It is not intended to be knitted; knitting will only output the script.*

- [LandCoverChange](https://raw.githubusercontent.com/PredictiveEcology/SpaDES-modules/master/modules/LCC2005/LCC2005.Rmd)

-----

## Installation

**Install development libraries:** building packages from source requires the appropriate development libraries for your operating system.
    
- *Windows:* install [Rtools](https://cran.r-project.org/bin/windows/Rtools/).

- *macOS:* install Xcode commandline tools from the terminal: `xcode-select install`. 
  
- *Debian/Ubuntu Linux:* ensure `r-base-dev` is installed.

See [here](https://support.rstudio.com/hc/en-us/articles/200486498-Package-Development-Prerequisites) for more details.

**Install suggested packages:** the `fastshp` package can be installed with:

```r
install.packages("fastshp", repos = "https://rforge.net", type = "source")
```

### Current stable release

**Install from CRAN:**

```r
install.packages("SpaDES")
```

**Install from GitHub:**
    
```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/SpaDES", dependencies = TRUE) # stable
```

### Development version (unstable)

**Install from GitHub:**

```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/SpaDES", ref = "development", dependencies = TRUE) # unstable
```

-----

## Getting started

- [Getting started guide](https://github.com/PredictiveEcology/SpaDES/wiki/Getting-Started-Guide)
- [Vignettes](https://github.com/PredictiveEcology/SpaDES/wiki/Help-Vignettes)
- [Wiki](https://github.com/PredictiveEcology/SpaDES/wiki)
- [Workshops](http://predictiveecology.org/2016/08/09/SpaDES-workshops.html)

## Tutorials

Two tutorials are currently available:
- [LCC2005 Tutorial](https://github.com/PredictiveEcology/SpaDES-modules/blob/master/modules/LCC2005/LCC2005.Rmd)
- [wolfAlps Tutorial](https://github.com/PredictiveEcology/SpaDES-modules/blob/master/modules/wolfAlps/wolfAlps.Rmd) 

## Getting help

A Q&A forum is available on the SpaDES Users Google Group. Here, you'll find useful minor fixes to problems other users have found - and that might have not yet been included in the CRAN version of the SpaDES-related packages - and a space to post issues and solutions you found. 

- [Q&A Forum - SpaDES Users Google Group](https://groups.google.com/forum/#!forum/spades-users)

## Reporting bugs

The `SpaDES` metapackage simply loads a number of other packages from the `SpaDES` ecosystem.
Bug reports should be reported to the specific package in question rather than the metapackage.
Contact us via the package's GitHub site:

- [quickPlot](https://github.com/PredictiveEcology/quickPlot/issues)
- [reproducible](https://github.com/PredictiveEcology/reproducible/issues)
- [SpaDES.addins](https://github.com/PredictiveEcology/SpaDES.addins/issues)
- [SpaDES.core](https://github.com/PredictiveEcology/SpaDES.core/issues)
- [SpaDES.shiny](https://github.com/PredictiveEcology/SpaDES.shiny/issues)
- [SpaDES.tools](https://github.com/PredictiveEcology/SpaDES.tools/issues)

-----

Copyright (C) 2017 Her Majesty the Queen in Right of Canada, as represented by the Minister of Natural Resources Canada
