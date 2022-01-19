# Spatial Discrete Event Simulation (SpaDES)

<!-- badges: start -->
[![R build status](https://github.com/PredictiveEcology/SpaDES/workflows/R-CMD-check/badge.svg)](https://github.com/PredictiveEcology/SpaDES/actions)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/SpaDES)](https://cran.r-project.org/package=SpaDES) [![CRAN_Release_Date](https://www.r-pkg.org/badges/ago/SpaDES)](https://cran.r-project.org/package=SpaDES)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/SpaDES)](https://cran.r-project.org/package=SpaDES)
[![DOI](https://zenodo.org/badge/17892/PredictiveEcology/SpaDES.svg)](https://zenodo.org/badge/latestdoi/17892/PredictiveEcology/SpaDES)
[![Codecov test coverage](https://codecov.io/gh/PredictiveEcology/SpaDES/branch/master/graph/badge.svg)](https://app.codecov.io/gh/PredictiveEcology/SpaDES?branch=master)
<!-- badges: end -->

<img align="right" width="80" pad="20" src="https://github.com/PredictiveEcology/SpaDES/raw/master/docs/images/SpaDES.png">

## Develop and run spatially explicit discrete event simulation models

Metapackage for implementing a variety of event-based models, with a focus on spatially explicit models.
These include raster-based, event-based, and agent-based models.
The core simulation components (provided by [`SpaDES.core`](https://spades-core.predictiveecology.org/)) are built upon a discrete event simulation (DES) framework that facilitates modularity, and easily enables the user to include additional functionality by running user-built simulation modules (see also [`SpaDES.tools`](https://spades-tools.predictiveecology.org/) and [`SpaDES.experiment`](https://spades-experiment.predictiveecology.org/)).
Included are numerous tools to visualize rasters and other maps (via [`quickPlot`](https://quickplot.predictiveecology.org/)), and caching methods for reproducible simulations (via [`reproducible`](https://reproducible.predictiveecology.org/)).
Additional functionality is provided by the [`SpaDES.addins`](https://spades-addins.predictiveecology.org/) and [`SpaDES.shiny`](https://spades-shiny.predictiveecology.org/) packages.

![](https://spades.predictiveecology.org/images/lcc05.png)
![](https://spades.predictiveecology.org/images/MapsSmall.gif)

## Getting started

- [Getting started guide](https://github.com/PredictiveEcology/SpaDES/wiki/Getting-Started-Guide)
- [LCC2005 tutorial](https://github.com/PredictiveEcology/SpaDES-modules/blob/master/modules/LCC2005/LCC2005.Rmd)
- ['SpaDES 4 Dummies' guide](https://github.com/CeresBarros/SpaDES4Dummies)
- [SpaDES users group](https://groups.google.com/d/forum/spades-users)
- [Vignettes](https://github.com/PredictiveEcology/SpaDES/wiki/Help-Vignettes)
- [Wiki](https://github.com/PredictiveEcology/SpaDES/wiki)
- [Known modules (existing or in development)](https://github.com/PredictiveEcology/SpaDES-modules/wiki/Current-modules-in-development)
- [SpaDES Best Practices Google Doc](https://docs.google.com/document/d/19QmQ5sErqbXF_mgv3M50SnRQJBciFvCV_LuJDsj0qKA/edit?usp=sharing)

### Workshops

- [SpaDES Workshops](https://spades-workshops.predictiveecology.org)

### Websites

**`SpaDES` metapackage:** [https://SpaDES.PredictiveEcology.org](https://SpaDES.PredictiveEcology.org)

**Other `SpaDES` packages:**

- `quickPlot`: https://quickplot.predictiveecology.org/ [![Codecov test coverage](https://codecov.io/gh/PredictiveEcology/quickPlot/branch/development/graph/badge.svg)](https://app.codecov.io/gh/PredictiveEcology/quickPlot?branch=development)

- `reproducible`: https://reproducible.predictiveecology.org/ [![Codecov test coverage](https://codecov.io/gh/PredictiveEcology/reproducible/branch/master/graph/badge.svg)](https://app.codecov.io/gh/PredictiveEcology/reproducible?branch=master)

- `Require`: https://require.predictiveecology.org/ [![Codecov test coverage](https://codecov.io/gh/PredictiveEcology/Require/branch/master/graph/badge.svg)](https://app.codecov.io/gh/PredictiveEcology/Require?branch=master)

- `SpaDES.addins`: https://spades-addins.predictiveecology.org [![Codecov test coverage](https://codecov.io/gh/PredictiveEcology/SpaDES.addins/branch/development/graph/badge.svg)](https://app.codecov.io/gh/PredictiveEcology/SpaDES.addins?branch=development)

- `SpaDES.core`: https://spades-core.predictiveecology.org/ [![Codecov test coverage](https://codecov.io/gh/PredictiveEcology/SpaDES.core/branch/master/graph/badge.svg)](https://app.codecov.io/gh/PredictiveEcology/SpaDES.core?branch=master)

- `SpaDES.experiment`: https://spades-experiment.predictiveecology.org/ [![Codecov test coverage](https://codecov.io/gh/PredictiveEcology/SpaDES.experiment/branch/development/graph/badge.svg)](https://app.codecov.io/gh/PredictiveEcology/SpaDES.experiment?branch=development)

- `SpaDES.shiny`: https://spades-shiny.predictiveecology.org/ [![Codecov test coverage](https://codecov.io/gh/PredictiveEcology/SpaDES.shiny/branch/development/graph/badge.svg)](https://app.codecov.io/gh/PredictiveEcology/SpaDES.shiny?branch=development)

- `SpaDES.tools`: https://spades-tools.predictiveecology.org/ [![Codecov test coverage](https://codecov.io/gh/PredictiveEcology/SpaDES.tools/branch/development/graph/badge.svg)](https://app.codecov.io/gh/PredictiveEcology/SpaDES.tools?branch=development)

- `SpaDES.project`: (currently being set up) https://spades-project.predictiveecology.org/ [![Codecov test coverage](https://codecov.io/gh/PredictiveEcology/SpaDES.project/branch/development/graph/badge.svg)](https://app.codecov.io/gh/PredictiveEcology/SpaDES.project?branch=development)

**Domain specific packages**

- `LandR`: https://LandR.predictiveecology.org/ [![Codecov test coverage](https://codecov.io/gh/PredictiveEcology/LandR/branch/development/graph/badge.svg)](https://app.codecov.io/gh/PredictiveEcology/LandR?branch=development)

A package to accompany the LandR landscape ecosystem modules.

- `NetLogoR`: https://NetLogoR.predictiveecology.org/ [![Codecov test coverage](https://codecov.io/gh/PredictiveEcology/NetLogoR/branch/development/graph/badge.svg)](https://app.codecov.io/gh/PredictiveEcology/NetLogoR?branch=development)

A reimplementation in pure R of the NetLogo dictionary.

**Predictive Ecology Blog:** <http://predictiveecology.org/>

**Canadian Forest Service Adaptation Toolkit:**
<https://www.nrcan.gc.ca/climate-change/impacts-adaptations/climate-change-impacts-forests/forest-change-adaptation-tools/17770>

**LandWeb Demonstration App:** <https://landweb.ca>

**Simple visuals**
[SpaDES_examples repository](https://predictiveecology.github.io/SpaDES_examples/docs/index.html)

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
install_github("PredictiveEcology/SpaDES", dependencies = TRUE) # master
```

### Development version (unstable)

**Install from GitHub:**

```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/SpaDES", ref = "development", dependencies = TRUE)
```

## Getting help

Q&A forum is available on the SpaDES Users Google Group.
This is the place to ask for help on setting up and running simulations, as well as module development.
Please do not file bug reports here.

- [Q&A Forum](https://groups.google.com/forum/#!forum/spades-users)

## Reporting bugs

The `SpaDES` metapackage simply loads a number of other packages from the `SpaDES` ecosystem.
Bug reports should be reported to the specific package in question rather than the metapackage, and should contain a concise [reproducible example](https://stackoverflow.com/q/5963269/1380598).
Contact us via the package's GitHub site:

- [LandR](https://github.com/PredictiveEcology/LandR/issues) 
- [quickPlot](https://github.com/PredictiveEcology/quickPlot/issues) 
- [Require](https://github.com/PredictiveEcology/Require/issues) 
- [reproducible](https://github.com/PredictiveEcology/reproducible/issues) 
- [SpaDES.addins](https://github.com/PredictiveEcology/SpaDES.addins/issues) 
- [SpaDES.core](https://github.com/PredictiveEcology/SpaDES.core/issues) 
- [SpaDES.experiment](https://github.com/PredictiveEcology/SpaDES.experiment/issues) 
- [SpaDES.project](https://github.com/PredictiveEcology/SpaDES.project/issues) 
- [SpaDES.shiny](https://github.com/PredictiveEcology/SpaDES.shiny/issues) 
- [SpaDES.tools](https://github.com/PredictiveEcology/SpaDES.tools/issues) 

-----

Copyright (C) 2021 Her Majesty the Queen in Right of Canada, as represented by the Minister of Natural Resources Canada
