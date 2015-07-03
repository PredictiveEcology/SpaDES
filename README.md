# Spatial Discrete Event Simulation (SpaDES)

Copyright (C) 2015 Her Majesty the Queen in Right of Canada, as represented by the Minister of Natural Resources Canada

### Develop and run spatially explicit discrete event simulation models

Easily implement a variety of simulation models, with a focus on spatially explicit models. These include raster-based, event-based, and agent-based models.
The core simulation components are built upon a discrete event simulation framework that facilitates modularity, and easily enables the user to include additional functionality by running user-built simulation modules. 
Included are numerous tools to rapidly visualize raster and other maps.

**Website:** [http://SpaDES.PredictiveEcology.org](http://SpaDES.PredictiveEcology.org)

**Wiki:** [https://github.com/PredictiveEcology/SpaDES/wiki](https://github.com/PredictiveEcology/SpaDES/wiki)

## Installation

Download the source tarball (`.tar.gz`) or Windows package binary (`.zip`), or install directly from GitHub.  The latter requires the `devtools` package, as well as the appropriate development libraries for your operating system (*e.g.*, Windows users should install [Rtools](http://cran.r-project.org/bin/windows/Rtools/)). In order to build the vignettes from source (which is done when installing a package from GitHub) you need to have a `LaTeX` distribution installed. We recommend [TexLive](https://www.tug.org/texlive/).

The suggested package `fastshp` can be installed with:

```r
install.packages("fastshp", repos="http://rforge.net", type="source")
```

### Current stable release [![Build Status](https://travis-ci.org/PredictiveEcology/SpaDES.svg?branch=master)](https://travis-ci.org/PredictiveEcology/SpaDES) [![Coverage Status](https://coveralls.io/repos/PredictiveEcology/SpaDES/badge.svg?branch=master)](https://coveralls.io/r/PredictiveEcology/SpaDES?branch=master) [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/SpaDES)](http://cran.r-project.org/package=SpaDES)

**Install from CRAN:**

```r
install.packages("SpaDES")
```

**Install from package file:**
    
1. Download:

    - [Windows binary (.zip)](https://github.com/PredictiveEcology/SpaDES/zipball/master)
    - [Source package (.tar.gz)](https://github.com/PredictiveEcology/SpaDES/tarball/master)

2. Install:
    
    ```r
    install.packages("path/to/file", repos=NULL)
    ```

**Install from GitHub:**
    
```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/SpaDES") # stable
```

### Development version (unstable) [![Build Status](https://travis-ci.org/PredictiveEcology/SpaDES.svg?branch=development)](https://travis-ci.org/PredictiveEcology/SpaDES) [![Coverage Status](https://coveralls.io/repos/PredictiveEcology/SpaDES/badge.svg?branch=development)](https://coveralls.io/r/PredictiveEcology/SpaDES?branch=development)

**Install from GitHub:**

```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/SpaDES", ref="development") # unstable
```

If the install from GitHub fails during vignette building, you can skip this step (and avoid having to install `LaTeX`) by using:

```r
install_github("PredictiveEcology/SpaDES", ref="development", build=FALSE)
```

## Getting started

**Vignettes:**

```r
browseVignettes(package="SpaDES")
```

**Wiki:**

[https://github.com/PredictiveEcology/SpaDES/wiki](https://github.com/PredictiveEcology/SpaDES/wiki)

## Reporting bugs

Contact us via the package GitHub site: [https://github.com/PredictiveEcology/SpaDES/issues](https://github.com/PredictiveEcology/SpaDES/issues).
