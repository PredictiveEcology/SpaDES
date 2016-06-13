# Spatial Discrete Event Simulation (SpaDES)

### Develop and run spatially explicit discrete event simulation models

Easily implement a variety of simulation models, with a focus on spatially explicit models. These include raster-based, event-based, and agent-based models.
The core simulation components are built upon a discrete event simulation framework that facilitates modularity, and easily enables the user to include additional functionality by running user-built simulation modules. 
Included are numerous tools to rapidly visualize raster and other maps.

**Website:** [http://SpaDES.PredictiveEcology.org](http://SpaDES.PredictiveEcology.org)

**Wiki:** [https://github.com/PredictiveEcology/SpaDES/wiki](https://github.com/PredictiveEcology/SpaDES/wiki)

## Installation

Building packages from source requires the appropriate development libraries for your operating system (*e.g.*, Windows users should install [Rtools](http://cran.r-project.org/bin/windows/Rtools/)).

The suggested package `fastshp` can be installed with:

```r
install.packages("fastshp", repos="http://rforge.net", type="source")
```

### Current stable release

[![Build Status](https://travis-ci.org/PredictiveEcology/SpaDES.svg?branch=master)](https://travis-ci.org/PredictiveEcology/SpaDES)
[![Coverage Status](https://coveralls.io/repos/github/PredictiveEcology/SpaDES/badge.svg?branch=master)](https://coveralls.io/github/PredictiveEcology/SpaDES?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/SpaDES)](https://cran.r-project.org/package=SpaDES)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/SpaDES)](https://cran.r-project.org/package=SpaDES)
[![DOI](https://zenodo.org/badge/17892/PredictiveEcology/SpaDES.svg)](https://zenodo.org/badge/latestdoi/17892/PredictiveEcology/SpaDES)

**Install from CRAN:**

```r
install.packages("SpaDES")
```

**Install from GitHub:**
    
```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/SpaDES") # stable
```

### Development version (unstable)

[![Build Status](https://travis-ci.org/PredictiveEcology/SpaDES.svg?branch=development)](https://travis-ci.org/PredictiveEcology/SpaDES)
[![Coverage Status](https://coveralls.io/repos/github/PredictiveEcology/SpaDES/badge.svg?branch=development)](https://coveralls.io/github/PredictiveEcology/SpaDES?branch=development)

**Install from GitHub:**

```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/SpaDES", ref="development") # unstable
```

## Getting started

**Vignettes:**

Available via our [wiki](https://github.com/PredictiveEcology/SpaDES/wiki/Help-Vignettes) or via `browseVignettes(package="SpaDES")`.

**Wiki:**

[https://github.com/PredictiveEcology/SpaDES/wiki](https://github.com/PredictiveEcology/SpaDES/wiki)

**Q&A Forum:**

[https://groups.google.com/d/forum/spades-users](https://groups.google.com/d/forum/spades-users)

## Reporting bugs

Contact us via the package GitHub site: [https://github.com/PredictiveEcology/SpaDES/issues](https://github.com/PredictiveEcology/SpaDES/issues).

-----

Copyright (C) 2015 Her Majesty the Queen in Right of Canada, as represented by the Minister of Natural Resources Canada
