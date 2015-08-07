---
layout: frontpage
title: PredictiveEcology - SpaDES
description: This is the main web page for the SpaDES R package for spatial discrete event simulation.
keywords: DES, R, spatially explicit models, ABM, agent based model, IBM, individual based model, landscape ecology, ecological forecasting
---

<head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="chrome=1">
    <link rel="stylesheet" href="stylesheets/styles.css">
    <link rel="stylesheet" href="stylesheets/pygment_trac.css">
    <meta name="viewport" content="width=device-width, initial-scale=1, user-scalable=no">
</head>

# Spatial Discrete Event Simulation (SpaDES)

### Develop and run spatially explicit discrete event simulation models

![](ageFourMaps.png)

Easily implement a variety of simulation models, with a focus on spatially explicit models. These include raster-based, event-based, and agent-based models.
The core simulation components are built upon a discrete event simulation framework that facilitates modularity, and easily enables the user to include additional functionality by running user-built simulation modules. 
Included are numerous tools to rapidly visualize raster and other maps.

**Blog:** [http://PredictiveEcology.org](http://PredictiveEcology.org)

**Website:** [http://SpaDES.PredictiveEcology.org](http://SpaDES.PredictiveEcology.org)

**Wiki:** [https://github.com/PredictiveEcology/SpaDES/wiki](https://github.com/PredictiveEcology/SpaDES/wiki)

**Proof of concept:** [https://spades.shinyapps.io/ForestChange_ProofOfConcept](https://spades.shinyapps.io/ForestChange_ProofOfConcept)

## Installation

Building packages from source the appropriate development libraries for your operating system (*e.g.*, Windows users should install [Rtools](http://cran.r-project.org/bin/windows/Rtools/)). In order to build the vignettes from source.

The suggested package `fastshp` can be installed with:

```r
install.packages("fastshp", repos="http://rforge.net", type="source")
```


### Current stable release [![Build Status](https://travis-ci.org/PredictiveEcology/SpaDES.svg?branch=master)](https://travis-ci.org/PredictiveEcology/SpaDES) [![Coverage Status](https://coveralls.io/repos/PredictiveEcology/SpaDES/badge.svg?branch=master)](https://coveralls.io/r/PredictiveEcology/SpaDES?branch=master) [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/SpaDES)](https://cran.r-project.org/package=SpaDES) [![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/SpaDES)](https://cran.rstudio.com/package=SpaDES)

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

### Development version (unstable) [![Build Status](https://travis-ci.org/PredictiveEcology/SpaDES.svg?branch=development)](https://travis-ci.org/PredictiveEcology/SpaDES)

**Install from GitHub:**

```r
install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/SpaDES", ref="development") # unstable
```

If the install from GitHub fails during vignette building, you can skip this step (and avoid having to install LaTeX) by using:
    
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

This project is maintained by Eliot McIntire (emcintir at nrcan.gc.ca) and Alex Chubaty (achubaty at nrcan.gc.ca)

<small>Hosted on GitHub Pages</small>
