---
layout: frontpage
title: PredictiveEcology - SpaDES
description: This is the main web page for the SpaDES R package for spatial discrete event simulation.
keywords: DES, Spatial, R, spatially explicit models, ABM, landscape ecology, forecasting, ecological forecasting
---

<head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="chrome=1">
    <link rel="stylesheet" href="stylesheets/styles.css">
    <link rel="stylesheet" href="stylesheets/pygment_trac.css">
    <meta name="viewport" content="width=device-width, initial-scale=1, user-scalable=no">
</head>

<div class="wrapper">
  <header>
    <h1>SpaDES</h1>
    <p>R package for developing Spatial Discrete Event Simulation models.</p>

    <p class="view"><a href="https://github.com/PredictiveEcology/SpaDES">View the Project on GitHub <small>PredictiveEcology/SpaDES</small></a></p>

    <ul>
      <li><a href="https://github.com/PredictiveEcology/SpaDES/zipball/master">Download <strong>ZIP File</strong></a></li>
      <li><a href="https://github.com/PredictiveEcology/SpaDES/tarball/master">Download <strong>TAR Ball</strong></a></li>
      <li><a href="https://github.com/PredictiveEcology/SpaDES">View On <strong>GitHub</strong></a></li>
    </ul>
  </header>
  
  
  <footer>
    <p>This project is maintained by
    <br>
  	Eliot McIntire (emcintir at nrcan.gc.ca), and
  	<br>
  	Alex Chubaty (achubaty at nrcan.gc.ca)</p>
    <p><small>Hosted on GitHub Pages &mdash; Theme by <a href="https://github.com/orderedlist">orderedlist</a></small></p>
  </footer>
</div>

<script src="javascripts/scale.fix.js"></script>

# Spatial Discrete Event Simulation (SpaDES)

*Develop and run spatially explicit discrete event simulation models.*

Easily implement a variety of simulation models, with a focus on spatially explicit agent based models. The core simulation components are built upon a discrete event simulation framework that facilitates modularity, and easily enables the user to include additional functionality by running user-built simulation modules. Included are numerous tools to visualize raster and other maps.

**Website:** [https://github.com/PredictiveEcology/SpaDES](https://github.com/PredictiveEcology/SpaDES)

**A live proof of concept version** [https://spades.shinyapps.io/](https://spades.shinyapps.io/ForestChange_ProofOfConcept)

## Installation

Download the source tarball (.tar.gz) or windows package binary (.zip):

+ Current stable release:
    - [Windows binary (.zip)](https://github.com/PredictiveEcology/SpaDES/raw/master/SpaDES_0.4.0.zip)
    - [Source package (.tar.gz)](https://github.com/PredictiveEcology/SpaDES/raw/master/SpaDES_0.4.0.tar.gz)
+ Development version (unstable):
    - [Windows binary (.zip)](https://github.com/PredictiveEcology/SpaDES/raw/development/SpaDES_0.5.0.9000.zip)
    - [Source package (.tar.gz)](https://github.com/PredictiveEcology/SpaDES/raw/development/SpaDES_0.5.0.9000.tar.gz)

Alternatively, install `SpaDES` directly from GitHub. You will need the `devtools` package, as well as the appropriate development libraries for your operating system (e.g., Windows users should install [Rtools](http://cran.r-project.org/bin/windows/Rtools/)). In order to build the vignettes from source (which is done when installing a package from GitHub) you need to have a LaTeX distribution installed. We recommend [TexLive](https://www.tug.org/texlive/).

    install.packages("devtools")
    library("devtools")
  
    install_github("PredictiveEcology/SpaDES") # stable
    install_github("PredictiveEcology/SpaDES", ref="development") # unstable
    
The suggested package `fastshp` can be installed with:

    install_github("s-u/fastshp")

If the install from GitHub fails during vignette building, you can skip this step (and avoid having to install LaTeX) by using `install_github("PredictiveEcology/SpaDES", ref="development", build=FALSE)`.
