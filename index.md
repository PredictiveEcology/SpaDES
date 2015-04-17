---
layout: frontpage
title: PredictiveEcology - SpaDES
description: This is the main web page for the SpaDES R package for spatial discrete event simulation.
keywords: DES, Spatial, R, spatially explicit models, ABM, landscape ecology, forecasting, ecological forecasting
---

Master Branch: [![Build Status](https://travis-ci.org/PredictiveEcology/SpaDES.svg?branch=master)](https://travis-ci.org/PredictiveEcology/SpaDES)

Development Branch: [![Build Status](https://travis-ci.org/PredictiveEcology/SpaDES.svg?branch=development)](https://travis-ci.org/PredictiveEcology/SpaDES)

-----

# Spatial Discrete Event Simulation (SpaDES)

*Develop and run spatially explicit discrete event simulation models.*

Easily implement a variety of simulation models, with a focus on spatially explicit agent based models. The core simulation components are built upon a discrete event simulation framework that facilitates modularity, and easily enables the user to include additional functionality by running user-built simulation modules. Included are numerous tools to visualize raster and other maps.

**Website:** [https://github.com/PredictiveEcology/SpaDES](https://github.com/PredictiveEcology/SpaDES)

**Wiki:** [https://github.com/PredictiveEcology/SpaDES/wiki](https://github.com/PredictiveEcology/SpaDES/wiki)

## Installation

Download the source tarball (.tar.gz) or windows package binary (.zip), or install directly from GitHub.  The latter requires the `devtools` package, as well as the appropriate development libraries for your operating system (*e.g.*, Windows users should install [Rtools](http://cran.r-project.org/bin/windows/Rtools/)). In order to build the vignettes from source (which is done when installing a package from GitHub) you need to have a LaTeX distribution installed. We recommend [TexLive](https://www.tug.org/texlive/).

The suggested package `fastshp` can be installed with:

    install_github("s-u/fastshp")

If the install from GitHub fails during vignette building, you can skip this step (and avoid having to install LaTeX) by using `install_github("PredictiveEcology/SpaDES", ref="development", build=FALSE)`.

+ **Current stable release:**
    
    Install from package file:
    
    - Download:
    
<ul style="list-style:none; height:40px; padding:0; background: #eee; background: -moz-linear-gradient(top, #f8f8f8 0%, #dddddd 100%); ackground: -webkit-gradient(linear, left top, left bottom, color-stop(0%,#f8f8f8), color-stop(100%,#dddddd)); background: -webkit-linear-gradient(top, #f8f8f8 0%,#dddddd 100%); background: -o-linear-gradient(top, #f8f8f8 0%,#dddddd 100%); background: -ms-linear-gradient(top, #f8f8f8 0%,#dddddd 100%); background: linear-gradient(top, #f8f8f8 0%,#dddddd 100%); border-radius:5px;
  border:1px solid #d2d2d2; box-shadow:inset #fff 0 1px 0, inset rgba(0,0,0,0.03) 0 -1px 0; width:270px;">
  <li style="width:89px; float:left; border-right:1px solid #d2d2d2; height:40px"><a href="https://github.com/PredictiveEcology/SpaDES/zipball/master" style="line-height:1; font-size:11px; color:#999; display:block; text-align:center; padding-top:6px; height:40px">Download <strong>ZIP File</strong></a></li>
  <li><a href="https://github.com/PredictiveEcology/SpaDES/tarball/master">Download <strong>TAR Ball</strong></a></li>
  <li><a href="https://github.com/PredictiveEcology/SpaDES">View On <strong>GitHub</strong></a></li>
</ul>

        - [Windows binary (.zip)](https://github.com/PredictiveEcology/SpaDES/zipball/master)
        - [Source package (.tar.gz)](https://github.com/PredictiveEcology/SpaDES/tarball/master)
    
    - Install:
    
            install.packages("path/to/file", repos=NULL)
    
    Install from GitHub:
    
        install.packages("devtools")
        library("devtools")
        install_github("PredictiveEcology/SpaDES") # stable

+ **Development version (unstable):**

    Install from GitHub:
    
        install.packages("devtools")
        library("devtools")
        install_github("PredictiveEcology/SpaDES", ref="development") # unstable

## Reporting bugs

Contact us via the package GitHub site: [https://github.com/PredictiveEcology/SpaDES/issues](https://github.com/PredictiveEcology/SpaDES/issues).

This project is maintained by Eliot McIntire (emcintir at nrcan.gc.ca) and Alex Chubaty (achubaty at nrcan.gc.ca)

<small>Hosted on GitHub Pages</small>
