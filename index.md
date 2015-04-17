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

*Develop and run spatially explicit discrete event simulation models*

Easily implement a variety of simulation models, with a focus on spatially explicit agent based models. The core simulation components are built upon a discrete event simulation framework that facilitates modularity, and easily enables the user to include additional functionality by running user-built simulation modules. Included are numerous tools to visualize raster and other maps.

**Website:** [http://SpaDES.PredictiveEcology.org](http://SpaDES.PredictiveEcology.org)

**Wiki:** [https://github.com/PredictiveEcology/SpaDES/wiki](https://github.com/PredictiveEcology/SpaDES/wiki)

## Installation

Download the source tarball (`.tar.gz`) or windows package binary (`.zip`), or install directly from GitHub.  The latter requires the `devtools` package, as well as the appropriate development libraries for your operating system (*e.g.*, Windows users should install [Rtools](http://cran.r-project.org/bin/windows/Rtools/)). In order to build the vignettes from source (which is done when installing a package from GitHub) you need to have a LaTeX distribution installed. We recommend [TexLive](https://www.tug.org/texlive/).

The suggested package `fastshp` can be installed with:

```r
install_github("s-u/fastshp")
```

If the install from GitHub fails during vignette building, you can skip this step (and avoid having to install LaTeX) by using `install_github("PredictiveEcology/SpaDES", ref="development", build=FALSE)`.

+ **Current stable release:** [![Build Status](https://travis-ci.org/PredictiveEcology/SpaDES.svg?branch=master)](https://travis-ci.org/PredictiveEcology/SpaDES)
    
    Install from package file:
    
    - Download:
    
        <ul style="list-style:none; height:40px; padding:0; background: #eee; background: -moz-linear-gradient(top, #f8f8f8 0%, #dddddd 100%); ackground: -webkit-gradient(linear, left top, left bottom, color-stop(0%,#f8f8f8), color-stop(100%,#dddddd)); background: -webkit-linear-gradient(top, #f8f8f8 0%,#dddddd 100%); background: -o-linear-gradient(top, #f8f8f8 0%,#dddddd 100%); background: -ms-linear-gradient(top, #f8f8f8 0%,#dddddd 100%); background: linear-gradient(top, #f8f8f8 0%,#dddddd 100%); border-radius:5px;
      border:1px solid #d2d2d2; box-shadow:inset #fff 0 1px 0, inset rgba(0,0,0,0.03) 0 -1px 0; width:270px;">
        <li style="width:89px; float:left; border-right:1px solid #d2d2d2; height:40px;"><a href="https://github.com/PredictiveEcology/SpaDES/zipball/master" style="line-height:1; font-size:11px; color:#999; display:block; text-align:center; padding-top:6px; height:40px;">Download <strong style="font-size:14px; display:block; color:#222;">ZIP File</strong></a></li>
        <li style="width:88px; float:left; border-left:1px solid #fff; border-right:1px solid #d2d2d2; height:40px;"><a href="https://github.com/PredictiveEcology/SpaDES/tarball/master" style="line-height:1; font-size:11px; color:#999; display:block; text-align:center; padding-top:6px; height:40px;">Download <strong style="font-size:14px; display:block; color:#222;">TAR Ball</strong></a></li>
        <li style="width:89px; float:left; border-left:1px solid #fff; height:40px;"><a href="https://github.com/PredictiveEcology/SpaDES" style="line-height:1; font-size:11px; color:#999; display:block; text-align:center; padding-top:6px; height:40px;">View On <strong style="font-size:14px; display:block; color:#222;">GitHub</strong></a></li>
        </ul>
      
    - Install:
    
        ```r
        install.packages("path/to/file", repos=NULL)
        ```
    
    Install from GitHub:
    
    ```r
    install.packages("devtools")
    library("devtools")
    install_github("PredictiveEcology/SpaDES") # stable
    ```

+ **Development version (unstable):** [![Build Status](https://travis-ci.org/PredictiveEcology/SpaDES.svg?branch=development)](https://travis-ci.org/PredictiveEcology/SpaDES)

    Install from GitHub:
    
    ```r
    install.packages("devtools")
    library("devtools")
    install_github("PredictiveEcology/SpaDES", ref="development") # unstable
    ```

## Reporting bugs

Contact us via the package GitHub site: [https://github.com/PredictiveEcology/SpaDES/issues](https://github.com/PredictiveEcology/SpaDES/issues).

This project is maintained by Eliot McIntire (emcintir at nrcan.gc.ca) and Alex Chubaty (achubaty at nrcan.gc.ca)

<small>Hosted on GitHub Pages</small>
