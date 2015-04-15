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
    
        - [Windows binary (.zip)](https://github.com/PredictiveEcology/SpaDES/raw/master/SpaDES_0.4.0.zip)
        - [Source package (.tar.gz)](https://github.com/PredictiveEcology/SpaDES/raw/master/SpaDES_0.4.0.tar.gz)
    
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
