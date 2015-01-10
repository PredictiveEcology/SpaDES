Master Branch: [![Build Status](https://travis-ci.org/achubaty/SpaDES.svg?branch=master)](https://travis-ci.org/achubaty/SpaDES)

Development Branch: [![Build Status](https://travis-ci.org/achubaty/SpaDES.svg?branch=development)](https://travis-ci.org/achubaty/SpaDES)

-----

# Spatial Discrete Event Simulation (SpaDES)

*Develop and run spatially explicit discrete event simulation models.*

Easily implement a variety of simulation models, with a focus on spatially explicit agent based models. The core simulation components are built upon a discrete event simulation framework that facilitates modularity, and easily enables the user to include additional functionality by running user-built simulation modules. Included are numerous tools to visualize raster and other maps.

**Website:** [https://github.com/achubaty/SpaDES](https://github.com/achubaty/SpaDES)

## Installation

Either download the source tarball (.tar.gz) or windows package binary (.zip) from [https://github.com/achubaty/SpaDES/tree/development](https://github.com/achubaty/SpaDES/tree/development).

Or install `SpaDES` directly from github you will need the `devtools` package:

    install.packages("devtools")
    library("devtools")
	
    install_github("achubaty/SpaDES", ref="development")
    
The suggested package `fastshp` can be installed with:

    install_github("s-u/fastshp")

**NOTE:** you may get errors relating to not having installed the software tools required for building R packages on your system:

1. For development purposes on a Windows machine, you'll need to install [Rtools](http://cran.r-project.org/bin/windows/Rtools/);
2. In order to build the vignettes from source (which is done when installing a package from GitHub) you need to have a LaTeX distribution installed. We recommend [TexLive](https://www.tug.org/texlive/).

If the install from GitHub fails during vignette building, you can skip this step (and avoid having to install LaTeX) by using `install_github("achubaty/SpaDES", ref="development", build=FALSE)`.

You can access prebuilt versions of the vignettes from here: [intro](https://github.com/achubaty/SpaDES/blob/development/vignettes/introduction.pdf?raw=true), [modules](https://github.com/achubaty/SpaDES/blob/development/vignettes/modules.pdf?raw=true), [plotting](https://github.com/achubaty/SpaDES/blob/development/vignettes/plotting.pdf?raw=true).

## Reporting bugs

Contact us via the package GitHub site: [https://github.com/achubaty/SpaDES/issues](https://github.com/achubaty/SpaDES/issues).
