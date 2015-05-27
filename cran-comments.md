## Resubmission

This is a resubmission. In this version I have:

* create a new environment for storing simulation objects, instead of modifying the global enviroment
* use `Additional_repositories` for installation of the suggested package `fastshp`, which is not available on CRAN

## Test environments

### Previous R versions
* Windows 7               (local), R 3.1.2
* Windows 7               (local), R 3.1.3

### Current R versions
* Debian Jessie           (local), R 3.2.0
* OS X Yosemite           (local), R 3.2.0
* Ubuntu 12.04        (travis-ci), R 3.2.0
* Ubuntu 14.04            (local), R 3.2.0
* Windows 7               (local), R 3.2.0
* Windows           (win-builder), R 3.2.0

### Development R version
* Debian:testing (rocker/r-devel), R 3.3.0 (2015-03-31 r68131)
* Windows           (win-builder), R 3.3.0 (2015-05-14 r68368)

## R CMD check results

There were no ERRORs or WARNINGs

There were 2 NOTES:

1. There are two parts to this note:

    a. This is a new CRAN package submission.

        * checking CRAN incoming feasibility ... NOTE
        Maintainer: 'Alex M Chubaty <achubaty@NRCan.gc.ca>
        New submission

    b. The `fastshp` package in Suggests is optionally installed from Rforge and not required to use the package. Instructions for installation are provided in the Description, README, and via a message to the user. We believe this should satisfy the CRAN policy requirement regarding additional dependencies.

        Suggests or Enhances not in mainstream repositories:
          fastshp
        
        Availability using Additional_repositories specification:
          fastshp   yes   http://rforge.net

2. We provide helper functions that assist with loading user data into the global environment, and additionally provides a wrapper function for assigning objects to the global environment. This functionality is documented and is invoked by the user.

        * checking R code for possible problems ... NOTE
        Found the following assignments to the global environment:
        File ‘SpaDES/R/load.R’:
          assign(objectNames[x], do.call(get(loadFun[x]), args = argument), 
            envir = .GlobalEnv)

## Downstream dependencies

There are currently no downstream dependencies of this package.
