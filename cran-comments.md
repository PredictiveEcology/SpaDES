## Updated release

This is a susbstantial update to the package, which has seen many enhancements and improvements since the previous release.
The size of the package had grown too large, so we have split the package into several, and have turned this package into a metapackage that, for backwards compatibility, attaches the new spinoff packages.

We have dropped support of R 3.2.x, following the changes to several dependencies.

Please note that package installation errors (and therefore package test errors) on R-oldrel are caused by failures in dependency package `DiagrammeR`, whose recent update (0.9.1) depends on `grDevices >= 3.4.0` (see https://cran.r-project.org/web/checks/check_results_DiagrammeR.html)
We have raised the issue with the package maintainer.

## Test environments

### Previous R versions
* Ubuntu 14.04        (travis-ci), R 3.3.3
* Windows              (appveyor), R 3.3.3
* Windows 7               (local), R 3.3.3

### Current R versions
* macOS Sierra         (local), R 3.4.0
* OS X El Capitan  (travis-ci), R 3.4.0
* Ubuntu 14.04     (travis-ci), R 3.4.0
* Ubuntu 16.04         (local), R 3.4.0
* Windows           (appveyor), R 3.4.0
* Windows        (win-builder), R 3.4.0
* Windows 7            (local), R 3.4.0

### Development R version
* Debian:testing  (rocker/drd), R 3.5.0 (2017-08-29 r73156)
* Ubuntu 14.04     (travis-ci), R 3.5.0 (2017-09-01 r73175)
* Ubuntu 16.04         (local), R 3.5.0 (2017-08-30 r73162)
* Windows           (appveyor), R 3.5.0 (2017-08-31 r73172)
* Windows        (win-builder), R 3.5.0 (2017-08-31 r73172)

## R CMD check results

There were no ERRORs or WARNINGs

There was 1 NOTE:

1. Some words were flagged as possibly mispelled, but they are false positives:

        Possibly mis-spelled words in DESCRIPTION: 
          DES (8:77)
          Metapackage (5:14)
          modularity (9:32)
          rasters (11:73)

## Downstream dependencies

There are currently no downstream dependencies of this package.
