## Updated release

This is a maintenance release which fixes html problems in package documentation.
See `NEWS.md` for a full list of changes.

## Test environments

### Previous R versions
* Ubuntu 20.04                 (GitHub), R 4.0.5
* Ubuntu 20.04                 (GitHub), R 4.1.3
* Windows                      (GitHub), R 4.0.5
* Windows                      (GitHub), R 4.1.3
* Windows                 (win-builder), R 4.1.3

### Current R versions
* macOS 11.6 Big Sur           (GitHub), R 4.2.1
* macOS 11.6 Big Sur            (local), R 4.2.1
* macOs (m1) Big Sur             (rhub), R 4.2.1
* Ubuntu 20.04                 (GitHub), R 4.2.1
* Ubuntu 20.04                  (local), R 4.2.1
* Windows                      (GitHub), R 4.2.1
* Windows                       (local), R 4.2.1
* Windows                 (win-builder), R 4.2.1

### Development R version
* Ubuntu 20.04                 (GitHub), R-devel (2022-08-18 r82725)
* Ubuntu 20.04                  (local), R-devel (2022-08-18 r82725)
* Windows                      (GitHub), R-devel (2022-08-18 r82725 ucrt)
* Windows                 (win-builder), R-devel (2022-08-18 r82725 ucrt)

## R CMD check results

There were no ERRORs or WARNINGs.

We see the following NOTEs:

1. Some words were flagged as possibly misspelled, but they are false positives:

        Possibly mis-spelled words in DESCRIPTION:
          DES (8:77)
          Matloff (9:9)
          Metapackage (5:14)
          modularity (10:32)
          rasters (12:73)

2. A URL was inaccessible by a CRAN machine but is available locally in web browser and via GitHub Actions.

        Found the following (possibly) invalid URLs:
          URL: https://support.rstudio.com/hc/en-us/articles/200486498-Package-Development-Prerequisites
            From: README.md
            Status: 403
            Message: Forbidden

## Downstream dependencies

There are currently no downstream dependencies of this package.
