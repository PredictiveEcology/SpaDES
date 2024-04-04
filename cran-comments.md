## Updated release

This release has been updated following restoration of dependency package `Require` to CRAN.
See `NEWS.md` for a full list of changes.

## Test environments

### Previous R versions
* Ubuntu 20.04                 (GitHub), R 4.2.3
* Windows                      (GitHub), R 4.2.3
* Windows                 (win-builder), R 4.2.3

### Current R versions
* macOS 12.6.3                 (GitHub), R 4.3.3
* macOS 13.3.1            (mac-builder), R 4.3.3
* macOS 14.4.1                  (local), R 4.3.3
* Ubuntu 20.04                 (GitHub), R 4.3.3
* Ubuntu 20.04                  (local), R 4.3.3
* Windows                      (GitHub), R 4.3.3
* Windows                       (local), R 4.3.3
* Windows                 (win-builder), R 4.3.3

### Development R version
* Ubuntu 20.04                 (GitHub), R-devel (2024-04-03 r86304)
* Ubuntu 20.04                  (local), R-devel (2024-04-03 r86304)
* Windows                      (GitHub), R-devel (2024-04-03 r86327 ucrt)
* Windows                 (win-builder), R-devel (2024-04-03 r86327 ucrt)

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
            URL: https://support.posit.co/hc/en-us/articles/200486498-Package-Development-Prerequisites
              From: README.md
              Status: 403
              Message: Forbidden

## Downstream dependencies

There are currently no downstream dependencies of this package.
