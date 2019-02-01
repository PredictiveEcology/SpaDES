## Updated release

This release fixes CRAN concerns about package declarations for tests and vignettes.

The maintainer email address has changed, about which I notified CRAN on March 28, 2018 and sent followup on June 12, 2018 (in response to `fpCompare` submission).

Since this is a metapackage, we have not included any examples as these are better suited for their respective packages.

## Test environments

### Previous R versions
* Ubuntu 14.04        (travis-ci), R 3.3.0
* Ubuntu 14.04        (travis-ci), R 3.4.0
* Windows              (appveyor), R 3.3.0
* Windows              (appveyor), R 3.4.0
* Windows 7               (local), R 3.4.4

### Current R versions
* macOS Mojave       (travis-ci), R 3.5.2
* macOS Mojave           (local), R 3.5.2
* Ubuntu 14.04       (travis-ci), R 3.5.2
* Ubuntu 18.04           (local), R 3.5.2
* Windows             (appveyor), R 3.5.2
* Windows          (win-builder), R 3.5.2
* Windows 7              (local), R 3.5.2

### Development R version
* Ubuntu 14.04       (travis-ci), R 3.6.0 (2019-01-06 r75950)
* Ubuntu 18.04           (local), R 3.6.0 (2019-01-07 r75958)
* Windows             (appveyor), R 3.6.0 (2019-01-06 r75950)
* Windows          (win-builder), R 3.6.0 (2019-01-07 r75956)

## R CMD check results

There were no ERRORs or WARNINGs

There was 1 NOTE:

1. Some words were flagged as possibly mispelled, but they are false positives:

        Possibly mis-spelled words in DESCRIPTION:
          DES (8:77)
          Matloff (9:9)
          Metapackage (5:14)
          modularity (10:32)
          rasters (12:73)

## Downstream dependencies

There are currently no downstream dependencies of this package.
