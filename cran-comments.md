## Updated release

This package remains a metapackage. The main purpose of this updated release is to address CRAN concerns about package declarations for vignettes.

Since this is a metapackage, we have not included any examples as these are better suited for their respective packages.

## Test environments

### Previous R versions
* Ubuntu 14.04        (travis-ci), R 3.3.0
* Ubuntu 14.04        (travis-ci), R 3.3.3
* Windows              (appveyor), R 3.3.3
* Windows 7               (local), R 3.3.3

### Current R versions
* macOS High Sierra    (local), R 3.4.3
* OS X El Capitan  (travis-ci), R 3.4.3
* Ubuntu 14.04     (travis-ci), R 3.4.3
* Ubuntu 16.04         (local), R 3.4.3
* Windows           (appveyor), R 3.4.3
* Windows        (win-builder), R 3.4.3
* Windows 7            (local), R 3.4.3

### Development R version
* Debian:testing  (rocker/drd), R 3.5.0 (2017-08-29 r73156)
* Ubuntu 14.04     (travis-ci), R 3.5.0 (2018-01-31 r74187)
* Ubuntu 16.04         (local), R 3.5.0 (2018-01-31 r74187)
* Windows           (appveyor), R 3.5.0 (2018-01-26 r74169)
* Windows        (win-builder), R 3.5.0 (2018-01-30 r74185)

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
