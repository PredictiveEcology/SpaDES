## Updated release

This is a maintenance release to address forthcoming changes to the `dplyr` package.
We also include a number of bug fixes and performance enhancements.

## Test environments

### Previous R versions
* Windows 7               (local), R 3.2.2
* Windows 7               (local), R 3.2.3
* Windows 7               (local), R 3.2.4
* OS X El Capitan         (local), R 3.2.4

### Current R versions
* Debian Jessie           (local), R 3.2.4 Revised (2016-03-16 r70336)
* Ubuntu 12.04        (travis-ci), R 3.2.4 Revised (2016-03-16 r70338)
* Ubuntu 14.04            (local), R 3.2.4 Revised (2016-03-16 r70336)
* Windows 7               (local), R 3.2.4 Revised (2016-03-16 r70336)
* Windows           (win-builder), R 3.2.4 Revised (2016-03-16 r70336)

### Development R version
* Debian:testing (rocker/r-base),  R 3.3.0 beta (2016-04-05 r70427)
* Debian:testing (rocker/r-devel), R 3.4.0 (2016-04-04 r70420)
* Windows           (win-builder), R 3.3.0 beta (2016-04-10 r70465)

## R CMD check results

There were no ERRORs or WARNINGs

There were 2 NOTEs:

1. There are two parts to this note:

    a. Several words were flagged as possibly mispelled, but they are not.
    
            Possibly mis-spelled words in DESCRIPTION:
              fastshp (11:61, 12:39)
              modularity (9:5)
              repos (12:49)

    b. The `fastshp` package in Suggests is optionally installed from Rforge and not required to use the package. Instructions for installation are provided in the Description, README, and via a message to the user. We believe this should satisfy the CRAN policy requirement regarding additional dependencies.

            Suggests or Enhances not in mainstream repositories:
              fastshp
          
            Availability using Additional_repositories specification:
              fastshp   yes   http://rforge.net

2. As noted above, we provide instructions for the user to install the suggested `fastshp` package.

        Package suggested but not available for checking: 'fastshp'

## Downstream dependencies

There are currently no downstream dependencies of this package.
