## Updated release

This is a maintenance release to fix CRAN check errors on OSX.

## Test environments

### Previous R versions
* Windows 7               (local), R 3.2.2
* Windows 7               (local), R 3.2.3
* Windows 7               (local), R 3.2.4
* Windows 7               (local), R 3.2.4 Revised (2016-03-16 r70336)

### Current R versions
* Debian Jessie           (local), R 3.2.5
* OS X El Capitan         (local), R 3.2.5 Patched (2016-04-18 r70517)
* Ubuntu 12.04        (travis-ci), R 3.2.4 Revised (2016-03-16 r70338)
* Ubuntu 14.04            (local), R 3.2.5
* Windows 7               (local), R 3.2.5
* Windows           (win-builder), R 3.2.5

### Development R version
* Debian:testing (rocker/r-base),  R 3.2.5
* Debian:testing (rocker/r-devel), R 3.4.0 (2016-04-04 r70420)
* Windows           (win-builder), R 3.3.0 beta (2016-04-14 r70486)

## R CMD check results

There were no ERRORs or WARNINGs

There were 2 NOTEs:

1. There are three parts to this note:

    a. We apologize for having missed these bugs when previously submitting to CRAN.
    
            Days since last update: 2

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
