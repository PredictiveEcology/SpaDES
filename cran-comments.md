## Resubmission

This is a resubmission. In this version we:

* create a new environment for storing simulation objects, instead of modifying the global environment;
* use `Additional_repositories` for installation of the suggested package `fastshp`, which is not available on CRAN.

## Test environments

### Current R versions
* Debian Jessie           (local), R 3.2.1
* OS X Yosemite           (local), R 3.2.0
* Ubuntu 12.04        (travis-ci), R 3.2.1
* Ubuntu 14.04            (local), R 3.2.1
* Windows 7               (local), R 3.2.0
* Windows           (win-builder), R 3.2.1

### Development R version
* Debian:testing (rocker/r-devel), R 3.3.0 (2015-07-25 r68744)
* Windows           (win-builder), R 3.3.0 (2015-07-26 r68748)

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

2. Where we attach the simulation environment, we follow best practices by 1) following the call to `attach()` with `on.exit(detach())`; 2) assigning a unique name to the attached enviroment.

        Found the following calls to attach():
        File ‘SpaDES/R/simulation.R’:
          attach(envir(sim), name = envName)
        See section ‘Good practice’ in ‘?attach’.

## Downstream dependencies

There are currently no downstream dependencies of this package.
