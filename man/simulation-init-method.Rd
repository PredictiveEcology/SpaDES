\docType{methods}
\name{sim.init}
\alias{sim.init}
\alias{sim.init,list,list,list,character-method}
\alias{sim.init,list,list,list,missing-method}
\title{Initialize a new simulation}
\usage{
sim.init(times, params, modules, path)

\S4method{sim.init}{list,list,list,character}(times, params, modules, path)

\S4method{sim.init}{list,list,list,missing}(times, params, modules)
}
\arguments{
  \item{times}{A named list of numeric simulation start and
  stop times, of the form \code{times=list(start=0.0,
  stop=10.0)}.}

  \item{params}{A named list of simulation parameters and
  their values.}

  \item{modules}{A named list of character strings
  specfying the names of modules to be loaded for the
  simulation. **Note:** the module name should correspond
  to the R source file from which the module is loaded.
  Example: a module named "caribou" will be sourced form
  the file \code{caribou.R}, located at the specified
  \code{path} (see below).}

  \item{path}{An optional character string specifying the
  location of the module source files. If no path is
  specified, it defaults to the current working directory.}
}
\value{
A \code{SimList} simulation object, pre-initialized from
values specified in the arguments supplied.
}
\description{
Create a new simulation object, preloaded with parameters,
modules, times, etc.

sim.init

sim.init
}
\examples{
\dontrun{mySim <- sim.init(times=list(start=0.0, stop=10.0), params=list(Ncaribou=100),
modules=list("habitat", "caribou"), path="/path/to/my/modules/")}
\dontrun{mySim}
}
\seealso{
\code{\link{dosim}}.
}

