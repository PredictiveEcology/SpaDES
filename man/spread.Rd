\name{spread}
\alias{spread}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Spread function
}
\description{
Very basic at this point
}
\usage{
spread(map, start, n, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{map}{
%%     ~~Describe \code{map} here~~
}
  \item{start}{
%%     ~~Describe \code{start} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (map, start, n, ...) 
{
    cr = raster(map)
    cr[] = NA
    cr[cellFromXY(cr, caribou@pos)] = 1
    fire = raster(map)
    fire[cr] = 1
    cradj = adjacent(cr, cellFromXY(cr, caribou@pos), sorted = T, 
        ...)
    for (i in 1:n) {
        fireadj = adjacent(fire, Which(fire == i, cells = T), 
            sorted = T, ...)
        fire[fireadj[, "to"][map[fireadj[, "to"]] < 20 & is.na(fire[fireadj[, 
            "to"]])]] = i + 1
    }
    return(fire)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
