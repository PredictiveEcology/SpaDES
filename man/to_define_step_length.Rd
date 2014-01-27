\name{to_define_step_length}
\alias{to_define_step_length}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Log Normal distribution sampling
}
\description{
Draw values from a log Normal distribution with a maximum threshold
}
\usage{
to_define_step_length(N_agents, mean_log_normal, sd_log_normal, max_step)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N_agents}{
%%     ~~Describe \code{N_agents} here~~
}
  \item{mean_log_normal}{
%%     ~~Describe \code{mean_log_normal} here~~
}
  \item{sd_log_normal}{
%%     ~~Describe \code{sd_log_normal} here~~
}
  \item{max_step}{
%%     ~~Describe \code{max_step} here~~
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
function (N_agents, mean_log_normal, sd_log_normal, max_step) 
{
    steps_simulated <- exp(rnorm(n = N_agents, mean = mean_log_normal, 
        sd = sd_log_normal))
    for (a in 1:N_agents) {
        while (steps_simulated[a] > max_step) {
            steps_simulated[a] <- exp(rnorm(1, mean = mean_log_normal, 
                sd = sd_log_normal))
        }
    }
    return(steps_simulated)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
