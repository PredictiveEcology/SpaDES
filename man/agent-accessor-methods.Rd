\docType{methods}
\name{agentID}
\alias{<-}
\alias{agentHeading}
\alias{agentHeading,mobileAgent-method}
\alias{agentHeading<-}
\alias{agentID}
\alias{agentID,agent-method}
\alias{agentID<-}
\alias{agentPosition}
\alias{agentPosition,pointAgent-method}
\alias{agentPosition<-}
\alias{getOther}
\alias{getOther,agent-method}
\alias{setOther}
\alias{setOther,agent-method}
\title{get the agent ID}
\usage{
agentID(object)

\S4method{agentID}{agent}(object)

agentID(object) <- value

getOther(object, name)

\S4method{getOther}{agent}(object, name)

setOther(object, name, value)

\S4method{setOther}{agent}(object, name, value)

agentPosition(object)

\S4method{agentPosition}{pointAgent}(object)

agentPosition(object) <- value

agentHeading(object)

\S4method{agentHeading}{mobileAgent}(object)

agentHeading(object) <- value
}
\arguments{
  \item{object}{A \code{*Agent} object.}

  \item{value}{The object to be stored at the slot.}
}
\value{
Returns or sets the value of the slot from the
\code{*Agent} object.
}
\description{
get the agent ID

get the agent ID

set the agent ID

set the agent ID

get other (non-default) attributes of agent

get other (non-default) attributes of agent

set other (non-default) attributes of agent

set other (non-default) attributes of agent

Currently, only get methods are defined. Set and subset
methods are not.

get pointAgent positions

set the agent position

set the pointAgent position

get agent heading

get mobileAgent heading

set the agent heading

set the mobileAgent heading
}
\seealso{
\code{\link{agent-class}},
\code{\link{polygonAgent-class}},
\code{\link{pointAgent-class}},
\code{\link{spreadAgent-class}},
\code{\link{mobileAgent-class}}
}

