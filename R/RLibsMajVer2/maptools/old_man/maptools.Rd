\name{maptools}
\alias{maptools}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Report version information and changes}
\description{
  Access version information and changes
}
\usage{
maptools(changes = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{changes}{If TRUE, reports changes, most recent last}
}

\value{
  Invisibly returns the change list, with members version and changes
  
}
\author{Roger Bivand}

\examples{
maptools(changes=TRUE)
}
\keyword{spatial}
