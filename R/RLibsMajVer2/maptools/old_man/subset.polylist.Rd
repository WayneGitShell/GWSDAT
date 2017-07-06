% Copyright 2003 by Roger S. Bivand
\name{subset.polylist}
\alias{subset.polylist}
\title{Subset polygon list objects}
\description{
  The function subsets a polygon list object, also subsetting region ID attributes and also map limits if required.
}
\usage{
\method{subset}{polylist}(x, subset, fit.bbox = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a polylist object}
  \item{subset}{a logical vector valued TRUE if the element is to be retained}
  \item{fit.bbox}{if TRUE (default) modifies the maplim attribute to bound the subset}
  \item{...}{other arguments passed through}
}

\value{
  returns a polylist object, lists of polygon boundary coordinates (divided by NA where the polygon object is represented by more than one polygon);
}

\author{Roger Bivand \email{Roger.Bivand@nhh.no}}

\examples{
nc <- read.shape(system.file("shapes/sids.shp", package = "maptools")[1])
mappolys <- Map2poly(nc, as.character(nc$att.data$FIPSNO))
submap <- subset(mappolys, nc$att.data$SID74 > 0)
plot(mappolys, col="orange")
plot(submap, add=TRUE, col="lightpink", forcefill=TRUE)
}
\keyword{spatial}

