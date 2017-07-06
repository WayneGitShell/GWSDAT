\name{plot.Map}
\alias{plot.Map}
%\alias{color.ramp}

\title{Plot a Map object (deprecated)}
\description{
  This function is deprecated. It is difficult to maintain and there are several alternatives, either by converting Map objects to sp class objects or polylist etc. objects. (The function plots a map object directly from the imported shapefile data).
}
\usage{
\method{plot}{Map}(x, recs, auxvar = NULL, add = FALSE, fg = "gray", ol = "black", 
  prbg = NULL, glyph = 16, color='red', type = "q", nclass = 5, \dots)
%color.ramp(nclass, color='red', nvec=NULL, type='q')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a Map object returned by read.shape()}
  \item{recs}{a vector with the shapes to plot, if missing defaults to all shapes; if fg is not equal to the length of recs, all fg are set to fg[1]}
  \item{auxvar}{if the Map has polygon shapes, a variable from which to derive polygon fill colours using the computed class intervals, must be same length as number of shapes}
  \item{add}{default FALSE, if TRUE add to existing plot}
  \item{fg}{foreground colour, can be used to pass through point and polygon colours, permitting the built-in class interval calculation to be avoided; if fg is not equal to the length of recs, all fg are set to fg[1]}
  \item{ol}{line/boundary colour; boundaries may be removed by setting to NA oe "transparent"}
  \item{prbg}{if not NULL, draw a rectangle around the plot in this colour}
  \item{glyph}{points plotted as this pch code}
  \item{color}{base colour for color.ramp(); default "red"}
  \item{type}{default "q" for quantile classes for color.ramp(), can also be "e" for equal intervals}
  \item{nclass}{number of classes for class intervals}
  \item{\dots}{passed through to plotting functions}
%  \item{color}{default "red"}
%  \item{nvec}{data vector to convert to class intervals}
}

\value{
  If the \code{auxvar} variable is not used, the function returns NULL, otherwise it returns the list constructed by \code{maptools:::color.ramp()} with components:
  \item{ramp}{vector of colours}
  \item{col.class}{vector of classes}
  \item{breaks}{class intervals given as argument to cut}
}

\author{Nicholas J. Lewin-Koh, modified by Roger Bivand \email{Roger.Bivand@nhh.no}}

\seealso{\code{\link{read.shape}}, \code{\link{readShapePoly}}, \code{\link{readShapeLines}}, \code{\link{readShapePoints}}, \code{\link{getinfo.shape}}}
\examples{
\dontrun{
x <- read.shape(system.file("shapes/sids.shp", package="maptools")[1])
plot(x)
nParts <- sapply(x$Shapes, function(x) attr(x, "nPart"))
table(nParts)
cols <- c("azure", "blue", "orange")
fgs <- cols[nParts]
plot(x, fg=fgs)
res <- plot(x, auxvar=x$att.data$BIR74)
str(res)
res <- plot(x, auxvar=x$att.data$BIR74, type="e")
str(res)
}
}
\keyword{spatial}

