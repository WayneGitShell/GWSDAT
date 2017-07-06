% Copyright 2001-4 by Roger S. Bivand
\name{plot.polylist}
\alias{plotpolys}
\alias{plot.polylist}
\alias{leglabs}
\title{Plot polygons}
\description{
A helper function for plotting polygons in a global bounding box, attempts to handle overplotting internally, holes should be coded by coordinates ordered anti-clockwise. Earlier behaviour requires forcefill=TRUE; this is now default, but will cease to be so from next release. Changes caused by some imported shapefiles data leading to erroneously plotting lakes/islands, and R polygon() overplotting internal polygons. 
\code{leglabs} makes character strings from the same break points. The \code{plot.polylist()} function may be used as a generic S3 method. NOTE! \code{plotpolys()} DEPRECATED: use generic plot() for polylist objects function instead.
}
\usage{
\method{plot}{polylist}(x, col, border = par("fg"), add = FALSE, xlim=NULL,
 ylim=NULL, xlab = "", ylab = "", xpd = NULL, density = NULL, angle = 45, 
 pbg=NULL, forcefill=TRUE, ...)
leglabs(vec, under="under", over="over", between="-")
plotpolys(pl, bb, col = NA, border = par("fg"), add = FALSE, xlim=NULL,
 ylim=NULL, ...)
}

\arguments{
  \item{pl, x}{list of polygons of class \code{polylist}}
  \item{bb}{matrix of polygon bounding boxes - columns are LL(x), LL(y), UR(x), UR(y); note that this argument may be omitted if the polygon list object has an "maplim" attribute, or if both xlim and ylim arguments are given}
  \item{col}{colurs to use for filling the polygons}
  \item{border}{the color to draw the border}
  \item{add}{add to existing plot}
  \item{xlim, ylim}{numeric of length 2, giving the x and y coordinates ranges}
  \item{xlab, ylab}{x and y axis labels, default no label}
  \item{xpd}{(where) should clipping take place?}
  \item{density}{the density of shading lines, in lines per inch}
  \item{angle}{the slope of shading lines, given as an angle in degrees (counter-clockwise)}
  \item{pbg}{colour to be used for hole and background fill, by default NULL (note that par("bg") may be "transparent")}
  \item{forcefill}{default TRUE \emph{until next release} - fill anyway, if FALSE: believe ring direction as indication of holes/lakes}
  \item{...}{other arguments passed to plot to set the plot window - not passed to polygon}
  \item{vec}{vector of break values}
  \item{under}{character value for under}
  \item{over}{character value for over}
  \item{between}{character value for between}
%  \item{y}{numeric vector to be broken into class intervals}
%  \item{rightmost.closed}{as in findInterval()}
%  \item{all.inside}{as in findInterval()}
}
\author{Roger Bivand \email{Roger.Bivand@nhh.no}}

\seealso{\code{\link{Map2poly}}, \code{\link{findInterval}}}

\examples{
try2 <- read.shape(system.file("shapes/columbus.shp", package="maptools")[1])
mappolys <- Map2poly(try2, as.character(try2$att.data$NEIGNO))
brks <- round(quantile(try2$att.data$CRIME, probs=seq(0,1,0.2)), digits=2)
colours <- c("salmon1", "salmon2", "red3", "brown", "black")
plot(mappolys, col=colours[findInterval(try2$att.data$CRIME, brks,
 all.inside=TRUE)], forcefill=FALSE)
legend(x=c(5.8, 7.1), y=c(13, 14.5), legend=leglabs(brks),
  fill=colours, bty="n")
invisible(title(main=paste("Columbus OH: residential burglaries and vehicle",
 "thefts per thousand households, 1980", sep="\n")))
try3 <- read.shape(system.file("shapes/sids.shp", package="maptools")[1])
mappolys <- Map2poly(try3, as.character(try3$att.data$FIPSNO))
cols <- c("pink", "orange", "red")
np <- sapply(mappolys, function(x) attr(x, "nParts"))
plot(mappolys, col=cols[np], forcefill=FALSE)
invisible(title(main="White: one part, orange: two part, red: three part polygons"))
plot(mappolys, density=c(10, 20, 30)[np], angle=c(-45, 0, 45)[np],
 axes=FALSE, forcefill=FALSE)
}
\keyword{spatial}
