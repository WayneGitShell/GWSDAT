% Copyright 2003 by Roger S. Bivand
\encoding{latin1}
\name{Map2poly}
\alias{Map2poly}
\alias{Map2poly1}
\alias{Map2lines}
\alias{Map2points}
%\alias{Map2maplim}
\alias{Map2bbs}
\alias{MapShapeIds}
\alias{shape2poly}
\alias{shape2lines}
\alias{shape2points}
%\alias{shp2maplim}
\alias{shape2bbs}
\alias{convert.pl}
%\alias{ringDir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create polygon lists and bounding boxes from imported shapefiles}
\description{
  Map2poly() is a function to make imported GIS vector polygons into a "polylist" object from a "Map" object. Map2bbs() retrieves polygon bounding boxes; analogous functions for the "shapefiles" package are shape2poly() and shape2bbs() (thanks to Stéphane Dray for his contribution); convert.pl() serves to convert the deprecated "multipart" "polylist" format to an NA-separated format now used by plot() for polylist objects and poly2nb(). 
%\code{ringDir()} attempts to find out whether polygon ring 
%coordinates run clockwise (external boundary of polygon object), or 
%anti-clockwise (internal boundary - hole). 
\code{*2lines()} and \code{*2points()} do the same as \code{*2poly()} for shapefiles of types 3=lines and 1=points; polygons are type=5.
}
\usage{
Map2poly1(Map, region.id = NULL, raw=TRUE)
Map2poly(Map, region.id = NULL, quiet=TRUE)
Map2lines(Map)
Map2points(Map)
%Map2maplim(Map)
Map2bbs(Map)
MapShapeIds(Map)
shape2poly(shape, region.id = NULL)
shape2lines(shape)
shape2points(shape)
%shp2maplim(shape)
shape2bbs(shape)
convert.pl(pl)
%ringDir(xy, ring)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Map}{object of class \code{"Map"}, imported from a GIS polygon vector file}
  \item{shape}{list returned by \code{read.shapefile()}, imported from a GIS polygon vector file using the package "shapefiles"}
  \item{region.id}{character vector of region ids to be added to the neighbours list as attribute \code{region.id}}
  \item{raw}{default TRUE \emph{until next release}, if TRUE, do not run sanity check for ring directions implying holes in no surrounding polygon}
  \item{quiet}{if TRUE, suppress reports of ring direction changes}
  \item{pl}{list of old-style multipart polygons of class \code{polylist}}
%  \item{xy}{the coordinate object for a single member of a polygon list}
%  \item{ring}{the ring (one of \code{1:attr(xy, "nParts"))} index number}
}
\details{
  From release 0.4-12, \code{Map2poly} replaces the plotting order heuristic with plotting only in strict decreasing order of top-level (multi)polygon area. In multipolygons, the components are plotted in decreasing area order. The multpolygons themselves are assigned their area sum for plotting order. Ring directions are all set to clockwise - very many shapefiles have been observed with quite unrealistic ring orders, so respecting input ring orders seems worse than imposing uniformity. The previous plot order assignment mechanism is retained using \code{Map2poly1}.
  From release 0.4-1, \code{Map2poly} tries to determine a plotting order for multiple parts of a single polygon object, and for lists of polygon objects to avoid overplotting. It also tries to respect the ESRI-stipulated rule that lakes are anti-clockwise and islands are clockwise: \url{http://shapelib.maptools.org/dl/shapefile.pdf}: "The order of vertices or orientation for a ring indicates which side of the ring is the interior of the polygon. The neighborhood to the right of an observer walking along the ring in vertex order is the neighborhood inside the polygon. Vertices of rings defining holes in polygons are in a counterclockwise direction. Vertices for a single, ringed polygon are, therefore, always in clockwise order" (p.8). If raw=FALSE, an additional attempt is made to enforce the final stipulation, where the polygon's bounding box does not fall wholly inside the bounding box of another polygon. This may generate warnings indicating when this has taken place, indicating an unexpected ring direction in the imported shapefile.
}

\value{
  \code{Map2poly}, \code{shape2poly}, \code{convert.pl} return polylist objects, lists of polygon boundary coordinates (divided by NA where the polygon object is represented by more than one polygon) with many attributes;
  \code{Map2lines}, \code{shape2lines} return lineslist objects;
  \code{Map2points}, \code{shape2points} return (n x 2) matrices; and
  \code{Map2bbs}, \code{shape2bbs} return bounding box matrixes, c(x1, y1, x2, y2) with coordinates for the lower left corner and upper right corner.
}

\author{Roger Bivand and Stéphane Dray, \email{Roger.Bivand@nhh.no}}
 
\seealso{\code{\link{read.shape}}, \code{\link[shapefiles]{shapefiles}}}


\examples{
try2 <- read.shape(system.file("shapes/columbus.shp", package="maptools")[1])
mappolys <- Map2poly(try2, as.character(try2$att.data$NEIGNO), quiet=FALSE)
plot(mappolys)
title(main="Polygons for Columbus OH from maptools package")
mappolys <- Map2poly1(try2, as.character(try2$att.data$NEIGNO), raw=FALSE)
plot(mappolys)
title(main="Polygons for Columbus OH from maptools package")
try3 <- read.shape(system.file("shapes/baltim.shp", package="maptools")[1])
baltpts <- Map2points(try3)
xylims <- attr(baltpts, "maplim")
plot(xylims$x, xylims$y, asp=1, type='n', xlab="", ylab="")
points(baltpts)
title(main="Baltimore points from maptools package")
try4 <- read.shape(system.file("shapes/fylk-val.shp", package="maptools")[1])
fylk.val <- Map2lines(try4)
xylims <- attr(fylk.val, "maplim")
plot(xylims$x, xylims$y, asp=1, type='n', xlab="", ylab="")
for (i in 1:length(fylk.val)) lines(fylk.val[[i]])
title(main="Norwegian river centrelines from maptools package")
}
\keyword{spatial}
