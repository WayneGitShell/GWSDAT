### R code from vignette source 'over.Rnw'

###################################################
### code chunk number 1: over.Rnw:102-104 (eval = FALSE)
###################################################
## A %over% B
## over(A, B)


###################################################
### code chunk number 2: over.Rnw:114-115 (eval = FALSE)
###################################################
## A[B,]


###################################################
### code chunk number 3: over.Rnw:118-119 (eval = FALSE)
###################################################
## A[!is.na(over(A,B)),]


###################################################
### code chunk number 4: over.Rnw:122-138
###################################################
library(sp)
x = c(0.5, 0.5, 1.2, 1.5)
y = c(1.5, 0.5, 0.5, 0.5)
xy = cbind(x,y)
dimnames(xy)[[1]] = c("a", "b", "c", "d")
pts = SpatialPoints(xy)

xpol = c(0,1,1,0,0)
ypol = c(0,0,1,1,0)
pol = SpatialPolygons(list(
	Polygons(list(Polygon(cbind(xpol-1.05,ypol))), ID="x1"),
	Polygons(list(Polygon(cbind(xpol,ypol))), ID="x2"),
	Polygons(list(Polygon(cbind(xpol,ypol-1.05))), ID="x3"),
	Polygons(list(Polygon(cbind(xpol+1.05,ypol))), ID="x4"),
	Polygons(list(Polygon(cbind(xpol+.4, ypol+.1))), ID="x5")
	))


###################################################
### code chunk number 5: over.Rnw:142-147
###################################################
plot(pol, xlim = c(-1.1, 2.1), ylim = c(-1.1, 1.6), border=2:6, axes=TRUE)
points(pts, col='red')
text(c(-1,0.1,0.1,1.1,0.45), c(0,0,-1.05,0,0.1), 
	c("x1", "x2", "x3", "x4", "x5"))
text(coordinates(pts), pos=1, row.names(pts))


###################################################
### code chunk number 6: over.Rnw:154-155
###################################################
over(pts, pol)


###################################################
### code chunk number 7: over.Rnw:159-160
###################################################
over(pts, pol, returnList = TRUE)


###################################################
### code chunk number 8: over.Rnw:163-164
###################################################
pts[pol]


###################################################
### code chunk number 9: over.Rnw:169-172
###################################################
over(pol, pts)
over(pol, pts, returnList = TRUE)
row.names(pol[pts])


###################################################
### code chunk number 10: over.Rnw:184-193
###################################################
zdf = data.frame(z1 = 1:4, z2=4:1, f = c("a", "a", "b", "b"),
	row.names = c("a", "b", "c", "d"))
zdf
ptsdf = SpatialPointsDataFrame(pts, zdf)

zpl = data.frame(z = c(10, 15, 25, 3, 0), zz=1:5, 
	f = c("z", "q", "r", "z", "q"), row.names = c("x1", "x2", "x3", "x4", "x5"))
zpl
poldf = SpatialPolygonsDataFrame(pol, zpl)


###################################################
### code chunk number 11: over.Rnw:197-198
###################################################
over(pts, poldf)


###################################################
### code chunk number 12: over.Rnw:206-207
###################################################
over(pts, poldf[1:2], fn = mean)


###################################################
### code chunk number 13: over.Rnw:212-213
###################################################
over(pts, poldf, returnList = TRUE)


###################################################
### code chunk number 14: over.Rnw:217-219
###################################################
over(pol, ptsdf)
over(pol, ptsdf[1:2], fn = mean)


###################################################
### code chunk number 15: over.Rnw:251-258
###################################################
plot(pol, xlim = c(-1.1, 2.1), ylim = c(-1.1, 1.6), border=2:6, axes=TRUE)
text(c(-1,0.1,0.1,1.1,0.45), c(0,0,-1.05,0,0.1), c("x1", "x2", "x3", "x4", "x5"))
l1 = Lines(Line(coordinates(pts)), "L1")
l2 = Lines(Line(rbind(c(1,1.5), c(1.5,1.5))), "L2")
L = SpatialLines(list(l1,l2))
lines(L, col = 'green')
text(c(0.52, 1.52), c(1.5, 1.5), c("L1", "L2"))


###################################################
### code chunk number 16: over.Rnw:266-275
###################################################
library(rgeos)
over(pol, pol)
over(pol, pol,returnList = TRUE)
over(pol, L)
over(L, pol)
over(L, pol, returnList = TRUE)
over(L, L)
over(pts, L)
over(L, pts)


###################################################
### code chunk number 17: over.Rnw:280-288
###################################################
data(meuse.grid)
gridded(meuse.grid) = ~x+y
Pt = list(x = c(178274.9,181639.6), y = c(329760.4,333343.7))
sl = SpatialLines(list(Lines(Line(cbind(Pt$x,Pt$y)), "L1")))
image(meuse.grid)
xo = over(sl, geometry(meuse.grid), returnList = TRUE)
image(meuse.grid[xo[[1]], ],col=grey(0.5),add=T)
lines(sl)


###################################################
### code chunk number 18: over.Rnw:299-305
###################################################
data(meuse.grid)
gridded(meuse.grid) = ~x+y
off = gridparameters(meuse.grid)$cellcentre.offset + 20
gt = GridTopology(off, c(400,400), c(8,11))
SG = SpatialGrid(gt)
agg = aggregate(meuse.grid[3], SG)


###################################################
### code chunk number 19: over.Rnw:315-317
###################################################
image(agg)
points(meuse.grid, pch = 3, cex=.2, col = "#80808080")


###################################################
### code chunk number 20: over.Rnw:326-329
###################################################
sl.agg = aggregate(meuse.grid, sl)
class(sl.agg)
as.data.frame(sl.agg)


