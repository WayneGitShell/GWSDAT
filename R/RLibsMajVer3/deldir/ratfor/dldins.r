subroutine dldins(a,b,c,d,ai,bi,rw,intfnd,bpt)

# Get a point ***inside*** the rectangular window on the ray from
# one circumcentre to the next one.  I.e. if the `next one' is
# inside, then that's it; else find the intersection of this ray with
# the boundary of the rectangle.
# Called by dirseg, dirout.

implicit double precision(a-h,o-z)
dimension rw(4)
logical intfnd, bpt

# Note that (a,b) is the circumcenter of a Delaunay triangle,
# and that (c,d) is the midpoint of one of its sides.
# When `dldins' is called by `dirout' it is possible for (c,d) to
# lie ***outside*** the rectangular window, and for the ray not to
# intersect the window at all.  (The point (c,d) might be the midpoint
# of a Delaunay edge connected to a `fake outer corner', added to facilitate
# constructing a tiling that completely covers the actual window.)
# The variable `intfnd' acts as an indicator as to whether such an
# intersection has been found.

# The variable `bpt' acts as an indicator as to whether the returned
# point (ai,bi) is a true circumcentre, inside the window (bpt == .false.),
# or is the intersection of a ray with the boundary of the window
# (bpt = .true.).


intfnd = .true.
bpt = .true.

# Dig out the corners of the rectangular window.
xmin = rw(1)
xmax = rw(2)
ymin = rw(3)
ymax = rw(4)

# Check if (a,b) is inside the rectangle.
if(xmin<=a&a<=xmax&ymin<=b&b<=ymax) {
        ai = a
        bi = b
	bpt = .false.
        return
}

# Look for appropriate intersections with the four lines forming
# the sides of the rectangular window.

# Line 1: x = xmin.
if(a<xmin) {
        ai = xmin
        s = (d-b)/(c-a)
        t = b-s*a
        bi = s*ai+t
        if(ymin<=bi&bi<=ymax) return
}

# Line 2: y = ymin.
if(b<ymin) {
        bi = ymin
        s = (c-a)/(d-b)
        t = a-s*b
        ai = s*bi+t
        if(xmin<=ai&ai<=xmax) return
}

# Line 3: x = xmax.
if(a>xmax) {
        ai = xmax
        s = (d-b)/(c-a)
        t = b-s*a
        bi = s*ai+t
        if(ymin<=bi&bi<=ymax) return
}

# Line 4: y = ymax.
if(b>ymax) {
        bi = ymax
        s = (c-a)/(d-b)
        t = a-s*b
        ai = s*bi+t
        if(xmin<=ai&ai<=xmax) return
}

intfnd = .false.
return
end
