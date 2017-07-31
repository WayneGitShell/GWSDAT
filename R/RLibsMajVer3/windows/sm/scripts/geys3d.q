#	This script may take a long time to execute on
#	some computers.  The default grid size for
#	evaluation of the estimate is 12.  This can be
#	increased by adding an argument such as "ngrid=20"
#	to sm.density.

with(geys3d, {
par(mfrow=c(1,2))
plot(Waiting, Duration)
sm.density(geys3d)
par(mfrow=c(1,1))
})
