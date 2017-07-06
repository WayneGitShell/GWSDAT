library(gam)
provide.data(smacker)
Presence <- Density
Presence[Presence > 0] <- 1
position <- cbind(Latitude=smack.lat, Longitude=smack.long)
Log.depth <- log(smack.depth)
model1 <- gam(Presence ~ lo(position) + lo(Log.depth)
              + lo(Temperature), family = binomial)
model2 <- gam(Presence ~ lo(position) + lo(Temperature),
              family = binomial)
model3 <- gam(Presence ~ lo(position) + lo(Log.depth),
              family = binomial)
model4 <- gam(Presence ~ lo(Log.depth) + lo(Temperature),
              family = binomial)
print(anova(model1))
par(mfrow=c(2,2))
sm.regression(position, Presence, h=c(0.3, 0.3), poly.index=0,
              zlim = c(0,0.8))
plot(model1, se = TRUE)
par(mfrow=c(1,1))
