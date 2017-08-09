library(gam)
Presence <- smacker$Density
Presence[Presence > 0] <- 1
position <- cbind(Latitude=smacker$smack.lat, Longitude=smacker$smack.long)
Log.depth <- log(smacker$smack.depth)
temperature <- smacker$Temperature
model1 <- gam(Presence ~ lo(position) + lo(Log.depth)
              + lo(temperature), family = binomial)
model2 <- gam(Presence ~ lo(position) + lo(temperature),
              family = binomial)
model3 <- gam(Presence ~ lo(position) + lo(Log.depth),
              family = binomial)
model4 <- gam(Presence ~ lo(Log.depth) + lo(temperature),
              family = binomial)
print(anova(model1))
par(mfrow=c(2,2))
sm.regression(position, Presence, h=c(0.3, 0.3), poly.index=0,
              zlim = c(0,0.8))
plot(model1, se = TRUE)
par(mfrow=c(1,1))
