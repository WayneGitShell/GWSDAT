library(gam)
provide.data(mackerel)
depth     <- mack.depth
latitude  <- mack.lat
longitude <- mack.long
model1  <- gam(log(Density) ~ lo(log(depth)) + lo(Temperature)
            + lo(latitude, longitude))
print(model1)
print(gam(log(Density) ~ lo(Temperature) + lo(latitude, longitude)))
print(gam(log(Density) ~ lo(log(depth))  + lo(latitude, longitude)))
print(gam(log(Density) ~ lo(log(depth))  + lo(Temperature)))
par(mfrow=c(2,2))
plot(model1, se = TRUE)
par(mfrow=c(1,1))
