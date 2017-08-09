library(gam)
model1  <- gam(log(Density) ~ lo(log(mack.depth)) + lo(Temperature)
            + lo(mack.lat, mack.long), data = mackerel)
print(model1)
print(gam(log(Density) ~ lo(Temperature) + lo(mack.lat, mack.long),
            data = mackerel))
print(gam(log(Density) ~ lo(log(mack.depth))  + lo(mack.lat, mack.long),
            data = mackerel))
print(gam(log(Density) ~ lo(log(mack.depth))  + lo(Temperature),
            data = mackerel))
par(mfrow=c(2,2))
plot.gam(model1, se = TRUE)
par(mfrow=c(1,1))
