library(gam)
with(trawl, {
ind       <- (Year == 0 & Zone == 1)
score1    <- Score1[ind]
latitude  <- Latitude[ind]
longitude <- Longitude[ind]
print(gam(score1 ~ lo(longitude) + lo(latitude)))
print(gam(score1 ~ lo(longitude)))
print(gam(score1 ~ lo(latitude)))
})
