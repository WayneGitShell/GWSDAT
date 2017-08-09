with(stanford, {
sm.survival(Age, Log.time, Status, h = 7)
sm.survival(Age, Log.time, Status, h = 7, p = 0.25,
        add = TRUE, lty = 2)
sm.survival(Age, Log.time, Status, h = 7, p = 0.10,
        add = TRUE, lty = 3)
})
