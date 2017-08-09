with(radioc, {
Calendar.age     <- Cal.age[Cal.age>2000 & Cal.age<3000]
Radiocarbon.age  <-  Rc.age[Cal.age>2000 & Cal.age<3000]
sm.regression(Calendar.age, Radiocarbon.age, h = 30, 
        display = "se")
})
