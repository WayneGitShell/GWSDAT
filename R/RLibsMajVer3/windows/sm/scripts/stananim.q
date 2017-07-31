with(stanford, {

x            <- Age
y            <- Log.time
status       <- Status
st.code      <- 1

hseq         <- seq(5, 10, by = 1)

model        <- sm.survival(x, y, status, h = hseq[1])
estimate.old <- model$estimate
for (i in (2:length(hseq))) {
  model <- sm.survival(x, y, status, h = hseq[i], 
  				display = "none")
  estimate.new <- model$estimate
  lines(model$eval.points, estimate.old, col = 0)
  lines(model$eval.points, estimate.new)
  text(x[status == st.code], y[status == st.code], "x")
  text(x[status != st.code], y[status != st.code], "o")
  estimate.old <- estimate.new
  }
})
