elementshift <-
function(x, dx) {
  if (dx<0) { dx<-length(x)-(abs(dx)%%length(x)) }
  return=(c(x[-(dx%%length(x)):0],x[1:(dx%%length(x))]))
}

