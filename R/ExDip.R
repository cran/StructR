ExDip <-
function(vert.ex = 2, dip = 45){
  exaggerated<-atan(vert.ex * tan((pi / 180) * dip)) * (180 / pi)
  return(exaggerated)
}

