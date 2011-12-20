AppDip <-
function(acute = 90, dip = 45 ){
  acute<-acute * (pi / 180)
  dip<-dip * (pi / 180)
  appDip<-atan(tan(dip) * sin(acute))
  appDip<-appDip * (180 / pi)
  return(appDip)
}

