RHR2DD <-
function(strike, dip){
direction<-(90 + strike) %% 360
dip.direction<-cbind(dip, direction)
  return(dip.direction)
}

