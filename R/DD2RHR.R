DD2RHR <-
function(dip, direction){
  strike<-(direction - 90) %% 360
  RHR<-cbind(strike, dip)
  return(RHR)
}

