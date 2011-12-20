ADD2RHR <-
function(strike, dip, direction){
  direction<-toString(direction)
  
  #Conditionals for dip direction and strike azimuth#
  if(direction == "n" | direction == "N"){
    strike<-ifelse(strike >  180,
      strike,
      (strike + 180) %% 360
    )
    RHR<-cbind(strike, dip)
    return(RHR)
  }
  if(direction == "e" | direction == "E"){
    strike<-ifelse(strike >  270 | strike < 90,
      strike,
      (strike + 180) %% 60
    )
    RHR<-cbind(strike, dip)
    return(RHR)
  }
  if(direction == "s" | direction == "S"){
    strike<-ifelse(strike > 0 & strike < 180,
      strike,
      (strike + 180) %% 360
    )
    RHR<-cbind(strike, dip)
    return(RHR)
  }
  if(direction == "w" | direction == "W"){
    strike<-ifelse(strike > 90 & strike < 270,
      strike,
      (strike + 180) %% 360
    )
    RHR<-cbind(strike, dip)
    return(RHR)
  }
  #Conditional for incorrect syntax#
  if(direction != "n" &
    direction != "N" &
    direction != "e" &
    direction != "E" &
    direction != "s" &
    direction != "S" &
    direction != "w" &
    direction != "W"){
    RHR<-cbind(NA, NA)
    return(RHR)
  }
}

