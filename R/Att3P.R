Att3P <-
function(coords){
  if(class(coords) == "matrix"){
    coords<-data.frame(coords)
  }
  
  #Set up data frame and order coords by elevation#
  names(coords) = c("X", "Y", "Z")
  coords<-coords[order(-coords$Z),]
  
  #Calculate distantance along tie line for strike line#
  dist.1<-sqrt((coords$X[1] - coords$X[3])^2 + (coords$Y[1] - coords$Y[3])^2 )
  elev.1<-coords$Z[1] - coords$Z[3]
  elev.2<-coords$Z[1] - coords$Z[2]
  dist.2<-(elev.2 / elev.1) * dist.1
  
  #Calculate the equation of the tie line, inter.1 is optional#
  slope.1<-(coords$Y[1] - coords$Y[3]) / (coords$X[1] - coords$X[3])
  inter.1<-coords$Y[3] - slope.1 * coords$X[3]
  
  #Determine coordinates of the tie-strike lines intersection#
  iCordx.1<-ifelse(coords$X[3] > coords$X[1],
    coords$X[1] + (dist.2 / sqrt(slope.1 * slope.1 + 1)),
    coords$X[1] - (dist.2 / sqrt(slope.1 * slope.1 + 1))
  )
  iCordy.1<-ifelse(coords$Y[3] > coords$Y[1],
    coords$Y[1] + (slope.1 * dist.2 / sqrt(slope.1*slope.1 + 1)),
    coords$Y[1] - (slope.1 * dist.2 / sqrt(slope.1*slope.1 + 1))
  )
  
  #Calculate equation parameters for the tie line#
  slope.2<-(coords$Y[2] - iCordy.1) / (coords$X[2] - iCordx.1)
  inter.2<-iCordy.1 - slope.2 * iCordx.1
  
  #Calculate equation parameters for the dip line#
  slope.3<-(-1) * (1 / slope.2)
  inter.3<-coords$Y[3] - slope.3 * coords$X[3]
  
  #Detemine coordinates for the dip-strike lines intersection#
  iCordx.2<-(inter.2 - inter.3) / (slope.3 - slope.2)
  iCordy.2<-slope.3 * iCordx.2 + inter.3
  
  #Set up length and height for dip calculation#
  dist.3<-sqrt((coords$X[2] - iCordx.2)^2 + (coords$Y[2] - iCordy.2)^2 )
  elev.3<-coords$Z[2] - coords$Z[3]
  
  #Calculate dip#
  dip<-(180 / pi) * (atan(elev.3 / dist.3))
  
  #Recenter dip line coordinates to determine dip direction#
  iCordx.3<-coords$X[3] - iCordx.2
  iCordy.3<-coords$Y[3] - iCordy.2
  
  #Run algorithm to determine dip direction azimuth#
  if(iCordx.3 >= 0 & iCordy.3 > 0){
    dip.dir<-abs(atan(iCordx.3 / iCordy.3))
  }
    if(iCordx.3 >= 0 & iCordy.3 <= 0){
    dip.dir<-(pi / 2) + abs(atan(iCordy.3 / iCordx.3))
  }
    if(iCordx.3 < 0 & iCordy.3 <= 0){
    dip.dir<-(pi) + abs(atan(iCordx.3 / iCordy.3))
  }
    if(iCordx.3 < 0 & iCordy.3 > 0){
    dip.dir<-((3 / 2) * pi) + abs(atan(iCordy.3 / iCordx.3))
  }
  
  #Convert to strike and degrees#
  dip.dir<-dip.dir * (180 / pi)
  strike<-(dip.dir - 90) %% 360
  RHR<-cbind(strike, dip)
  
  return(RHR)
}

