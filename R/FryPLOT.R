FryPLOT <-
function(fry.data){
  #Determine plot window parameters#
  x.range <- summary(fry.data$x)
  x.range <- x.range[5]
  y.range <- summary(fry.data$y)
  y.range <- y.range[5]
  
  #Plot data and estimate apogee#
  x11(xpos=-25, ypos=0)
  plot(fry.data, 
       xlim = c((-1 * x.range), x.range), 
       ylim = c((-1 * y.range), y.range), 
       asp = 1, ann = FALSE, axes = FALSE, 
       pch = 19, col = "#AAAAAA")
  box()
  title(main = "Select central void apogee")
  points(0, 0, pch = 3, col = "red")
  apogee <- locator(n = 1, type="n")
  
  #Plot data and estimate perigee#
  plot(fry.data, 
       xlim = c((-1 * x.range), x.range), 
       ylim = c((-1 * y.range), y.range), 
       asp = 1, ann = FALSE, axes = FALSE, 
       pch = 19, col = "#AAAAAA")
  box()
  title(main = "Select central void perigee")
  points(0, 0, pch = 3, col = "red")
  
  lines(x = c(-1 * as.numeric(apogee[1]), as.numeric(apogee[1])),
        y = c(-1 * as.numeric(apogee[2]), as.numeric(apogee[2])),
        col = "red", lwd = 2)
  
  perigee <- locator(n = 1, type="n")
  
  lines(x = c(-1 * as.numeric(perigee[1]), as.numeric(perigee[1])),
        y = c(-1 * as.numeric(perigee[2]), as.numeric(perigee[2])),
        col = "blue", lwd = 2)

  #Create data frame of central void parameters#
  elli.coords <- matrix(rbind(apogee, perigee, deparse.level = 0), ncol = 2)
  
  a.half.axis <- sqrt(as.numeric(elli.coords[1,1])^2 + as.numeric(elli.coords[1,2])^2)
  b.half.axis <- sqrt(as.numeric(elli.coords[2,1])^2 + as.numeric(elli.coords[2,2])^2)
  
  #Run algorithm to determine dip direction azimuth#
  if(as.numeric(elli.coords[1,1]) >= 0 & as.numeric(elli.coords[1,2]) > 0){
    rake.azimuth<-abs(atan(as.numeric(elli.coords[1,1]) / as.numeric(elli.coords[1,2])))
  }
    if(as.numeric(elli.coords[1,1]) >= 0 & as.numeric(elli.coords[1,2]) <= 0){
    rake.azimuth<-(pi / 2) + abs(atan(as.numeric(elli.coords[1,2]) / as.numeric(elli.coords[1,1])))
  }
    if(as.numeric(elli.coords[1,1]) < 0 & as.numeric(elli.coords[1,2]) <= 0){
    rake.azimuth<-(pi) + abs(atan(as.numeric(elli.coords[1,1]) / as.numeric(elli.coords[1,2])))
  }
    if(as.numeric(elli.coords[1,1]) < 0 & as.numeric(elli.coords[1,2]) > 0){
    rake.azimuth<-((3 / 2) * pi) + abs(atan(as.numeric(elli.coords[1,2]) / as.numeric(elli.coords[1,1])))
  }
  
  elli.azimuth <- ((5 / 2) * pi - rake.azimuth) %% pi
  
  #Plot fitted ellipse#
  Ellipse2D(a.half.axis, b.half.axis, theta = elli.azimuth, sty = "black")
  elli.par <- data.frame(a.half.axis, b.half.axis, elli.azimuth)
  return(elli.par)
}

