Nadai <-
function(Lx = 2, Ly = 1, Lz = .5, Plot=TRUE){
  axDat<-data.frame(Lx, Ly, Lz)
  names(axDat)=c("X","Y","Z")

  #Calculate Lode's parameter#
  lodes<-(2*(log(axDat$Y)-log(axDat$X)-log(axDat$Z)))/(log(axDat$X)-log(axDat$Z))

  #Calculate the octahedral shear strain#
  octSS<-(sqrt((log(axDat$X)-log(axDat$Y))^2+(log(axDat$Y)-log(axDat$Z))^2+(log(axDat$Z)-log(axDat$X))^2)/sqrt(3))
  
  if(Plot == FALSE){
    parameters<-data.frame(lodes, octSS)
    names(parameters) = c("lodes","octSS")
    return(parameters)
  }
  if(Plot == TRUE){
    #Calculate cartesian coords#
    xcoord<-octSS*sin((pi/6)*lodes)
    ycoord<-octSS*cos((pi/6)*lodes)
    
    #Function for drawing arcs from package:plotrix#
    draw.arc <- function (x = 1, y = NULL, radius = 1, angle1 = deg1 * pi/180, angle2 = deg2 * pi/180, deg1 = 0, deg2 = 45, n = 35, col = 1, ...){
      draw.arc.0 <- function(x, y, radius, angle1, angle2, n, col = col, ...){
        xylim <- par("usr")
        plotdim <- par("pin")
        ymult <- (xylim[4] - xylim[3])/(xylim[2] - xylim[1]) * plotdim[1]/plotdim[2]
        angle <- angle1 + seq(0, length = n) * (angle2 - angle1)/n
        p1x <- x + radius * cos(angle)
        p1y <- y + radius * sin(angle) * ymult
        angle <- angle1 + seq(length = n) * (angle2 - angle1)/n
        p2x <- x + radius * cos(angle)
        p2y <- y + radius * sin(angle) * ymult
        segments(p1x, p1y, p2x, p2y, col = col, ...)
      }
      xy <- xy.coords(x, y)
      x <- xy$x
      y <- xy$y
      a1 <- pmin(angle1, angle2)
      a2 <- pmax(angle1, angle2)
      angle1 <- a1
      angle2 <- a2
      args <- data.frame(x, y, radius, angle1, angle2, n, col, stringsAsFactors = FALSE)
      for (i in 1:nrow(args)) do.call("draw.arc.0", c(args[i, ], ...))
      invisible(args)
    }
    
    #Create standardized plot#
    x11(width=10,height=7.5)
    par(mfrow=c(1,2),mar=c(0,0,0,0))
    plot(0,0, pch=NA,xlab=NA,ylab=NA,xlim=c(-2.25,2.25),ylim=c(0,4.5),asp=1,xaxt='n',yaxt='n',frame.plot=FALSE)
    text(0,4.75,"NADAI PLOT",cex=1.5)
    text(0,4.25,expression(nu),cex=1.5)
    text(-2,3.75,"-1")
    text(2,3.75,"1")
    text(-1,4.1,"Prolate",srt=15)
    text(1,4.1,"Oblate",srt=345)
    text(-1.75,1.75,expression(bar(epsilon)[s]),cex=1.5)
    text(.25,0,"0")
    text(.75,.866,"1")
    text(1.25,1.732,"2")
    text(1.75,2.598,"3")
    text(2.25,3.464,"4")
    lines(c(0,0),c(0,4),lwd=1.5)
    lines(c(0,2),c(0,3.46),lwd=1.5)
    lines(c(0,-2),c(0,3.46),lwd=1.5)
    lines(c(0,1.04),c(0,3.86),lwd=1)
    lines(c(0,-1.04),c(0,3.86),lwd=1)
    draw.arc(x=0,y=0,radius=4,angle=(pi)/3,angle2=(2*pi)/3,lwd = 1.5)
    draw.arc(x=0,y=0,radius=3,angle=(pi)/3,angle2=(2*pi)/3,lwd = 1)
    draw.arc(x=0,y=0,radius=2,angle=(pi)/3,angle2=(2*pi)/3,lwd = 1)
    draw.arc(x=0,y=0,radius=1,angle=(pi)/3,angle2=(2*pi)/3,lwd = 1)
    points(xcoord,ycoord,pch=20)
    index<-1:(length(axDat$X))
 
    #Create data table#
    nadPar<-data.frame(index,octSS,lodes)
    plot(0,0, pch=NA,xlab=NA,ylab=NA,xlim=c(-2,2),ylim=c(0,4.5),asp=1,xaxt='n',yaxt='n',frame.plot=FALSE)
    text(-1,4.25,"Index",cex=1.5,adj=1)
    text(-.375,4.25,expression(bar(epsilon)[s]),cex=1.5)
    text(.375,4.25,expression(nu),cex=1.5)
    lines(c(-2,2),c(4,4),lwd=1.5)
    
    #Add side table of compiled data#
    i<-0
    while(i<length(axDat$X)){
      i<-i+1
      text(-1,4.25-(i*.5),index[i],col='red',adj=1)
      text(-.375,4.25-(i*.5),round(octSS[i],3))
      text(.375,4.25-(i*.5),round(lodes[i],3))
      lines(c(-2,2),c(4-(i*.5),4-(i*.5)))
    }
  }
}

