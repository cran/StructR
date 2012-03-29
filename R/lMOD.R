lMOD <-
function(trend = 0, plunge = 0, x.loc = 0, y.loc = 0, z.loc = 0, scale.factor = 1, lweight = 1, color = "blue"){
  x.a <- 0
  y.a <- .5
  z.a <- 0
  x.b <- 0
  y.b <- (-.5)
  z.b <- 0
  
  a <- c(x.a, y.a, z.a)
  b <- c(x.b, y.b, z.b)
  
  lmat <- matrix(rbind(a,b), ncol = 3)
  lmat <- rotate3d(lmat, plunge * (pi / 180), -1, 0, 0)
  lmat <- rotate3d(lmat, ((trend + 180) %% 360) * (pi / 180), 0, 0, 1)
  lmat <- scale3d(lmat, scale.factor, scale.factor, scale.factor)
  lmat <- translate3d(lmat, x.loc, y.loc, z.loc)
  lines3d(lmat, col = color, lwd = lweight)
}

