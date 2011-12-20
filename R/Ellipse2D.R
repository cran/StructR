Ellipse2D <-
function(hlaxa = 1, hlaxb = 1, theta = 0, xc = 0, yc = 0, npoints = 50, sty="red"){
  angle <- function(x, y){
    angle2 <- function(xy) {
      x <- xy[1]
      y <- xy[2]
      if (x > 0){
        atan(y / x)
      }
      else{
        if (x < 0 & y != 0){
          atan(y / x) + sign(y) * pi
        }
        else{
          if(x < 0 & y == 0){
            pi
          }
          else{
            if(y != 0){
              (sign(y) * pi) / 2
            }
            else{
              NA
            }
          }
        }
      }
    }
    apply(cbind(x, y), 1, angle2)
  }

  a <- seq(0, 2 * pi, length = npoints + 1)
  x <- hlaxa * cos(a)
  y <- hlaxb * sin(a)
  alpha <- angle(x, y)
  rad <- sqrt(x^2 + y^2)
  xp <- rad * cos(alpha + theta) + xc
  yp <- rad * sin(alpha + theta) + yc
  lines(xp, yp, col = sty, lwd =2)
  invisible()
}

