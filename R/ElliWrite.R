ElliWrite <-
function(path, section.a, section.b, section.c, ort.a, ort.b, ort.c){
  strike.a <- ort.a[1]
  strike.b <- ort.b[1]
  strike.c <- ort.c[1]
  
  dip.a <- ort.a[2]
  dip.b <- ort.b[2]
  dip.c <- ort.c[2]
  
  rake.a <- round((pi - section.a$elli.azimuth) * (180 / pi), 1)
  rake.b <- round((pi - section.b$elli.azimuth) * (180 / pi), 1)
  rake.c <- round((pi - section.c$elli.azimuth) * (180 / pi), 1)
  
  norm <- (1 / min(c(section.a$a.half.axis, section.a$b.half.axis, section.b$a.half.axis, section.b$b.half.axis, section.c$a.half.axis, section.c$b.half.axis)))
  
  
  l.a <- round(section.a$a.half.axis * norm, 1)
  s.a <- round(section.a$b.half.axis * norm, 1)
  l.b <- round(section.b$a.half.axis * norm, 1)
  s.b <- round(section.b$b.half.axis * norm, 1)
  l.c <- round(section.c$a.half.axis * norm, 1)
  s.c <- round(section.c$b.half.axis * norm, 1)
  
  #Write .elli file for parameters#
  sink(file = path)
  cat("#", "strike","dip","rake","long axis","short axis","","","","","","","\n",sep="\t")
  cat("1", "\t", strike.a, "\t", dip.a, "\t", rake.a, "\t", l.a, "\t", s.a, "\t", 1, "\n", sep="")
  cat("2", "\t", strike.b, "\t", dip.b, "\t", rake.b, "\t", l.b, "\t", s.b, "\t", 1, "\n", sep="")
  cat("3", "\t", strike.c, "\t", dip.c, "\t", rake.c, "\t", l.c, "\t", s.c, "\t", 1, "\n\n", sep="")
  sink()
}

