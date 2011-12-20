kmlDPP <-
function(NAME = "Sandy Hollow", LAT = 45.45384, LON = -112.5655, DIST = 2500, TREND = 350, PLUNGE = 17, FILE=NULL){
  if(missing(FILE)){
    sink(file="kmlDPP.kml")
  }
  else(sink(file=FILE))
  
  cat('<?xml version="1.0" encoding="UTF-8"?>',"\n",sep="")
  cat("<kml>\n")
  cat("\t<Document>\n")
  
  i<-0
  while(i < length(NAME)){
    i<-i+1
    cat("\t\t<Placemark>\n")
    cat("\t\t<name>",toString(NAME[i]),"</name>\n",sep="")
    cat("\t\t\t<LookAt>\n")
    cat("\t\t\t\t<longitude>",LON[i],"</longitude>\n",sep="")
    cat("\t\t\t\t<latitude>",LAT[i],"</latitude>\n")
    cat("\t\t\t\t<altitude>0</altitude>\n")
    cat("\t\t\t\t<range>",DIST,"</range>\n",sep="")
    cat("\t\t\t\t<tilt>",(90-PLUNGE[i]),"</tilt>\n",sep="")
    cat("\t\t\t\t<heading>",TREND[i],"</heading>\n",sep="")
    cat("\t\t\t</LookAt>\n")
    cat("\t\t\t<Style>\n")
    cat("\t\t\t\t<IconStyle>\n")
    cat("\t\t\t\t\t<Icon>\n")
    cat("\t\t\t\t\t\t<href>http://maps.google.com/mapfiles/kml/shapes/water.png</href>\n")
    cat("\t\t\t\t\t</Icon>\n")
    cat("\t\t\t\t</IconStyle>\n")
    cat("\t\t\t</Style>\n")
    cat("\t\t\t<Point>\n")
    cat("\t\t\t\t<coordinates>",LON[i],",",LAT[i],",0","</coordinates>\n",sep="")
    cat("\t\t\t</Point>\n")
    cat("\t\t</Placemark>\n")
  }
  cat("\t</Document>\n")
  cat("</kml>\n")
  sink()
}

