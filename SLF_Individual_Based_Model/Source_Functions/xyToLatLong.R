#functions to convert cartesian coords to lat/long (warning: using linear interpolation)
coord2long<-function(inX,minX,maxX,minlong, maxlong){
  outLong = (inX - minX) * (maxlong - minlong) / (maxX - minX) + minlong
  return(outLong)
}

coord2lat<-function(inY,minY,maxY, minlat, maxlat){
  outLat = (inY - minY) * (maxlat - minlat) / (maxY - minY) + minlat
  return(outLat)
}


long2coord<-function(longIn,minX, maxX, minlong, maxlong){
  outX <- (maxX - minX) * (longIn - minlong)/ (maxlong-minlong) + minX
  return(floor(outX))
}

lat2coord<-function(latIn,minY, maxY, minlat, maxlat){
  outY <- (maxY - minY) * (latIn - minlat)/ (maxlat-minlat) + minY
  return(floor(outY))
}


