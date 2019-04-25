#find the corners of the grid
find_vertex_based_on_centroid = function(x, lon.length = 0.05, lat.width = 0.05) {
  #this function is only for finding vertex of a square based on its centroid
  #x, longitude/latitude of the center points, 
  #which must be a matrix or data frame of 2 columns(first one is lon, second is lat)
  #lon.length and lat.width are the length and width of the square
  vertex.dir = matrix(c(1, 1, 1, -1, -1, -1, -1, 1, 1, 1), nr = 5, byrow = T)
  vertex.loc = c(lon.length, lat.width) * vertex.dir / 2
  vertex.coors = as.matrix(x) + matrix(vertex.loc[1,], nr = dim(x)[1], nc = 2, byrow = T)
  for (l in 2:5){
    Temp = x + matrix(vertex.loc[l,], nr = dim(x)[1], nc = 2, byrow = T)
    vertex.coors = cbind(vertex.coors, Temp)
  }
  colnames(vertex.coors) = c(paste0("vertex", rep(1:5, each = 2), rep(c("Lon", "Lat"), times = 5)))
  return(as.matrix(vertex.coors))
}