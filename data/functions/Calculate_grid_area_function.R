#use the corners to calculate area
Calculate_grid_area = function(x) {
  #x should be a matrix in the form of (lon1, lat1, lon2, lat2, ....)
  Area_in_survey_km2 = numeric(dim(x)[1])
  for (l in 1:dim(x)[1]) {
    Tmp = matrix(x[l,], nc = 2, byrow = T)
    #plot(Tmp)
    #identify(x=Tmp, labels = 1:5)
    Area_in_survey_km2[l] = areaPolygon(Tmp)/1E6
  }
  return(Area_in_survey_km2)
}