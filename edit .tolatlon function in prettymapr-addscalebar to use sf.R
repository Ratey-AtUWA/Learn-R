# issue seems to be just with this function '.tolatlon'

# nominal x and y values, and desired epsg:
x <- 400100; y <- 6468200 ; epsg <- 32750

.tolatlon <- function(x, y, epsg) {
  # for lat/lon coordinates just return x and y
  if(epsg == 4326) return(cbind(x, y))
  
  # require sf for actual projections
  if(!requireNamespace("sf")) stop("package 'sf' is required when coordinates are not WGS84 lat/lon")

  coords <- data.frame(x=x, y=y)
  spoints <- sf::st_as_sf(coords, coords=c("x","y"), crs=sf::st_crs(epsg))
  spnew <- sf::st_transform(spoints, crs=sf::st_crs(4326))
  as.vector(sf::st_coordinates(spnew))
}
