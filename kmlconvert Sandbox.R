kmlconvert <- function(file = NULL, utmzone = 50, hemi = "south") {
  require(sf)
  require(maptools)
  LongLat <- st_crs(4326)
  UTM <- st_crs(paste0("+proj=utm +zone=",utmzone," +",hemi))
  
  kml_0 <- getKMLcoordinates(file, ignoreAltitude = T)
  if(is.matrix(kml_0[[1]])){
    coords_0 <- as.data.frame(matrix(unlist(kml_0), length(unlist(kml_0))/2, 2))
    colnames(coords_0) <- c("x","y")
    LL_0 <- st_as_sf(coords_0, coords=c("x","y"), crs = LongLat)
    utm_0 <- st_transform(LL_0, crs = UTM)
    rslt_0 <- as.data.frame(cbind(st_coordinates(LL_0), st_coordinates(utm_0)))
    colnames(rslt_0) <- c("Longitude", "Latitude", "Easting", "Northing")
    cat("Path, line, or polygon\n")
    print(rslt_0, digits=8, row.names=F)
    write.table(rslt_0, file="clipboard", row.names = F, sep="\t")
    cat("\nOutput data also copied to clipboard\n")
  } else {
    coords_0 <- as.data.frame(matrix(unlist(kml_0), length(kml_0), 2, byrow=T))
    colnames(coords_0) <- c("x","y")
    LL_0 <- st_as_sf(coords_0, coords=c("x","y"), crs = LongLat)
    utm_0 <- st_transform(LL_0, crs = UTM)
    rslt_0 <- as.data.frame(cbind(st_coordinates(LL_0), st_coordinates(utm_0)))
    colnames(rslt_0) <- c("Longitude", "Latitude", "Easting", "Northing")
    cat("Individual points\n")
    print(rslt_0, digits=8, row.names=F)
    write.table(rslt_0, file="clipboard", row.names = F, sep="\t")
    cat("\nOutput data also copied to clipboard\n")
  }
  return(rslt_0)
  rm(list = ls(pattern = "_0"))
}
