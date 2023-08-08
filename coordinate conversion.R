#### Coordinate conversion in R ####
# load required packages
library(sp)
library(rgdal)

# define coordinate reference systems (CRS)
# we want to convert between...

LongLat <- CRS("+proj=longlat +ellps=WGS84 
               +datum=WGS84 +no_defs") # uses Earth ellipsis specs from WGS84 datum
UTM50 <- CRS("+proj=utm +zone=50 +south") # just for Zone 50, S hemisphere!

### 1. converting UTM to LongLat ####

# make a temporary object containing the UTM coordinates
# Example uses data frame called data1 - edit to suit!
utm.temp <- SpatialPoints(coords = data1[,c("Easting","Northing")], 
                                   proj4string = UTM50)

# convert to long-lat with results in another temp object
longlat.temp <- spTransform(utm.temp, CRSobj = LongLat)

# correctly name columns in temporary long-lat object
colnames(longlat.temp@coords) <- c("Longitude","Latitude")

# write converted coordinates back into data frame
data1$Longitude <- longlat.temp@coords[,"Longitude"]
data1$Latitude <- longlat.temp@coords[,"Latitude"]

# check them
data1[,c("Longitude", "Latitude")]

### 2. converting Long-Lat to UTM ####

# make a temporary object containing the Long-Lat coordinates
# Example uses data frame data1 (that we just made Long-Lat for)
ll.temp <- SpatialPoints(coords = data1[,c("Longitude","Latitude")], 
                          proj4string = LongLat)

# convert to long-lat with results in another temp object
utm.temp <- spTransform(utm.temp, CRSobj = UTM50)

# correctly name columns in temporary UTM object
colnames(utm.temp@coords) <- c("E_UTM","N_UTM")

# write converted coordinates back into data frame
# the names you give columns are your choice!
data1$E_UTM <- longlat.temp@coords[,"E_UTM"]
data1$N_UTM <- longlat.temp@coords[,"N_UTM"]

# check them
data1[,c("E_UTM", "N_UTM")]

# remove temporary objects
rm(list = c("ll.temp","utm.temp"))

# another useful CRS (sometimes)
webMercator <- CRS(paste0("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0",
                          " +a=6378137 +b=6378137 +units=m +no_defs"))

