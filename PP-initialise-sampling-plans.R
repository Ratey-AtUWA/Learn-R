# check packages
packages <- c("sf", "maptiles","prettymapr","TeachingDemos")
install.packages(setdiff(packages, installed.packages()[,1]))

# load packages
library(sf)
library(maptiles)
library(prettymapr)
library(TeachingDemos)

pp_samparea <- 
  st_read("https://github.com/Ratey-AtUWA/Learn-R/raw/main/PP/sampling_area.kml") |> 
  st_transform(crs=st_crs(32750))

sa1 <- as.matrix(st_coordinates(pp_samparea)[,1:2])
sah1 <- 
  st_read("https://github.com/Ratey-AtUWA/Learn-R/raw/main/PP/rushes.kml") |> 
  st_transform(crs=st_crs(32750)) |> 
  st_coordinates() |> as.matrix()
sah2 <- 
  st_read("https://github.com/Ratey-AtUWA/Learn-R/raw/main/PP/dense-veg.kml") |> 
  st_transform(crs=st_crs(32750)) |> 
  st_coordinates() |> as.matrix()
sah3 <- 
  st_read("https://github.com/Ratey-AtUWA/Learn-R/raw/main/PP/depot.kml") |> 
  st_transform(crs=st_crs(32750)) |> 
  st_coordinates() |> as.matrix()
samppoly <- st_polygon(list(sa1,sah1[,1:2],sah2[,1:2],sah3[,1:2])) 
samppoly <- st_sfc(samppoly, crs=st_crs(32750))

pp0 <- 
  read.csv(file="https://github.com/Ratey-AtUWA/Learn-R/raw/main/PP/PelicanPtResBorder.csv")
ppb <- st_polygon(list(as.matrix(pp0))) |> st_sfc(crs=st_crs(32750))

ppext <- st_as_sf(data.frame(x= c(388550,389150), y=c(6460300,6460700)),
                  coords = c("x","y"), crs=st_crs(32750))
ppmap <- get_tiles(ppext, crop=T, zoom=18, provider="CartoDB.Positron")

