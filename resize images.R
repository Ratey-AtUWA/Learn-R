library(jpeg)
library(imager)

idir <- "C:/Users/00028958/OneDrive - The University of Western Australia/My Documents/aaTeaching/ENVT3361 EnvAssess/Field ENVT3361/AFR-photos-2024/"

flist <- list.files(idir)
# flist1 <- flist[1:2]
# i <- 1
for(i in 1:length(flist)){
  pic <- load.image(paste0(idir,flist[i]))
  pic <- imresize(pic, scale=0.2)
  save.image(pic,
             file = paste0(idir,substr(flist[i],0,8),"_s",".jpg"))
}
