library(imager)
library(RColorBrewer)
png(file="./images/rcb-col.png", width = 600, height = 600)
par(mar=c(0,4,0,0))
display.brewer.all(colorblindFriendly = TRUE)
text(c(2,2,2),c(29.3,10.3,6.3),pos=2,font=2,
     labels=c("Gradational","Qualitative","Diverging"))
par(mar = c(3.5,3.5,0.5,0.5), font.lab=2, mgp=c(1.6,0.3,0), tcl=0.25, 
    lend="square", ljoin="mitre")
dev.off()
rcb_col <- imager::load.image("./images/rcb-col.png")
rcb_gry <- grayscale(rcb_col)
save.image(rcb_gry, "./images/rcb-gry.png")
logo <- readPNG("./images/rcb-gry.png") # add our own image
par(mar=c(0,0,0,0))
plot(c(0,1),type="n",axes=F,bty="n",xlab="",ylab="",xaxs="i",yaxs="i")
addImg(logo, x = 1.5, y = 0.5, width = .8)
par(mar = c(3.5,3.5,0.5,0.5), font.lab=2, mgp=c(1.6,0.3,0), tcl=0.25, 
    lend="square", ljoin="mitre")
file.remove("./images/rcb-col.png")
file.remove("./images/rcb-gry.png")
