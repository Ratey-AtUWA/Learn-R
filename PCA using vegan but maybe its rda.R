library(vegan)
pcadata <- na.omit(afs20_clr)
names(pcadata)[9] <- "species"
names(pcadata)[5] <- "sites"
rda0 <- rda(scale(pcadata[,10:38]))
biplot(rda0)

biplot(rda0,
       display = c("sites", 
                   "species"),
       type = c("text",
                "points"))
ordiellipse(rda0,
         group = pcadata$species,
         col=c("black","royalblue","gold3"),
         lty=c(1,2,4), lwd=c(1,2,3), 
         conf = 0.95)
legend("topright", seg.len=4,
       col = c("black","royalblue","gold3"), 
       lty = c(1,2,4), lwd=c(1,2,3),
       legend = levels(pcadata$species),
       inset=0.02, bty="n",
       title="Sampling Zone")
