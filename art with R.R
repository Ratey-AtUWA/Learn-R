par(mar = c(.1,.1,.1,.1), xpd=T)
plot(c(0,1),c(0,1), axes=F, type="n",ann=F)
for (i in 1:100) {
  symbols(jitter(seq(0,1,l = 100)),sample(seq(0,1,l=100),100), circles=sample(seq(0,.01,l=100),100),add=T,
          fg="transparent", 
          bg = rainbow(100,s=0.75, v=0.75, start=0.5, end = 0.725, alpha=0.5), lwd=2, inches=F)
}

plot(c(0,1),c(0,1), axes=F, type="n",ann=F)
for (i in 1:100) {
  symbols(sample(seq(0,1,0.01),100),sample(seq(0,1,0.01),100), squares = sample(seq(0,1,0.01),100),add=T,
          fg="transparent", 
          bg = sample(heat.colors(100, alpha=0.5),100), lwd=2, inches=0.5)
}

plot(c(0,1),c(0,1), axes=F, type="n",ann=F)
for (i in 1:100) {
  symbols(sample(seq(0,1,0.01),100),sample(seq(0,1,0.01),100), add=T, 
          rectangles = as.matrix(cbind(sample(seq(0,1,0.01),100),sample(seq(0,1,0.01),100))),
          fg="transparent", 
          bg = sample(terrain.colors(100, alpha=0.5),100), lwd=2, inches=0.5)
}