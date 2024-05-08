# code authored by andrew rate (c) the university of western australia 2016-2024
#
# example for zinc - case-sensitive search & replace to make plots for other
# variables, and the dataset used is called 'sv18', so you may need to
# find/replace this too!
#
# make an object containing a table of means
Znstats <- with(sv18, tapply(log10(Zn), Type, mean, na.rm=T))
#
# plot the boxplot with nice axis titles etc.
par(mar=c(4,4,1,1), mgp=c(2,0.6,0), ljoin="mitre", lend="square", font.lab=2)
boxplot(log10(Zn)~Type, data=sv18, id.method="y", xlab="Sample Type", 
        ylab="log10 { Zn (mg/kg) }", notch=T, cex.lab=1.6, cex.axis=1.4, 
        col="moccasin")
#
# plot the means as a symbol 
points(Znstats, col="blue", pch=3, cex=1.5, lwd=2)
#
# optionally add a horizontal line to represent some threshold or guideline
# values
abline(h=log10(200),lty=2,col="red",lwd=2)
#
# add a legend (especially if a threshold line was added!)
# Note use of \n to add a line break
legend("bottomright",legend=c("Mean","Pretend threshold \nat 200 mg/kg"),
       pch=c(3,3), pt.cex=c(1.5,0), pt.lwd=c(2,-1), cex=1, col=c("blue","red"),
       lty=c(-1,2),lwd=c(-1,2), bty="n", inset=0.02)
text(.4,3,labels="Means on boxplot", pos=4, cex=1.5, font=3)
#
# delete the object contaning the table of means (to keep R tidy)...
rm(Znstats)
#
# end code
