# read data
sv18 <- read.csv("https://github.com/Ratey-AtUWA/Learn-R/raw/main/SL18.csv")

##### ____1. manipulating graphics parameters ____ ####
# after using par() these stay changed...
# ...until changed again with par() . For example:
plot(sv18$Ca ~ sv18$pH, log="y")
# now try changing some graphics options using the par() function
# and re-running the simple plot
par(mar=c(4,4,1,1), mgp=c(1.8,0.7,0), font.lab=2, cex=1.5, cex.axis=1.25, cex.lab=1.5,
    tcl=0.2)
plot(sv18$Ca ~ sv18$pH, log="y")
#
par(mfrow=c(2,2), mar=c(4,4,1,1), mgp=c(1.2,0.2,0), 
    font.lab=2, cex=1., cex.axis=.8, cex.lab=1.,
    tcl=0.2)
plot(sv18$Ca ~ sv18$pH, log="y", pch=0)
plot(sv18$Mg ~ sv18$pH, log="y", pch=1)
plot(sv18$Na ~ sv18$pH, log="y", pch=2)
plot(sv18$Sr ~ sv18$pH, log="y", pch=3)
#
##### ____2. plots of variable distributions ____ ####
#
# Cumulative Distribution Function using plot.ecdf()
# custom y-axis label explains plot! 
# (note use of \u to insert Unicode character by 4-digit code 
#     [2264 is 'less than or equal to'] )
plot.ecdf(sv18$pH, xlab="x = pH", ylab="Proportion of samples with pH \u2264 x", main="") 
#
# a better cumulative plot is the normal quantile or 'q-q' plot
# specially transformed axes mean that a normally distributed variable
# will plot as a straight line. qqPlot() from the car package is best;
# the plot shows the theoretical line and the 95% confidence range.
require(car)
qqPlot(sv18$pH, 
       xlab="Theoretical normal distribution quantiles",
       ylab="Soil pH")
# q-q- plots can be grouped by a factor:
qqPlot(sv18$pH~sv18$Type, 
       xlab="Theoretical normal distribution quantiles",
       ylab="Soil pH")
#
# histograms - we've seen these in a previous Workshop
# here's another customization
hist(log10(sv18$Zn), breaks=seq(0.4,3.2,0.2),
     xlab="log10(Zn, mg/kg)", main="")
# add a vertical line at a threshold value (e.g guideline)
abline(v=log10(300), lty=2, col="red")
# ...and another
hist(log10(sv18$Zn), 
     freq=FALSE,
     breaks=seq(0.4,3.2,0.2),
     xlab="log10(Zn, mg/kg)", main="")
# add smooth probability density curve
lines(density(na.omit(log10(sv18$Zn))), 
      lty=2, col="purple")
#
# boxplots - we've also seen these in a previous Workshop
# here's a similar customization
boxplot(log10(sv18$Zn) ~ sv18$Type, 
        xlab="Sample Type", ylab="log10(Zn, mg/kg)", main="")
# plot it again with better font size to copy-paste into a report
# plus complex formatting for y-axis label
par(mar=c(4,4,1,1), mgp=c(2,0.6,0), font.lab=2)
boxplot(log10(sv18$Zn) ~ sv18$Type, 
        xlab="Sample Type", 
        ylab=expression(bold(paste(log[10],"(Zn, mg/kg)"))), 
        main="",
        cex.axis=1.5, cex.lab=1.5,
        col="moccasin")
# add a HORIZONTAL line at a threshold value (e.g guideline)
abline(h=log10(300), lty=2, col="red")
# label it using text()
text(1,log10(300), labels="Zn limit 300 mg/kg", 
     cex=1.4, col="red", pos=3)
#
##### ____3. variations on scatter plots ____ ####
#
plot(sv18$Fe ~ sv18$Depth_lower)
plot(sv18$Fe ~ sv18$Depth_upper)
# can't use a formula in a formula! ...
# ...so change syntax for plot variables to put calculation in 
plot(((sv18$Depth_upper+sv18$Depth_lower)/2), sv18$Fe)
# (we could also do this by calculating a new variable, e.g.:
#   sv18$Depth.mean <- (sv18$Depth_upper+sv18$Depth_lower)/2
#   and use sv18$Depth.mean as the variable)
#
# put depth on [more intuitive] vertical axis
plot(sv18$Fe, ((sv18$Depth_upper+sv18$Depth_lower)/2))
plot(sv18$Fe, ((sv18$Depth_upper+sv18$Depth_lower)/2), ylim=c(0,100))
# use ylim() to reverse direction of y axis - swap (0,100) for (100,0)
plot(sv18$Fe, ((sv18$Depth_upper+sv18$Depth_lower)/2), ylim=c(100,0))
#
# now tidy up plot margins etc. - use help to understand arguments...
par(mar=c(4,4,1,1),
    mgp=c(1.6,0.5,0),
    font.lab=2)
#... and plot it again with nice axis labels
plot(sv18$Fe, ((sv18$Depth_upper+sv18$Depth_lower)/2), ylim=c(100,0), 
     xlab="Fe (mg/kg)", ylab="Mean depth (cm)")
# what if we want the horizontal axis 'on top'?
# omit axis plotting and axis label...
plot(sv18$Fe, ((sv18$Depth_upper+sv18$Depth_lower)/2),
     xlab="", xaxt="n",
     ylim=c(100,0), 
     ylab="Mean depth (cm)")
# ... then plot axis manually, first adjusting plot margins
par(mar=c(1,4,4,1)) # won't change anything except margins
plot(sv18$Fe, ((sv18$Depth_upper+sv18$Depth_lower)/2),
     xlab="", xaxt="n",
     ylim=c(100,0), 
     ylab="Mean depth (cm)")
# two code lines for manual axis plus label
axis(3)
mtext(side=3, line=1.6, font=2, text="Fe (mg/kg)")
#
##### ____4. scatterplot matrices ____ ####
# a scatterplot matrix can be a very useful data exploration tool
# (NB you might need to change the default colours)
require(car)
scatterplotMatrix(~pH + Al + Ca + Fe, data=sv18, smooth=FALSE)
# or
scatterplotMatrix(~pH + Al + Ca + Fe + Na | Type, data=sv18, smooth=FALSE)
#
scatterplotMatrix(~pH + Al.log + Ca.log + Fe.log + Na.log | Type, data=sv18, smooth=FALSE)
#
##### ____5. =-=-=-=-=- maps =-=-=-=-= ____ ####
# maps are [mostly] just scatterplots with a map background
# we will spend a separate session on maps in R - later
plot(UWA.map, removeMargin = F)
points(places$Easting,places$Northing, 
       pch=c(16,17)[places$Type], 
       col=c("sienna","blue")[places$Type])
text(places$Easting,places$Northing, labels=places$Type, 
     pos=4, col=c("sienna","blue")[places$Type])
axis(1)
mtext(side=1, line=2, text="UTM Easting (m)", font=2, cex=1.4)
axis(2)
mtext(side=2, line=2, text="UTM Northing (m)", font=2, cex=1.4)
