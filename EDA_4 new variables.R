#### -+-+-+-+- EDA WORKFLOW 4: New Variables -+-+-+-+- ####

# Transformations ####
# We would base any transformation of variables on the 
# results of our normality tests and visualisations.
# So we should SAVE OUR OUTPUT as we go 
# (in R, Excel, etc.)

# In many cases it's easy enough to do the
# transformations we need one by one:
afs20$Ba.log <- log10(afs20$Ba)
afs20$Ce.pow <- afs20$Ce^0.5818
afs20$La.pow <- afs20$La^0.5675
afs20$Pb.pow <- afs20$Pb^0.2447
afs20$Y.pow <- afs20$Y^0.3629

# In other instances we could program a loop for 
# many transformations

# Checking transformed variables
shapiro.test(afs20$Ba.log)
boxplot(afs20$Ba.log)

# Creating NEW VARIABLES (not by transformation) ####

# A common new variable to create is that of distance from 
# a source, or distance downstream in a drain or river, etc.
# We have spatial data, so it makes sense to make use of the
# coordinates in various ways.
# For example, in the afs20 dataset, we have a grid of 
# samples in two wetland ponds which we can visualise 
# with the following code:
par(mar = c(4,4,1,1), mgp = c(1.7,0.3,0), tcl = 0.3,
    lend = "square", ljoin = "mitre", font.lab = 2)
with(afs20, 
     plot(Northing ~ Easting,
          pch = c(1, 15, 3)[Zone],
          col = c(1, 4, 6)[Zone],
          asp = 1, font.lab=2)
     )
lines(c(400053,400417), c(6467971, 6468330),
      lwd = 3, col = 5)
text(c(400240,400050,400050,400205),
     c(6468250,6468200,6468050,6468120),
     labels = c("N","NW","S","Chapman Drain"),
     col = c(1,4,6,5), cex = c(1.5,1.5,1.5,1),
     pos = c(2,2,3,4))

# Focusing on the N wetland:
# The simplest analysis would be to assume that if
# contamination is coming from the drain, we can 
# use the Easting values to represent distance from
# the drain.
# First separate the N wetland data:
afs20N <- subset(afs20, Zone=="N")

# Then make distance increase away from the drain
# by subtracting Easting from a value >= maximum:
afs20N$dist <- max(afs20N$Easting)-afs20N$Easting

# Let's label the points with the calculated distance:
with(afs20N,
     plot(Northing ~ Easting, asp=1, 
          pch = 10, cex = 1.5, col = "red2")
     )
with(afs20N,
     text(Easting, Northing, labels = dist, pos=1)
)
# So we can see that the values we calculated are not 
# really that useful, e.g. in a regression model.

# We can do better by assuming an input point; if we 
# look at Google Earth, there seems to be one at 
# approximately 400306 E, 6468230 N. 
# We can use Pythagoras' Theorem to calculate distances
# from this point: 
afs20N$dist <- with(afs20N,
                    sqrt((Easting - 400306)^2 + 
                          (Northing - 6468230)^2)
                    )
# then plot similar to before:
with(afs20N,
     plot(Northing ~ Easting, asp=1, 
          pch = 10, cex = 1.5, col = "red2")
)
with(afs20N,
     text(Easting, Northing, labels = round(dist,1), pos=1)
)
# this looks better, so let's try something:
with(afs20N,
     plot(Cu ~ dist, pch = 3, lwd = 2, cex = 1.5, col = 4,
          xlab = "Distance (m)", ylab = "Cu (mg/kg)")
)
abline(lm(afs20N$Cu ~ afs20N$dist), col = 8)
summary(lm(afs20N$Cu ~ afs20N$dist))

# Interesting!

# We don't know for sure that 400306 E, 6468230 N is the 
# input point, and there seems to be another at 
# about 400253 E, 6468208 N.
# Try recalculating and re-plotting, and see if this 
# makes any sense!

# Of course the input from the drain may not be from a
# single point, but just from overflow along the length
# of the drain itself, so we should calculate distance  
# from the drain. In that case we need an equation for 
# the drain line. Two points on the drain are:
# 400053 E 6467971 N, and 400417 E 6468330 N
# (we used them above to plot the drain line),
# so let's get the equation:
chapman <- lm(c(6467971, 6468330) ~ c(400053,400417))
draindata <- summary(chapman)
draindata
# (we could also calculate slope and intercept using algebra)
# We would like more precision for the slope and intercept:
signif(as.numeric(draindata$coefficients), 7)

# Now we use a formula for the shortest distance from a
# point to a line:
# If Easting is x and Northing is y, our linear equation is
#      y = 6073413 + 0.9862637x
# We need the line to be in the form ax + by + c = 0
# So our equation is 0.9862637x - y + 6073413 = 0
# meaning a = 0.9862637, b = -1, and c = 6073413
#
# The formula for (orthogonal) distance of a point (x0, y0) 
# to a line is:
#     d = (|ax0 + by0 + c|)/sqrt(a^2 + b^2)
# which we can code into R:
a <- 0.9862637; b <- -1; c <- 6073413
a;b;c # check them!
# the formula:
afs20N$dist = with(afs20N,
                (abs(a*Easting + b*Northing + c))/(sqrt(a^2 + b^2))
              )

# The plots
par(mfrow = c(2,1))
# "Map"
with(afs20N,
     plot(Northing ~ Easting, asp=1, 
          pch = 10, cex = 1.5, col = "red2")
)
with(afs20N,
     text(Easting, Northing, labels = round(dist,1), pos=1, cex=.8)
)
mtext("Labels are orthogonal distances\nfrom the drain (m)", 
      side = 3, line = -2, adj = 0.98, cex = 0.8)
lines(c(400053,400417), c(6467971, 6468330),
      lwd = 3, col = 5)
text(400307,6468220, labels = "Chapman\nDrain",
     col = 5, cex = 0.8, pos = 4)
# Concentration vs. distance
with(afs20N,
     plot(Pb ~ dist, pch = 12, lwd = 2, cex = 1.2, col = 4,
          xlab = "Distance (m)", ylab = "Pb (mg/kg)")
)
abline(lm(afs20N$Pb ~ afs20N$dist), col = 8)
mtext("Line is simple linear model\n(Pb ~ Distance)", 
      side = 3, line = -2, adj = 0.98, cex = 0.8, col = 8)
summary(lm(afs20N$Pb ~ afs20N$dist))
par(mfrow = c(1,1))

# Tidy up
rm(list = c("a","b","c","chapman","draindata"))
