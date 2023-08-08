#### -+-+-+-+- EDA WORKFLOW 5: New Factors -+-+-+-+- ####

# 5.1 New Factors based on distributions of variables ####
hist(log10(sv18$Ca), breaks = 16, freq = FALSE)
lines (density(log10(sv18$Ca)), col = 4, lwd = 2)
abline(v = 3.6, col = 2, lty = 2, lwd = 2)

# use cut() to make a new factor based on separating 
# the 2 populations based on Ca concentration
sv18$Ca_split <- cut(sv18$Ca, 
                     breaks = c(0,10^3.6,999999), 
                     labels = c("low Ca","high Ca"))

# See if the 2 apparent populations correspond to
# differences in any other variables
# Initially try Gd and Zn

boxplot(log10(sv18$Zn) ~ sv18$Ca_split, notch = T)
# try a t-test, we could also use wilcox.test()
t.test(log10(sv18$Zn) ~ sv18$Ca_split)
tapply(sv18$Zn, sv18$Ca_split, mean, na.rm = TRUE)

# we can use a loop to use factor to test means for
# many variables:
# make names vector and data frame before running loop
varz <- names(sv18)
length(10:40)
table <- data.frame(Variable = varz[10:40],
                    t_test_p = rep(NA,31), 
                    Wilcox_p = rep(NA,31),
                    Mean_near = rep(NA,31), 
                    Mean_far = rep(NA,31))
# run loop using vector of selected column numbers
for(i in c(10:11,15:40)){
  tt0 <- with(sv18, t.test(log10(sv18[,i]) ~ Ca_split))
  wx0 <- with(sv18, wilcox.test(sv18[,i] ~ Ca_split))
  meanz <- tapply(sv18[,i], sv18$Ca_split, mean, na.rm=T)
  table[i-9,2:5] <- c(round(tt0$p.value,5), 
                      round(wx0$p.value,5), 
                      round(meanz[1],1), 
                      round(meanz[2],1))
}
print(table)
rm(list = c("varz","table","tt0","wx0","meanz"))

# 5.2 New Factors based on spatial information ####

## dividing by sampling zone ####
# this is just one way of doing it!

# first plot the points using spatial coordinates
par(mar=c(3,3,1,1), mgp = c(1.7,0.3,0), tcl = 0.3, 
    font.lab = 2)
with(afs20, plot(Northing ~ Easting, asp = 1))

# then make a temporary factor to separate west-east...
afs20$Loc0 <- cut(afs20$Easting, breaks = c(0,400171,999999),
    labels = c("W","N"))

# and another temporary factor to separate north-south.
afs20$Loc1 <- cut(afs20$Northing, 
                  breaks = c(0,6468123,9999999),
                  labels = c("S","N_NW"))
afs20[,c("Loc0","Loc1")] # inspect cuts

# combine the 2 temporary factors...
# ...this gives unique strings for each location
afs20$Loc <- paste0(afs20$Loc0,afs20$Loc1)
# then use gsub() to make better unique strings
afs20$Loc <- gsub("NN_NW","N",afs20$Loc)
afs20$Loc <- gsub("WN_NW","NW",afs20$Loc)
afs20$Loc <- gsub("WS","S",afs20$Loc)
# fix up the incorrect missing value codes
afs20$Loc[42] <- NA
afs20$Loc[57] <- NA
afs20$Loc[58] <- NA
# finally make the unique strings into a factor
afs20$Loc <- as.factor(afs20$Loc)
afs20$Loc # check it

# remove the temporary factors (columns)
# we don't use rm() as these are not objects, instead 
# they are columns within a data frame
afs20$Loc0 <- NULL
afs20$Loc1 <- NULL

# plot again, distinguishing points by the 
# factro we just created
with(afs20, plot(Northing ~ Easting, asp = 1,
                 pch = c(15,16,17)[Loc],
                 col = c(2,4,6)[Loc]))

# use the new factor in a statistical test & plot
with(afs20, boxplot(log10(Zn) ~ Loc))
with(afs20, kruskal.test(log10(Zn) ~ Loc))

# 5.3 New factor from a new variable ####

# use the code from the EDA WORKFLOW 4 session
# to calculate distance from the Chapman Drain for 
# all 2020 sample locations
# (assume linear drain ax + by + c = 0)
a <- 0.9862637; b <- -1; c <- 6073413
afs20$dist.Chapman  <-  with(afs20,
        (abs(a*Easting + b*Northing + c))/(sqrt(a^2 + b^2)))

# then cut this new variable into a 2-level factor
afs20$prox.Chapman <- cut(afs20$dist.Chapman, 
                          breaks = c(0,50,999), 
                          labels = c("lt.50m","gt.50m"))
# NW wetland is not near the drain, so:
#   - subset the data using subset()
#   - remove unused factor levels using droplevels()
#     (!= means 'not equal to')
afs20_Chapman <- droplevels(subset(afs20, 
                                   subset = afs20$Loc!="NW"))

# test all this using a loop running mean comparison tests
# make names vector and data frame before running loop
varz <- names(afs20_Chapman)
table <- data.frame(Variable = varz[9:38],
                    t_test_p = rep(NA,30), 
                    Wilcoxon_p = rep(NA,30),
                    Mean_near = rep(NA,30), 
                    Mean_far = rep(NA,30))
# set up the loop with desired columns
for(i in 9:38){
  tt0 <- with(afs20_Chapman, 
              t.test(afs20_Chapman[,i] ~ prox.Chapman))
  wx0 <- with(afs20_Chapman, 
              wilcox.test(afs20_Chapman[,i] ~ prox.Chapman))
  meanz <- tapply(afs20_Chapman[,i], 
                  afs20_Chapman$prox.Chapman, mean, na.rm=T)
  table[i-8,2:5] <- c(round(tt0$p.value,4), 
                      round(wx0$p.value,4), 
                      round(meanz[1],1), 
                      round(meanz[2],1))
}
print(table)
rm(list = c("varz","table","tt0","wx0","meanz"))

# alternatively analyze data graphically with new factor
# make names vector and set multi-frame plot before loop
varz <- names(afs20_Chapman)
par(mfrow = c(3,3), mar = c(3,3,1,1), mgp = c(1.5,0.3,0),
    tcl = 0.25, font.lab = 2)
# set up the loop with your desired columns
for(i in c(9,11,12,14,16,19,20,31,32)){
  boxplot(afs20[,i] ~ afs20$prox.Chapman,
          xlab = "Dist. range from drain (m)",
          ylab="", names = c("< 50","> 50"))
  mtext(paste(varz[i],"(mg/kg)"), 
        side = 2, line = 1.5, font = 2)
}
print(table)
rm("varz")
