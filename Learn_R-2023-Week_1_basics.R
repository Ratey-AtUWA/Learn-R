# Week 1 ENVT3361 Workshop ####
#    ___           _                  
#   |_ _|  _ __   | |_   _ __    ___  
#    | |  | '_ \  | __| | '__|  / _ \ 
#    | |  | | | | | |_  | |    | (_) |
#   |___| |_| |_|  \__| |_|     \___/ 
#                                     
#        _                 ____       
#       | |_    ___       |  _ \      
#       | __|  / _ \      | |_) |     
#       | |_  | (_) |     |  _ <      
#        \__|  \___/      |_| \_\     
#                                     
# This guide gets you started with reading data into R (in the RStudio
# environment) from a file, and checking that the data have been read in
# correctly.

# If you need or would like a more basic introduction to R, you could first read
# our 'Guide to R and RStudio for absolute beginners'.

# _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.

# Reading the data ####
hubbard <- read.csv(file = "hubbard.csv", stringsAsFactors = TRUE)
# ... and do a quick check
is.data.frame(hubbard) # check that it worked

# These data are from the Hubbard Brook Experimental Forest near Woodstock in
# New Hampshire, USA

# First proper check - summarise some of the data

summary(hubbard[,1:10]) # just the first 10 columns

# The summary() function creates a little table for each column - note that
# these little tables do not all look the same. 

# Integer or numeric columns get a numeric summary with minimum, mean, etc., and
# sometime the number of missing (NA) values.

# Categorical (Factor) columns show the number of samples (rows) in each
# category (unless there are too many categories).

# These summaries are useful to check if there are zero or negative values in
# columns, how many missing observations there might be, and if the data have
# been read correctly into R.
# [Note: we could have specified something like hubbard[1:10,] which
# would have worked on the first 10 rows (also called ‘observations’ or
# ’samples), or hubbard[1:20,6:10] which would have used only the first 20 rows
# of columns 6 to 10.]

# _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.

#  Final checks of the data frame ####

# Usually we would not restrict the output as done below with [,1:20]. We only
# do it here so we’re not bored with pages of similar-looking output. You should
# look at structure for the whole data frame using str(hubbard) (or substitute
# 'hubbard' for whatever your data object is called).

str(hubbard[,1:20]) # 'str' gives the structure of an object

# We can see that some columns are integer (int) values (e.g. PLOT,
# UTM_EASTING), some columns contain Factor values i.e. in fixed categories
# (e.g. Rel.To.Brook, Transect), and some columns are numeric (num) (e.g. PH,
# OM.pct, Ni). Applying the str() function to a data object is always a good
# idea, to check that the data have read correctly into R. [NOTE that other
# variable types are possible such as character chr, date (Date or POSIXct),
# logical, etc.]

# _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.

# Base R plotting: x-y plot using plot() ####

# We can use either plot(x, y, ...) OR plot(y ~ x, ...)
# In R the ~ symbol means ‘as a function of’, so ~ indicates a formula.

# In R we need to tell the program which ‘object’ our variables are in. We’ve
# just made a Data Frame (a type of data object) called hubbard.

# The following 3 styles of code do exactly the same thing:
  
# 1. Specifying the data frame using with() syntax – (we recommend this one!)
with(hubbard,
     plot(EXCH_Al ~ PH)
)

# ...which can just be written on a single line:
with(hubbard, plot(EXCH_Al ~ PH))

# 2. Specifying the data frame using the dollar-sign operator
plot(hubbard$EXCH_Al ~ hubbard$PH) # look at axis titles!
# Notice the axis titles!

# 3. Specifying the data frame using attach() and detach() (not recommended)
attach(hubbard)
plot(EXCH_Al ~ PH)
detach(hubbard)

# Without changing any of the (numerous) options or parameters in the plot()
# function, the plot is not very attractive (e.g. axis titles!).

# We can also change the overall plot appearance by using the function par()
# before plotting; par() sets graphics parameters. Let’s try some variations:
  
# _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.

# Setting some overall graphics parameters using par() ####

# mar= sets margins in ‘lines’ units: c(bottom,left,top,right)
# mgp= sets distance of text from axes: c(titles, tickLabels, ticks)
# font.lab= sets font style for axis titles: 2=bold, 3=italic, etc.
# and within plot(), xlab= and ylab= set axis titles

par(mar=c(4,4,1,1), mgp=c(2,0.6,0), font.lab=2)

# We'll also include some better axis title text using xlab, ylab
with(hubbard,
     plot(EXCH_Al ~ PH, 
          xlab="Soil pH", 
          ylab="Exchangeable Al (proportion of CEC)")
)

# This is starting to look a lot better!
  
# We can still add more information to the graph; for example, by making use of
# the factors (categories) in the dataset. We also need to learn these graphics
# parameters:

# col = plotting colour(s) - it’s easiest to use words like  "red", "darkblue",
# and so on see https://www.statmethods.net/advgraphs/images/colorchart.png or
# just run the R function colors() for a list of all 657 names!
#
# pch = plot character(s) - numbers from 0 to 24 ( run help(points) )

par(mar=c(4,4,1,1), mgp=c(2,0.6,0), font.lab=2)
with(hubbard,
     plot(EXCH_Al ~ PH, xlab="Soil pH",
          ylab="Exchangeable Al (proportion of CEC)",
          pch=c(1,16)[Rel.To.Brook], 
          col=c("blue","darkred")[Rel.To.Brook])
)

# The parameter pch=c(1,16)[Rel.To.Brook] separates the points by the
# information in the Factor column Rel.To.Brook, shown inside [ ]. This column
# is a 2-level factor, so can be one of two categories (North or South), and so
# we need a vector with two numbers in it (pch=c(1,16)). The code for specifying
# colour is very similar, except our vector has 2 colour names in it.
# 
# There is still one thing missing; a graph legend. We can add one using the
# legend() function. We will use the following options:
#   
# "topleft" position of legend – run help(legend) for options, or we can use 
#     x-y coordinates
# legend = a vector of names identifying the plot symbols - we have used the
#     categories in the factor ‘Rel.To.Brook’, levels(hubbard$Rel.To.Brook), 
#     but we could have used legend=c("North","South") instead

# pch = plot symbols - should be exactly the same vector as in the plot function
# col = plot colours - should be exactly the same vector as in the plot function
# title = a title for the legend - optional

par(mar=c(4,4,1,1), mgp=c(2,0.6,0), font.lab=2)
with(hubbard,
     plot(EXCH_Al ~ PH, xlab="Soil pH", 
          ylab="Exchangeable Al (proportion of CEC)",
          pch=c(1,16)[Rel.To.Brook], 
          col=c("blue","darkred")[Rel.To.Brook])
)
legend("topleft", legend=levels(hubbard$Rel.To.Brook), pch=c(1,16), 
       col=c("blue","darkred"), title="Location")

# =~=~=~=~=~=~=~=~=~
# Alternative to base-R plot: scatterplot() (from the car package) ####

# The R package car (Companion to Applied Regression) has many useful additional
# functions that extend the capability of R. The next two examples produce
# nearly the same plot as in the previous examples, using the scatterplot()
# function in the car package.

# load required package(s)
library(car)
# par() used to set overall plot appearance using options within 
# par(), e.g.
#     mar sets plot margins, mgp sets distance of axis title and 
#     tick labels from axis
par(font.lab=2, mar=c(4,4,1,1), mgp=c(2.2,0.7,0.0))
# draw scatterplot with customised options 
# remember pch sets plot character (symbol); 
# we will also use the parameter cex which sets symbol sizes and
# 
scatterplot(EXCH_Al ~ PH, data=hubbard, smooth=FALSE, 
            legend = c(coords="topleft"), 
            cex=1.5, cex.lab=1.5, cex.axis=1.2)

# Note that we get some additional graph features by default:
  
  #  boxplots for each variable in the plot margins – these are useful for
  #  evaluating the distribution of our variables and any extreme values
  # a linear regression line showing the trend of the relationship (it’s
  # possible to add this in base R plots, too)

# We can turn both of these features off if we want - run help(scatterplot) in
# the RStudio Console, and look under Arguments and Details.

# Also, we separately specify the dataset to be used as a function argument,
# i.e., data=hubbard.

# _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.

# Scatterplot (car) with groups, Hubbard Brook soil data ####

# 'require()' loads package(s) if they haven't already been loaded
require(car)
# adjust overall plot appearance using options within par()
# mar sets plot margins, mgp sets distance of axis title and tick 
#   labels from axis
par(font.lab=2, mar=c(4,4,1,1), mgp=c(2.2,0.7,0.0))
# create custom palette with nice colours :)
# this lets us specify colours using numbers - try it!
palette(c("black","red3","blue3","darkgreen","sienna"))
# draw scatterplot with points grouped by a factor (Rel.To.Brook) 
scatterplot(EXCH_Al ~ PH | Rel.To.Brook, data=hubbard, smooth=FALSE,
            legend = c(coords="topleft"), col=c(5,3,1), 
            pch=c(16,0,2), cex=1.2, cex.lab=1.3, cex.axis=1.0)

# The scatterplot() function creates a legend automatically if we plot by factor
# groupings (note the different way that the legend position is specified within
# the scatterplot() function). This is pretty similar to the base R plot above
# (we can also customise the axis titles in scatterplot(), using xlab= and ylab=
# as before).

# _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.

# Other types of data presentation: Plot types and Tables ####

# We’ll give you some starting code chunks, and the output from them. You can
# then use the help in RStudio to try to customise the plots according to the
# suggestions below each plot!
  
# =~=~=~=~=~=~=~=~=~
## Box plots ####
boxplot(MOISTURE.pct ~ Rel.To.Brook, data=hubbard)

# For box plots, try the following:
  
  # include informative axis labels (titles)
  # plotting boxes separated by categories in a Factor
  # make boxes a different colour (all the same, and all different!)
  # add notches to boxes representing approximate 95% confidence intervals
  #     around the median
  # give the (vertical) y-axis a log10 scale
  # ...and so on.

# =~=~=~=~=~=~=~=~=~
## Histograms ####
with(hubbard, hist(MOISTURE.pct))

# For histograms, try the following:
  
  # add suitable axis labels (titles)
  # make bars a different colour (all the same)
  # change the number of cells (bars) on the histogram to give wider or narrower intervals
  # log10-transform the x-axis (horizontal axis)
  # remove the title above the graph (this information would usually go in a caption)
  # ...and so on.

# =~=~=~=~=~=~=~=~=~
## Strip Charts and Plots of Means (two plots together) ####

# We use the mfrow= or mfcol= argument in the par() function to plot multiple
# graphs

require(RcmdrMisc) # needed for plotMeans() function
# use the mfrow or mfcol argument in the par() function to plot 
# multiple graphs
par(mfrow=c(1,2)) # c(1,2) is 1 row, 2 columns
stripchart(hubbard$OM.pct, main="Strip Chart")
with(hubbard,plotMeans(OM.pct, Transect, error.bars = "conf.int"))
par(mfrow=c(1,1)) # to get back to single plots

# For one or both plots, try the following:
 
  # add suitable axis labels (titles), in bold font;
  # plotting means separated by a different factor;
  # for plot of means, rotate the tick labels so that none ore omitted;
  # make strip chart symbols a different shape ± colour (all the same, and all
  #   different!);
  # make the strip chart vertical instead of horizontal;
  # apply some ‘jitter’ (noise) to the strip chart symbols so that overlapping
  #   points are easier to see;
  # log10-transform the numerical axis of the strip chart so that overlapping
  #   points are easier to see;
  # remove the titles above the graphs (this information would usually go in
  #   caption/s)
  # ...and so on.

# _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.

# Tables ####

# There are a few ways to make useful tables in R to summarise your data. Here
# are a couple of examples.
#
# =~=~=~=~=~=~=~=~=~
## Using the tapply() function in base R

# use the cat() [conCATenate] function to make a Table heading 
#     (\n is a line break)
cat("One-way table of means\n")
with(hubbard, tapply(X = EXCH_Ni, INDEX=Transect, 
                     FUN=mean, na.rm=TRUE))
cat("\nTwo-way table of means\n")
with(hubbard, tapply(X = EXCH_Ni, 
                     INDEX=list(Transect,Rel.To.Brook), 
                     FUN=mean, na.rm=TRUE))

# For tapply() tables, try the following:
  
# we have used the mean function (FUN=mean) – try another function to get
# minima, maxima, standard deviations, etc.
# try copying the output to Word or Excel and using this to make a table in that
# software
# ...and so on.

# =~=~=~=~=~=~=~=~=~

## Using the numSummary() function in the ‘RcmdrMisc’ R package ####
require(RcmdrMisc)
# use the cat() [conCATenate] function to make a Table heading 
#   (\n is a line break)
cat("Summary statistics for EXCH_Ni\n")
numSummary(hubbard$EXCH_Ni)
cat("\nSummary statistics for EXCH_Ni grouped by Rel.To.Brook\n")
numSummary(hubbard$EXCH_Ni, groups=hubbard$Rel.To.Brook)

# For numSummary() tables, try the following:
#
# generating summary tables for more than one variable at a time 
# generating summary tables with fewer statistical parameters (e.g. omit IQR) or
#     more statistical parameters (e.g. include skewness) (use R Studio Help!)
# try copying the output to Word or Excel and using this to make a table in that
#     software 
# ...and so on. 

# =~=~=~=~=~=~=~=~=~
# Tables using print() on a data frame ####

# Data frames are themselves tables, and if they already contain the type of
# summary we need, we can just use the print() function to get output. Let’s do
# something [slightly] fancy (see if you can figure out what is going on here¹):
  
output <- 
  numSummary(hubbard[,c("PH","MOISTURE.pct","OM.pct","Al.pct","Ca.pct","Fe.pct")],
             statistics = c("mean","sd","quantiles"), quantiles=c(0,0.5,1))
mytable <- t(cbind(output$table,output$NAs))
row.names(mytable) <- c("Mean","Std.dev.","Min.","Median","Max.","Missing")
# here's where we get the output
print(mytable, digits=3)
write.table(mytable,"clipboard",sep="\t")
cat("\nThe table has now been copied to the clipboard, so you can paste it into Excel!\n")

# If you want to take this further, we can start making really nice Tables for
# reports with various R packages. I use the flextable package and sometimes the
# kable() function from the knitr package.
#
# ¹Hints: we have made two dataframe objects; t() is the transpose function;
# there are also some other useful functions which might be new to you like:
# row.names(), cbind(), print(), write.table() . . .
#
# The following two websites can extend your basic knowledge of using R and
# RStudio:
#
# A great free resource for R beginners is An Introduction to R by Alex Douglas,
# Deon Roos, Francesca Mancini, Ana Couto & David Lusseau.
#
# Getting used to R, RStudio, and R Markdown is an excellent (and free) eBook by
# Chester Ismay which is super-helpful if you want to start using R Markdown for
# reproducible coding and reporting.