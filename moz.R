#####################################################################

#  Example to be completed in class, if there is time
#  Conduct a structured analysis using the Mosquito1 data set

#  1. Create a boxplot with the factors are ordered from smallest mean to largest

#  2. Using your understanding of the image Export options,
#  copy  the plot in an appropriate format with clearly legible scale and resolution.

#  3. Conduct a group variance test
#  save a copy of the plot in an appropriate scale and resolution.


#  4. Using the one.way function conduct an appropriate ANOVA test

#  5. Conduct appropriate pair-wise testing.

#  6.  Collate your results into a word document where you have:
#  (i)   specified an appropriate null and alternate hypothesis for each test
#         (group variance test, ANOVA test and pairwise tests)
#  (ii)  added your figure, including adding a caption label.  
#        Note: the figure should first be saved as a separate png file. 
#  (iii) Included an appropriate summary table of your pairwise results
#  (iv)  make a statement about the which insecticide(s) you would recommend 
#        Note: Lower values are preferred
#   (v)  Comment on any notable aspect of the results 

moz <- read.csv("Mosquito1.csv", stringsAsFactors = T)
head(moz)
summary(moz)

moz$Insecticide <- factor(moz$Insecticide, 
                          levels = c("Fenitrothion","Malathion","Fenthion",
                                     "Chlorpyrifos","Temephos"))

with(moz,boxplot(Ratio ~ Insecticide, 
                 col = c("pink","moccasin","khaki1","darkseagreen1","powderblue"),
                 ylab = "Mosquito mrtality ratio (treated/control)"))
meanz <- tapply(moz$Ratio, moz$Insecticide, mean)
points(seq(1:nlevels(moz$Insecticide)), meanz, pch = 3, cex = 1.5, type = "b")
legend("topleft", legend = c("Arithmetic mean"), pch = 3, pt.cex = 1.5,
       title = expression(italic("Non-standard boxplot symbols")),
       bty = "n", inset = 0.025)
rm(meanz)

with(moz, hist(log(Ratio)))
with(moz, shapiro.test(Ratio))
with(moz, shapiro.test(log10(Ratio)))
tapply(moz$Ratio, moz$Insecticide, shapiro.test)

with(moz,bartlett.test(log(Ratio) ~ Insecticide)) # for normally distrib variable
with(moz,car::leveneTest(Ratio ~ Insecticide)) # less sensitive to non-normality

with(moz,oneway.test(log10(Ratio) ~ Insecticide, var.equal = T)) # if accept bartlett

with(moz,pairwise.t.test(log10(Ratio), Insecticide, pool.sd = T))

with(moz,pairwise.t.test(Ratio, Insecticide, pool.sd = F, 
                         p.adjust.method = "none"))

# OR NON-PARAMETRIC for steps 3, 4, 5 ####
library(PMCMRplus)

with(moz,fligner.test(Ratio ~ Insecticide))

with(moz,kruskal.test(Ratio ~ Insecticide))

with(moz, PMCMRplus::kwAllPairsConoverTest(Ratio ~ Insecticide, p.adjust.method = "holm"))
