# test all this using a loop running mean comparison tests
# make names vector and data frame before running loop
varz <- names(sv17_soil)
c0 <- 9  # column to start with
cn <- 36 # column to end with
table <- data.frame(Variable = varz[c0:cn],
                    t_test_p = rep(NA,length(c0:cn)), 
                    Wilcoxon_p = rep(NA,length(c0:cn)),
                    Mean_near = rep(NA,length(c0:cn)), 
                    Mean_far = rep(NA,length(c0:cn)))
# set up the loop with desired columns
for(i in c0:cn){
  tt0 <- with(sv17_soil, 
              t.test(sv17_soil[,i] ~ Reserve))
  wx0 <- with(sv17_soil, 
              wilcox.test(sv17_soil[,i] ~ Reserve))
  meanz <- tapply(sv17_soil[,i], 
                  sv17_soil$Reserve, mean, na.rm=T)
  table[i-(c0-1),2:5] <- c(round(tt0$p.value,4), 
                      round(wx0$p.value,4), 
                      round(meanz[1],1), 
                      round(meanz[2],1))
}
print(table)
rm(list = c("varz","table","tt0","wx0","meanz"))
