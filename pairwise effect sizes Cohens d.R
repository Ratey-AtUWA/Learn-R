require(effsize)
sv2017$Fe.log <- log10(sv2017$Fe)
testvar <- "Fe.log"
n <- nlevels(sv2017$Type) # for n groups there are (n*(n-1))/2 pairs
imax <- (n*(n-1))/2
results_table <- data.frame(Variable=NA, Level_1=NA, Level_2=NA, Cohens_d=NA,
                            CI_lower=NA, CI_upper=NA, conf_level=NA, EffectSizeTerm=NA)
for (k in 1:(n-1)){
  for (i in k:(n-1)){
    subset0 <- droplevels(subset(sv2017[,c("Type", "Fe.log")], 
                                 as.numeric(sv2017$Type) == k | 
                                   as.numeric(sv2017$Type) == i+1))
    fx0 <- with(subset0,
                cohen.d(Fe.log ~ Type)
    )
    results_table <- rbind(results_table, 
                           c(testvar,levels(subset0$Type)[1], levels(subset0$Type)[2], 
                             signif(fx0$est,6), signif(fx0$conf.int[1], 6),
                             signif(fx0$conf.int[2], 6), fx0$conf.level, as.character(fx0$magnitude)))
  }
}
results_table <- results_table[-1,]
row.names(results_table) <- NULL
{cat("Pairwise effect sizes for Fe.log\n-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-\n")
  print(results_table)
  write.csv(results_table, file = "clipboard", row.names = FALSE)
  cat("\n»» Results table is in clipboard, paste into other app before additional copy/cut!\n")
}
#
rm(list=c("testvar","n","i","imax","results_table","k"))
