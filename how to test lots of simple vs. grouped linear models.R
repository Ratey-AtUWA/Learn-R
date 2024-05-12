v0 <- c("As","Cr","Cu","Gd","Mn","Ni","P","Pb","Zn")
tableOut <- data.frame(Var=v0, 
                       Rsq_simp=rep(NA, length(v0)), 
                       P_simp=rep(NA, length(v0)), 
                       Rsq_group=rep(NA, length(v0)), 
                       P_group=rep(NA, length(v0)),
                       AOVtest=rep(NA, length(v0)))
for(i in 1:length(v0)){
  lm00 <- lm(log10(afs2021[,v0[i]]) ~ afs2021$Fe)
  slm00 <- summary(lm00) ; flm00 <- slm00$fstat
  lm01 <- lm(log10(afs2021[,v0[i]]) ~ afs2021$Fe * afs2021$Zone)
  slm01 <- summary(lm01) ; flm01 <- slm01$fstat
  aov0 <- anova(lm00,lm01)
  tableOut[i,2:6] <- c(round(slm00$r.squared,3), 
                       round((1-pf(flm00[1],flm00[2],flm00[3])),4),
                       round(slm01$r.squared,3), 
                       round((1-pf(flm01[1],flm01[2],flm01[3])),4),
                       round(aov0$'Pr(>F)'[2],4))
}
print(tableOut) # or flextable(tableOut) 
afs2021$Cu[56] <- NA

