perkg <- 2000
sbsmpl <- 10 # mass in g; everything assumes 1kg of original sample
ssmpl2 <- 2 # mass in g; everything assumes 1kg of original sample
reps <- 10 # number of repeats for sims

palette(c("#000000FF", viridis::turbo(100, alpha = 0.65)));showpal(ncol=6,cex=0.75)
# object to store output
# counts_out <- as.data.frame(matrix(rep(NA,2600), nrow = 100))
# colnames(counts_out) <- paste0("n_",seq(0,25))

# derived input
mean_n <- perkg*(sbsmpl/1e3) # expected count in stage 1 subsample
n_sbsmpl <- 1000/sbsmpl # number of stage 1 sub-samples in 1 kg
n_ssmpl2 <- sbsmpl/ssmpl2 # No. of stage 2 sub-samples from stage 1 sub-sample

# simulate
s10g <- data.frame(in_10g=rep(NA,reps), s2g_1=rep(NA,reps),
                   s2g_2=rep(NA,reps), s2g_3=rep(NA,reps),
                   s2g_4=rep(NA,reps), s2g_5=rep(NA,reps))
rems <- data.frame(rem8g = rep(NA,n_ssmpl2),rem6g=rep(NA,n_ssmpl2),
                   rem4g=rep(NA,n_ssmpl2),rem2g=rep(NA,n_ssmpl2))
for (i in 1:10){
  (s10g[i,1] <- rpois(1,perkg*(sbsmpl/1000))) # taking 1 10g sample from [1kg @ 2000particles/kg]
  (s10g[i,2] <- rpois(1,round(s10g[i,1]/(n_ssmpl2))))
  rems[i,1] <- round(s10g[i,1] - sum(s10g[i,2:6], na.rm=T)); s10g[i,3] <- rpois(1,(rems[i,1])/(n_ssmpl2-1))
  rems[i,2] <- round(s10g[i,1] - sum(s10g[i,2:6], na.rm=T)); s10g[i,4] <- rpois(1,(rems[i,2])/(n_ssmpl2-2))
  rems[i,3] <- round(s10g[i,1] - sum(s10g[i,2:6], na.rm=T)); s10g[i,5] <- rpois(1,(rems[i,3])/(n_ssmpl2-3))
  rems[i,4] <- round(s10g[i,1] - sum(s10g[i,2:6], na.rm=T)); s10g[i,6] <- rpois(1,(rems[i,4])/(n_ssmpl2-4))
  cat(paste0("rep_",i),":",s10g[i,1]," --> ",s10g[i,2],s10g[i,3],s10g[i,4],s10g[i,5],s10g[i,6],"\n")
}


s10g <- data.frame(in_10g=rep(NA,reps), s2g_1=rep(NA,reps),
                   s2g_2=rep(NA,reps), s2g_3=rep(NA,reps),
                   s2g_4=rep(NA,reps), s2g_5=rep(NA,reps))
rems <- data.frame(rem8g = rep(NA,n_ssmpl2),rem6g=rep(NA,n_ssmpl2),
                   rem4g=rep(NA,n_ssmpl2),rem2g=rep(NA,n_ssmpl2))
for (i in 1:reps){
  (s10g[i,1] <- rpois(1,perkg*(sbsmpl/1000))) # taking 1 10g sample from [1kg @ 2000particles/kg]
  (s10g[i,2] <- rpois(1,round(s10g[i,1]/(n_ssmpl2))))
  for (j in 2:n_ssmpl2){
    rems[i,j-1] <- round(s10g[i,1] - sum(s10g[i,2:6], na.rm=T))
    s10g[i,j+1] <- rpois(1,(rems[i,j-1])/(n_ssmpl2-(j-1)))
    }
  # cat(paste0("rep_",i),":",s10g[i,1]," --> ",
  #     s10g[i,2],s10g[i,3],s10g[i,4],s10g[i,5],s10g[i,6],"\n")
  }
(s10g <- data.frame(s10g, sumSub=rowSums(s10g[,2:6], na.rm=T)))


# _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.
# _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.

# figuring things out
p_cum <- 0
for (i in 0:25){
  p_int <- ppois(i,5) - ppois(i-1,5)
  p_cum <- p_cum + p_int
  cat("count ≤ ",i,", p = ",signif(p_int,3)," | cumul.p = ",signif(p_cum,3),"\n",sep="")
}

pmean <- 12
pois0 <- data.frame(count=0:round(pmean*2.5))
pois0$p_incr <- with(pois0, ppois(count,pmean) - ppois(count-1,pmean))
pois0$P_cumul <- cumsum(pois0$p_incr)
plot(pois0[,c(1,3)], type="s")
lines(pois0[,1:2], type="s", col = 4)
