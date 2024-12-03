library(phytools)


## simulate a pure-birth (Yule) tree using pbtree
tree<-pbtree(n=12,scale=100)
## split our plotting area in two
par(mfrow=c(2,1))

## graph our phylogeny
plotTree(tree,ftype="off",mar=c(4.1,4.1,2.1,1.1))
## compute the lineages through time using ltt
obj<-ltt(tree,plot=FALSE)
## draw vertical lines at each lineage accumulation event
abline(v=obj$times,lty="dotted",
       col=make.transparent("blue",0.5))
## add a horizontal axis and plot label
axis(1,cex.axis=0.8)
mtext("(a)",line=1,at=-10)
## create a second plot graphing our LTT
plot(obj,mar=c(5.1,4.1,2.1,1.1),bty="n",
     log.lineages=FALSE,las=1,cex.axis=0.8)
## add the same vertical lines as in panel a)
abline(v=obj$times,lty="dotted",
       col=make.transparent("blue",0.5))
## label our plot
mtext("(b)",line=1,at=-10)
