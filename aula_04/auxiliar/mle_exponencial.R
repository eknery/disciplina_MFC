nloglik<- function(x,q) sum(-dexp(x=x,rate=q,log=T))

xs = c(1,1)


dexp(x=xs,rate=q,log=T)

optimize(f= nloglik, x= xs,interval = c(0,10))
