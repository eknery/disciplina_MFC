
### definindo parâmetos
x_range <- seq(0, 10, by=0.1)

q = 0.4
### Criando o gráfico
plot(x = x_range, 
     y= dexp(x = x_range, rate = q, log = FALSE) , 
     type="l", col="darkred",lwd= 6,
     main = paste0("vermelho para laranja, q = ", q_vl),
     xlab="tempo até a transição", ylab="p(x)"
)


nloglik<- function(x,q) sum(-dexp(x=x,rate=q,log=T))

xs = c(1,1)


dexp(x=xs,rate=q,log=T)

optimize(f= nloglik, x= xs,interval = c(0,1))



