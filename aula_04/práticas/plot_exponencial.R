### definindo função exponencial
fx_exponencial = function(x, lambda) {lambda*exp(1)^(-lambda*x)}

### definindo parâmetos
x_range <- seq(0, 10, by=0.1)

q_vl = 0.32
### Criando o gráfico
plot(x = x_range, 
     y= dexp(x = x_range, rate = q_vl, log = FALSE) , 
     type="l", col="darkgray",lwd= 6,
     main = paste0("vermelho para laranja, q = ", q_vl),
     xlab="tempo até a transição", ylab="p(x)"
)

q_vr = 0.35
### Criando o gráfico
plot(x = x_range, 
     y= dexp(x = x_range, rate = q_vr, log = FALSE) , 
     type="l", col="darkgray",lwd= 6,
     main = paste0("vermelho para roxo, q = ", q_vr),
     xlab="tempo até a transição", ylab="p(x)"
)


nloglik<- function(x,q) sum(-dexp(x=x,rate=q,log=T))

xs = c(2.25)
optimize(f= nloglik,x= xs,interval = c(0,10))
