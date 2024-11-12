### definindo função exponencial
fx_exponential = function(x, lambda) {lambda*exp(1)^(-lambda*x)}

### definindo parâmetos
x_range <- seq(0, 10, by=0.1)
lambda = 0.22 
### Criando o gráfico
plot(x_range, fx_exponential(lambda = lambda, x = x_range), 
     type="l", col="darkred",lwd=2,
     main=paste0("lambda = ", lambda),
     xlab="tempo para transição", ylab="p(x)")
