### definindo função exponencial
fx_exponencial = function(x, lambda) {lambda*exp(1)^(-lambda*x)}

### definindo parâmetos
x_range <- seq(0, 10, by=0.1)

### Criando o gráfico
plot(x_range, fx_exponencial(lambda = 0.84, x = x_range), 
     type="l", col="darkred",lwd=4,
     xlab="tempo para transição", ylab="p(x)")
lines(x = x_range,
      y = fx_exponencial(lambda = 0.53, x = x_range),
      type = "l", col = "orange", lwd=4
)
lines(x = x_range,
      y = fx_exponencial(lambda = 0.16, x = x_range),
      type = "l",col = "purple", lwd=4
)

