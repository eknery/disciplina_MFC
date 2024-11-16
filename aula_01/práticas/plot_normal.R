### definindo parâmetos
x_range <- seq(-5, 5, by=0.1)

### Criando o gráfico
plot(x_range, dnorm(x_range, mean = 0, sd = 2), 
     type="l", col="darkgray",lwd=6,
     xlab="x", ylab="p(x)")

