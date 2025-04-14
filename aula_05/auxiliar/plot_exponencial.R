
### definindo parâmetos
x_range <- seq(0, 10, by=0.1)

q = 0.19
### Criando o gráfico
plot(x = x_range, 
     y= dexp(x = x_range, rate = q, log = FALSE) , 
     type="l", col="darkred",lwd= 6,
     ylim= c(0, 0.20),
     main = paste0("saída de (V), q = ", q),
     xlab="tempo até a transição", ylab="p(x)",
     cex.axis=1,cex.lab=1.2
)





