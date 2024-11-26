
### definindo parâmetos
x_range <- seq(0, 10, by=0.1)

q = 0.15
### Criando o gráfico
plot(x = x_range, 
     y= dexp(x = x_range, rate = q, log = FALSE) , 
     type="l", col="darkred",lwd= 6,
     ylim= c(0, 0.35),
     main = paste0("(R) para (V), q = ", q),
     xlab="tempo até a transição", ylab="p(x)",
     cex.axis=1,cex.lab=1.2
)





