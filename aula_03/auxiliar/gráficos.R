tamanho = c(4,5,6,3,7)
tempo = c(9,9.5,10,9, 9.75)

fit.lm = lm(tempo ~ tamanho +0 )
summary(fit.lm)

plot(x = tamanho,
     y = tempo,
     xlab = "Tamanho do corpo",
     ylab = "Tempo de geração",
     pch=21,bg="gray",cex=2,
     cex.lab=1.5
     )
abline(fit.lm, lwd = 3, col = "gray",lty = 3)


text.string<-"(((A:0.5,B:0.5):0.5,C:1):4,(D:4.5,E:4.5):0.5);"
tree = read.tree(text = text.string)
plot(tree)

pic.tamanho = c(-1,  -1.13, -1.3,  0.05)
pic.tempo =   c(-0.5,-0.56, -0.25, 0.07)

fit.pic = lm(pic.tempo ~ pic.tamanho +0 )
summary(fit.pic)

plot(x = pic.tamanho,
     y = pic.tempo,
     xlab = "PIC Tamanho do corpo",
     ylab = "PIC Tempo de geração",
     pch=21,bg="gray",cex=2,
     cex.lab=1.5
)
abline(fit.pic, lwd = 3, col = "gray",lty = 1)
