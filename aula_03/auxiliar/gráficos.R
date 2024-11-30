library(ape)

tamanho = c(4,5,6,3,7)
tempo = c(9,9.5,10,9, 9.75)

fit.lm = lm(tempo ~ tamanho)
summary(fit.lm)

plot(x = tamanho,
     y = tempo,
     xlab = "Tamanho do corpo",
     ylab = "Tempo de geração",
     pch=21,bg="gray",cex=2,
     cex.lab=1.5
     )
abline(fit.lm, lwd = 3, col = "gray",lty = 1)


text.string<-"(((A:0.5,B:0.5):0.5,C:1):4,(D:0.25,E:0.25):4.75);"
tree = read.tree(text = text.string)
plot(tree)

pic.tamanho<-pic(tamanho,tree)
pic.tempo<-pic(tempo,tree)

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
