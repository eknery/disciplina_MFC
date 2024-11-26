
tamanho = c("A" = 4,
            "B" = 5,
            "C" = 6,
            "D" = 3,
            "E" = 7
            )

tempo = c("A" = 9,
          "B" = 9.5,
          "C" = 10,
          "D" = 9, 
          "E" = 9.75
          )

pic.tamanho<-pic(tamanho,tree)
pic.tempo<-pic(tempo,tree)

plot(x = pic.tamanho,
     y = pic.tempo,
     xlab = "PIC Tamanho do corpo",
     ylab = "PIC Tempo de geração",
     pch=21,bg="gray",cex=2,
     cex.lab=1.5
)
