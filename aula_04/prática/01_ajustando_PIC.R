# Nessa prática vamos investigar se existe uma correlaçao evolutiva entre o 
# tamanho das folhas e das inflorescências dentro de um clado de Miconia.
# A expectativa é que a evolução de folhas maiores aumente a assimilação de 
# de carbono e consequentemente permita sustentar inflorescências maiores.
# Para isso, vamos usar os contrastes filogenéticos independentes(PICs).

####################### CARREGANDO BIBLIOTECAS E DADOS #########################

### bibliotecas
if (!require("phytools")) install.packages("phytools"); library("phytools")
if (!require("geiger")) install.packages("geiger"); library("geiger")

### carregando dados fenotípicos
miconia.data<-read.csv("dados/miconia.csv", row.names=1, h= T)
head(miconia.data)

### carregando filogenia
miconia.tree<-read.tree("dados/miconia.nwk")
print(miconia.tree,printlen=2)

############################## VISUALIZANDO DADOS ##############################

### visualizando a distribuição dos dados 
plot(inflor.size~leaf.size, data=miconia.data,
     xlab="tamanho da folha (cm)",
     ylab="tamanho da inflorescência (cm)",
     pch=21,bg="gray",cex=1.2,log="xy",las=1,cex.axis=0.7,
     cex.lab=0.9,bty="n")

# PARA PENSAR:
#   Baseado no gráfico, existe uma relação entre o tamanho das inflorescências e 
#   das folhas nesse clado de plantas?

####################### AJUSTANDO MODELO LINEAR ORDINÁRIO ######################

### ajustando regressão linear ordinária
fit.ols<-lm(log(inflor.size)~log(leaf.size), data=miconia.data)

### verificando Normalidade dos resíduos
shapiro.test(resid(fit.ols))

### verificando sustentação
summary(fit.ols)

### visualizando modelo ordinário
plot(log(inflor.size)~log(leaf.size),data=miconia.data,
     xlab="log(tamanho da folha)",
     ylab="log(tamanho da inflorescência)",
     pch=21,bg="gray",cex=1.2,las=1,
     cex.axis=0.7,cex.lab=0.9,bty="n")
abline(fit.ols, lwd=2,col="darkgray")

# PARA PENSAR:
#   A relação entre o tamanho da inflorescência e o tamanho da folha
#   tem sustentação estatística? O modelo de regressão (a reta) parece 
#   descrever a relação entre os dois fenótipos?

############################# CALCULANDO OS CONTRASTES ########################

### visualizando a árvores filogenética
plotTree(miconia.tree,ftype="i",fsize=0.7,lwd=1)
nodelabels(bg="white",cex=0.5,frame="circle")

### gerando vetores distintos para fenótipo 
leaf.size<-setNames(miconia.data[,"leaf.size"], rownames(miconia.data))
inflor.size<-setNames(miconia.data[,"inflor.size"], rownames(miconia.data))

### calculando os contrastes para cada fenótipo
pic.leaf<-pic(log(leaf.size),miconia.tree)
pic.inflor<-pic(log(inflor.size),miconia.tree)

### verificar valores dos contrastes
pic.leaf
pic.inflor

################### AJUSTANDO MODELO LINEAR AOS CONTRASTES ######################

### ajustando regressão linear - passando pela origem
fit.pic<-lm(pic.inflor~pic.leaf+0)

### verificando Normalidade dos resíduos
shapiro.test(resid(fit.pic))

### verificando sustentação 
summary(fit.pic)

plot(pic.inflor~pic.leaf,
     xlab="PICs log(tamanho da folha)",
     ylab="PICs log(tamanho da inflorescência)",
     pch=21,bg="gray",cex=1.2,las=1,
     cex.axis=0.7,cex.lab=0.9,bty="n"
     )
## graph our fitted line
abline(fit.pic,lwd=2,col="darkgray")

# PARA PENSAR:
#   Comparando os modelo com dados brutos e com PICs, existe diferença na 
#   sustentação estatística? E quanto ao ajuste dos modelos? 
#   As relações inferidas são as mesmas (inclinação da reta)?
