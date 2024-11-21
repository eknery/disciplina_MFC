For example, one might wonder why some species of mammals have such large home range
areas, while others have home ranges that are quite small (Garland et al. 1992). One reasonable
hypothesis is that range size is driven by body size, and if so, then we might expect range size
and body size to evolve together. When body size increases, so should range size. If body size
evolves to be smaller, range size should concordantly shrink

####################### CARREGANDO BIBLIOTECAS E DADOS #########################

### bibliotecas
if (!require("phytools")) install.packages("phytools"); library("phytools")
if (!require("geiger")) install.packages("geiger"); library("geiger")

### carregando dados fenotípicos
mammalHR<-read.csv("dados/mammalHR.csv",row.names=1)

### carregando árvore filogenética
mammal.tree<-read.tree("dados/mammalHR.phy")

############################## VISUALIZANDO DADOS ##############################

### visualizando a distribuição dos dados 
plot(homeRange~bodyMass,data=mammalHR,
     xlab="body mass (kg)",
     ylab=expression(paste("home range (km"^"2",")")),
     pch=21,bg="gray",cex=1.2,log="xy",las=1,cex.axis=0.7,
     cex.lab=0.9,bty="n")

# PARA PENSAR:
#   Baseado no gráfico, existe uma relação entre o tamanho corpóreo e 
#   o tamanho da área de ocorrência dos mamíferos?

####################### AJUSTANDO MODELO LINEAR ORDINÁRIO ######################

### ajustando regressão linear ordinária
fit.ols<-lm(log(homeRange)~log(bodyMass),data=mammalHR)
summary(fit.ols)

### visualizando modelo ordinário
plot(log(homeRange) ~ log(bodyMass),data=mammalHR,
     xlab="log(body mass)",
     ylab="log(home range)",
     pch=21,bg="gray",cex=1.2,log="xy",las=1,
     cex.axis=0.7,cex.lab=0.9,bty="n")
abline(fit.ols, lwd=2,col="darkgray")

# PARA PENSAR:
#   A relação entre o tamanho corpóreo e o tamanho da área de ocorrência
#   tem sustentação no modelo de regressão convencional? O modelo de regressão 
#   (a reta) parece descrever bem essa relação?

############################# CALCULANDO OS CONTRASTES ########################

### visualizando a árvores filogenética
plotTree(mammal.tree,ftype="i",fsize=0.7,lwd=1)
nodelabels(bg="white",cex=0.5,frame="circle")

### gerando vetores distintos para fenótipo 
homeRange<-setNames(mammalHR[,"homeRange"], rownames(mammalHR))
bodyMass<-setNames(mammalHR[,"bodyMass"], rownames(mammalHR))

### calculando os contrastes para cada fenótipo
pic.homerange<-pic(log(homeRange),mammal.tree)
pic.bodymass<-pic(log(bodyMass),mammal.tree)

### verificar valores dos contrastes
pic.homerange
pic.bodymass

################### AJUSTANDO MODELO LINEAR COM CONTRASTES ######################

### ajustando regressão linear - passando pela origem
fit.pic<-lm(pic.homerange~pic.bodymass+0)
summary(fit.pic)

plot(pic.homerange~pic.bodymass,
     xlab="PICs for log(body mass)",
     ylab="PICs for log(range size)",
     pch=21,bg="gray",cex=1.2,las=1,
     cex.axis=0.7,cex.lab=0.9,bty="n")
## reset graphing limits of the plot to the
## x/y range of our PICs
clip(min(pic.bodymass),max(pic.bodymass),
     min(pic.homerange),max(pic.homerange))
## graph our fitted line
abline(fit.pic,lwd=2,col="darkgray")

