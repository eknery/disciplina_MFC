####################### CARREGANDO BIBLIOTECAS E DADOS #########################

### bibliotecas
if (!require("phytools")) install.packages("phytools"); library("phytools")
if (!require("geiger")) install.packages("geiger"); library("geiger")
if (!require("nlme")) install.packages("nlme"); library("nlme")

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
fit.ols<-lm(log(homeRange)~log(bodyMass), data=mammalHR)

### verificando Normalidade dos resíduos
shapiro.test(resid(fit.ols))

### verificando sustentação
summary(fit.ols)

### visualizando modelo ordinário
plot(log(homeRange) ~ log(bodyMass),data=mammalHR,
     xlab="log(body mass)",
     ylab="log(home range)",
     pch=21,bg="gray",cex=1.2,las=1,
     cex.axis=0.7,cex.lab=0.9,bty="n")
abline(fit.ols, lwd=2,col="darkgray")

########################## AJUSTANDO MODELOS EVOLUTIVOS ########################

### organizando a variável resposta em um vetor nomeado
ln_range =log(mammalHR$homeRange)
names(ln_range) = rownames(mammalHR) 

### ajustando modelos evolutivos
fitBM = fitContinuous(phy = mammal.tree, 
                      dat = ln_range,  
                      model = "BM")

fitOU = fitContinuous(phy = mammal.tree, 
                      dat = ln_range,  
                      model = "OU")

### comparando modelos evolutivos
aic = setNames(c(AIC(fitBM) , AIC(fitOU)), c("BM", "OU"))
aicw = aic.w(aic)
aicw

# PARA PENSAR:
#   Qual modelo melhor representa a evolução do tamanho da área de ocorrência?
#   Esse modelo difere do modelo que é assumido no métodos dos contrastes?

########################## CALCULANDO ESTRUTURA DE CORRELAÇÃO ##################

### vetor com o nome das espécies
spp = rownames(mammalHR)

### nome do melhor modelo
best_model = names(which(aicw == max(aicw)))

### calculando correlação de acordo com melhor modelo
if(best_model == "BM" ){
  sigma_sq = fitBM$opt$sigsq
  cor_str = corBrownian(value = sigma_sq, phy=mammal.tree, form=~spp)
}
if(best_model == "OU" ){
  alpha = fitOU$opt$alpha
  cor_str = corMartins(value = alpha, phy=mammal.tree, form=~spp, fixed = T)
}

# IPORTANTE:
#   Note que a estrutura de correlação é calculada com base em diferentes parâmetros,
#   dependendo do modelo que foi escolhido para representar a evolução da característica. 

################################### AJUSTANDO PGLS #############################

### ajustando PGLS aos dados
fit.pgls<-gls(log(homeRange)~log(bodyMass),
              data = mammalHR,
              correlation = cor_str)

### verificando normalidade dos resíduos
shapiro.test(resid(fit.pgls))

### verificando sustentação do modelo
summary(fit.pgls)

### visualizando modelo PGLS
plot(log(homeRange) ~ log(bodyMass),data=mammalHR,
     xlab="log(body mass)",
     ylab="log(home range)",
     pch=21,bg="gray",cex=1.2,las=1,
     cex.axis=0.7,cex.lab=0.9,bty="n")
abline(fit.pgls, lwd=2,col="darkgray")

# PARA PENSAR:
#   Comparando os modelos de regressão, existe diferença na sustentação estatística?
#   E quanto ao ajuste dos modelos? As relações inferidas são as mesmas, ou existem
#   direnças quanto a direção e a intensidade (inclinação da reta)?