# Nesta prática vamos investigar se existe uma correlaçao evolutiva entre o 
# tamanho das folhas e das inflorescências dentro de um clado de Miconia.
# A expectativa é que a evolução de folhas maiores aumente a assimilação de 
# de carbono e consequentemente permita sustentar inflorescências maiores.
# Para isso, vamos usar o método dos quadrados mínimos filogenéticos (PGLS)

####################### CARREGANDO BIBLIOTECAS E DADOS #########################

### bibliotecas
if (!require("phytools")) install.packages("phytools"); library("phytools")
if (!require("geiger")) install.packages("geiger"); library("geiger")
if (!require("nlme")) install.packages("nlme"); library("nlme")

### carregando dados fenotípicos
miconia.data<-read.csv("dados/miconia.csv",row.names=1)

### carregando árvore filogenética
miconia.tree<-read.tree("dados/miconia.nwk")

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

########################## AJUSTANDO MODELOS EVOLUTIVOS ########################

### organizando a variável resposta em um vetor nomeado
ln_inflor =log(miconia.data$inflor.size)
names(ln_inflor) = rownames(miconia.data) 

### ajustando modelos evolutivos
fitBM = fitContinuous(phy = miconia.tree, 
                      dat = ln_inflor,  
                      model = "BM")

fitOU = fitContinuous(phy = miconia.tree, 
                      dat = ln_inflor,  
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
spp = rownames(miconia.data)

### nome do melhor modelo
best_model = names(which(aicw == max(aicw)))

### CALCULANDO VCV: 

## Se BM for o melhor modelo:
  sigma_sq = fitBM$opt$sigsq
  cor_str = corBrownian(value = sigma_sq, phy=miconia.tree, form=~spp)

## Se OU for o melhor modelo:
  alpha = fitOU$opt$alpha
  cor_str = corMartins(value = alpha, phy=miconia.tree, form=~spp, fixed = T)

# IPORTANTE:
#   Note que a estrutura de correlação é calculada com base em diferentes parâmetros
#   dependendo do modelo que foi escolhido para representar a evolução da característica. 

################################### AJUSTANDO PGLS #############################

### ajustando PGLS aos dados
fit.pgls<-gls(log(inflor.size)~log(leaf.size),
              data = miconia.data,
              correlation = cor_str)

### verificando normalidade dos resíduos
shapiro.test(resid(fit.pgls))

### verificando sustentação do modelo
summary(fit.pgls)

### visualizando modelo PGLS
plot(log(inflor.size)~log(leaf.size),data=miconia.data,
     xlab="log(tamanho da folha)",
     ylab="log(tamanho da inflorescência)",
     pch=21,bg="gray",cex=1.2,las=1,
     cex.axis=0.7,cex.lab=0.9,bty="n")
abline(fit.pgls, lwd=2,col="darkgray")

# PARA PENSAR:
#   Comparando os modelos, existe diferença na sustentação estatística?
#   E quanto ao ajuste dos modelos? As relações inferidas são as mesmas?