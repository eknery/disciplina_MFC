# Nesta prática vamos investigar se existe uma correlaçao evolutiva entre
# o número de lamelas e o comprimento dos membros anteriores nos lagartos Anolis.
# A expectativa é que a evolução de membros maiores aumentou o número de lamelas
# devido ao efeito de genes que atuam na formação das duas características.
# Para isso, vamos usar diferentes métodos para inferir a correlação evolutiva.

####################### CARREGANDO BIBLIOTECAS E DADOS #########################

### bibliotecas
if (!require("phytools")) install.packages("phytools"); library("phytools")
if (!require("geiger")) install.packages("geiger"); library("geiger")
if (!require("nlme")) install.packages("nlme"); library("nlme")

### carregando dados fenotípicos
data = read.csv("dados/centrarchidae_data.csv",  h= T,  row.names=1)
head(data)

### carregando filogenia
tree = read.nexus("dados/centrarchidae_tree.nexus")
print(tree)

############################## PROCESSANDO DADOS ###############################

### verificando correspondência entre dados e árvore
chk = name.check(tree,data)
chk

### nomes das variáveis preditora e resposta
xname = "gape.width"
yname = "buccal.length"
### extraíndo variáveis
xvar = data[,xname]
yvar = data[,yname]
### nomeando valores por espécie
names(xvar) = names(yvar) = row.names(data)
### verificando vetores
xvar
yvar 

############################## VISUALIZANDO DADOS ##############################

### visualizando a distribuição dos dados 
plot(
  buccal.length~gape.width, 
  data = data,
  xlab="log largura bucal (mm)",
  ylab="log comprimento da boca (mm)",
  pch =21,
  bg ="gray",
  cex = 1.2,
  las =1,
  cex.axis = 0.7,
  cex.lab = 0.9
  )

### visualizando a árvores filogenética
plotTree(
  tree, 
  ftype="i",
  fsize=0.7,
  lwd=1)
nodelabels(bg="white",cex=0.5,frame="circle")

# PARA PENSAR:
#   Baseado no gráfico, existe uma relação entre o tamanho das inflorescências e 
#   das folhas nesse clado de plantas?

##################### INFERÊNCIA POR REGRESSÃO ORDINÁRIA ######################

### ajustando regressão linear ordinária
fit.ols<-lm(LAM~FLL, data= data)

### verificando Normalidade dos resíduos
shapiro.test(resid(fit.ols))

### verificando sustentação
summary(fit.ols)

### visualizando modelo ordinário
plot(LAM~FLL, data=anole.data,
     xlab="comprimeto do membro anterior (cm)",
     ylab="número de lamelas",
     pch=21,bg="gray",cex=1.2,las=1,
     cex.axis=0.7,cex.lab=0.9,bty="n")
abline(fit.ols, lwd=2,col="darkgray")

# PARA PENSAR:
#   A relação entre o tamanho da inflorescência e o tamanho da folha
#   tem sustentação estatística? O modelo de regressão (a reta) parece 
#   descrever a relação entre os dois fenótipos?

############################ INFERÊNCIA POR CONTRASTES ########################



### gerando vetores distintos para fenótipo 
fll<-setNames(anole.data[,"FLL"], rownames(anole.data))
lam<-setNames(anole.data[,"LAM"], rownames(anole.data))

### calculando os contrastes para cada fenótipo
pic.fll<-pic(fll, anole.tree)
pic.lam<-pic(lam, anole.tree)

### verificar valores dos contrastes
pic.fll
pic.lam

### ajustando regressão linear - passando pela origem
fit.pic<-lm(pic.lam~pic.fll+0)

### verificando Normalidade dos resíduos
shapiro.test(resid(fit.pic))

### verificando sustentação 
summary(fit.pic)

plot(pic.lam~pic.fll,
     xlab="PIC comprimeto do membro anterior",
     ylab="PIC número de lamelas",
     pch=21,bg="gray",cex=1.2,las=1,
     cex.axis=0.7,cex.lab=0.9,bty="n"
     )
## graph our fitted line
abline(fit.pic,lwd=2,col="darkgray")

# PARA PENSAR:
#   Comparando os modelo com dados brutos e com PICs, existe diferença na 
#   sustentação estatística? E quanto ao ajuste dos modelos? 
#   As relações inferidas são as mesmas (inclinação da reta)?

############################# INFERÊNCIA POR PGLS #############################

### ajustando modelos evolutivos
fitBM = fitContinuous(phy = tree, 
                      dat = data[,y],  
                      model = "BM")

fitOU = fitContinuous(phy = tree, 
                      dat = lam,  
                      model = "OU")

### comparando modelos evolutivos
aic = setNames(c(AIC(fitBM) , AIC(fitOU)), c("BM", "OU"))
aicw = aic.w(aic)
aicw

# PARA PENSAR:
#   Qual modelo melhor representa a evolução do tamanho da área de ocorrência?
#   Esse modelo difere do modelo que é assumido no métodos dos contrastes?

### vetor com o nome das espécies
spp = rownames(anole.data)

### nome do melhor modelo
best_model = names(which(aicw == max(aicw)))

### CALCULANDO VCV: 

## Se BM for o melhor modelo:
sigma_sq = fitBM$opt$sigsq
cor_str = corBrownian(value = sigma_sq, phy=anole.tree, form=~spp)

## Se OU for o melhor modelo:
alpha = fitOU$opt$alpha
cor_str = corMartins(value = alpha, phy=miconia.tree, form=~spp, fixed = T)

# IPORTANTE:
#   Note que a estrutura de correlação é calculada com base em diferentes parâmetros
#   dependendo do modelo que foi escolhido para representar a evolução da característica. 

### ajustando PGLS aos dados
fit.pgls<-gls(lam~fll,
              data = anole.data,
              correlation = cor_str)

### verificando normalidade dos resíduos
shapiro.test(resid(fit.pgls))

### verificando sustentação do modelo
summary(fit.pgls)

### visualizando modelo PGLS
plot(lam~fll,data=miconia.data,
     xlab="comprimeto do membro anterior (cm)",
     ylab="número de lamelas",
     pch=21,bg="gray",cex=1.2,las=1,
     cex.axis=0.7,cex.lab=0.9,bty="n")
abline(fit.pgls, lwd=2,col="darkgray")

# PARA PENSAR:
#   Comparando os modelos, existe diferença na sustentação estatística?
#   E quanto ao ajuste dos modelos? As relações inferidas são as mesmas?