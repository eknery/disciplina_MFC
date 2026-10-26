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

### nome das espécies
spp = row.names(data)
### nomes das variáveis preditora e resposta
xname = "gape.width"
yname = "buccal.length"
### extraíndo variáveis
xvar = data[,xname]
yvar = data[,yname]
### nomeando valores por espécie
names(xvar) = names(yvar) = spp
### verificando vetores
xvar
yvar 

### retirando estados das linhagens atuais
tipstates = data[tree$tip.label,"feeding.mode"]
names(tipstates) = tree$tip.label

### mapeando cores para categorias
color_map = c(
  "pisc" = "black", 
  "non" = "darkgray"
)

############################## VISUALIZANDO DADOS ##############################

## dividir visualização
par(mfrow=c(1,2))
### visualizando a distribuição dos dados 
plot(
  buccal.length~gape.width, 
  data = data,
  col = color_map[data$feeding.mode],
  xlab="log largura bucal (mm)",
  ylab="log comprimento bucal (mm)",
  pch = 19,
  cex = 1.2,
  las =1,
  cex.axis = 0.7,
  cex.lab = 0.9
)
### visualizando a árvores filogenética
par(mar = c(2, 1, 1, 1))
plot(
  tree, 
  ftype = "i",
  fsize = 0.7,
  lwd = 1
  )
tiplabels(
  pie = to.matrix(tipstates, sort(unique(tipstates))), 
  piecol = color_map, 
  cex=0.40
  )
axisPhylo()
## retornar visualização única
par(mfrow=c(1,1))

# PARA PENSAR:
#   Baseado no gráfico, existe uma relação entre a largura e o comprimento bucal? 

############################ INFERÊNCIA POR CONTRASTES ########################

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

### ajustando modelo BM
fitBM = fitContinuous(
  phy = tree, 
  dat = yvar,  
  model = "BM"
  )
### ajustando modelo OU
fitOU = fitContinuous(
  phy = tree, 
  dat = yvar,  
  model = "OU"
  )
### comparando modelos evolutivos
aic = setNames(c(AIC(fitBM) , AIC(fitOU)), c("BM", "OU"))
aic

# PARA PENSAR:
#   Qual modelo melhor representa a evolução do tamanho da boca?
#   Esse modelo difere do modelo que é assumido no método dos contrastes?

### CALCULANDO VCV: 
## Se BM for o melhor modelo:
sigma_sq = fitBM$opt$sigsq
cor_str = corBrownian(value = sigma_sq, phy = tree, form=~spp)
## Se OU for o melhor modelo:
alpha = fitOU$opt$alpha
cor_str = corMartins(value = alpha, phy = tree, form=~spp, fixed = T)
# IPORTANTE:
#   Note que a estrutura de correlação é calculada com base em diferentes parâmetros
#   dependendo do modelo que foi escolhido para representar a evolução da característica. 

### ajustando PGLS simples (sem efeito do modo de alimentação)
fit.pgls1 = gls(
  buccal.length~gape.width,
  data = data,
  correlation = cor_str
  )
### verificando normalidade dos resíduos
shapiro.test(resid(fit.pgls1))
### calculando sustentação do modelo
sum.fit.pgls1 = summary(fit.pgls1)
### verificando parâmetros e sustentação
sum.fit.pgls1

### ajustando PGLS aninhado (com efeito do modo de alimentação)
fit.pgls2 = gls(
  buccal.length~gape.width:feeding.mode,
  data = data,
  correlation = cor_str
)
### verificando normalidade dos resíduos
shapiro.test(resid(fit.pgls2))
### calculando sustentação do modelo
sum.fit.pgls2 = summary(fit.pgls2)
### verificando parâmetros e sustentação
sum.fit.pgls2

### comparando PGLS
aic = setNames(c(sum.fit.pgls1$AIC, sum.fit.pgls2$AIC), c("Simples", "Aninhado"))
aic

# PARA PENSAR:
#   Qual modelo de PGLS teve o melhor ajuste aos dados?
#   O que esse modelo indica sobre a correlaçãoe evolutiva entre as características?


### visualizando modelos PGLS 
## dividir visualização
par(mfrow=c(1,2))
## primeiro modelo 
plot(
  buccal.length~gape.width, 
  data = data,
  col = "darkgray",
  xlab="log largura bucal (mm)",
  ylab="log comprimento bucal (mm)",
  main = "PGLS simples",
  pch = 19,
  cex = 1.2,
  las =1,
  cex.axis = 0.7,
  cex.lab = 0.9
)
abline(fit.pgls1, lwd=2,col="darkgray")
## segundo modelo 
plot(
  buccal.length~gape.width, 
  data = data,
  col = color_map[data$feeding.mode],
  xlab="log largura bucal (mm)",
  ylab="log comprimento bucal (mm)",
  main = "PGLS aninhada",
  pch = 19,
  cex = 1.2,
  las =1,
  cex.axis = 0.7,
  cex.lab = 0.9
)
abline(
  a = fit.pgls2$coefficients[1], 
  b = fit.pgls2$coefficients[2],
  lty = 3,
  lwd = 2,
  col = "darkgray"
  )
abline(
  a = fit.pgls2$coefficients[1], 
  b = fit.pgls2$coefficients[3],
  lty = 1,
  lwd = 2,
  col = "black"
  )
## retornar visualização única
par(mfrow=c(1,1))

# PARA PENSAR:
#   Comparando os modelos, existe diferença na sustentação estatística?
#   E quanto ao ajuste dos modelos? As relações inferidas são as mesmas?