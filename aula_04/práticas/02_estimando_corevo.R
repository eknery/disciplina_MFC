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
if (!require("MASS")) install.packages("MASS"); library("MASS")
if (!require("mnormt")) install.packages("mnormt"); library("mnormt")

### carregando dados fenotípicos
data = read.csv("dados/centrarchidae_data.csv",  h= T,  row.names=1)
head(data)

### carregando filogenia
tree = readRDS("dados/centrarchidae_tree.RDS")
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

### estados das linhagens atuais
tipstates = data[tree$tip.label,"feeding.mode"]
names(tipstates) = tree$tip.label
### estados das linhagens ancestrais
nodestates = tree$node.label

### mapeando cores para categorias
color_map = c(
  "non" = "lightgray",
  "pisc" = "black"
)

############################## VISUALIZANDO DADOS ##############################

## dividir visualização
par(mfrow=c(1,2))
### visualizando a distribuição dos dados 
plot(
  buccal.length~gape.width, 
  data = data,
  col = color_map[data$feeding.mode],
  xlab= "log largura bucal (mm)",
  ylab= "log comprimento bucal (mm)",
  pch = 19,
  cex = 1.2,
  las =1,
  cex.axis = 0.7,
  cex.lab = 0.9
)
### visualizando a árvores filogenética
plotSimmap(
  tree,
  colors = color_map,
  mar = c(2, 1, 1, 1)
  )
axisPhylo()
## retornar visualização única
par(mfrow=c(1,1))

# PARA PENSAR:
#   Baseado no gráfico, existe uma relação entre a largura e o comprimento bucal? 

############################ INFERÊNCIA POR CONTRASTES ########################

### calculando os contrastes para cada característica
xpic = pic(xvar, tree)
ypic = pic(yvar, tree)
### verificar valores dos contrastes
xpic
ypic

### ajustando regressão simples (sem efeito do modo de alimentação)
fit.pic1 = lm(ypic~xpic+0)
### verificando Normalidade dos resíduos
shapiro.test(resid(fit.pic1))
### calculando sustentação do modelo
sum.fit.pic1 = summary(fit.pic1)
### verificando parâmetros e sustentação
sum.fit.pic1

### ajustando regressão aninhada (com efeito do modo de alimentação)
fit.pic2 = lm(ypic~xpic:nodestates+0)
### verificando Normalidade dos resíduos
shapiro.test(resid(fit.pic2))
### calculando sustentação do modelo
sum.fit.pic2 = summary(fit.pic2)
### verificando parâmetros e sustentação
sum.fit.pic2

### visualizando modelos PGLS 
## dividir visualização
par(mfrow=c(1,2))
## primeiro modelo 
plot(
  ypic~xpic,
  col = "darkgray",
  main = "PIC simples",
  xlab="PIC largura bucal",
  ylab="PIC comprimento bucal",
  pch = 19,
  cex = 1.2,
  las =1,
  cex.axis = 0.7,
  cex.lab = 0.9
)
abline(fit.pic1,lwd=2,col="darkgray")
## segundo modelo 
plot(
  ypic~xpic,
  col = color_map[nodestates],
  main = "PIC aninhado",
  xlab="PIC largura bucal",
  ylab="PIC comprimento bucal",
  pch = 19,
  cex = 1.2,
  las =1,
  cex.axis = 0.7,
  cex.lab = 0.9
)
abline(
  a = 0, 
  b = fit.pic2$coefficients[1],
  lty = 3,
  lwd = 2,
  col = "darkgray"
)
abline(
  a = 0, 
  b = fit.pic2$coefficients[2],
  lty = 1,
  lwd = 2,
  col = "black"
)
## retornar visualização única
par(mfrow=c(1,1))

### extraindo valores de R2
r2 = setNames(
  c(sum.fit.pic1$r.squared , sum.fit.pic2$r.squared), 
  c("Simples", "Aninhado")
)
### comparando modelos PIC
r2

# PARA PENSAR:
#   Comparando os modelos PIC, qual possui o melhor ajuste?
#   Baseado no melhor modelo, como é a correlação evolutiva das características?


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
### extraindo valores de AIC
aic = setNames(c(AIC(fitBM) , AIC(fitOU)), c("BM", "OU"))
### comparando modelos
aic

# PARA PENSAR:
#   Qual modelo melhor representa a evolução do tamanho da boca?
#   Esse modelo difere do modelo que é assumido no método dos contrastes?

### calculando VCV: 
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

### extraíndo valores de AIC
aic = setNames(
  c(sum.fit.pgls1$AIC, sum.fit.pgls2$AIC), 
  c("Simples", "Aninhado")
)
### comparando modelos de PGLS
aic

# PARA PENSAR:
#   Comparando os modelos PGLS, qual possui o melhor ajuste?
#   Baseado no melhor modelo, como é a correlação evolutiva das características?

############################# INFERÊNCIA POR MATRIZ R ##########################

### ajustando matriz com taxas e correlação fixas (sem efeito do modo de alimentação)
fit.r1 = evolvcv.lite(
  tree = tree, 
  X = data[,c(xname,yname)],
  models = "1"
)
### verificando ajuste
fit.r1
### extraíndo correlação evolutiva
r1.corevo =  setNames(
  c(cov2cor(fit.r1$model$R)[1,2], cov2cor(fit.r1$model$R)[1,2]),
  c("non", "pisc")
)
### verificando correlação evolutiva
r1.corevo 

### ajustando matriz matriz com taxas e correlação variáveis (com efeito do modo de alimentação)
fit.r2 = evolvcv.lite(
  tree = tree, 
  X = data[,c(xname,yname)],
  models = "3"
)
### verificando ajuste
fit.r2
### extraíndo correlação evolutiva
r2.corevo =  setNames(
  c(cov2cor(fit.r2$model$R[[1]])[1,2], cov2cor(fit.r2$model$R[[2]])[1,2]),
  c("non", "pisc")
)
### verificando correlação evolutiva
r2.corevo 

### extraíndo valores de AIC
aic = setNames(
  c(fit.r1$model$AIC, fit.r2$model$AIC), 
  c("Fixo", "Variável")
)
### comparando modelos de correlação evolutiva
aic

# PARA PENSAR:
#   Comparando as matrizes de correlação, qual possui o melhor ajuste?
#   Baseado na melhor matriz, como é a correlação evolutiva das características?