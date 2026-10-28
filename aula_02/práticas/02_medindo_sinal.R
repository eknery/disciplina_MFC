# Nessa prática vamos investigar se a similaridade entre espécies de primatas 
# está associada com a velocidade com que as suas características evoluem.
# Nós hipotetizamos que características que evoluíram mais rápido terão 
# uma menor similaridade entre espécies próximas. Assim, nossa expectativa é 
# que características com taxas evolutivas maiores tenham menor sinal filogenético. 
# Nós vamos considerar duas medidas de sinal: lambda de Pagel e K de Bloomberg.

######################## CARREGANDO BIBLIOTECAS E DADOS #######################

### bibliotecas
if (!require("phytools")) install.packages("phytools"); library("phytools")
if (!require("geiger")) install.packages("geiger"); library("geiger")


### carregando filogenia
tree = read.tree("dados/primate_tree.nwk")
print(tree)

### carregando dados fenotípicos
data = read.csv("dados/primate_data.csv",  h= T,  row.names=1)
head(data)

############################## PROCESSANDO DADOS ###############################

### escolhendo uma característica
trait_name = "skull.length"
### transformação logarítimica (facilita comparação entre escalas)
trait = log(data[,trait_name]) 
### nomenado vetor
names(trait) = row.names(data)
### verificando vetor da característica
trait

### verificando correspondência entre dados e filogenia
name.check(tree, trait)

############################### VISUALIZANDO DADOS #############################

### visualizando filogenia e característica
plotTree.barplot(
  tree = tree,
  x = trait,
  args.plotTree = list(fsize = 0.4),
  args.barplot = list(xlab = trait_name)
)

# PARA PENSAR:
# Existe algum padrão de similaridade entre linhagens próximas?

############################### AJUSTANDO MODELOS #############################

### ajustando modelo Pontuado
fitPunctual = fitContinuous(
  phy = tree,
  dat = trait,
  model ="kappa"
)

### ajustando modelo de Caminhada Aleatória
fitRWalk = fitContinuous(
  phy = tree,
  dat = trait,
  model ="BM"
)

### ajustando modelo Direcional
fitDirectional = fitContinuous(
  phy = tree,
  dat = trait,
  model ="mean_trend"
)

################################ COMPARANDO MODELOS ############################

### extraíndo valores de AIC
aic = setNames(c(fitPunctual$opt$aic,
                 fitRWalk$opt$aic,
                 fitDirectional$opt$aic),
               c("Punctual","Random Walk","Directional")
)
### verificando valores de AIC
aic


### extraíndo estimativas de taxa evolutiva
sigma.sq = setNames(c(fitPunctual$opt$sigsq,
                      fitRWalk$opt$sigsq,
                      fitDirectional$opt$sigsq),
                    c("Punctual","Random Walk","Directional")
)
### verificando taxas evolutivas
sigma.sq

################################ MEDINDO SINAL #################################

### testando sinal filogenético por lambda de Pagel
lambda = phylosig(
  tree = tree,
  x = trait,
  method="lambda",
  test = TRUE
  )

### verificando resultados
lambda
plot(lambda,las=1,cex.axis=0.9)

# PARA PENSAR:
# O valor de lambda foi alto ou baixo? O que isso indica? 
# O valor de P foi significativo? O que isso indica?

### testando sinal filogenético por K de Bloomberg
kbloom = phylosig(
  tree = tree,
  x = trait,
  method= "K",
  test = TRUE,
  nsim = 1000
  )

### verificando resultados
kbloom
plot(kbloom,las=1,cex.axis=0.9)

## PARA PENSAR:
# O valor de K foi alto ou baixo? O que isso indica? 
# O valor de P foi significativo? O que isso indica?

##################################### TAREFA ####################################

# Infira os valores de sigma, lambda e K  para as demais características. 
# Os valores de sigma e de sinal filogenético parecem ter alguma relação? 

