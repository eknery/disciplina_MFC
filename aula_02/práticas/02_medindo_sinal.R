# Nessa prática vamos investigar o sinal filogenético de diferentes caracteríticas
# medidas em linhagens de primatas. Nós hipotezimaos que características que
# evoluem mais rápido terão uma menor similaridade entre linhagens próximas.
# Assim, nossa expectativa é que taxas evolutivas maiores estejam associadas
# com valores de menores de sinal filogenético. Nós vamos considerar duas medidas 
# de sinal filogenético: lambda de Pagel e K de Bloomberg.

######################## CARREGANDO BIBLIOTECAS E DADOS #######################

### bibliotecas
if (!require("phytools")) install.packages("phytools"); library("phytools")
if (!require("geiger")) install.packages("geiger"); library("geiger")

### carregando dados fenotípicos
data = read.csv("dados/primateEyes.csv",  h= T)
head(data)

### carregando filogenia
tree = read.tree("dados/primate.tre")
print(tree,printlen=2)

############################## PROCESSANDO DADOS ###############################

### escolha um trait
trait_name = "Skull_length"

### trait, com escala padronizanda
xmean = mean(data[,trait_name])
xsd = sd(data[,trait_name])
trait = (data[,trait_name] - xmean) /xsd

### vetor nomeado
names(trait) = data$Genus_species
trait

### verificando correspondência entre dados e filogenia
name.check(tree, trait)

############################### VISUALIZANDO DADOS #############################

### gráfico da filogenia
plotTree.barplot(
  tree = tree,
  x = trait,
  args.plotTree =list(fsize=0.4)
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

### valores de AIC
aic = setNames(c(fitPunctual$opt$aic,
                 fitRWalk$opt$aic,
                 fitDirectional$opt$aic),
               c("Punctual","Random Walk","Directional")
)
### ver valores de AIC
aic

### estimativas de taxa evolutiva
sigma.sq = setNames(c(fitPunctual$opt$sigsq,
                      fitRWalk$opt$sigsq,
                      fitDirectional$opt$sigsq),
                    c("Punctual","Random Walk","Directional")
)
### ver taxas evolutivas
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
  nsim = 10000
  )

### verificando resultados
kbloom
plot(kbloom,las=1,cex.axis=0.9)

## PARA PENSAR:
# O valor de K foi alto ou baixo? O que isso indica? 
# O valor de P foi significativo? O que isso indica?

##################################### TAREFA ####################################
# Infira os valores de sigma, lambda e K de Bloomber para as demais características. 
# Os valores de sigma e de sinal filogenético parecem ter alguma relação? 

