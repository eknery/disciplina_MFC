# Nessa prática vamos investigar se características reprodutivas possuem maior 
# estabilidade evolutiva do que características vegetativas em plantas. Para isso, 
# vamos analisar dados sobre o tamanho do corpo, das folhas, das inflorescências 
# e das flores de um clado do gênero Miconia. Nós hipotetizamos que as 
# carcaterísticas reprodutivas teriam evoluído principalmente na especiação 
# devido ao seu papel no isolamento reprodutivo, enquanto que as características 
# vegetativas teriam evoluído ao longo das gerações por processos estocásticos. 
# Assim, esperamos que o tamanho das inflorescências e das flores tenham um melhor 
# ajuste com o modelo pontuado, enquanto que o tamanho da planta e da folhas 
# tenham um melhor ajuste com o modelo de caminhada aleatória. 

######################### CARREGANDO BIBLIOTECAS E DADOS ########################

### bibliotecas
if (!require("phytools")) install.packages("phytools"); library("phytools")
if (!require("geiger")) install.packages("geiger"); library("geiger")

### carregando dados fenotípicos
data = read.csv("dados/miconia_data.csv",  row.names=1, h= T)
head(data)

### carregando filogenia
phy = read.tree("dados/miconia_tree.nwk")
print(phy)

############################## PROCESSANDO DADOS ###############################

### verificando correspondência entre dados e árvore
chk = name.check(phy,data)
chk

### retirando terminais ausentes nos dados fenotípicos
phy_fix = drop.tip(phy, chk$tree_not_data)
### verificando correspondência após ajuste
chk = name.check(phy_fix ,data)
chk

############################## VISUALIZANDO DADOS ##############################

### selecionando uma característica
trait_name = "leaf.length.cm"

### selecionando uma característica
trait = data[,trait_name]
names(trait) = rownames(data)
### verificando vetor
trait

### dividindo visualização
par(mfrow=c(1,2))
### visualizando árvore filogenética
plotTree(
  phy,
  fsize= 0.5,
  mar = c(2, 1, 1, 1)
)
axisPhylo()
### visualizando distribuição da característica
hist(
  trait,
  main= trait_name
)
### retornando visualização única
par(mfrow=c(1,1))

# PARA PENSAR:
# A característica parece ter seguido alguma tendência? 

############################### AJUSTANDO MODELOS ##############################

### ajustando modelo Pontuado
fitPunctual = fitContinuous(
  phy = phy_fix,
  dat = trait,
  model ="kappa"
)

### ajustando modelo de Caminhada Aleatória
fitRWalk = fitContinuous(
  phy = phy_fix,
  dat = trait,
  model ="BM"
)

### ajustando modelo Direcional
fitDirectional =fitContinuous(
  phy = phy_fix,
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

# PARA PENSAR:
#   Baseado apenas no AIC, qual modelo evolutivo teve o melhor ajuste?
#   As diferernças de AIC são contrastantes?

### número de parâmetros dos modelos
k = setNames(c(fitPunctual$opt$k,
                 fitRWalk$opt$k,
                 fitDirectional$opt$k),
               c("Punctual","Random Walk","Directional")
)
### ver número de parâmetros
k 

# PARA PENSAR:
#   Existe diferença na complexidade dos modelos? Qual o modelo mais simples?
#   Os modelos mais complexos tem um ganho muito maior de ajuste? 

### estimativas de taxa evolutiva
sigma.sq = setNames(c(fitPunctual$opt$sigsq,
                      fitRWalk$opt$sigsq,
                      fitDirectional$opt$sigsq),
                    c("Punctual","Random Walk","Directional")
                    )
### ver taxas evolutivas
sigma.sq

## PARA PENSAR:
# As estimativas de taxas evolutivas diferiram muito entre os modelos? 

### estimativas de fenótipo ancestral
z0 = setNames(c(fitPunctual$opt$z0,
               fitRWalk$opt$z0,
               fitDirectional$opt$z0),
             c("Punctual","Random Walk","Directional")
)
### ver estimativas de fenótipo ancestral
z0

## PARA PENSAR:
# As estimativas de fenótipo ancestral diferiram muito? 
# Essas estimativas fazem sentido biologicamente?

#################################### TAREFA ####################################

# Repita o processo de ajuste de modelos para as demais características.
# Qual é o melhor modelo para cada característica? Seus parâmetros são razoáveis?
# Esses modelos corroboram ou refutam a nossa hipótese inicial?
