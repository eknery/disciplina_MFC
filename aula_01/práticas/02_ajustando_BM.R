# Nessa prática vamos investigar qual foi o modo e o tempo de evolução do tamanho 
# das folhas e das inflorescências dentro de um clado de Miconia. 
# Essas plantas são lenhosas e ocorrem tanto em ambientes abertos quanto 
# em florestas úmidas da América do Sul. Nossa expectativa é que as folhas 
# teriam evoluído mais rápido em resposta aos diferentes ambientes ocupados 
# pelas espécies de Miconia. Nós vamos considerar três modos 
# de evolução: equilibrio pontuado, caminha aleatória e evolução direcional.

######################### CARREGANDO BIBLIOTECAS E DADOS ########################

### bibliotecas
if (!require("phytools")) install.packages("phytools"); library("phytools")
if (!require("geiger")) install.packages("geiger"); library("geiger")

### carregando dados fenotípicos
miconia.data<-read.csv("dados/miconia.csv", row.names=1, h= T)
head(miconia.data)

### carregando filogenia
miconia.tree<-read.tree("dados/miconia.nwk")
print(miconia.tree,printlen=2)

############################## VISUALIZANDO DADOS ###############################

### valores de interesse em um vetor nomeado
leaf.size<- miconia.data[,"leaf.size"]
names(leaf.size)<-rownames(miconia.data)
leaf.size

### verificando a distribuição dos valores
hist(leaf.size)

### verificando correspondência entre dados e filogenia
name.check(miconia.tree, miconia.data)

### gráfico da filogenia
plotTree.barplot(tree = miconia.tree,
                 x = leaf.size,
                 args.plotTree=list(fsize=0.7)
)

# PARA PENSAR:
# O tamanho das folhas parece ter seguido alguma tendência? 

############################### AJUSTANDO MODELOS ##############################

### ajustando modelo Pontuado
fitPunctual <-fitContinuous(phy = miconia.tree,
                            dat = leaf.size,
                            model ="kappa"
)

### ajustando modelo de Random Walk
fitRWalk <-fitContinuous(phy = miconia.tree,
                         dat = leaf.size,
                         model ="BM"
)

### ajustando modelo Direcional
fitDirectional <-fitContinuous(phy = miconia.tree,
                               dat = leaf.size,
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
#   Baseado apenas no AIC, qual modelo evolutivo teve o melhor ajuste ao
#   tamanho das plantas nesse clado de Miconia? As diferernças de AIC são 
#   contrastantes?

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

# PARA PENSAR:
#   As estimativas de taxas evolutivas diferiram muito entre os modelos? 

### estimativas de fenótipo ancestral
z0= setNames(c(fitPunctual$opt$z0,
               fitRWalk$opt$z0,
               fitDirectional$opt$z0),
             c("Punctual","Random Walk","Directional")
)

### ver estimativas de fenótipo ancestral
z0

# PARA PENSAR:
#   As estimativas de fenótipo ancestral diferiram muito? Essas estimativas 
#   fazem sentido biologicamente?
