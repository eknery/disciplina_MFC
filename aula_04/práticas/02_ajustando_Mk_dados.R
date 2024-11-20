
######################## CARREGANDO DADOS E BIBLIOTECAS ########################

### bibliotecas
if (!require("phytools")) install.packages("phytools"); library("phytools")
if (!require("geiger")) install.packages("geiger"); library("geiger")

### carregando dados fenotípicos
sqData<-read.csv("dados/squamate_data.csv",row.names=1)
sqData

### carregando árvore filogenética
sqTree<-read.nexus("dados/squamate.tre")
print(sqTree,printlen=2)

### visualizando árvore
plotTree(sqTree,type="fan",lwd=1,fsize=0.3,ftype="i")

############################# TRATANDO OS DADOS ################################

### verificando correspondência entre dados e árvore
chk<-name.check(sqTree,sqData)
summary(chk)

### retirando terminais ausentes nos dados fenotípicos
sqTree.pruned<-drop.tip(sqTree,chk$tree_not_data)

### retirando dados fenotípicos ausentes na árvore
sqData.pruned<-sqData[!(rownames(sqData)%in%
                          chk$data_not_tree),,drop=FALSE]

### organizando dados fenotípicos num vetor nomeado
toes<-setNames(as.factor(sqData.pruned[,"rear.toes"]),
               rownames(sqData.pruned))
toes

########################## AJUSTANDO MATRIZES PADRÃO ###########################

### ajustando matriz com uma única taxa
fitER<-fitDiscrete(phy = sqTree.pruned,
                   dat = toes,
                   model= "ER")
plot(fitER)

### ajustando matriz com taxas simétricas
fitSYM<-fitDiscrete(phy = sqTree.pruned,
                   dat = toes,
                   model= "SYM")
plot(fitSYM)

### ajustando matriz com todas as taxas diferentes
fitARD<-fitDiscrete(phy = sqTree.pruned,
                    dat = toes,
                    model= "ARD")
plot(fitARD)

###################### AJUSTANDO MATRIZES CUSTOMIZADA ##########################

### marriz com estados ordenados
ordered.model<-matrix(c(
  0,1,0,0,0,0,
  2,0,3,0,0,0,
  0,4,0,5,0,0,
  0,0,6,0,7,0,
  0,0,0,8,0,9,
  0,0,0,0,10,0),6,6,byrow=TRUE,
  dimnames=list(0:5,0:5))

### ver matriz
ordered.model

# IMPORTANTE:
#   Nas matrizes customizadas, os números NÃO indicam os valores das taxas de 
#   transição, mas sim o 'grupo' ao qual aquela transição pertence. Apenas o valor
#   zero (0) indica que a taxa é nula.

## fit bi-directional ordered model
fitOrdered<-fitDiscrete(phy = sqTree.pruned,
                        dat = toes,
                        model = ordered.model,
                        surpressWarnings=TRUE)

plot(fitOrdered, show.zeros=FALSE)

############################### COMPARANDO MODELOS #############################

aic<-setNames(c(AIC(fitER),AIC(fitSYM),AIC(fitARD), AIC(fitOrdered)),
              c("ER","SYM","ARD", "Ordered"))
aic

aic.w(aic)
