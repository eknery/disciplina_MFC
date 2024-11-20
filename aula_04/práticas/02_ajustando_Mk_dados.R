
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

###################### AJUSTANDO MATRIZES COM TAXAS IGUAIS ######################

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