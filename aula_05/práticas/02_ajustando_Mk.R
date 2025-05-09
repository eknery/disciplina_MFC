# Nesta prática vamos investigar a evolução do número de dígitos nas patas 
# dos lagartos (Squamata).Mais especificamente queremos saber se a ausência
# de dígitos evoluiu de forma gradativa (perdas consecutivas de dígitos) ou 
# de forma abrupta (perda total de todos os dígitos)

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
plotTree(sqTree,type="fan",
         lwd=1,
         fsize=0.3,
         ftype="i")

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

############################# VISUALIZANDO DADOS ##############################

### cores para cada estado
cols<-c("0" = "white",
        "1" = "lightblue",
        "2" = "darkblue",
        "3" = "purple",
        "4" = "red",
        "5" = "darkred")

### plotando árvore com estados
dotTree(tree = sqTree.pruned,
        x = toes,
        colors=cols,
        fsize = 0.3,
        pt.cex = 0.3,
        legend = F)
legend("bottomleft",
       legend=names(cols),
       pch=22,
       pt.cex=1.5,
       pt.bg=cols,
       bty="n",
       cex=0.8)

# PARA PENSAR:
#   Baseado na figura, o estado de nenhum dígito (0) teria evoluído mais de uma vez?

########################## AJUSTANDO MATRIZES PADRÃO ###########################

### ajustando matriz com uma única taxa
fitER<-fitDiscrete(phy = sqTree.pruned,
                   dat = toes,
                   model= "ER")

plot(fitER, show.zeros=FALSE)

### ajustando matriz com taxas simétricas
fitSYM<-fitDiscrete(phy = sqTree.pruned,
                   dat = toes,
                   model= "SYM")

plot(fitSYM, show.zeros=FALSE)

### ajustando matriz com todas as taxas diferentes
fitARD<-fitDiscrete(phy = sqTree.pruned,
                    dat = toes,
                    model= "ARD")

plot(fitARD, show.zeros=FALSE)

######################## AJUSTANDO MATRIZ CUSTOMIZADA ##########################

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
#   Os números NÃO indicam os valores das taxas de transição, mas sim o 'grupo' 
#   de taxas ao qual aquela transição pertence. Números iguais indicam que as 
#   transições seguem a mesma taxa. Apenas o zero (0) indica que a taxa é nula.

## fit bi-directional ordered model
fitOrdered<-fitDiscrete(phy = sqTree.pruned,
                        dat = toes,
                        model = ordered.model,
                        surpressWarnings=TRUE)

plot(fitOrdered, show.zeros=FALSE)

############################### COMPARANDO MODELOS #############################

### extraindo valores de AIC
aic<-setNames(c(AIC(fitER),AIC(fitSYM),AIC(fitARD), AIC(fitOrdered)),
              c("ER","SYM","ARD", "Ordered"))
aic

### peso relativo dos modelos
aic.w(aic)

# PARA PENSAR: 
#   Qual modelo obeteve maior sustentação? Com base nesse modelo, o que podemos 
#   inferir sobre a evolução dos dígitos nas linhagens de lagartos?