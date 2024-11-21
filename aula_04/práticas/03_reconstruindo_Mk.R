# Nessa prática vamos reconstruir a evolução do número de dígitos nas patas traseiras
# dos lagartos (Squamata).Para isso, vamos utilizar o modelo de transição de 
# melhor ajuste aos dados, mas considerando duas abordagens de reconstrução: 
# 1) otimização global e 2) otimização marginal.

######################## CARREGANDO DADOS E BIBLIOTECAS ########################

### bibliotecas
if (!require("phytools")) install.packages("phytools"); library("phytools")
if (!require("geiger")) install.packages("geiger"); library("geiger")
if (!require("corHMM")) install.packages("corHMM");library("corHMM")

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

############################# VISUALIZANDO DADOS ##############################

### cores para cada estado
cols<-c("0" = "white",
        "1" = "lightblue",
        "2" = "darkblue",
        "3" = "purple",
        "4" = "red",
        "5" = "darkred")

### visulizando árvore com estados atuais
dotTree(tree = sqTree.pruned,
        x = toes,
        colors= cols,
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

################################ RECONSTRUÇÃO ANCESTRAL ########################

### matriz com estados ordenados
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

### organizando dados em dataframe
toes.data = data.frame(Genus_sp= rownames(sqData.pruned), 
                     toes=sqData.pruned[,"rear.toes"])
toes.data

### reconstrução global
fit.joint<-corHMM(phy = sqTree.pruned,
                  data = toes.data,
                  node.states= "joint",
                  rate.cat=1,
                  rate.mat= ordered.model
                  )
fit.joint

### visualizando reconstrução global
dotTree(tree = sqTree.pruned,
        x = toes,
        colors=cols,
        fsize = 0.3,
        cex = 0.1,
        legend = F)
nodelabels(pie=to.matrix(levels(toes)[fit.joint$phy$node.label], levels(toes) ),
           piecol=cols,
           cex=0.4)
legend("bottomleft",
       legend=names(cols),
       pch=22,
       pt.cex=1.5,
       pt.bg=cols,
       bty="n",
       cex=0.8)

# PARA PENSAR:
#   Qual é o estado mais provável para o último ancestral comum de todos os lagartos?
#   Quantas vezes os dígitos foram totalmente perdidos ?
  
### reconstrução marginal
fit.marginal<-corHMM(phy = sqTree.pruned,
                  data = toes.data,
                  node.states= "marginal",
                  rate.cat=1,
                  rate.mat= ordered.model
)
fit.marginal

### visualizando reconstrução marginal
dotTree(tree = sqTree.pruned,
        x = toes,
        colors=cols,
        fsize = 0.3,
        cex = 0.1,
        legend = F)
nodelabels(pie= as.data.frame(fit.marginal$states,col.names = names(cols)),
           piecol=cols,
           cex=0.4)
legend("bottomleft",
       legend=names(cols),
       pch=22,
       pt.cex=1.5,
       pt.bg=cols,
       bty="n",
       cex=0.8)

# PARA PENSAR:
#   Qual é o estado mais provável para o último ancestral comum de todos os lagartos?
#   Quantas vezes os dígitos foram totalmente perdidos ? Existem diferenças em 
#   relação a reconstrução global?
  