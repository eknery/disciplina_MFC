
################################ CARREGANDO DADOS #############################

library(geiger)

## read data matrix
sqData<-read.csv("dados/squamate_data.csv",row.names=1)
sqData

## read phylogenetic tree
sqTree<-read.nexus("dados/squamate.tre")
print(sqTree,printlen=2)

## plot our tree
plotTree(sqTree,type="fan",lwd=1,fsize=0.3,ftype="i")

############################### PROCESSANDO DADOS ##############################

## check name matching
chk<-name.check(sqTree,sqData)
summary(chk)

## drop tips of tree that are missing from data matrix
sqTree.pruned<-drop.tip(sqTree,chk$tree_not_data)
## drop rows of matrix that are missing from tree
sqData.pruned<-sqData[!(rownames(sqData)%in%
                          chk$data_not_tree),,drop=FALSE]

## extract discrete trait
toes<-setNames(as.factor(sqData.pruned[,"rear.toes"]),
               rownames(sqData.pruned))
toes
