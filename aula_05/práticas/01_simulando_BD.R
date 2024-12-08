if (!require("phytools")) install.packages("phytools"); library("phytools")

############################ SIMULANDO APENAS ESPECIAÇÃO ########################

## tempo total das simulações
tf = 50

## determinar taxa de especiação
lambda = 0.1

## simular árvore apenas com especiação
tree_b <-pbtree(b= lambda,
                t= tf,
                method="direct"
                )
## visualizar árvore
plot(tree_b)

## calcular LTT (lineage through time)
ltt_b<-ltt(tree_b, plot=FALSE)

## visualizar LTT
plot(ltt_b,
     bty="n",
     log.lineages=FALSE,
     las=1,cex.axis=0.8)

# PARA PENSAR:
#   Como acontece o acúmulo de espécies no tempo? Parece um crescimento linear?

## ajustando modelo linear N species ~ tempo
lm.fit1 = lm(log(ltt_b$ltt) ~ ltt_b$times)
summary(lm.fit1)

## visualizando modelo
plot(x = ltt_b$times,
     y = log(ltt_b$ltt),
     xlab = "Tempo (milhões de anos)",
     ylab = "log(N de espécies)",
     main = paste0("lambda: ", lambda, " mu: ", 0),
     pch=21,bg="gray",cex=1.2,las=1
     )
abline(lm.fit1, lwd=2,col="darkgray")

# PARA PENSAR:
#   A regressão linear entre número de espécies e tempo teve sustentação estatística?
#   A regressão possui um bom ajuste aos dados? Qual é o coeficiente angular?

########################## SIMULANDO ESPECIAÇÃO E EXTINÇÃO #####################

## determinar taxa de especiação e extinção
mu = 0.01

## simular árvore apenas com especiação
tree_bd <-pbtree(b = lambda,
                 d = mu, 
                 t = tf,
                 method ="direct"
)
## visualizar árvore
plot(tree_bd)

## calcular LTT (lineage through time)
ltt_bd<-ltt(tree_bd, plot=FALSE)

## visualizar LTT
plot(ltt_bd,
     bty="n",
     log.lineages=FALSE,
     las=1,cex.axis=0.8)

# PARA PENSAR:
#   Como acontece o acúmulo de espécies no tempo? Parece um crescimento linear?

## ajustando modelo linear N species ~ tempo
lm.fit2 = lm(log(ltt_bd$ltt) ~ ltt_bd$times)
summary(lm.fit2)

## visualizando modelo
plot(x = ltt_bd$times,
     y = log(ltt_bd$ltt),
     xlab = "Tempo (milhões de anos)",
     ylab = "N de espécies",
     main = paste0("lambda: ", lambda, " mu: ", mu),
     pch=21,bg="gray",cex=1.2,las=1
)
abline(lm.fit2, lwd=2,col="darkgray")

# PARA PENSAR:
#   A regressão linear entre número de espécies e tempo teve sustentação estatística?
#   A regressão possui um bom ajuste aos dados? Qual é o coeficiente angular?

############################## SEM LINHAGENS FÓSSEIS ###########################

## retirar linhagens fósseis e rescalar árvore
tree_bd_ext <- drop.tip(tree_bd, getExtinct(tree_bd))
tree_bd_ext$root.edge<- tf- max(nodeHeights(tree_bd_ext))
tree_bd_ext<-rootedge.to.singleton(tree_bd_ext)

## visualizar árvores
plot(tree_bd_ext)

## calcular LTT (lineage through time)
ltt_bd_ext <- ltt(tree_bd_ext, plot=FALSE)

## visualizar LTT
plot(ltt_bd_ext,
     bty="n",
     log.lineages=FALSE,
     las=1,cex.axis=0.8)

# PARA PENSAR:
#   Como acontece o acúmulo de espécies no tempo? Parece um crescimento linear?

## ajustando modelo linear N species ~ tempo
lm.fit3 = lm(log(ltt_bd_ext$ltt) ~ ltt_bd_ext$times)
summary(lm.fit3)

## visualizando modelo
plot(x = ltt_bd_ext$times,
     y = log(ltt_bd_ext$ltt),
     xlab = "Tempo (milhões de anos)",
     ylab = "N de espécies",
     main = paste0("lambda: ", lambda, " mu: ", mu),
     pch=21,bg="gray",cex=1.2,las=1
)
abline(lm.fit3, lwd=2,col="darkgray")

# PARA PENSAR:
#   A regressão linear entre número de espécies e tempo teve sustentação estatística?
#   A regressão possui um bom ajuste aos dados? Qual é o coeficiente angular?

################################# CONTRASTANDO CENÁRIOS ########################

par(lend=1,mar=c(5.1,4.1,2.1,2.1))
plot(ltt_b,
     bty="n",
     log.lineages=FALSE,
     lwd=2,xlim=c(0,tf),las=1,cex.axis=0.8)
plot(ltt_bd,
     log.lineages=FALSE,
     col="black",
     lty="dotted",
     lwd=2,
     add=TRUE)
plot(ltt_bd_ext,
     log.lineages=FALSE,
     col="darkgray",
     lwd=2,
     add=TRUE)
