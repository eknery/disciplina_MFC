if (!require("phytools")) install.packages("phytools"); library("phytools")

############################ SIMULANDO APENAS ESPECIAÇÃO ########################

## selecionar taxa de especiação
lambda = 0.3

## simular árvore apenas com especiação
tree_b <-pbtree(b= lambda,
                 t= 10,
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
     ylab = "N de espécies",
     main = paste0("lambda: ", lambda, " mu: ", 0),
     pch=21,bg="gray",cex=1.2,las=1
     )
abline(lm.fit, lwd=2,col="darkgray")
text(lm.fit$coefficients["ltt_b$ltt"])

PARA PENSAR:
  A regressão linear entre 
