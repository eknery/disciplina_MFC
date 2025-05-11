# Nesta prática vamos investigar a diversificação da tribo Cassieae.
# Nós queremos estimar suas taxas de especiação e extinção e verificar se essas
# estimativas produzem cenários razoáveis para nossa filogenia.

########################## CARREGANDO BIBLIOTECAS E DADOS #####################

if (!require("phytools")) install.packages("phytools"); library("phytools")
if (!require("geiger")) install.packages("geiger"); library("geiger")

### carregando filogenia
tree = read.tree("dados/cassieae.tree")

### filogenia ultramétrica
tree = force.ultrametric(tree = tree)

### visulizando árvores
plotTree(tree,
         type= "fan",
         fsize= 0.1,
         lwd=1,
         part=0.88)
### altura máxima 
hmax = as.integer(max(nodeHeights(tree)))
### escala de tempo
obj<-axis(1,
          pos=-1,
          at= seq(from = 0, to = hmax, by = 14),
          padj = -3.5,
          cex.axis=0.4,
          labels=T)

############################## TESTE DE PURO NASCIMENTO ########################

### teste de desvio do puro nascimento
ltt_obj <- ltt(tree, plot=FALSE)
ltt_obj

### ltt esperado
tf = max(nodeHeights(tree))
expected_ltt = log(Ntip(tree)) /tf

### visualizando teste
par(mfrow = c(1,1))
plot(ltt_obj,
     log.lineages=T,
     lty="dotted",
     lwd=2,
     col="blue",
     cex.axis=0.8
)
abline(a = 0, 
       b = expected_ltt,
       col = "blue",
       lwd = 1.5
)

############################## AJUSTANDO MODELOS ################################

### ajustando modelo Pure Birth
fitPure = fit.yule(tree, rho = 0.4)

### ajustando modelo Birth-Death
fitBD = fit.bd(tree, rho = 0.4)

### comparando modelos
AIC(fitPure, fitBD)

##################### SIMULANDO ÁRVORES COM MESMA DIVERSIFICAÇÃO ##############

### tempo total das simulações
tf = max(nodeHeights(tree))

### IMPORTANTE: 
#Colete as taxas de especiação (lambda) e extinção (mu) do melhor modelo!
lambda = c()
mu = c()

### simulação de árvores
sim_trees = pbtree(
 b= lambda,
 d= mu,
 t= tf,
 nsim = 25
)

### contrastando simualações e filogenia
hist(Ntip(sim_trees),
     breaks = 15,
     main = "",
     xlab= "Número de espécies simuladas"
     )
abline(v = Ntip(tree),
       col = "blue"
       )
