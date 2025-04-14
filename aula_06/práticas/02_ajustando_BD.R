# Nessa prática vamos investigar a diversificação do gênero Chamaecrista. 
# Essas plantas são leguminosas lenhosas que ocupam principalmente ambientes abertos 
# da América do Sul. Esses ambientes provavelmente passaram por ciclos de expansão
# e contração durante o Quaternário (~ 2.8 mya), o que pode ter propiciado tanto
# o aumento da especiação quanto da extinção das linhagens de plantas.

########################## CARREGANDO BIBLIOTECAS E DADOS #####################

if (!require("phytools")) install.packages("phytools"); library("phytools")
if (!require("geiger")) install.packages("geiger"); library("geiger")

### carregando filogenia
tree = read.tree("dados/chamaecrista.tree")

### visulizando árvores
plotTree(tree,
         type= "fan",
         fsize= 0.3,
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

################################# PROCESSANDO DADOS ############################

### retirando linhagens fora do gênero
tree_prun = keep.tip(tree, tip = tree$tip.label[grepl("C_", tree$tip.label)] )

### visulizando árvores
plotTree(tree_prun,
         type= "fan",
         fsize= 0.3,
         lwd=1,
         part=0.88)
### altura máxima 
hmax = as.integer(max(nodeHeights(tree_prun)))
### escala de tempo
obj<-axis(1,
          pos=-1,
          at= seq(from = 0, to = hmax, by = 10.25),
          padj = -3.5,
          cex.axis=0.4,
          labels=T)

############################## TESTE DE PURO NASCIMENTO ########################

### teste de desvio do puro nascimento
ltt_obj <- ltt(tree_prun, log.lineages=T)
ltt_obj

### considerando amostragem parcial
mccr_obj <- mccr(ltt_obj,
                 rho= Ntip(tree_prun)/366,
                 nsim=500
                 )
mccr_obj

### visualizando teste
plot(mccr_obj,
     las=1,
     cex.axis=0.8)

############################## AJUSTANDO MODELOS ################################

### ajustando modelo de puro nascimento
yule_obj<-fit.yule(tree_prun,
                   rho = Ntip(tree_prun)/366
                   )
### vendo estimativas
yule_obj

### ajustando modelo BD
bd_obj = fit.bd(tree_prun,
               rho = Ntip(tree_prun)/366
               )
### vendo estimativas
bd_obj

### comparando modelos
AIC(yule_obj, bd_obj)
