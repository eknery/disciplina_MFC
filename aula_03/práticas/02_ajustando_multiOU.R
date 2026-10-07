# Nesta prática vamos investigar se o hábito de vida influenciou a evolução 
# dos lagartos do gênero Anolis. Nós hipotetizamos que lagartos que habitam
# a copa das árvores foram selecionadas para uma maior capacidade de escalada 
# do que lagartos que habitam o chão. Assim, esperamos que os linhagens das copas 
# das árvores tenham evoluído em direção a um ótimo de membros maiores e patas
# com maior capacidade de atrito do que as linhagens que habitam o chão. 
# Para isso, vamos inferir o modo e o tempo de evolução do comprimento dos
# membros anteriores e posteriores e do número de lamelas da patas.

######################### CARREGANDO BIBLIOTECAS E DADOS #######################

### bibliotecas
if (!require("phytools")) install.packages("phytools"); library("phytools")
if (!require("geiger")) install.packages("geiger"); library("geiger")
if (!require("OUwie")) install.packages("OUwie"); library("OUwie")

### carregando dados fenotípicos
data=read.csv("dados/anolis_data.csv", row.names=1)
### verificando dados
data

### carregando dados de hábito atual
regime = read.csv("dados/anolis_ecomorph.csv", row.names=1,  stringsAsFactors=TRUE)
### verificando dados de hábito
regime

### As siglas dos hábitos (regimes) são:
# CG = Crown-giant
# GB = Grass-bush
# TC = Trunk-crown
# TG = Trunk-ground
# Tr = Trunk
# Tw = Twig

### carregando árvore filogenética 'mapeada'
tree_map=read.simmap(file="dados/anolis_tree.nexus",  format="nexus", version=1.5)
### verificando árvore filogenética
tree_map

############################# TRATANDO OS DADOS ################################

### verificando correspondência entre dados e árvore
chk = name.check(tree_map,data)
summary(chk)

### retirando dados das espécies ausentes na árvore
data_fix = data[-which(rownames(data) %in% chk$data_not_tree),]

### verificando correspondência entre dados e árvore
chk = name.check(tree_map,data_fix)
chk

############################# ORGANIZANDO OS DADOS ##############################

### selecionando característica de interesse
trait = data_fix[,"FLL"]

## IMPORTANTE:
# Para testar nossa hipótese, mas investigar três características:
# o comprimento dos membros anteriores (forelimb, FLL) 
# o comprimento dos membros posteriores (hindlimb, HLL)
# o número de lamelas (lamellae number, LAM).
# Essa variáveis representa o fenótipo das patas.

### organizando dados na tabela OUwie
ouwie_table= data.frame(
  species = rownames(data_fix),
  regime = regime[rownames(data_fix),],
  trait = trait
 )
### vendo tabela
ouwie_table

### nomeando vetor da característica
names(trait) = rownames(data_fix)

############################### VISUALIZANDO DADOS ##############################

### cores para cada hábito
cols = c(
  "CG" = "lightblue", 
  "GB" = "orchid",
  "TC" = "darkblue", 
  "TG" = "darkred",
  "Tr" = "orange", 
  "Tw" = "darkgreen"
)

### estados das espécies atuais
tips = getStates(tree_map,"tips")
## cores para as espécies atuais
tip_cols = cols[tips]

### visualizar estados de hábito
plotSimmap(
  tree = tree_map,
  colors = cols,
  fsize= 0.5,
  )
legend(
  "topright",
   levels(regime[,1]),
   pch=22,
   pt.bg=cols,
   pt.cex=1,
   cex=0.5
  )

### visualizar a característica
plotTree.barplot(
  tree = tree_map,
  x = trait,
  args.plotTree = list(fsize=0.3),
  args.barplot = list(col=tip_cols, cex.lab=0.5)
  )
legend(
  "bottomright",
  levels(regime[,1]),
  pch=22,
  pt.bg=cols,
  pt.cex=1,
  cex=0.5
  )


# PARA PENSAR: 
# Como os valores estão distribuídos entre as linhagens?
# Os valores tendem a serem iguais entre linhagens próximas filogeneticamente? 
# O tipo de hábito parece ter alguma relação com os valores da característica?

############################### AJUSTANDO MODELOS #############################

### ajustando "Ruído branco"
fitWN = fitContinuous(
  phy = tree_map,
  dat = trait,
  model ="white"
  )
## verificar resultados
fitWN

### ajustando BM com uma única taxa de variação
fitBM = OUwie(
  phy = tree_map,
  data = ouwie_table,
  model ="BM1",
  simmap.tree = TRUE
 )
## verificar resultados
fitBM

### ajustando BM com múltiplas taxas de variação
fitBMS = OUwie(
  phy = tree_map,
  data = ouwie_table,
  model ="BMS",
  simmap.tree = TRUE
  )
## verificar resultados
fitBMS

### ajustando OU com múltiplos ótimos
fitOUM = OUwie(
  phy = tree_map,
  data = ouwie_table,
  model ="OUM",
  simmap.tree = TRUE
  )
## verificar resultados
fitOUM

############################## COMPARANDO MODELOS ##############################

### extraindo valores de AIC 
aic=setNames(c(fitWN$opt$aic,fitBM$AIC,fitBMS$AIC,fitOUM$AIC),
              c("WN","BM","BMS","OUM"))
aic

### peso ralativo dos modelos
aic.w(aic)

# PARA PENSAR: 
# Qual modelo teve o melhor ajuste aos dados? 
# Que processo evolutivo esse modelo representa? 
# O que indicam as estimativas dos parâmetros desse modelo?

################################### TAREFA ####################################
# Descubra o melhor modelo e os parâmetros para as demais características. 
# Existe similaridade de modelos e parâmetros entre as características? 
