# Nesta prática vamos investigar a evolução do número de dígitos nas patas 
# dos lagartos (Squamata).Mais especificamente queremos saber se a ausência
# de dígitos evoluiu de forma gradativa (perdas consecutivas de dígitos) ou 
# de forma abrupta (perda total de todos os dígitos)

######################## CARREGANDO DADOS E BIBLIOTECAS ########################

### bibliotecas
if (!require("phytools")) install.packages("phytools"); library("phytools")
if (!require("geiger")) install.packages("geiger"); library("geiger")

### carregando árvore filogenética
phy = read.tree("dados/arecaceae_phy.tre")
phy

### carregando dados fenotípicos
data = read.csv("dados/arecaceae_data.csv",  h= T,  row.names=1)
head(data)

############################# TRATANDO OS DADOS ################################

### verificando correspondência entre dados e árvore
chk = name.check(phy,data)
summary(chk)

### retirando terminais ausentes nos dados fenotípicos
phy_fix = drop.tip(phy,chk$tree_not_data)

### vetor com a característica mais provável
trait = colnames(data)[max.col(data)]
### nomeando vetor
names(trait) = row.names(data)
### verificando vetor de característica
trait

############################# VISUALIZANDO DADOS ##############################

### cores para cada estado
col_map =c(
  "absent" = "yellow",
  "present" = "darkblue"
  )

### plotando árvore com estados
plotTree(
  tree = phy_fix,
  fsize = 0.4,
  mar = c(2, 1, 1, 1)
  )
tiplabels(
  pie = data,
  piecol = col_map,
  cex = 0.2
  )
legend(
  "bottomleft",
   legend = colnames(data),
   pch = 22,
   pt.cex = 1.5,
   pt.bg = col_map,
   bty= "n",
   cex=1
  )
axisPhylo()

# PARA PENSAR:
#   Baseado na figura, o estado de nenhum dígito (0) teria evoluído mais de uma vez?

####################### AJUSTANDO MATRIZES DE TRANSIÇÂO ########################S

### ajustando matriz com uma única taxa
fitER = fitDiscrete(
  phy = phy_fix,
  dat = trait,
  model= "ER"
  )
### visualizando matriz
plot(fitER, show.zeros=FALSE)

### ajustando matriz com todas as taxas diferentes
fitARD=fitDiscrete(
  phy = phy_fix,
  dat = trait,
  model= "ARD"
  )
### visualizando matriz
plot(fitARD, show.zeros=FALSE)

######################## AJUSTANDO MATRIZ CUSTOMIZADA ##########################

### matriz com estados ordenados
ordered.model=matrix(c(
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

## ajustando matriz ordenada
fitOrdered=fitDiscrete(phy = phy_fix,
                        dat = trait,
                        model = ordered.model,
                        surpressWarnings=TRUE)

plot(fitOrdered, show.zeros=FALSE)

############################### COMPARANDO MODELOS #############################

### extraindo valores de AIC
aic=setNames(c(AIC(fitER),AIC(fitSYM),AIC(fitARD), AIC(fitOrdered)),
              c("ER","SYM","ARD", "Ordered"))
aic

### peso relativo dos modelos
aic.w(aic)

# PARA PENSAR: 
#   Qual modelo obeteve maior sustentação? Com base nesse modelo, o que podemos 
#   inferir sobre a evolução dos dígitos nas linhagens de lagartos?