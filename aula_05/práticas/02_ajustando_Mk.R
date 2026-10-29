# Nesta prática vamos investigar a evolução do número de dígitos nas patas 
# dos lagartos (Squamata).Mais especificamente queremos saber se a ausência
# de dígitos evoluiu de forma gradativa (perdas consecutivas de dígitos) ou 
# de forma abrupta (perda total de todos os dígitos)

######################## CARREGANDO DADOS E BIBLIOTECAS ########################

### bibliotecas
if (!require("phytools")) install.packages("phytools"); library("phytools")
if (!require("geiger")) install.packages("geiger"); library("geiger")
if (!require("corHMM")) install.packages("corHMM");library("corHMM")

### carregando árvore filogenética
phy = read.tree("dados/arecaceae_phy.tre")
phy

### carregando dados fenotípicos
data = read.csv("dados/arecaceae_data.csv",  h= T,  row.names=1)
head(data)

############################# PROCESSANDO DADOS ###############################

### verificando correspondência entre dados e árvore
chk = name.check(phy,data)
summary(chk)

### retirando terminais ausentes nos dados fenotípicos
phy_fix = drop.tip(phy,chk$tree_not_data)

### vetor com o estado mais provável
trait = colnames(data)[max.col(data)]
### nomeando vetor
names(trait) = row.names(data)
### verificando vetor de estados
trait

### vector with name of states
state_names = colnames(data)
### numbers of states
state_n = length(state_names)
  
############################# VISUALIZANDO DADOS ##############################

### cores para cada estado
col_map =c(
  "armless" = "yellow",
  "spiny" = "darkblue"
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

####################### AJUSTANDO MATRIZES DE TRANSIÇÂO ########################

### matriz com taxa única (equal rates, ER)
er.mtx = matrix(c(
  0,1,
  1,0
),state_n,state_n,byrow=TRUE,
dimnames = list(state_names,state_names))
### ajustando matriz com taxa única
fitER = fitDiscrete(
  phy = phy_fix,
  dat = trait,
  model = er.mtx
  )
### visualizando matriz
plot(fitER, show.zeros=FALSE)

### matriz com taxa diferentes (all rates different, ARD)
ard.mtx = matrix(c(
  0,1,
  2,0
),state_n,state_n,byrow=TRUE,
dimnames = list(state_names,state_names))
### ajustando matriz com todas as taxas diferentes
fitARD = fitDiscrete(
  phy = phy_fix,
  dat = trait,
  model = ard.mtx
  )
### visualizando matriz
plot(fitARD, show.zeros=FALSE)

### matriz com 'ganhos' apenas
gain.mtx = matrix(c(
  0,1,
  0,0
  ),state_n,state_n,byrow=TRUE,
  dimnames = list(state_names,state_names))
### ajustando matriz de 'ganhos' apenas
fitGain = fitDiscrete(
  phy = phy_fix,
  dat = trait,
  model = gain.mtx
)
### visualizando matriz
plot(fitGain, show.zeros=FALSE)

### matriz com 'perdas' apenas
loss.mtx = matrix(c(
  0,0,
  1,0
),state_n,state_n,byrow=TRUE,
dimnames = list(state_names,state_names))
### ajustando matriz de 'ganhos' apenas
fitLoss = fitDiscrete(
  phy = phy_fix,
  dat = trait,
  model = loss.mtx
)
### visualizando matriz
plot(fitLoss, show.zeros=FALSE)

### extraíndo valores de AIC
aic = setNames(
  c(fitER$opt$aic,fitARD$opt$aic,fitGain$opt$aic, fitLoss$opt$aic),
  c("ER","ARD","just Gain","just Loss")
)
### verificar valores de AIC
aic

# PARA PENSAR: 
# Qual modelo teve o melhor ajuste aos dados? 
# Que processo evolutivo esse modelo representa? 
# O que indicam as estimativas dos parâmetros desse modelo?

############### RECONSTRUÇÃO CORHMM - APENAS ESTADOS OBSERVADOS ################

### função para extrair estado mais provável
get_state = function(prob_data) {
  if (is.null(colnames(prob_data))) {
    stop("Input data must have column names representing the states.")
  }
  apply(prob_data, 1, function(row) {
    max_prob = max(row, na.rm = TRUE)
    best_states = names(row)[which(row == max_prob)]
    paste(best_states, collapse = "&")
  })
}

### tabela de entrada no CorHMM
corhmm_table = data.frame( rownames(data), get_state(data) )
names(corhmm_table) = c("species", "state")
### verificar tabela
corhmm_table

### reconstrução global
rec.joint = corHMM(
  phy = phy_fix,
  data = corhmm_table,
  node.states = "joint",
  rate.cat = 1,
  rate.mat = er.mtx
)
### verificar recosntrução global
rec.joint

### reconstrução marginal
rec.marg = corHMM(
  phy = phy_fix,
  data = corhmm_table,
  node.states = "marginal",
  rate.cat = 1,
  rate.mat = er.mtx
)
### verificar recosntrução global
rec.marg

### dividir visualização
par(mfrow=c(1,2))
### plotando recosntrução global
plotTree(
  tree = phy_fix,
  fsize = 0.4,
  mar = c(2, 1, 1, 1)
  )
tiplabels(
  pie = data,
  piecol = col_map,
  cex = 0.4
  )
nodelabels(
  pie = to.matrix(state_names[rec.joint$phy$node.label], state_names ),
  piecol = col_map,
  cex=0.6
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
title("Reconstrução global")
### plotando reconstrução marginal
plotTree(
  tree = phy_fix,
  fsize = 0.4,
  mar = c(2, 1, 1, 1)
)
tiplabels(
  pie = data,
  piecol = col_map,
  cex = 0.4
)
nodelabels(
  pie = as.data.frame(rec.marg$states, col.names = state_names),
  piecol = col_map,
  cex=0.6
)
title("Reconstrução marginal")
### retornar visualização única
par(mfrow=c(1,1))

# PARA PENSAR: 
# Qual o estado do último ancestral comum nas duas reconstruções? 
# Quantas vezes a defesa por espinhos evoluiu em cada reconstrução?

################### RECONSTRUÇÃO CORHMM - COM ESTADOS OCULTOS ##################

### matriz com estados ocultos
hidd.mtx = matrix(c(
  0,0,1,0,
  0,0,0,1,
  1,0,0,1,
  0,1,1,0
),4,4,byrow=TRUE)

### reconstrução marginal
rec.marg2 = corHMM(
  phy = phy_fix,
  data = corhmm_table,
  node.states = "marginal",
  rate.cat = 2,
  rate.mat = hidd.mtx
)
### verificar recosntrução global
rec.marg2

### extraíndo valores de AIC
aic = setNames(
  c(rec.marg$AIC, rec.marg2$AIC),
  c("ER obs","ER hid")
)
### verificar valores de AIC
aic

# PARA PENSAR: 
# Qual modelo teve o melhor ajuste aos dados? 
# Que processo evolutivo esse modelo representa? 

### dividir visualização
par(mfrow=c(1,2))
### plotando reconstrução marginal
plotTree(
  tree = phy_fix,
  fsize = 0.4,
  mar = c(2, 1, 1, 1)
)
tiplabels(
  pie = data,
  piecol = col_map,
  cex = 0.4
)
nodelabels(
  pie = as.data.frame(rec.marg$states, col.names = state_names),
  piecol = col_map,
  cex=0.6
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
title("Sem estados ocultos")
### plotando reconstrução com estados ocultos
plotTree(
  tree = phy_fix,
  fsize = 0.4,
  mar = c(2, 1, 1, 1)
)
tiplabels(
  pie = data,
  piecol = col_map,
  cex = 0.4
)
nodelabels(
  pie = as.data.frame(rec.marg2$states[,c(1,2)], col.names = state_names),
  piecol = col_map,
  cex=0.6
)
title("Com estados ocultos")
### retornar visualização única
par(mfrow=c(1,1))

# PARA PENSAR: 
# Qual o estado do último ancestral comum nas duas reconstruções? 
# Quantas vezes a defesa por espinhos evoluiu em cada reconstrução?
