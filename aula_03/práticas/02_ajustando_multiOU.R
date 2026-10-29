# Nesta prática vamos investigar se o hábito de vida influenciou a evolução 
# da capacidade de escalada das espécies de lagartos Anolis. Nós hipotetizamos 
# que espécies habitando a copa das árvores sofreram uma seleção favorecendo
# uma maior capacidade de escalada, enquanto que espécies habitando o chão 
# não sofreram esse mesmo tipo de seleção. Assim, esperamos que espécies 
# habitando a copa das árvores tenham um ótimo adaptativo de membros maiores e 
# patas com maior atrito, enquanto que espécies habitando o chão tenham 
# um ótimo adaptativo de membros menores e patas com menor atrito.

######################### CARREGANDO BIBLIOTECAS E DADOS #######################

### bibliotecas
if (!require("phytools")) install.packages("phytools"); library("phytools")
if (!require("geiger")) install.packages("geiger"); library("geiger")
if (!require("OUwie")) install.packages("OUwie"); library("OUwie")

### carregando árvore filogenética 'mapeada'
phy = readRDS("dados/anolis_phy.RDS")
### verificando árvore filogenética
phy

### carregando dados fenotípicos
data = read.csv("dados/anolis_data.csv", row.names=1)
### verificando dados
head(data)

### carregando dados de hábito 
regime = read.csv("dados/anolis_habitat.csv", row.names=1,  stringsAsFactors=TRUE)
### verificando dados de hábito
head(regime)

############################# VERIFICANDO OS DADOS ###############################

### verificando correspondência entre dados e árvore
chk = name.check(phy,data)
summary(chk)

### retirando dados das espécies ausentes na árvore
data_fix = data[-which(rownames(data) %in% chk$data_not_tree),]

### verificando correspondência entre dados e árvore
chk = name.check(phy,data_fix)
chk

############################# PROCESSANDO OS DADOS #############################

### selecionando característica de interesse
trait = data_fix[,"FLL"]
### nomeando vetor da característica
names(trait) = rownames(data_fix)
### verificando vetor das características 
trait

## IMPORTANTE:
# Para testar nossa hipótese, vamas investigar três características:
# o comprimento dos membros anteriores (forelimb, FLL) 
# o comprimento dos membros posteriores (hindlimb, HLL)
# o número de lamelas (lamellae number, LAM).
# Essa variáveis representam o fenótipo das patas.

### organizando dados na tabela OUwie
ouwie_table= data.frame(
  species = rownames(data_fix),
  regime = regime[rownames(data_fix),],
  trait = trait
 )
### verificando tabela
head(ouwie_table)

############################### VISUALIZANDO DADOS ##############################

### cores para cada hábito
color_map  = c(
  "Crown" = "darkgreen", 
  "Ground" = "orchid",
  "Trunk" = "darkgray" 
)

### dividir visualização
par(mfrow=c(1,2))
### visualizar estados de hábito
plotSimmap(
  tree = phy,
  colors = color_map ,
  fsize= 0.5,
  mar = c(2, 1, 1, 1)
  )
legend(
  "bottomleft",
   levels(regime[,1]),
   pch = 22,
   pt.bg = color_map ,
   pt.cex = 1,
   cex=0.5
  )
axisPhylo()
### visualizando a distribuição dos dados 
plot(
  ouwie_table$regime,
  ouwie_table$trait,
  col = color_map,
)
### retornar visualização única
par(mfrow=c(1,1))

# PARA PENSAR: 
# Qual é o hábito ancestral? Os demais hábitos evoluíram mais de uma vez?
# O tipo de hábito parece ter alguma relação com os valores da característica?

############################### AJUSTANDO MODELOS #############################

### ajustando "Ruído branco"
fitWN = fitContinuous(
  phy = phy,
  dat = trait,
  model ="white"
  )
## verificar resultados
fitWN

### ajustando BM com uma única taxa de variação
fitBM = OUwie(
  phy = phy,
  data = ouwie_table,
  model ="BM1",
  simmap.tree = TRUE
 )
## verificar resultados
fitBM

### ajustando BM com múltiplas taxas de variação
fitBMS = OUwie(
  phy = phy,
  data = ouwie_table,
  model ="BMS",
  simmap.tree = TRUE
  )
## verificar resultados
fitBMS

### ajustando OU com múltiplos ótimos
fitOUM = OUwie(
  phy = phy,
  data = ouwie_table,
  model ="OUM",
  simmap.tree = TRUE
  )
## verificar resultados
fitOUM

############################## COMPARANDO MODELOS ##############################

### extraindo valores de AIC 
aic = setNames(
  c(fitWN$opt$aic,fitBM$AIC,fitBMS$AIC,fitOUM$AIC),
  c("WN","BM","BMS","OUM")
  )
### verificando valores de AIC
aic

# PARA PENSAR: 
# Qual modelo teve o melhor ajuste aos dados? 
# Que processo evolutivo esse modelo representa? 
# O que indicam as estimativas dos parâmetros desse modelo?

################################### TAREFA ####################################

# Descubra o melhor modelo e os parâmetros para as demais características. 
# Existe similaridade do melhor modelo entre as características? 
# Existe similaridade nas estimativas dos parâmetros?
