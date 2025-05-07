# Nesta prática vamos investigar se o hábito de vida influenciou a evolução 
# das patas dos lagartos do gênero Anolis. Para isso, vamos inferir o 
# modo e tempo de evolução de diferentes características das patas que foram
# medidas em espécies de com distintos hábitos de vida. Vamos considerar 
# diferentes modelos para cada uma dessas características.

######################### CARREGANDO BIBLIOTECAS E DADOS #######################

### bibliotecas
if (!require("phytools")) install.packages("phytools"); library("phytools")
if (!require("geiger")) install.packages("geiger"); library("geiger")
if (!require("OUwie")) install.packages("OUwie"); library("OUwie")

### carregando dados fenotípicos
anole.morphology<-read.csv("dados/anole.data.csv",
                           row.names=1)

### carregando dados fenotípicos
anole.ecomorph<-read.csv("dados/ecomorph.csv",
                         row.names=1,
                         stringsAsFactors=TRUE)

### carregando árvore filogenética 'mapeada'
anole.tree<-read.simmap(file="dados/anolis_mapped.nexus",
                           format="nexus",
                           version=1.5)
anole.tree

############################# TRATANDO OS DADOS ################################

### verificando correspondência entre dados e árvore
chk<-name.check(anole.tree,anole.morphology)
summary(chk)

### retirando dados das espécies ausentes na árvore
anole.data<-anole.morphology[-which(rownames(anole.morphology)%in%chk$data_not_tree),]

### verificando correspondência entre dados e árvore
chk<-name.check(anole.tree,anole.data)
chk

############################# ORGANIZANDO DOS DADOS ##############################

### selecionando característica de interesse
trait = anole.data[,"FLL"]

## IMPORTANTE:
# Para testar nossa hipótese, mas investigar três características:
# o comprimento dos membros anteriores (forelimb, FLL) 
# o comprimento dos membros posteriores (hindlimb, HLL)
# o número de lamelas (lamellae number, LAM).
# Essa variáveis representa o fenótipo das patas.

### organizando dados na tabela OUwie
ouwie.data<-data.frame(species = rownames(anole.data),
                       regime = anole.ecomorph[rownames(anole.data),],
                       trait = trait
                       )
ouwie.data

### nomeando vetor da característica
names(trait) = rownames(anole.data)

############################### VISUALIZANDO DADOS ##############################

### cores para cada hábito
cols = c(
  "CG" = "darkblue", 
  "GB" = "darkgreen", 
  "TC" = "darkmagenta", 
  "TG" = "darkred", 
  "Tr" = "red", 
  "Tw" = "darkgoldenrod"
)

# CG = Crown-giant
# GB = Grass-bush
# TC = Trunk-crown
# TG = Trunk-ground
# Tr = Trunk
# tw = Twig

### estados das espécies atuais
tips<-getStates(anole.tree,"tips")
## cores para as espécies atuais
tip.cols<-cols[tips]

### visualizar estados de hábito
plotSimmap(tree = anole.tree,
           colors = cols,
           fsize= 0.5,
           )
legend("topright",
       levels(anole.ecomorph[,1]),
       pch=22,
       pt.bg=cols,
       pt.cex=1,
       cex=0.5)


### plot da árvores mais a característica
plotTree.barplot(tree = anole.tree,
                 x = trait,
                 args.plotTree=list(fsize=0.3),
                 args.barplot=list(col=tip.cols,
                                   cex.lab=0.5)
                 )
legend("topright",
       levels(anole.ecomorph[,1]),
       pch=22,
       pt.bg=cols,
       pt.cex=1,
       cex=0.5)

# PARA PENSAR: 
# Como os valores estão distribuídos entre as linhagens?
# Os valores tendem a serem iguais entre linhagens próximas? 
# O tipo de hábito parece ter alguma relação com os valores da característica?

############################### AJUSTANDO MODELOS #############################

### ajustando "Ruído branco"
fitWN <-fitContinuous(phy = anole.tree,
                            dat = trait,
                            model ="white"
                            )
## verificar resultados
fitWN

### ajustando BM com uma única taxa de variação
fitBM<-OUwie(phy = anole.tree,
             data = ouwie.data,
             model ="BM1",
             simmap.tree = TRUE
             )
## verificar resultados
fitBM

### ajustando BM com múltiplas taxas de variação
fitBMS<-OUwie(phy = anole.tree,
              data = ouwie.data,
              model ="BMS",
              simmap.tree = TRUE
              )
## verificar resultados
fitBMS

### ajustando OU com múltiplos ótimos
fitOUM<-OUwie(phy = anole.tree,
              data = ouwie.data,
              model ="OUM",
              simmap.tree = TRUE
              )
## verificar resultados
fitOUM

############################## COMPARANDO MODELOS ##############################

### extraindo valores de AIC 
aic<-setNames(c(fitWN$opt$aic,fitBM$AIC,fitBMS$AIC,fitOUM$AIC),
              c("WN","BM","BMS","OUM"))
aic

### peso ralativo dos modelos
aic.w(aic)

# PARA PENSAR: 
# Qual modelo teve o melhor ajuste aos dados? 
# Que processo evolutivo esse modelo representa? 
# O que indicam as estimativas dos parâmetros desse modelo?

