# Nessa prática vamos investigar uma hipótese de evolução convergente entre as patas
# dos lagartos do gênero Anolis. Mais especificamente, queremos saber se lagartos
# de hábito arbóreo evoluíram patas com fenótipo similar. Para isso,
# vamos focar no comprimento das patas e no seu número de lamelas nos dígitos
# (escama expandidas que aumentam o atrito com superfície).  

######################### CARREGANDO DADOS E BIBLIOTECAS #######################

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
ecomorph.tree<-read.simmap(file="dados/anolis_mapped.nexus",
                           format="nexus",
                           version=1.5)
ecomorph.tree

############################# TRATANDO OS DADOS ################################

### verificando correspondência entre dados e árvore
chk<-name.check(ecomorph.tree,anole.morphology)
summary(chk)

### retirando dados das espécies ausentes na árvore
ecomorph.data<-anole.morphology[-which(rownames(anole.morphology)%in%chk$data_not_tree),]

### verificando correspondência entre dados e árvore
chk<-name.check(ecomorph.tree,ecomorph.data)
chk

############################# ORDENAÇÃO DOS DADOS ##############################

### PCA filogenética = relação entre medições, consideração correlação filogenética
pca<-phyl.pca(ecomorph.tree,ecomorph.data)

### verificar relação entre as variáveis
print(pca)
## IMPORTANTE:
# o eixo PC3 está positivamente relacionado com o comprimento dos membros 
# anteriores (forelimb, FLL) e com o comprimento dos membros posteriores 
# (hindlimb, HLL), mas negativamente ralacionado com o número de lamelas
# (lamellae number, LAM).Essa variável então representerá o fenótipo das patas.

### organizando dados de interesse
ouwie.data<-data.frame(Genus_species=rownames(scores(pca)),
                       Reg=anole.ecomorph[rownames(scores(pca)),],
                       X=as.numeric(scores(pca)[,3]))
ouwie.data

############################### VISULIZANDO DADOS ##############################

cols = c(
  
)

### estados das espécies atuais
tips<-getStates(ecomorph.tree,"tips")
## cores para as espécies atuais
tip.cols<-cols[tips]

### plot da árvores mais as características 
plotTree.barplot(tree = ecomorph.tree,
                 x = scores(pca)[,3],
                 args.plotTree=list(fsize=0.4),
                 args.barplot=list(col=tip.cols,
                                   xlab=expression(paste("PC3")),
                                   cex.lab=0.8))
## adicionar legenda
legend("topright",levels(anole.ecomorph[,1]),
       pch=22,pt.bg=cols,pt.cex=1.5,cex=0.9)

# PARA PENSAR: 
# Como os valores de PC3 estão distribuídos entre as linhagens?
# Os valores tendem a serem iguais entre linhagens próximas? O tipo de hábito
# parece ter alguma relação com os valores de PC3?

############################### AJUSTANDO MODELOS #############################

### ajustando BM com uma única taxa de variação
fitBM<-OUwie(phy = ecomorph.tree,
             data = ouwie.data,
             model ="BM1",
             simmap.tree = TRUE
             )
## verificar resultados
fitBM

### ajustando BM com múltiplas taxas de variação
fitBMS<-OUwie(phy = ecomorph.tree,
              data = ouwie.data,
              model ="BMS",
              simmap.tree = TRUE
              )
## verificar resultados
fitBMS

### ajustando OU com múltiplos ótimos
fitOUM<-OUwie(phy = ecomorph.tree,
              data = ouwie.data,
              model ="OUM",
              simmap.tree = TRUE
              )
## verificar resultados
fitOUM

############################## COMPARANDO MODELOS ##############################

### extraindo valores de AIC 
aic<-setNames(c(fitBM$AIC,fitBMS$AIC,fitOUM$AIC),
              c("BM1","BMS","OUM"))
aic

### peso ralativo dos modelos
aic.w(aic)

# PARA PENSAR: 
#  Qual modelo teve o melhor ajuste aos dados? Que modo de evolução esse modelo
#  representa? O que indicam os parâmetros desse modelo?

