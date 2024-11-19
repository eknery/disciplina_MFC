# In Anolis lizards, lamellae are expanded scales on their digits that
# they use to enhance clinging on smooth surfaces (Glossip and Losos 1997; Losos 2009). Lots
# of previous work suggested that more arboreal lizards will tend to evolve shorter limbs and
# more toepad lamellae (Losos 2009),

# Our resultmakes a lot of sense becausewe hypothesize that lizards using differentmicrohabitats
# should be subject to different regimes of natural selection and that this natural selection
# should likewise cause ecologically similar species in different parts of the phylogeny to have
# more similar phenotypes than expected under Brownian motion (Losos 2009).

### http://www.phytools.org/Rbook/

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

### plotando a árvore mapeada
cols<-setNames(rainbow(n=6),levels(anole.ecomorph[,1]))
plot(ecomorph.tree,
     cols,
     lwd=2,
     ftype="i",
     fsize=0.4,
     ylim=c(-4,82),
     outline=TRUE)
add.simmap.legend(colors=cols,
                  prompt=FALSE,
                  x=0,
                  y=-2,
                  vertical=FALSE,
                  fsize=0.9)

############################# TRATANDO OS DADOS ################################

### verificando correspondÊncia entre dados e árvore
chk<-name.check(ecomorph.tree,anole.morphology)
summary(chk)

### retirando dados das espécies ausentes na árvore
ecomorph.data<-anole.morphology[-which(rownames(anole.morphology)%in%chk$data_not_tree),]

### verificando correspondência entre dados e árvore
chk<-name.check(ecomorph.tree,ecomorph.data)
chk

############################# PREPARANDO OS DADOS ##############################

### PCA filogenética = relação entre medições, consideração correlação filogenética
pca<-phyl.pca(ecomorph.tree,ecomorph.data)
print(pca)

## IMPORTANTE:
# o eixo PC3 está positivamente relacionado com o comprimento dos membros 
# anteriores (forelimb, FLL) e com o comprimento dos membros posteriores 
# (hindlimb, HLL), mas negativamente ralacionado com o número de lamelas
# (lamellae number, LAM).

### dados de hábito e dos membros dos lagartos
ouwie.data<-data.frame(Genus_species=rownames(scores(pca)),
                       Reg=anole.ecomorph[rownames(scores(pca)),],
                       X=as.numeric(scores(pca)[,3]))
head(ouwie.data,n=10)

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
# Qual modelo teve o melhor ajuste aos dados? Que modo de evolução esse mdoelo
# representa? O que indicam os parâmetros desse modelo?

##################################### GRÁFICO ##################################

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
# Linhagens ocupando o mesmo habitat são mais similares ou mais diferentes com
# relação aos valores de PC3? 
# Os valores de PC3 são similares ou diferentes entre os tipos de habitat?
