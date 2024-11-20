
### bibliotecas
if (!require("phytools")) install.packages("phytools"); library("phytools")
if (!require("geiger")) install.packages("geiger"); library("geiger")

### carregando dados fenotípicos
bacteria.data<-read.csv("dados/bac_rates.csv", row.names=1)
head(bacteria.data,3)

### carregando filogenia
bacteria.tree<-read.tree("dados/bac_rates.phy")
print(bacteria.tree,printlen=2)

### gráfico da filogenia
plotTree(bacteria.tree,
         ftype="i",
         fsize=0.5,
         lwd=1,
         mar=c(2.1,2.1,0.1,1.1)
)
axis(1,at=seq(0,1,length.out=5),cex.axis=0.8)

### verificando correspondÊncia entre dados e filogenia
name.check(bacteria.tree,bacteria.data)

################################ PROCESSANDO DADOS #############################

### valores de interesse em um vetor nomeado
genome_size<-bacteria.data[,"Genome_Size_Mb"]
names(genome_size)<-rownames(bacteria.data)
genome_size

#### ACÚMULO DE MUTAÇÕES
### valores de interesse em um vetor nomeado
ln_mutation<-log(bacteria.data[,"Accumulation_Rate"])
names(ln_mutation)<-rownames(bacteria.data)
ln_mutation

################################ LAMBDA DE PAGEL ###############################

### testando sinal filogenético
lambda_gs = phylosig(tree = bacteria.tree,
         x = genome_size,
         method="lambda",
         test = TRUE)

### verificando resultados
lambda_gs
plot(lambda_gs,las=1,cex.axis=0.9)

### testando sinal filogenético
lambda_ar = phylosig(tree = bacteria.tree,
         x = ln_mutation,
         method="lambda",
         test = TRUE)
### verificando resultados
lambda_ar
plot(lambda_ar,las=1,cex.axis=0.9)

################################ K DE BLOOMBERG ################################

### testando sinal filogenético
K_gs<-phylosig(tree = bacteria.tree,
               x = genome_size,
               method= "K",
               test = TRUE,
               nsim = 10000)

### verificando resultados
K_gs
plot(K_gs,las=1,cex.axis=0.9)


### testando sinal filogenético
K_ar<-phylosig(tree = bacteria.tree,
               x = ln_mutation,
               method= "K",
               test = TRUE,
               nsim = 10000)

### verificando resultados
K_ar
plot(K_ar,las=1,cex.axis=0.9)

