### http://www.phytools.org/Rbook/ 

### bibliotecas
library(phytools)
library(geiger)

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

#### TAMANHO DO GENOMA

### valores de interesse em um vetor nomeado
genome_size<-bacteria.data[,"Genome_Size_Mb"]
names(genome_size)<-rownames(bacteria.data)
genome_size

### verificando a distribuição dos valores
hist(genome_size)

### ajustando o modelo BM aos valores
fitBM_gs<-fitContinuous(bacteria.tree,genome_size)
fitBM_gs

#### ACÚMULO DE MUTAÇÕES

### valores de interesse em um vetor nomeado
mutation<-bacteria.data[,"Accumulation_Rate"]
names(mutation)<-rownames(bacteria.data)
mutation

### verificando a distribuição dos valores
hist(mutation)

### transformação em log
ln_mutation <- log(mutation)

### verificando a distribuição dos valores
hist(ln_mutation)

### ajustando o modelo BM aos valores
fitBM_ar<-fitContinuous(bacteria.tree,ln_mutation)
fitBM_ar
