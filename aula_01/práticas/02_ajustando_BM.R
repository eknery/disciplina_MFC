### http://www.phytools.org/Rbook/ 

### bibliotecas
if (!require("phytools")) install.packages("phytools"); library("phytools")
if (!require("geiger")) install.packages("geiger"); library("geiger")

### carregando dados fenotípicos
bacteria.data<-read.csv("dados/bac_rates.csv", row.names=1)
head(bacteria.data,3)

### carregando filogenia
bacteria.tree<-read.tree("dados/bac_rates.phy")
print(bacteria.tree,printlen=2)

############################## VISUALIZANDO DADOS ###############################

### valores de interesse em um vetor nomeado
genome_size<-bacteria.data[,"Genome_Size_Mb"]
names(genome_size)<-rownames(bacteria.data)
genome_size

### verificando a distribuição dos valores
hist(genome_size)

### verificando correspondência entre dados e filogenia
name.check(bacteria.tree,bacteria.data)

### gráfico da filogenia
plotTree.barplot(tree = bacteria.tree,
              x = genome_size,
              args.plotTree=list(fsize=0.7)
)
axis(1,at=seq(0,1,length.out=5),cex.axis=0.8)

############################### AJUSTANDO MODELOS ##############################

### ajustando o modelo White Noise
fitWN <-fitContinuous(phy = bacteria.tree,
                        dat = genome_size,
                        model ="white"
                        )

## verificar resultados!
fitWN

### ajustando o modelo BM
fitBM <-fitContinuous(phy = bacteria.tree,
                      dat = genome_size,
                      model ="BM"
)

## verificar resultados!
fitBM

### ajustando o modelo Direcional
fitEB <-fitContinuous(phy = bacteria.tree,
                      dat = genome_size,
                      model ="EB"
)

## verificar resultados!
fitEB

