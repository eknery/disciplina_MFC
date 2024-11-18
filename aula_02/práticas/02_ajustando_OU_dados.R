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

### ajustando BM
fitBM_gs<-fitContinuous(phy = bacteria.tree,
                        dat = genome_size,
                        model="BM")
## verificar resultados
fitBM_gs

### ajustando OU
fitOU_gs<-fitContinuous(phy = bacteria.tree,
                        dat = genome_size,
                        model="OU")
## ARENTÇÃO PARA A MENSAGEM DE AVISO !
## verificar resultados
fitOU_gs

### ajustando OU com margens maiores para alfa
fitOU_gs<-fitContinuous(bacteria.tree,genome_size,
                        model="OU",
                        bounds=list(alpha=c(0,10))
                        )
## verificar resultados
fitOU_gs

### comparando ajuste de modelos 
aic_gs<-setNames(c(AIC(fitBM_gs),AIC(fitOU_gs)),c("BM","OU"))
aic_gs

### 'peso' relativo dos modelos
aic.w(aic_gs)

### PARA PENSAR:
### Qual modelo teve o melhor ajuste ao tamanho do genoma? O que esse modelo
### indica sobre como essa caractarística evoluíu?

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

### ajustando OU
fitBM_ar<-fitContinuous(phy = bacteria.tree,
                        dat = ln_mutation,
                        model="BM"
                        )

### ajustando OU
fitOU_ar<-fitContinuous(phy = bacteria.tree,
                        dat = ln_mutation,
                        model="OU",
                        bounds=list(alpha=c(0,100))
                        )

### comparando ajuste de modelos
aic_ar<-setNames(c(AIC(fitBM_ar),AIC(fitOU_ar)),c("BM","OU"))
aic_ar

### 'peso' relativo dos modelos
aic.w(aic_ar)

### PARA PENSAR:
### Qual modelo teve o melhor ajuste ao tamanho do genoma? O que esse modelo
### indica sobre como essa caractarística evoluíu?

### PARA PENSAR:
### Comparando as estimativas de alfa para o tamanho do genoma e para o acúmulo de mutações,
### o que pode ser inferido sobre a evolução dessas caracterísitcas?
