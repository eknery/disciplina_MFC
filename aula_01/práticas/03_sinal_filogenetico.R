
### bibliotecas
if (!require("phytools")) install.packages("phytools"); library("phytools")
if (!require("geiger")) install.packages("geiger"); library("geiger")

### carregando dados fenotípicos
miconia.data<-read.csv("dados/miconia.csv", row.names=1, h= T)
head(miconia.data)

### carregando filogenia
miconia.tree<-read.tree("dados/miconia.nwk")
print(miconia.tree,printlen=2)

############################## VISUALIZANDO DADOS ###############################

### valores de interesse em um vetor nomeado
plant.size<- miconia.data[,"plant.size"]
names(plant.size)<-rownames(miconia.data)
plant.size

### verificando a distribuição dos valores
hist(plant.size)

### verificando correspondência entre dados e filogenia
name.check(miconia.tree, miconia.data)

### gráfico da filogenia
plotTree.barplot(tree = miconia.tree,
                 x = plant.size,
                 args.plotTree=list(fsize=0.7)
)

# PARA PENSAR:
#   Existe algum padrão de similaridade de tamanho entre linhagens próximas?

################################ LAMBDA DE PAGEL ###############################

### testando sinal filogenético por lambda de Pagel
lambda_gs = phylosig(tree = miconia.tree,
                     x = plant.size,
                     method="lambda",
                     test = TRUE
                     )

### verificando resultados
lambda_gs
plot(lambda_gs,las=1,cex.axis=0.9)

# PARA PENSAR:
#   O valor de lambda foi alto ou baixo? O que isso indica? 
#   O valor de P foi significativo? O que isso indica?

### testando sinal filogenético por K de Bloomberg
K_gs<-phylosig(tree = miconia.tree,
               x = plant.size,
               method= "K",
               test = TRUE,
               nsim = 10000)

### verificando resultados
K_gs
plot(K_gs,las=1,cex.axis=0.9)

# PARA PENSAR:
#   O valor de K foi alto ou baixo? O que isso indica? 
#   O valor de P foi significativo? O que isso indica?


