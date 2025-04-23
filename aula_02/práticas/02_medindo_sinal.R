# Nessa prática vamos investigar o sinal filogenético do tamanho das folhas e
# das inflorescências dentro de um clado de Miconia. Nossa expectativa é que 
# o tamanho da inflorescência tenha sido mais conservado na evolução
# devido ao seu papel na polinização, que restringiria grandes mudanças. 
# Nós vamos considerar duas medidas de sinal filogenético: lambda e K.

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
trait<- miconia.data[,"leaf.size"]
names(trait)<-rownames(miconia.data)
trait

### verificando a distribuição dos valores
hist(trait)

### verificando correspondência entre dados e filogenia
name.check(miconia.tree, miconia.data)

### gráfico da filogenia
plotTree.barplot(tree = miconia.tree,
                 x = trait,
                 args.plotTree =list(fsize=0.4)
)

# PARA PENSAR:
# Existe algum padrão de similaridade de tamanho entre linhagens próximas?

################################ MEDINDO SINAL ###############################

### testando sinal filogenético por lambda de Pagel
lambda_gs = phylosig(tree = miconia.tree,
                     x = trait,
                     method="lambda",
                     test = TRUE
                     )

### verificando resultados
lambda_gs
plot(lambda_gs,las=1,cex.axis=0.9)

# PARA PENSAR:
# O valor de lambda foi alto ou baixo? O que isso indica? 
# O valor de P foi significativo? O que isso indica?

### testando sinal filogenético por K de Bloomberg
K_gs<-phylosig(tree = miconia.tree,
               x = trait,
               method= "K",
               test = TRUE,
               nsim = 10000)

### verificando resultados
K_gs
plot(K_gs,las=1,cex.axis=0.9)

## PARA PENSAR:
# O valor de K foi alto ou baixo? O que isso indica? 
# O valor de P foi significativo? O que isso indica?


## EM GRUPO:
# Execute o script para a folha e para a inflorescência, anotando os valores de
# lambda e K para cada uma das características. Os valores de sinal filogenético
# parecem ter alguma relação com os modelos de melhor ajuste dessas características?

