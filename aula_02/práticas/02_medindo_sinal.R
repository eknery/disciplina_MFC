# Nessa prática vamos investigar o sinal filogenético do tamanho das folhas e
# das inflorescências dentro de um clado de Miconia. Nossa expectativa é que 
# o tamanho da inflorescência tenha sido mais conservado na evolução
# devido ao seu papel na polinização, que restringiria grandes mudanças. 
# Nós vamos considerar duas medidas de sinal filogenético: lambda e K.

if (!require("phytools")) install.packages("phytools"); library("phytools")
if (!require("geiger")) install.packages("geiger"); library("geiger")

### carregando dados fenotípicos
data = read.csv("dados/primateEyes.csv",  h= T)
head(data)

### carregando filogenia
tree = read.tree("dados/primate.tre")
print(tree,printlen=2)

############################## PROCESSANDO DADOS ###############################

### valores de interesse em um vetor nomeado
trait = data[,"Skull_length"]
names(trait) = data$Genus_species
trait

### verificando correspondência entre dados e filogenia
name.check(tree, trait)

############################### VISUALIZANDO DADOS #############################

### gráfico da filogenia
plotTree.barplot(
  tree = tree,
  x = trait,
  args.plotTree =list(fsize=0.4)
)

# PARA PENSAR:
# Existe algum padrão de similaridade entre linhagens próximas?

################################ MEDINDO SINAL ###############################

### testando sinal filogenético por lambda de Pagel
lambda = phylosig(
  tree = tree,
  x = trait,
  method="lambda",
  test = TRUE
  )

### verificando resultados
lambda
plot(lambda,las=1,cex.axis=0.9)

# PARA PENSAR:
# O valor de lambda foi alto ou baixo? O que isso indica? 
# O valor de P foi significativo? O que isso indica?

### testando sinal filogenético por K de Bloomberg
kbloom = phylosig(
  tree = tree,
  x = trait,
  method= "K",
  test = TRUE,
  nsim = 10000
  )

### verificando resultados
kbloom
plot(kbloom,las=1,cex.axis=0.9)

## PARA PENSAR:
# O valor de K foi alto ou baixo? O que isso indica? 
# O valor de P foi significativo? O que isso indica?

## EM GRUPO:
# Execute o script para as demais características, anotando os seus respectivos 
# valores de lambda e K. Os valores de sinal filogenético parecem ter alguma relação com os modelos de melhor ajuste dessas características?

