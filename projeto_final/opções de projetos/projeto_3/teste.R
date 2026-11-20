### bibliotecas
if (!require("phytools")) install.packages("phytools"); library("phytools")
if (!require("geiger")) install.packages("geiger"); library("geiger")
if (!require("OUwie")) install.packages("OUwie"); library("OUwie")

### carregando árvore filogenética
phy = readRDS("dados/anolis_phy.RDS")
### verificando árvore filogenética
phy

### carregando dados fenotípicos
data = read.csv("dados/anolis_data.csv", row.names=1)
### verificando dados
head(data)
