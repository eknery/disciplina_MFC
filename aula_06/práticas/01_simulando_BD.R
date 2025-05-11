


## carregando pacote
if (!require("phytools")) install.packages("phytools"); library("phytools")

######################  SIMULANDO CENÁRIOS DE DIVERSIFICAÇÃO ###################

## tempo total das simulações (em milhões de anos)
tf = 20

## diversificação Pure Birth
evo_pure <- pbtree(b= 0.10,
                   t= tf,
                   method= "direct"
                   )

## diversificação Birth-Death
evo_bd <- pbtree(b= 0.15,
                 d= 0.05,
                 t= tf,
                 method= "direct"
                 )

## visualizando história evolutiva real
par(mfrow = c(1,2))
plot(ladderize(evo_pure), cex = 0.2, main = "Pure Birth")
plot(ladderize(evo_bd), cex = 0.2, main = "Birth-Death")

####################### OBTENDO RECONSTRUÇÕES FILOGENÉTICAS #############

## amostrando todas as linhagens vivas
phylo_pure<-drop.tip(evo_pure,
                     getExtinct(evo_pure))

## amostrando todas as linhagens vivas
phylo_bd<-drop.tip(evo_bd,
                  getExtinct(evo_bd))

## visualizando reconstruções filogenéticas
par(mfrow = c(1,2))
plot(ladderize(phylo_pure), cex = 0.2, main = "Pure Birth")
plot(ladderize(phylo_bd), cex = 0.2, main = "Birth-Death")

####################### COMPARANDO NÚMERO DE ESPÉCIES ##########################

# Extraindo número de espécies viventes para cada cenário
N_pure = Ntip(phylo_pure)
N_bd = Ntip(phylo_bd)

# Comparando o número de espécies geradas pelos cenários de diversificação:
c("Pure Birth" = N_pure, "Birth-Death"= N_bd)

## EM GRUPO:
# Vamos armazenar o número de espécies geradas por cada cenário:
samp_pure = c()
samp_bd = c()

# Vamos testar se existe diferença no número de espécies entre cenários:
t.test(x = samp_pure, 
       y = samp_bd, 
       alternative = "two.sided",
       paired = FALSE, 
       var.equal = FALSE
)

## PARA PENSAR:
# Os cenários de diversificação produziram um número de espécies distinto?
# Qual o motivo para esse padrão?

####################### NÚMERO DE LINHAGENS NO TEMPO #########################

# calculando LTT empírico
ltt_pure<-ltt(phylo_pure, plot=FALSE)
ltt_bd<-ltt(phylo_bd, plot=FALSE)

# estimando LTT teórico
expected_pure = log(Ntip(phylo_pure)) /tf
expected_bd = log(Ntip(phylo_bd)) /tf

# visualizando LTT
par(mfrow = c(1,2))
plot(ltt_pure,
     log.lineages=T,
     lty="dotted",
     lwd=2,
     col="blue",
     cex.axis=0.8,
     main = "Pure Birth"
     )
abline(a = 0, 
       b = expected_pure,
       col = "blue",
       lwd = 1.5)
plot(ltt_bd,
     log.lineages=T,
     lty="dotted",
     lwd=2,
     col="red",
     main = "Birth-Death"
     )
abline(a = 0, 
       b = expected_bd,
       col = "red",
       lwd = 1.5)

# Verificar probabilidade do teste Gamma
c("Pure Birth" = ltt_pure$p,"Birth-Death"= ltt_bd$p)

## PARA PENSAR
# O número de linhagens acumuladas difere entre os cenários de diversificação?
# O que o teste gamma aponta para cada um dos cenários de diversificação?
