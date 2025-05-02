# Nesta prática vamos simular a evolução de uma característica hipotéticas (Y) 
# em duas linhagens-irmãs. A evolução ocorrerá sob diferentes modelos.
# Após as simulações, vamos quantificar a similaridade da caracteríticas entre 
# as duas linhagens-irmãs e avaliar se essa similaridade foi afetada pelos 
# modelos evolutivos que geraram as características.

############################## CENÁRIO TEMPORAL GERAL ##########################

# tempo final (em milhões de anos)
tf <- 1 
# tempo entre geração (em milhões de anos)
dt <- 0.0001  
# vetor com o tempo de todas as gerações
ts <- seq(0, tf, by = dt)  

################################# SIMULANDO BM #################################

## Parâmetros do modelo BM
# Taxa de variação
sigma_sq1 <- 0.05

## vetores para armazenar os valores das características
Ybm1 <- c() 
Ybm2 <- c() 
## valores iniciais das características
Ybm1[1] <- Ybm2[1] <- 1 

## Simulação 
for (i in 2:length(ts)) {
  dS1 <-  rnorm(1, mean = 0, sd = sqrt(sigma_sq1) )  # variação estocástica 
  dS2 <-  rnorm(1, mean = 0, sd = sqrt(sigma_sq1) )  # variação estocástica 
  Ybm1[i] <- Ybm1[i-1] + dS1
  Ybm2[i] <- Ybm2[i-1] + dS2
}

## Plot da simulação
plot(x = ts, 
     y = Ybm1, 
     type = "l", 
     col = "blue", 
     
     xlab = "Tempo (milhões de anos)", 
     ylab = "Característica",
     main = paste0("BM ",
                   "; sigma_sq: ", sigma_sq1
                   
     )
)
lines(x = ts, 
      y = Ybm2, 
      type = "l", 
      col = "blue")

#  Calcule a similaridades entre as linhagens-irmãs ao final da simulação:
dissimBM = abs(Ybm1[length(ts)] - Ybm2[length(ts)]) # desvio absoluto
simBM = 1/(1+dissimBM)

#  O valor de similaridade sob BM: 
simBM

########################## SIMULANDO EVOLUÇÃO DIRECIONAL #########################

## Parâmetros do modelo direcional
# Taxa de variação
sigma_sq2 <- 0.05
# Tendência evolutiva
mu = 0.01

## vetores para armazenar os valores das características
Yde1 <- c() 
Yde2 <- c() 
## valores iniciais das características
Yde1[1] <- Yde2[1] <- 1 

## Simulação 
for (i in 2:length(ts)) {
  dS1 <-  rnorm(1, mean = mu, sd = sqrt(sigma_sq2) )  # variação estocástica 
  dS2 <-  rnorm(1, mean = mu, sd = sqrt(sigma_sq2) )  # variação estocástica 
  Yde1[i] <- Yde1[i-1] + dS1
  Yde2[i] <- Yde2[i-1] + dS2
}

## Plot da simulação
plot(x = ts, 
     y = Yde1, 
     type = "l", 
     col = "red", 
     
     xlab = "Tempo (milhões de anos)", 
     ylab = "Característica",
     main = paste0("Direcional ",
                   " sigma_sq: ", sigma_sq2,
                   " mu: ", mu
     )
)
lines(x = ts, 
      y = Yde2, 
      type = "l", 
      col = "red")

#  Calcule a similaridades entre as linhagens-irmãs ao final da simulação:
dissimDE = abs(Yde1[length(ts)] - Yde2[length(ts)]) # desvio absoluto
simDE = 1/(1+dissimDE)
# O valor de similaridades sob DE: 
simDE

############################### COMPARANDO MODELOS ############################

## PARA PENSAR:
# Qual dos dois modelos gerou maior similaridade entre as linhagens-irmãs?
c("BM" = simBM, "DE" = simDE)

## EM GRUPO:
# Agora vamos testar se os modelos geraram similaridades de magnitude distintas: 

## similaridades calculadas pela turma
sim_bm = c() ## similaridades sob BM
sim_de = c() ## similaridades sob DE
sim = c(sim_bm, sim_de)

## vetor para diferenciar resultados de cada modelo
model = c(rep("BM", length(sim_bm)), rep("DE", length(sim_de)))

## organizar em tabela
table = as.data.frame(cbind(sim,modelo) )

## testar diferença ente modelos com ANOVA
summary(aov(sim ~ model, data = table))

## PARA PENSAR:
# Segundo a ANOVA, os modelos produzem similaridades de magnitude distintas?