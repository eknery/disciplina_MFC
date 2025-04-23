
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
sigma_sq <- 0.1

## vetores para armazenar os valores das características
Ybm1 <- c() 
Ybm2 <- c() 
## valores iniciais das características
Ybm1[1] <- Ybm2[1] <- 1 

## Simulação 
for (i in 2:length(ts)) {
  dS1 <-  rnorm(1, mean = 0, sd = sqrt(sigma_sq) )  # variação estocástica 
  dS2 <-  rnorm(1, mean = 0, sd = sqrt(sigma_sq) )  # variação estocástica 
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
                   " sigma_sq: ", sigma_sq
                   
     )
)
lines(x = ts, 
      y = Ybm2, 
      type = "l", 
      col = "blue")

# PARA PENSAR:
#  Calcule a dissimilaridades entre as linhagens-irmãs ao final da simulação:
abs(Ybm1[length(ts)] - Ybm2[length(ts)])  # Diferença modular
#  Guarde o valor de dissimilaridade sob BM: 

########################## SIMULANDO EVOLUÇÃO DIRECIONAL #########################

## Parâmetros do modelo direcional
# Taxa de variação
sigma_sq <- 0.1
# Tendência evolutiva
mu = 0.01

## vetores para armazenar os valores das características
Yde1 <- c() 
Yde2 <- c() 
## valores iniciais das características
Yde1[1] <- Yde2[1] <- 1 

## Simulação 
for (i in 2:length(ts)) {
  dS1 <-  rnorm(1, mean = mu, sd = sqrt(sigma_sq) )  # variação estocástica 
  dS2 <-  rnorm(1, mean = mu, sd = sqrt(sigma_sq) )  # variação estocástica 
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
                   " sigma_sq: ", sigma_sq,
                   " mu: ", mu
     )
)
lines(x = ts, 
      y = Yde2, 
      type = "l", 
      col = "red")

# PARA PENSAR:
#  Calcule a dissimilaridades entre as linhagens-irmãs ao final da simulação:
abs(Yde1[length(ts)] - Yde2[length(ts)])  # Diferença modular
#  Guarde o valor da dissimilaridades sob DE: 

############################### COMPARANDO MODELOS ############################

## Plotar resultado de todas as simulações
plot(x = ts, 
     y = Yde1, 
     type = "l", 
     col = "red", 
     
     xlab = "Tempo (milhões de anos)", 
     ylab = "Característica",
     main = paste0("Direcional: vermelho; ",
                   "BM: azul"
     )
)
lines(x = ts, 
      y = Yde2, 
      type = "l", 
      col = "red")
lines(x = ts, 
      y = Ybm1, 
      type = "l", 
      col = "blue")
lines(x = ts, 
      y = Ybm2, 
      type = "l", 
      col = "blue")

## PARA PENSAR:
# Qual dos dois modelos gerou maior dissimilaridade entre as linhagens-irmãs?

## EM GRUPO:
# Agora vamos testar se os modelos geram dissimilaridades de magnitude distintas: 

## dissimilaridades calculadas pela turma
dif_bm = c() ## dissimilaridades sob BM
dif_de = c() ## dissimilaridades sob DE
dif = c(dif_bm, dif_de)

## vetor para diferenciar resultados de cada modelo
model = c(rep("BM", length(dif_bm)), rep("DE", length(dif_de)))

## organizar em tabela
table = as.data.frame(cbind(dif,modelo) )

## testar diferença com ANOVA
summary(aov(dif ~ model, data = table))

## PARA PENSAR:
# Segundo a ANOVA, os modelos produzem dissimilaridades de magnitude distintas?