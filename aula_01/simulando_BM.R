############################ APRESENTAÇÃO DO MODELO ############################

## Parâmetros do modelo BM
# Taxa de variação
sigma <- 0.1

## Cenário temporal
# tempo final (em milhões de anos)
tf <- 1 
# tempo entre geração (em milhões de anos)
dt <- 0.0001  
# vetor com o tempo de todas as gerações
ts <- seq(0, tf, by = dt)  

## vetor para armazenar os valores da característica
Y <- c() 
# valor inicial da característica
Y[1] <- 1 

## Simulação 
for (i in 2:length(ts)) {
  dS <-  rnorm(1, mean = 0, sd = sigma)  # variação estocástica
  Y[i] <- Y[i-1] + dS
}

## Plot da simulação
plot(x = ts, 
     y = Y, 
     type = "l", 
     col = "blue", 
     
     xlab = "Tempo (milhões de anos)", 
     ylab = "Valor de Y",
     main = paste0("BM ",
                   " sigma: ", sigma
                  )
     )

#################### SIMULANDO DIFERENTES TAXAS DE VARIAÇÂO ###################

## Parâmetros do modelo BM
# Taxa de variação
sigma2 <- 0.05

## Cenário temporal
# vetor com o tempo de todas as gerações
ts2 <- ts

## Vetor para armazenar os valores da característica
Y2 <- c()
# valor inicial da característica
Y2[1] <- 1 

## Simulação
for (i in 2:length(ts2)) {
  dS <- rnorm(1, mean = 0, sd = sigma2)  # variação estocástica
  Y2[i] <- Y2[i-1] + dS
}

## Plot da simulação
plot(x = ts, 
     y = Y, 
     type = "l", 
     col = "blue", 
     
     xlab = "Tempo (milhões de anos)", 
     ylab = "Valor de Y",
     main = paste0("BM ",
                   " sigma (1): ", sigma,
                   " sigma (2): ", sigma2
                  )
    )
lines(x = ts, 
      y = Y2, 
      type = "l", 
      col = "red")


### PARA PENSAR:
## O que faria duas linhagens-irmãs terem taxas de variação diferentes?

################### SIMULANDO O EFEITO DO TEMPO DE GERAÇÃO ####################

## Parâmetros do modelo BM
# Taxa de variação
sigma3 <- 0.1

## Cenário temporal
# intervalo de tempo entre geração (em milhões de anos)
dt3 <- 0.001  
# vetor com o tempo de todas as gerações
ts3 <- seq(0, tf, by = dt3)  

## Vetor para armazenar os valores da característica
Y3 <- c()
# valor inicial da característica
Y3[1] <- 1 

## Simulação
for (i in 2:length(ts3)) {
  dS <- rnorm(1, mean = 0, sd = sigma3)  # variação estocástica
  Y3[i] <- Y3[i-1] + dS
}

## Plot da simulação
plot(x = ts, 
     y = Y, 
     type = "l", 
     col = "blue", 
     
     xlab = "Tempo (milhões de anos)", 
     ylab = "Valor de Y",
     main = paste0("BM ",
                   " dt (1): ", dt,
                   " dt (3): ", dt3
     )
)
lines(x = ts3, 
      y = Y3, 
      type = "l", 
      col = "purple")


### PARA PENSAR:
## Qual é o efeito do tempo de geração sobre a evolução da característica?

### IMPORTANTE:
## Nos modelos de macroevolução, o tempo de geração é assumido como igual entre linhagens,
## e apenas o tempo de divergência entre linhagens é levado em conta!

############################# O DILEMA DOS FÓSSEIS ############################

## Parâmetros do modelo BM
# Taxa de variação
sigma4 <- 0.1

## Cenário temporal
# intervalo de tempo entre geração (em milhões de anos)
dt4 <- 0.0001  
# vetor com o tempo de todas as gerações
ts4 <- seq(0, tf/2, by = dt4)  

## Vetor para armazenar os valores da característica
Y4 <- c()
# valor inicial da característica
Y4[1] <- 1 

## Simulação do processo OU
for (i in 2:length(ts4) ) {
  dS <- rnorm(1, mean = 0, sd = sigma4)  # variação estocástica
  Y4[i] <- Y4[i-1] + dS
}

## Plot da simulação
plot(x = ts, 
     y = Y, 
     type = "l", 
     col = "blue", 
     
     xlab = "Tempo (milhões de anos)", 
     ylab = "Valor de Y",
     main = paste0("BM ",
                   " tf (1): ", tf,
                   " tf (4): ", tf/2
     )
)
lines(x = ts4, 
      y = Y4, 
      type = "l", 
      col = "black")

### PARA PENSAR:
## A linhagem fóssil necessariamente reflete o fenótipo do último ancestral?