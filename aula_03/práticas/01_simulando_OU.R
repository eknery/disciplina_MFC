# Nessa prática vamos simular a evolução de uma característica hipotética (Y) 
# em linhagens de um milhões de anos sob o processo Ornstein–Uhlenbeck (OU).
# Nós vamos modificar diferentes parâmetros do modelo para verificar seus efeitos
# sobre como a característica evolue e seus valores  finais. Em outras palavras, 
# vamos 'brincar' de ver a evolução em tempo real, fazendo 'experimentos' com
# a nossa características hipotética.


########################### CENÁRIO TEMPORAL COMUM ############################

## Cenário temporal
# tempo final (em milhões de anos)
tf <- 1 
# intervalo de tempo entre geração (em milhões de anos)
dt <- 0.0001  
# vetor com o tempo de todas as gerações
ts <- seq(0, tf, by = dt)  

############################ APRESENTAÇÃO DO MODELO ############################

## Parâmetros do modelo OU
# Taxa de variação
sigma_sq <- 0.1
# Tendência central
theta <- 1  
# Taxa de reversão
alpha <- 0.01

## vetor para armazenar os valores da característica
Y <- c() 
# valor inicial da característica
Y[1] <- 1 

## Simualação
for (i in 2:length(ts)) {
  dS <- rnorm(1, mean = 0, sd = sqrt(sigma_sq) )  # variação estocástica
  dD <- (alpha * (theta - Y[i-1]) ) # variação determinística
  Y[i] <- Y[i-1] + dD + dS
}

## Plot da simulação
plot(x = ts, 
     y = Y, 
     type = "l", 
     col = "blue", 
     
     xlab = "Tempo (milhões de anos)", 
     ylab = "Valor de Y",
     main = paste0("Processo OU",
                   "; sigma_sq: ", sigma_sq,
                   "; theta: ", theta, 
                   "; alpha: ", alpha
                   
                  )
     )
lines(x = ts, 
      y = rep(theta, length(ts)),
      col = "blue",
      type = "l", 
      lty = 2)

################### EFEITO DE DIFERENTES ÓTIMOS FENOTÍPICOS ###################

## Parâmetros do modelo OU
# Taxa de variação
sigma_sq2 <- 0.1
# Tendência central
theta2 <- 3
# Taxa de reversão
alpha2 <- 0.01 

## Vetor para armazenar os valores da característica
Y2 <- c()
# valor inicial da característica
Y2[1] <- 1 

## Simualação
for (i in 2:length(ts)) {
  dS <- rnorm(1, mean = 0, sd = sqrt(sigma_sq2) ) # variação estocástica
  dD <- (alpha2 * (theta2 - Y2[i-1]) ) # variação determinística
  Y2[i] <- Y2[i-1] + dD + dS
}

## Plot da simualação
plot(x = ts, 
     y = Y, 
     type = "l", 
     col = "blue", 
     xlab = "Tempo (milhões de anos)", 
     ylab = "Característica Y",
     main = paste0(
              "theta (1): ", theta, "; ",
              "theta (2): ", theta2
              )
     )
lines(x = ts, 
      y = Y2, 
      type = "l", 
      col = "red")
lines(x = ts, 
      y = rep(theta, length(ts)),
      col = "blue",
      type = "l", 
      lty = 2)
lines(x = ts, 
      y = rep(theta2, length(ts)),
      col = "red",
      type = "l", 
      lty = 2)

### PARA PENSAR:
## O que faria duas linhagens-irmãs evoluírem ao redor de ótimos diferentes?

################### EFEITO DE DIFERENTES TAXAS DE REVERSÃO ####################

## Parâmetros do modelo OU
# Taxa de variação
sigma_sq3 <- 0.1
# Tendência central
theta3 <- 1
# Taxa de reversão
alpha3 <- 0.05 

## Vetor para armazenar os valores da característica
Y3 <- c()
# valor inicial da característica
Y3[1] <- 1 

## Simualação
for (i in 2:length(ts)) {
  dS <- rnorm(1, mean = 0, sd = sqrt(sigma_sq3) ) # variação estocástica
  dD <- (alpha3 * (theta3 - Y3[i-1]) ) # variação determinística
  Y3[i] <- Y3[i-1] + dD + dS
}

## Plot da simualação
plot(x = ts, 
     y = Y, 
     type = "l", 
     col = "blue", 
     xlab = "Tempo (milhões de anos)", 
     ylab = "Característica Y",
     main = paste0(
       "alpha (1): ", alpha, "; ",
       "alpha (3): ", alpha3
        )
     )
lines(x = ts, 
      y = Y3, 
      type = "l", 
      col = "purple")
lines(x = ts, 
      y = rep(theta, length(ts)),
      col = "blue",
      type = "l", 
      lty = 2)

### PARA PENSAR:
## Qual processo biológico poderia aumentar ou diminuir a taxa de reversão? 

################### EFEITO DE TAXAS DE REVERSÃO NULA ####################

## Parâmetros do modelo OU
# Taxa de variação
sigma_sq4 <- 0.1
# Tendência central
theta4 <- 1
# Taxa de reversão
alpha4 <- 0

## Vetor para armazenar os valores da característica
Y4 <- c()
# valor inicial da característica
Y4[1] <- 1 

## Simualação
for (i in 2:length(ts)) {
  dS <- rnorm(1, mean = 0, sd = sqrt(sigma_sq4) ) # variação estocástica
  dD <- (alpha4 * (theta4 - Y4[i-1]) ) # variação determinística
  Y4[i] <- Y4[i-1] + dD + dS
}

## Plot da simualação
plot(x = ts, 
     y = Y4, 
     type = "l", 
     col = "black", 
     xlab = "Tempo (milhões de anos)", 
     ylab = "Característica Y",
     main = paste0(
       "alpha (1): ", alpha, "; ",
       "alpha (4): ", alpha4
     )
)
lines(x = ts, 
      y = Y, 
      type = "l", 
      col = "blue")
lines(x = ts, 
      y = rep(theta, length(ts)),
      col = "blue",
      type = "l", 
      lty = 2)

### PARA PENSAR:
## O que representaria uma linhagem sem taxa de reversão? 
