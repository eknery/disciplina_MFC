############################ APRESENTAÇÃO DO MODELO ############################

## Parâmetros do modelo OU
# Tendência central
theta <- 1  
# Taxa de reversão
alpha <- 0.25 
# Taxa de variação
sigma <- 3

## Cenário temporal
# tempo final (em milhões de anos)
tf <- 1 
# intervalo de tempo entre geração (em milhões de anos)
dt <- 0.0001  
# vetor com o tempo de todas as gerações
ts <- seq(0, tf, by = dt)  

## vetor para armazenar os valores da característica
Y <- c() 
# valor inicial da característica
Y[1] <- 1 

## Simulação do processo OU
for (i in 2:length(ts)) {
  dS <- dt * rnorm(1, mean = 0, sd = sigma)  # variação estocástica
  dD <- dt * (alpha * (theta - Y[i-1]) ) # variação determinística
  Y[i] <- Y[i-1] + dD + dS
}

## Plot do processo OU
plot(x = ts, 
     y = Y, 
     ylim = c(0.9,1.1),
     type = "l", 
     col = "blue", 
     
     xlab = "Tempo (milhões de anos)", 
     ylab = "Valor de Y",
     main = paste0("Processo OU ",
                   " theta: ", theta, 
                   " alpha: ", alpha,
                   " sigma: ", sigma
                  )
     )
lines(x = ts, 
      y = rep(theta, length(ts)),
      col = "blue",
      type = "l", 
      lty = 2)

#################### SIMULANDO DIFERENTES ÓTIMOS ADAPTATIVOS ###################

## Parâmetros do modelo OU
# Tendência central
theta2 <- 1.05
# Taxa de reversão
alpha2 <- 0.25 
# Taxa de variação
sigma2 <- 3

## Vetor para armazenar os valores da característica
Y2 <- numeric(length(ts))
# valor inicial da característica
Y2[1] <- 1 

## Simulação do processo OU
for (i in 2:length(ts)) {
  dS <- dt * rnorm(1, mean = 0, sd = sigma2)  # variação estocástica
  dD <- dt * (alpha2 * (theta2 - Y[i-1]) ) # variação determinística
  Y2[i] <- Y2[i-1] + dD + dS
}

## Plot do processo OU
plot(x = ts, 
     y = Y, 
     ylim = c(0.9,1.1),
     type = "l", 
     col = "blue", 
     xlab = "Tempo (milhões de anos)", 
     ylab = "Característica Y",
     main = paste0(
              "theta1: ", theta, "  ",
              "theta2: ", theta2
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
      y = rep(theta1, length(ts)),
      col = "red",
      type = "l", 
      lty = 2)

################### SIMULANDO DIFERENTES PRESSÕES SELETIVAS ####################

## Parâmetros do modelo OU
# Tendência central
theta3 <- 1
# Taxa de reversão
alpha3 <- 0.0025 
# Taxa de variação
sigma3 <- 3

## Vetor para armazenar os valores da característica
Y3 <- numeric(length(ts))
# valor inicial da característica
Y3[1] <- 1 

## Simulação do processo OU
for (i in 2:length(ts)) {
  dS <- dt * rnorm(1, mean = 0, sd = sigma3)  # variação estocástica
  dD <- dt * (alpha3 * (theta3 - Y[i-1]) ) # variação determinística
  Y3[i] <- Y3[i-1] + dD + dS
}

## Plot do processo OU
plot(x = ts, 
     y = Y, 
     ylim = c(0.9,1.1),
     type = "l", 
     col = "blue", 
     xlab = "Tempo (milhões de anos)", 
     ylab = "Característica Y",
     main = paste0(
       "alpha1: ", alpha, "  ",
       "alpha3: ", alpha3
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
