

 

######################## SIMULANDO EVOLUÇÃO CORRELACIONADA  ####################

# 1) Produza uma simulação com os valores iniciais fornecidos no sript.
# 
# 2) Produza uma simulação com uma correlação evolutiva 10X menor que o valor inicial. 
#
# 3) Produza uma simulação com uma correlação evolutiva 10X menor que o valor inicial,
# e com uma taxa evolutiva 10X menor para a característica Z.

## Cenário temporal 
# tempo total (em milhões de anos)
tf <- 1 
# tempo entre geração (em milhões de anos)
dt <- 0.0001  
# vetor com o tempo de todas as gerações
ts <- seq(0, tf, by = dt) 

## Parâmetros do modelo BM
# Taxa de variação de Y
sigma_sqY <- 0.05 # inicial: 0.05
# Taxa de variação de Z
sigma_sqZ <- 0.05 # inicial: 0.05

## Correlação evolutiva entre Y e Z
covYZ <- 0.5 # incial: 0.5

## vetores para armazenar os valores das características
Y <- c() 
Z <- c() 
## valores iniciais das características
Y <- 5 
Z <- 1

## Simulação 
for (i in 2:length(ts)) {
  dY <-  rnorm(1, mean = 0, sd = sqrt(sigma_sqY) )  # variação estocástica de Y
  dZ <-  rnorm(1, mean = 0, sd = sqrt(sigma_sqZ) )  # variação estocástica de Z
  Y[i] <- Y[i-1] + dY + (sqrt(covYZ) * dZ) 
  Z[i] <- Z[i-1] + dZ + (sqrt(covYZ) * dY)
}

## Plot da simulação
plot(x = ts, 
     y = Y, 
     type = "l", 
     col = "blue", 
     
     xlab = "Tempo (milhões de anos)", 
     ylab = "Características",
     main = paste0("BM",
                   "; sigma_sq Y: ", sigma_sqY,
                   "; sigma_sq Z: ", sigma_sqZ,
                   "; cov YZ: ", covYZ
                   
     )
)
lines(x = ts, 
      y = Z, 
      type = "l", 
      col = "orange")

