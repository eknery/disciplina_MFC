

 

######################## SIMULANDO EVOLUÇÃO CORRELACIONADA  ####################

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
sigma_sqZ <- 0.005 # inicial: 0.05

## Correlação evolutiva entre Y e Z
corevo <- 0.07 # incial: 0.7

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
  Y[i] <- Y[i-1] + dY + (corevo * dZ) 
  Z[i] <- Z[i-1] + dZ + (corevo * dY)
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
                   "; Corr Evo: ", corevo
                   
     )
)
lines(x = ts, 
      y = Z, 
      type = "l", 
      col = "orange")

## EM GRUPO:

## Vamos verificar os valores finais das características
Y[length(ts)]
Z[length(ts)]

# Vamos armazenar os valores finais de Y e Z das nossas simulações:
Ys = c(-11.6, -17.4, -2.49, -22.6)
Zs = c(-11.9, -8.0, 14.0, 2.6)

# Vamos calcular a correlação padrão entre as características
cor(Ys,Zs)

# Agora vamos comparar a correlação padrão e a correlação evolutiva.
# Os valores são similares? 
c("Padrão" = cor(Ys,Zs), "Evolutiva" = corevo)

## NOVOS EXPERIMENTOS:
# 2) Produza uma simulação com uma correlação evolutiva 10X menor que o valor inicial.
#
# 3) Produza uma simulação com uma correlação evolutiva 10X menor que o valor inicial,
# e com uma taxa evolutiva 10X menor para a característica Z. 