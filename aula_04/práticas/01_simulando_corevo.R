# Nesta prática vamos simular a evolução correlacionada de duas características
# hipotéticas, Y e Z, em uma mesma linhagem. Faremos simulações considerando
# diferentes cenários de taxas e correlação evolutiva. Ao final das simulações,
# vamos quantificar a correlação padrão das características e avaliar sua relação
# com a correlação evolutiva.

######################## SIMULANDO EVOLUÇÃO CORRELACIONADA  ####################

## Cenário temporal 
# tempo total (em milhões de anos)
tf <- 1 
# tempo entre geração (em milhões de anos)
dt <- 0.0001  
# vetor com o tempo de todas as gerações
ts <- seq(0, tf, by = dt) 

## modelo BM multivariado:
# Matriz R
R <- matrix(c(0.05, 0.04, 
              0.04, 0.05), 
               nrow = 2)

# Taxas evolutivas
sigmaY <- R[1, 1]
sigmaZ <- R[2, 2]
covYZ <- R[1, 2]

# Correlação evolutiva
corevo = covYZ/ sqrt(sigmaY*sigmaZ)

## vetores para armazenar os valores das características
Y <- c() 
Z <- c() 
## valores iniciais das características
Y <- 5 
Z <- 1

## Simulação 
for (i in 2:length(ts)) {
  dS = MASS::mvrnorm(n = 1, mu = c(0, 0), Sigma = R)
  Y[i] <- Y[i-1] + dS[1]
  Z[i] <- Z[i-1] + dS[2] 
}

## Plot da simulação
par(mfrow = c(1,2))
plot(x = ts, 
     y = Y, 
     type = "l", 
     col = "blue", 
     xlab = "Tempo (milhões de anos)", 
     ylab = "Características Y",
     main = paste0(
                   "sigma_sq Y: ", sigmaY,
                   "; Cor Evo: ", corevo
     )
)
plot(x = ts, 
     y = Z, 
     type = "l", 
     col = "orange", 
     xlab = "Tempo (milhões de anos)", 
     ylab = "Características Z",
     main = paste0(
                   "sigma_sq Z: ", sigmaZ,
                   "; Cor Evo: ", corevo
     )
)

## EM GRUPO:

## Vamos verificar os valores finais das características
Y[length(ts)]
Z[length(ts)]

# Vamos armazenar os valores finais de Y e Z das nossas simulações:
Ys = c()
Zs = c()

# Vamos calcular a correlação padrão entre as características
cor(Ys,Zs)
# simulação 1 cor padrão = 0.86; cor evol = 0.8
# simulação 2 cor padrão = 0.18; cor evol = 0.08
# simulação 3 cor padrão = -0.63; cor evol = 0

# Agora vamos comparar a correlação padrão e a correlação evolutiva.
# Os valores são similares? 
c("Padrão" = cor(Ys,Zs), "Evolutiva" = corevo )


## NOVOS EXPERIMENTOS:
# 2) Produza uma simulação com uma correlação evolutiva 10X menor que o valor original.
#
# 3) Produza uma simulação sem correlação evolutiva e com tempo 10 X menor que o original.
