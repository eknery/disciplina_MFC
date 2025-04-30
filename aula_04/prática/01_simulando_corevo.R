# Nesta prática vamos simular a evolução correlacionada de duas características
# hipotéticas, Y e Z, em uma mesma linhagem. Vamos quantificar a correlação padrão
# das simulações e contrastar com as correlações evolutivas para verificar se
# existe relação entre essas medidas.

######################## SIMULANDO EVOLUÇÃO CORRELACIONADA  ####################

## Cenário temporal 
# tempo total (em milhões de anos)
tf <- 1 
# tempo entre geração (em milhões de anos)
dt <- 0.0001  
# vetor com o tempo de todas as gerações
ts <- seq(0, tf, by = dt) 

## Parâmetros do modelo BM correlacionado:

# Matriz VCV
vcv <- matrix(c(0.1, 0.09, 
                0.09, 0.1), 
                nrow = 2)

# Correlação evolutiva
corevo = sqrt(vcv[1,2])

## vetores para armazenar os valores das características
Y <- c() 
Z <- c() 
## valores iniciais das características
Y <- 5 
Z <- 1

## Simulação 
for (i in 2:length(ts)) {
  dS = MASS::mvrnorm(n = 1, mu = c(0, 0), Sigma = vcv)
  Y[i] <- Y[i-1] + dS[1]
  Z[i] <- Z[i-1] + dS[2] 
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
Ys = c()
Zs = c()

# Vamos calcular a correlação padrão entre as características
cor(Ys,Zs)

# Agora vamos comparar a correlação padrão e a correlação evolutiva.
# Os valores são similares? 
c("Padrão" = cor(Ys,Zs), "Evolutiva" = corevo )

## NOVOS EXPERIMENTOS:
# 2) Produza uma simulação com uma correlação evolutiva 10X menor que o valor inicial.
#
# 3) Produza uma simulação com uma correlação evolutiva 10X menor que o valor inicial,
# e com uma taxa evolutiva 10X menor para a característica Z. 