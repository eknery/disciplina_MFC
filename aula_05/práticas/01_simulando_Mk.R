# Nesta prática vamos simular a evolução de uma característica hipotética (Y) que 
# possui três estados (1, 2 e 3) ao longo de uma linhagem de 10 milhões de anos. 
# Para isso, vamos construir diferentes matrizes de transição (Q) para
# essa característica e simular as transições de estado. Assim, poderemos verificar
# como modificações na matriz alteram a forma de evolução da característica.

############################ MATRIZ COM TAXA ÚNICA ############################

## Parâmetros do modelo de Markov
# Estados 
estados <- c("1", "2", "3")

# taxas de transição entre estados (em milhões de anos)
matriz_q <- matrix(c( -1, 0.5, 0.5,
                     0.5,  -1, 0.5,
                     0.5, 0.5,  -1
                     ), 
                   nrow = 3, byrow = TRUE)
rownames(matriz_q) <- estados
colnames(matriz_q) <- estados
# ver  matriz de transição
matriz_q

## Cenário temporal
# tempo final (em milhões de anos)
tf <- 10
# tempo atual 
ti <- 0
# vetor para armazenar o tempo de todos os estados
ts = ti

# estado atual
yi = "1"
## vetor para armazenar todos os estados
Y = yi

while (ti < tf) {
    # Taxa de saída do estado atual
    q <- matriz_q[yi, yi]
    # Tempo até a próxima transição 
    dt <- rexp(1, rate = -q)
    # Rodar simulação até o tempo final
    if (ti + dt > tf) break
      # Atualização de tempo
      ti <- ti + dt
      ts <- c(ts, ti)
      # Probabilidades de transição para os outros estados
      probs_transicao <- matriz_q[yi, ]
      probs_transicao[yi] <- 0  # Não podemos mudar para o estado atual
      probs_transicao <- probs_transicao / (-q)  # Normalizar para probabilidades
      # Escolher o próximo estado
      yi <- sample(estados, 1, prob = probs_transicao)
      # Armazenar estado novo
      Y <- c(Y, yi)
}

plot(x = ts, 
     y = as.numeric(Y), 
     type = "s",
     col = "blue",
     xlab = "Tempo (milhões de anos)", 
     ylab = "Estado de Y",
     )

##################### EFEITO DA MATRIZ COM TAXAS SIMÉTRICAS ####################

# taxas de transição entre estados (em milhões de anos)
matriz_q2 <- matrix(c( -0.3, 0.25, 0.05,
                       0.25, -0.5, 0.25,
                       0.05, 0.25, -0.3
                      ), 
                      nrow = 3, byrow = TRUE)
rownames(matriz_q2) <- estados
colnames(matriz_q2) <- estados
# ver  matriz de transição
matriz_q2

## Cenário temporal
# tempo final (em milhões de anos)
tf <- 10
# tempo atual 
ti <- 0
# vetor para armazenar o tempo 
ts2 = ti

# estado atual
yi = "1"
## vetor para armazenar todos os estados
Y2 = yi

while (ti < tf) {
  # Taxa de saída do estado atual
  q <- matriz_q2[yi, yi]
  # Tempo até a próxima transição 
  dt <- rexp(1, rate = -q)
  # Rodar simulação até o tempo final
  if (ti + dt > tf) break
  # Atualização de tempo
  ti <- ti + dt
  ts2 <- c(ts2, ti)
  # Probabilidades de transição para os outros estados
  probs_transicao <- matriz_q2[yi, ]
  probs_transicao[yi] <- 0  # Não podemos mudar para o estado atual
  probs_transicao <- probs_transicao / (-q)  # Normalizar para probabilidades
  # Escolher o próximo estado
  yi <- sample(estados, 1, prob = probs_transicao)
  # Armazenar estado novo
  Y2 <- c(Y2, yi)
}


plot(x = ts, 
     y = Y, 
     type = "s",
     col = "blue",
     xlab = "Tempo (milhões de anos)", 
     ylab = "Estado de Y",
)
lines(x = ts2,
     y = Y2,
     type = "s",
     col = "red"
)

# PARA PENSAR:
#   Comparando as duas simulações, existem diferenças aparentes na frequência 
#   de algum tipo de transição? Qual?
    
####################### EFEITO DA MATRIZ COM TAXAS ORDENADAS ###################

# taxas de transição entre estados (em milhões de anos)
matriz_q3 <- matrix(c(-0.5, 0.5, 0,
                       0,  -0.5, 0.5,
                       0,   0,   0
                      ), 
                      nrow = 3, byrow = TRUE)
rownames(matriz_q3) <- estados
colnames(matriz_q3) <- estados
# ver  matriz de transição
matriz_q3

## Cenário temporal
# tempo final (em milhões de anos)
tf <- 10
# tempo atual 
ti <- 0
# vetor para armazenar o tempo de todos os estados
ts3 = ti

# estado atual
yi = "1"
## vetor para armazenar todos os estados
Y3 = yi

while (ti < tf) {
  # Taxa de saída do estado atual
  q <- matriz_q3[yi, yi]
  # Tempo até a próxima transição 
  dt <- rexp(1, rate = -q)
  # Rodar simulação até o tempo final
  if (ti + dt > tf) break
  # Atualização de tempo
  ti <- ti + dt
  ts3 <- c(ts3, ti)
  # Probabilidades de transição para os outros estados
  probs_transicao <- matriz_q3[yi, ]
  probs_transicao[yi] <- 0  # Não podemos mudar para o estado atual
  probs_transicao <- probs_transicao / (-q)  # Normalizar para probabilidades
  # Escolher o próximo estado
  yi <- sample(estados, 1, prob = probs_transicao)
  # Armazenar estado novo
  Y3 <- c(Y3, yi)
}

plot(x = ts, 
     y = Y, 
     type = "s",
     col = "blue",
     xlab = "Tempo (milhões de anos)", 
     ylab = "Estado de Y",
)
lines(x = ts3,
      y = Y3,
      type = "s",
      col = "darkgreen"
)

# PARA PENSAR:
#   A matriz utilizada possuia valores zerados para algumas taxas. Qual foi o efeito
#   dessas taxas zeradas nas transições? Que tipo de hipótese biológica esse
#   tipo de matriz representatia?