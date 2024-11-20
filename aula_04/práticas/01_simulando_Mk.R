# Nessa prática vamos simular a evolução de uma característica hipotética (Y) que 
# possui dois estados (0 e 1). Para isso, vamos construir a matriz de transição (Q)
# dessa características e simular as transições de estado ao longo de uma linhagem
# com um tempo de vida muito longo (10 milhões de anos). Posteriormente, vamos
# modificar a matriz de transição para ver os efeitos sobre a evolução da característica.

############################ APRESENTAÇÃO DO MODELO ############################

## Parâmetros do modelo de Markov
# Estados 
estados <- c("0", "1")
# taxas de transição entre estados (em milhões de anos)
matriz_q <- matrix(c(-0.5, 0.5,
                      0.5,-0.5), 
                   nrow = 2, byrow = TRUE)
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
yi = "0"
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

######################### EFEITO DA MATRIZ ASSIMÉTRICA #########################

## Parâmetros do modelo de Markov
# Estados 
estados <- c("0", "1")
# taxas de transição entre estados (em milhões de anos)
matriz_q2 <- matrix(c(-0.5, 0.5,
                      0.1, -0.1), 
                   nrow = 2, byrow = TRUE)
rownames(matriz_q2) <- estados
colnames(matriz_q2) <- estados
# ver  matriz de transição
matriz_q2

## Cenário temporal
# tempo final (em milhões de anos)
tf <- 10
# tempo atual 
ti <- 0
# vetor para armazenar o tempo de todos os estados
ts2 = ti

# estado atual
yi = "0"
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

##################### SIMULANDO TENDÊNCIAS E RESTRIÇÕES ########################

## Parâmetros do modelo de Markov
# Estados 
estados <- c("0", "1", "2")
# taxas de transição entre estados (em milhões de anos)
matriz_q3 <- matrix(c(1, 1, 0,
                      1, 5, 1,
                      0, 1, 1), 
                    nrow = 3, byrow = TRUE)
rownames(matriz_q3) <- estados
colnames(matriz_q3) <- estados
# ver  matriz de transição
matriz_q3

# gerando processo de Markov
markov3 <- simula_markov(matriz = matriz_q3, 
                         y0 = 0,
                         tf = tf)

plot(x = ts, 
     y = Y, 
     type = "s",
     col = "blue",
     xlab = "Tempo (milhões de anos)", 
     ylab = "Estado de Y",
)
lines(x = markov3$ts,
      y = markov3$Y,
      type = "s",
      col = "darkgreen"
)
