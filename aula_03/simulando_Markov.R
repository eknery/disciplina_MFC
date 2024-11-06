############################ APRESENTAÇÃO DO MODELO ############################

## Parâmetros do modelo de Markov
# Estados 
estados <- c("0", "1", "2")
# taxas de transição entre estados (em milhões de anos)
matriz_q <- matrix(c(1, 1, 1,
                     1, 1, 1,
                     1, 1, 1), 
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
yi = "0"
## vetor para armazenar todos os estados
Y = yi

while (ti < tf) {
    # Taxa de saída do estado atual
    taxa <- matriz_q[yi, yi]
    
    # Tempo até a próxima transição 
    dt <- rexp(1, rate = taxa)
    
    # Rodar simulação até o tempo final
    if (ti + dt > tf) break
      
      # Atualização de tempo
      ti <- ti + dt
      ts <- c(ts, ti)
    
      # Probabilidades de transição para os outros estados
      probs_transicao <- matriz_q[yi, ]
      probs_transicao[yi] <- 0  # Não podemos mudar para o estado atual
      probs_transicao <- probs_transicao / taxa  # Normalizar para probabilidades
    
      # Escolher o próximo estado
      yi <- sample(estados, 1, prob = probs_transicao)
      # Armazenar estado novo
      Y <- c(Y, yi)
}

plot(x = ts, 
     y = Y, 
     type = "s",
     col = "blue",
     xlab = "Tempo (milhões de anos)", 
     ylab = "Estado de Y",
     )

############################# SIMULANDO EVOLUÇÃO LENTA #########################

source("simula_markov.R")

## Parâmetros do modelo de Markov
# Estados 
estados <- c("0", "1", "2")
# taxas de transição entre estados (em milhões de anos)
matriz_q2 <- matrix(c(0.1, 0.5, 0.5,
                      0.5, 0.1, 0.5,
                      0.5, 0.5, 0.1), 
                   nrow = 3, byrow = TRUE)
rownames(matriz_q2) <- estados
colnames(matriz_q2) <- estados
# ver  matriz de transição
matriz_q2

# gerando processo de Markov
markov2 <- simula_markov(matriz = matriz_q2, 
                         y0 = 0,
                         tf = tf)

plot(x = ts, 
     y = Y, 
     type = "s",
     col = "blue",
     xlab = "Tempo (milhões de anos)", 
     ylab = "Estado de Y",
)
lines(x = markov2$ts,
     y = markov2$Y,
     type = "s",
     col = "red"
)

##################### SIMULANDO TENDÊNCIAS E RESTRIÇÕES ########################

source("simula_markov.R")

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
