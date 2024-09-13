simula_markov = function(matriz, y0, tf){
  
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
    taxa <- matriz[yi, yi]
    
    # Tempo até a próxima transição 
    dt <- rexp(1, rate = taxa)
    
    # Rodar simulação até o tempo final
    if (ti + dt > tf) break
    
    # Atualização de tempo
    ti <- ti + dt
    ts <- c(ts, ti)
    
    # Probabilidades de transição para os outros estados
    probs_transicao <- matriz[yi, ]
    probs_transicao[yi] <- 0  # Não podemos mudar para o estado atual
    probs_transicao <- probs_transicao / taxa  # Normalizar para probabilidades
    
    # Escolher o próximo estado
    yi <- sample(estados, 1, prob = probs_transicao)
    # Armazenar estado novo
    Y <- c(Y, yi)
  }
  
  # agrupando resultados
  df = data.frame(cbind(ts, Y))
  
  return(df)
}
