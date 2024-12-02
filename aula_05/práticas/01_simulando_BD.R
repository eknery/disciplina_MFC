

################### PROCESSO DE NASCIMENTO-MORTE SIMPLES #######################

## Parâmetros do modelo Birth-Death
# Taxa de especiação por unidade de tempo
lambda <- 0.5
# Taxa de extinção por unidade de tempo
mu <- 0.05  

## Cenário temporal
# tempo final (em milhões de anos)
tf <- 20
# tempo atual 
ti <- 0
# vetor para armazenar o tempo 
ts = ti

## Cenário de diversidade
# número atual de espécies 
n <- 1
# Vetor de diversidade de espécies
n_spp <- n

while (ti < tf) {
    # Tempo até a próxima especiação 
    lambda_t <- rexp(1, rate = lambda)
    # tempo até a próxima extinção
    mu_t <- rexp(1, rate = mu)
    # se a especiação acontece primeiro
    if(lambda_t < mu_t){
      if (lambda_t + ti > tf) break
      # adicionar uma espécie
      n = n + 1
      # atualizar vetor de diversidade
      n_spp = c(n_spp, n)
      # atualizar tempo
      ti = ti + lambda_t
    }
    # se a extinção acontece primeiro
    if(lambda_t > mu_t){
      if (mu_t + ti > tf) break
      # remover uma espécie
      n = n - 1
      # atualizar vetor de diversidade
      n_spp = c(n_spp, n)
      # atualizar tempo
      ti = ti + mu_t
    }
    # armazenar novo tempo após especiação/extinção
    ts = c(ts, ti)
}

# Visualiza os resultados
plot(x = ts,
     y = n_spp, 
     type = "l", col = "blue",
     xlab = "Tempo (em milhões de anos)", 
     ylab = "Número de espécies",
     main = paste0("BD ", "lambda: ", lambda, " mu: ", mu)
     )
