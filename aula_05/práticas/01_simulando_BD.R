

################### PROCESSO DE NASCIMENTO-MORTE SIMPLES #######################

## Parâmetros do modelo Birth-Death
# Taxa de especiação por unidade de tempo
lambda <- 0.5
# Taxa de extinção por unidade de tempo
mu <- 0.01

## Cenário temporal
# tempo final (em milhões de anos)
tf <- 10
# tempo atual
ti = 0
# vetor de tempo
ts = ti

## Cenário de diversidade
# número atual de espécies 
n <- 1
# Vetor de diversidade de espécies
n_spp <- n

## iterarar pelo tempo total
while (ti < tf) {
      # tempo até a próxima especiação 
      lambda_t <- rexp(1, rate = lambda)
      # tempo até a próxima extinção
      mu_t <- rexp(1, rate = mu)
      # se a especiação acontece primeiro
      if( lambda_t < mu_t ){
        # está dentro to tempo de simulação?
        if(lambda_t + ti > tf) next
        # adicionar uma espécie
        n = n + 1
        # adicionar tempo
        ti = ti + lambda_t
        ts = c(ts, ti)
      }
      # se a extinção acontece primeiro
      if( lambda_t > mu_t ){
        # está dentro to tempo de simulação?
        if(mu_t + ti > tf) next
        # remover uma espécie
        n = n - 1
        # adicionar tempo
        ti = ti + mu_t
        ts = c(ts, ti)
      }
  # se extinção, parar!
  if(n <= 0) break
  # atualizar vetor de diversidade
  n_spp = c(n_spp, n)
}

# Visualiza os resultados
plot(x = ts,
     y = n_spp, 
     type = "l", col = "blue",
     xlab = "Tempo (em milhões de anos)", 
     ylab = "Número de espécies",
     main = paste0("BD ", "lambda: ", lambda, " mu: ", mu)
     )
