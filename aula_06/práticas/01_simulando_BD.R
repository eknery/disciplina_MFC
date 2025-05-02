


## Parâmetros do model BD
lambda <- 0.5  # Taxa de nascimento
mu <- 0.01  # Taxa de morte

## Cenário temporal
t <- 0  # tempo atual
ts = t  # tempo de todos os eventos
tf <- 10 # tempo máximo da simulação

## Cenário de diversidade
n <- 1   # número de espécies atual
N <- n  # número de espécies ao longo da simulação

# Simulação do processo Birth-Death
while (t < tf) {
  # tempos até os próximos eventos de nascimento e morte
  t_birth <- rexp(1, n *lambda)
  t_death <- rexp(1, n* mu)
  # qual evento ocorre antes?
  ti <- min(t_birth, t_death)
  # atualizando a diversidade
  if (ti == t_birth) {
    n <- n + 1 # 'nasce' uma espécie
  } else {
    n <- n - 1 # 'morre' uma espécie
  }
  # atualizando o tempo
  t <- t + ti
  ts <- c(ts, t)
  # armazenando o histórico da diversidade
  N <- c(N, n)
}

## Plot da simulação
plot(x = ts, 
     y = N, 
     type = "l", 
     col = "blue", 
     xlab = "Tempo (milhões de anos)", 
     ylab = "N de espécies",
     main = paste0("Processo BD",
                   "; lambda: ", lambda, 
                   "; mu: ", mu)
                   
    )
