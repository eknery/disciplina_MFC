
############################## MODELO PURE BIRTH #############################

## Parâmetros do modelo PB (Yule)
# Taxa de nascimento
lambda <- 0.5

## Cenário temporal
t <- 0  # tempo atual
ts = t  # tempo de todos os eventos
tf <- 20 # tempo máximo da simulação

## Cenário de diversidade
n <- 1   # número de espécies atual
N <- n  # número de espécies ao longo da simulação

# Simulação do processo Pure Birth
while (t < tf) {
  # tempos até o próximos evento de nascimento
  t_birth <- rexp(1, n*lambda)
  # atualizando o tempo
  t <- t + t_birth
  ts <- c(ts, t)
  # atualizando a diversidade
  n <- n + 1 # 'nasce' uma espécie
  N <- c(N, n)
}

## Plot da simulação
par(mfrow = c(1,2))
plot(
  x = ts, 
  y = N, 
  type = "l", 
  col = "blue", 
  xlab = "Tempo (milhões de anos)", 
  ylab = "N de espécies",
  main = paste0("Processo Pure Birth",
                "; lambda: ", lambda
  )
)
plot(
  x = ts, 
  y = log(N), 
  type = "l", 
  col = "blue", 
  xlab = "Tempo (milhões de anos)", 
  ylab = "ln(N de espécies)",
  main = paste0("LTT")
)


############################## MODELO BIRTH-DEATH #############################

## Parâmetros do modelo BD
# Taxa de nascimento
lambda <- 0.5 
# Taxa de morte
mu <- 0.01  

## Cenário temporal
t <- 0  # tempo atual
ts = t  # tempo de todos os eventos
tf <- 20 # tempo máximo da simulação

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
  # atualizando o tempo
  t <- t + ti
  ts <- c(ts, t)
  # atualizando a diversidade
  if (ti == t_birth) {
    n <- n + 1 # 'nasce' uma espécie
  } else {
    n <- n - 1 # 'morre' uma espécie
  }
  N <- c(N, n)
}

## Plot da simulação
par(mfrow = c(1,2))
plot(
  x = ts, 
  y = N, 
  type = "l", 
  col = "red", 
  xlab = "Tempo (milhões de anos)", 
  ylab = "N de espécies",
  main = paste0("Processo BD",
                 "; lambda: ", lambda, 
                 "; mu: ", mu
                )
)
plot(
  x = ts, 
  y = log(N), 
  type = "l", 
  col = "red", 
  xlab = "Tempo (milhões de anos)", 
  ylab = "ln(N de espécies)",
  main = paste0("LTT")
)
