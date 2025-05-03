
############################## MODELO PURE BIRTH #############################

## Parâmetros do modelo PB (Yule)
# Taxa de nascimento
lambda1 <- 0.3

## Cenário temporal
t <- 0  # tempo atual
ts1 = t  # tempo de todos os eventos
tf <- 20 # tempo máximo da simulação

## Cenário de diversidade
n <- 1   # número de espécies atual
N1 <- n  # número de espécies ao longo da simulação

# Simulação do processo Pure Birth
while (t < tf) {
  # tempos até o próximos evento de nascimento
  t_birth <- rexp(1, n * lambda1)
  # atualizando o tempo
  t <- t + t_birth
  ts1 <- c(ts1, t)
  # atualizando a diversidade
  n <- n + 1 # 'nasce' uma espécie
  N1 <- c(N1, n)
}

## Plot da simulação
par(mfrow = c(1,1))
plot(
  x = ts1, 
  y = N1, 
  type = "l", 
  col = "blue", 
  xlab = "Tempo (milhões de anos)", 
  ylab = "N de espécies",
  main = paste0("Processo Pure Birth",
                "; lambda: ", lambda
  )
)

# Veja o número final de espécies da simulação
Nb= N1[length(N1)]
Nb

############################## MODELO BIRTH-DEATH #############################

## Parâmetros do modelo BD
# Taxa de nascimento
lambda2 <- 0.35
# Taxa de morte
mu2 <- 0.05 

## Cenário temporal
t <- 0  # tempo atual
ts2 = t  # tempo de todos os eventos
tf <- 20 # tempo máximo da simulação

## Cenário de diversidade
n <- 1   # número de espécies atual
N2 <- n  # número de espécies ao longo da simulação

# Simulação do processo Birth-Death
while (t < tf) {
  # tempos até os próximos eventos de nascimento e morte
  t_birth <- rexp(1, n * lambda2)
  t_death <- rexp(1, n * mu2)
  # qual evento ocorre antes?
  ti <- min(t_birth, t_death)
  # atualizando o tempo
  t <- t + ti
  ts2 <- c(ts2, t)
  # atualizando a diversidade
  if (ti == t_birth) {
    n <- n + 1 # 'nasce' uma espécie
  } else {
    n <- n - 1 # 'morre' uma espécie
  }
  N2 <- c(N2, n)
}

## Plot da simulação
par(mfrow = c(1,1))
plot(
  x = ts2, 
  y = N2, 
  type = "l", 
  col = "red", 
  xlab = "Tempo (milhões de anos)", 
  ylab = "N de espécies",
  main = paste0("Processo BD",
                "; lambda: ", lambda2, 
                "; mu: ", mu2
  )
)

# Veja o número final de espécies da simulação
Nbd = N2[length(N2)]
Nbd

############################### COMPARANDO MODELOS #############################

## Plot das duas simulações
par(mfrow = c(1,1))
plot(
  x = ts1, 
  y = N1, 
  type = "l", 
  col = "blue", 
  xlab = "Tempo (milhões de anos)", 
  ylab = "N de espécies",
  main = paste0("Pure Birth: Azul;  BD: Vermelho"
                )
)
lines(
  x = ts2, 
  y = N2, 
  type = "l", 
  col = "red"
)



