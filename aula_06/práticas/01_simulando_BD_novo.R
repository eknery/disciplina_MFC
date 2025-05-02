


## Parâmetros do model BD
lambda <- 0.1  # Taxa de nascimento
mu <- 0.01  # Taxa de morte

## Cenário temporal
t <- 0  # tempo atual
ts = c()
tf <- 10 # tempo máximo da simulação

## Cenário de diversidade
n <- 1   # diversidade atual
N <- c() # diversidade ao longo da simulção

# Simulação do processo Birth-Death
while (t < tf) {
  # tempos até os próximos eventos de nascimento e morte
  next_birth <- rexp(1, lambda)
  next_death <- rexp(1, mu)
  # qual evento ocorre antes?
  ti <- min(next_birth, next_death)
  # atualizando a diversidade
  if (ti == next_birth) {
    n <- n + 1 # nasce mais uma linhagem
    
  } else {
    n <- n - 1 # morre uma linhagem
  }
  # atualizando o tempo
  t <- t + ti
  # Armazenando o histórico da diversidade
  N <- c(N, n)
}

# Resultados
time_steps <- seq(0, tf, length.out = length(population_history))
plot(time_steps, population_history, type = "l", col = "blue", 
     xlab = "Tempo", ylab = "População",
     main = "Simulação do Processo Birth-Death")