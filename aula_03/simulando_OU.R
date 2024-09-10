# Parâmetros do modelo OU
theta <- 10   # Tendência central
alpha <- 0.15 # Taxa de reversão 
sigma <- 0.2  # Taxa evolutiva
ti <- 1         # tempo final (em milhões de anos)
N <- 10000      # Número de passos no tempo 
dt <- ti / N    # Tamanho do passo (gerações)
ts <- seq(0, t_fim, by = dt)  # Linha do tempo

# Vetor para armazenar os valores do processo
X <- numeric(length(t))
X[1] <- 1 # Valor inicial do processo OU

# Simulação do processo OU
for (i in 2:length(ts)) {
  dW <- rnorm(1, mean = 0, sd = sigma )  # Incremento browniano
  X[i] <- X[i-1] + (alpha * (theta - X[i-1])) * dt + (sigma * dW)
}

# Plot do processo OU
plot(ts, X, type = "l", 
     col = "blue", 
     ain = "Simulação do Processo Ornstein-Uhlenbeck",
     xlab = "Tempo", 
     ylab = "Característica X")
