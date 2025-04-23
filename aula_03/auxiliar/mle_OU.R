library(MASS) # Para a função mvrnorm

likelihood_ou_two_species <- function(x1, x2, t, s, sigma2, alpha, theta) {
  # Média esperada (assume atração para o valor ótimo theta)
  mean_vector <- c(theta, theta)
  
  # Calcula os elementos da matriz de covariância
  v <-  (1 - exp(-2 * alpha * t)) # Variância
  c <-  (1 - exp(-2 * alpha * s) * exp(-2 * alpha * (t-s) )) # Covariância
  
  # Matriz de covariância
  cov_matrix <- (sigma2 / (2 * alpha)) * matrix(c(v, c, 
                                                  c, v), 
                                                 nrow = 2, ncol = 2)
  
  # Vetor de traços observados
  traits <- c(x1, x2)
  
  # Calcula a log-verossimilhança
  lnL <- -dmvnorm(x = traits, 
                  mu = mean_vector, 
                  Sigma = cov_matrix, 
                  log = TRUE
                  )
  
  return(lnL)
}

t = 1
s = 0.5
sigma2 = c(0.1)
alpha = c(0.1, 0.2, 0.3, 0.4, 0.5)

plot(x = alpha,
     y = (sigma2 / (2 * alpha)) * (1 - exp(-2 * alpha * t)),
     ylab = "Var(Y)"
     )

plot(x = alpha,
     y = (1 - exp(-2 * alpha * t)),
     )

