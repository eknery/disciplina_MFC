library(MASS) # Para a função mvrnorm

likelihood_ou_two_species <- function(x1, x2, dt, sigma2, alpha, theta) {
  # Média esperada (assume atração para o valor ótimo theta)
  mean_vector <- c(theta, theta)
  
  # Calcula os elementos da matriz de covariância
  v <- (sigma2 / (2 * alpha)) * (1 - exp(-2 * alpha * dt)) # Variância
  c <- (sigma2 / (2 * alpha)) * (1 - exp(-alpha * dt))     # Covariância
  
  # Matriz de covariância
  cov_matrix <- matrix(c(v, c, 
                         c, v), 
                       nrow = 2, ncol = 2)
  
  # Vetor de traços observados
  traits <- c(x1, x2)
  
  # Calcula a log-verossimilhança
  log_likelihood <- dmvnorm(traits, mean = mean_vector, sigma = cov_matrix, log = TRUE)
  
  return(log_likelihood)
}

sigma2 = c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)
alpha = 0.1

plot(x = sigma2 ,
     y = (sigma2 / (2 * alpha)) * (1 - exp(-2 * alpha * dt))
     )
