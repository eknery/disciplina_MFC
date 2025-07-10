library(MASS) # Para a função mvrnorm

likelihood_ou_two_species <- function(x1, x2, t, s, sigma2, alpha, theta) {
  # Média esperada (assume atração para o valor ótimo theta)
  mean_vector <- c(theta, theta)
  
  # Calcula os elementos da matriz de covariância
  # Variância
  v <-  (1 - exp(-2 * alpha * t)) 
  # Covariância
  c <-  (1 - exp(-2 * alpha * s) ) * ( exp(-2 * alpha * (t-s) )) 
  
  # Matriz de covariância
  vcv <- (sigma2 / (2 * alpha)) * matrix(c(v, c, 
                                           c, v), 
                                         nrow = 2, ncol = 2)
  
  # Vetor de traços observados
  traits <- c(x1, x2)
  
  # Calcula a log-verossimilhança
  lnL <- -dmvnorm(x = traits, 
                  mu = mean_vector, 
                  Sigma = vcv, 
                  log = TRUE
                  )
  
  return(lnL)
}

