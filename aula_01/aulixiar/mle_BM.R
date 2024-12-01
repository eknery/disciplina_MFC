library(emdbook) # Para a função mvrnorm

##################################### FUNÇÃO BM ################################

lnlBM <- function(x1, x2, dt, sigma2) {
  # Média esperada do ancestral igual à média
  mu <- mean(c(x1, x2))
  mean_vector <- c(mu, mu)
  # Matriz de covariância
  cov_matrix <- sigma2 * matrix(c(dt, 0, 
                                  0, dt), 
                                nrow = 2, ncol = 2)
  # Vetor com fenótipo observados
  traits <- c(x1, x2)
  # Calculo da log-verossimilhança
  lnL <- -dmvnorm(x = traits, 
                  mu = mean_vector, 
                  Sigma = cov_matrix, 
                  log = TRUE
                )
  return(lnL)
}

##########################

optimize(f= lnlBM,
         x1 = 0.1,
         x2 = 0.3, 
         dt = 1,
         interval = c(0,10)
         )
