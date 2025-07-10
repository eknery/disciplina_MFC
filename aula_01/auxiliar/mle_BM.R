library(emdbook) # Para a função mvrnorm
library(stats4)
library(ape)

############################### FUNÇÃO ML PARA BM ##############################

lnlBM <- function(sigma_sq, x1, x2, t, s) {
  # Vetor com fenótipo observados
  traits <- c(x1, x2)
  # Fenótipo ancestral igual à média
  mu <- mean(c(x1, x2))
  mean_vector <- c(mu, mu)
  # Matriz de covariância
  vcv <- sigma_sq * matrix(c(t, s, 
                             s, t), 
                           nrow = 2, ncol = 2)
  # Calculo da log-verossimilhança
  lnl <- -dmvnorm(x = traits, 
                  mu = mean_vector, 
                  Sigma = vcv, 
                  log = TRUE
                )
  # Retorna a log-verossimilhança
  return(lnl)
}

###################################### MLE ###################################

## árvore 
text.string<-"((A:0.25,B:0.25):0.75);"
tree = read.tree(text = text.string)
plot(tree)

### valores observados
t = 1
s = 0.75
x1 =  1.5
x2 = 0.5

### achar MLE
mle(minuslogl = lnlBM, 
    start = list(sigma_sq = 1, x1 = x1, x2 = x2, t = t, s = s), 
    fixed = list(x1 = x1, x2 = x2, t = t, s = s),
    method = "BFGS"
    )