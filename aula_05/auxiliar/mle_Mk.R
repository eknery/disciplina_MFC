library(emdbook) # Para a função mvrnorm
library(stats4)
library(ape)

############################## FUNÇÃO ML PARA MK ###############################

lnlMK <- function(q12, q21, x1, x2, t) {
  # Matriz de transição
  P = matrix(c(-q12, q12, 
                q21,-q21), nrow = 2, byrow = T)
  # Eleva a matriz de transição à potência t
  Pt <- P
  for (i in 2:t) {
    Pt <- Pt %*% P
  }
  # A probabilidade conjunta dos estados observados
  likelihood <- Qt[x1, x2]
  # Retorna a log-verossimilhança
  lnl <- log(likelihood)
  # Retorna a log-verossimilhança
  return(lnl)
}

###################################### MLE #####################################

## árvore 
text.string<-"((A:0.25,B:0.25):0.75);"
tree = read.tree(text = text.string)
plot(tree)

### valores observados
t = 10
x1 =  1
x2 = 2

### matriz inicial
Q = matrix(c(2, 1, 
             1, 2), nrow = 2, byrow = T)

### achar MLE
mle(minuslogl = lnlMK, 
    start = list(Q = Q, x1 = x1, x2 = x2, t = t), 
    fixed = list(x1 = x1, x2 = x2, t = t),
    method = "BFGS"
)
