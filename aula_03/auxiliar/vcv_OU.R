############################### EFEITO DO ALPHA NA VCV ########################

# tempo total das linhagens
t = 1
# tempo compartilhado
s = 0.1
# taxa evolutiva
sigma2 = 0.6

## diferentes valores de alpha
alpha_vals = c(0, 0.2 ,0.4, 0.6, 0.8, 1, 2, 4)

## plot
par(mfrow = c(1,2))
plot(x = alpha_vals,
     y = (sigma2 / (2 * alpha_vals)) * (1 - exp(-2 * alpha_vals * t)),
     col = "blue",
     xlab = "alfa",
     ylab = "Variância"
)

plot(x = alpha_vals,
     y = (sigma2 / (2 * alpha_vals)) * (1 - exp(-2 * alpha_vals * s) ) * ( exp(-2 * alpha_vals * (t-s) )),
     col = "red",
     xlab = "alfa",
     ylab = "Covariância"
)
