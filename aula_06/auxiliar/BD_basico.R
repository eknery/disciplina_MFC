
############################## MODELO PURE BIRTH #############################

## Parâmetros do modelo Pure Birth (Yule)
# Taxa de nascimento
lambda1 = 0.3

## Cenário temporal
t = 0  # tempo atual
ts1 = t  # tempo de todos os eventos
tf = 20 # tempo máximo da simulação

## Cenário de diversidade
n = 1   # número de espécies atual
N1 = n  # número de espécies ao longo da simulação

# Simulação do processo Pure Birth
while (t < tf) {
  # tempos até o próximos evento de nascimento
  t_birth = rexp(1, n * lambda1)
  # atualizando o tempo
  t = t + t_birth
  ts1 = c(ts1, t)
  # atualizando a diversidade
  n = n + 1 # 'nasce' uma espécie
  N1 = c(N1, n)
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
                "; lambda: ", lambda1
  )
)

# Veja o número final de espécies da simulação
Nf1 = N1[length(N1)]
Nf1

############################## MODELO BIRTH-DEATH #############################

## Parâmetros do modelo BD
# Taxa de nascimento
lambda2 = 0.4
# Taxa de morte
mu2 = 0.1

## Cenário temporal
t = 0  # tempo atual
ts2 = t  # tempo de todos os eventos
tf = 20 # tempo máximo da simulação

## Cenário de diversidade
n = 1   # número de espécies atual
N2 = n  # número de espécies ao longo da simulação

# Simulação do processo Birth-Death
while (t < tf) {
  # tempos até os próximos eventos de nascimento e morte
  t_birth = rexp(1, n * lambda2)
  t_death = rexp(1, n * mu2)
  # qual evento ocorre antes?
  ti = min(t_birth, t_death)
  # atualizando o tempo
  t = t + ti
  ts2 = c(ts2, t)
  # atualizando a diversidade
  if (ti == t_birth) {
    n = n + 1 # 'nasce' uma espécie
  } else {
    n = n - 1 # 'morre' uma espécie
  }
  N2 = c(N2, n)
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
Nf2 = N2[length(N2)]
Nf2

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

## EM GRUPO
# Compare o número de espécies geradas pelos modelos:
c("Pure Birth" = Nf1, "BD"= Nf2)

# Vamos armazenar o número de espécies geradas por cada modelo:
Npure = c()
Nbd = c()

# Vamos testar se existe diferença no número de espécies:
t.test(x = Npure, 
       y = Nbd, 
       alternative = "two.sided",
       paired = FALSE, 
       var.equal = FALSE
)

## PARA PENSAR:
# Os modelos produziram um número de espécies distinto?
# Qual o motivo para esse padrão?


## EM GRUPO
# Vamos ajustar uma regressão linear entre N de spécies e tempo:
lm_pure = summary(lm(log(N1)~ ts1))
lm_bd = summary(lm(log(N2)~ ts2))

# Vamos extrair a taxa de diversificação para cada modelo:
r_pure = lm_pure$coefficients[2,1]
r_bd = lm_bd$coefficients[2,1]

# Contrastando estimativas de taxa de diversificação:
c("Pure Birth" = r_pure, "BD"= r_bd)

# Vamos extrair o tempo onde ocorreram especiações:
sptime_pure = ts1[-1]
sptime_bd = ts2[-1][diff(N2) == 1]

# Vamos calcular o tempo de espera entre especiações:
wtsp_pure = diff(ts1)
wtsp_bd = diff(ts2)[diff(N2) == 1]

summary(lm(log(wtsp_pure) ~ sptime_pure))
summary(lm(log(wtsp_bd)   ~ sptime_bd))


plot(sptime_pure, log(wtsp_pure) )
plot(sptime_bd, log(wtsp_bd) )
