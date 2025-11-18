dados = read.table("C:/Users/201220000417.UFS/Downloads/consumoagua2.txt", header = T)
dados$dado = NULL
attach(dados)
# Correlação
cor.test(dados$consumo, dados$diasconsumo, method = "pearson") # correlação fraca

corrplot::corrplot(cor(dados), method = "number", type = "upper")


# Modelo linear multiplo
modelo1 = lm(valor ~ consumo + diasconsumo + Construindo, data = dados)

# Definição de hipoteses
# H0: Parametros = 0
# H1: Pelo menos 1 dos parametros será diferente de 0

# alpha = 0.05

# Regra de decisão: se p-valor < alpha, logo não possuirei evidências para aceitar H0

# Estatistica de teste: Teste F de Snedecor

summary(modelo)

# Conclusão:
# Intercepto (B0) = -1.7294 (p-valor = 0.88809);
# consumo (B1) = 2.7564 (< 2e-16 ***);
# diasconsumo (B2) = -0.2747 (p-valor = 0.50322);
# Construindo (B3) = 3.6809 (p-valor = 0.00424 **).

modelo2 = lm(valor ~ consumo + diasconsumo + Construindo -1, data = dados) # retirado o intercepto (B0)

# Cálculo de B (manual)
n = length(dados$valor)

X = matrix(c(rep(1,n), dados$consumo, dados$diasconsumo, dados$Construindo), nrow = n, ncol = 4)

X_trans = t(X)

modelo3=lm(valor~consumo+ I(consumo^2))
modelo4=lm(valor~I(diasconsumo^2))

solve(X%*%X_trans) 
#testando Crit´erio de Sele¸c˜ao de modelos
AIC(modelo1,modelo2,modelo3,modelo4)
BIC(modelo1,modelo2,modelo3,modelo4)


#7) Usando Critérios de modelos encaixados
install.packages('lmtest')
library(lmtest)
waldtest(modelo1,modelo2)

#8) Usando criterio de modelos não encaixados
encomptest(modelo1,modelo3)
# Critério de seleção de modelos

m2=lm(valor ~ consumo + diasconsumo + Construindo -1)
# Normalidade dos resíduos
install.packages("tseries")
library(tseries) 

# com dados de consumo de energia 
dados = read.table("C:/Users/201220000417.UFS/Downloads/consumoagua2.txt", header = T)
