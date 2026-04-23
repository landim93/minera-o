install.packages("ipeadatar")
install.packages("ggplot2")
install.packages("scales")
library(ipeadatar)
library(dplyr)
library(ggplot2)
library(scales)
View(ipeadatar::available_series())
ipeadatar::ipeadata(code = "ANDA_PFERTILIZ")
dados <- ipeadata(code = "ANDA_PFERTILIZ")
head(dados)
str(dados)
################# Analise descritiva dos dados#######
summary(dados$value)

mean(dados$value, na.rm = TRUE)
median(dados$value, na.rm = TRUE)
sd(dados$value, na.rm = TRUE)
min(dados$value, na.rm = TRUE)
max(dados$value, na.rm = TRUE)

###### gráfico da série temporal###########
ggplot(dados, aes(x = date, y = value)) +
  geom_line(color = "blue") +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = "Série temporal - Fertilizantes",
       x = "Ano",
       y = "Valor") +
  theme_minimal()
#######################Distribuição dos dados###################
ggplot(dados, aes(x = value)) +
  geom_histogram(bins = 30, fill = "darkgreen", color = "white") +
  scale_x_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = "Distribuição dos valores",
       x = "Valor",
       y = "Frequência") +
  theme_minimal()
