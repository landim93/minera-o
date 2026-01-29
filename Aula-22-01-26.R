####################################################
###### Aula: Treinamento e teste usando ARIMA 
###### Data: 22/01/2026
###### Analise de series Temporais
####################################################

## Pacotes 
library(fpp3)
library(tseries)

### Serie Original
beer <- aus_production |>
  filter(year(Quarter) >= 1992) |>
  select(Quarter,Beer)

beer 

autoplot(beer,Beer) + 
  geom_line(color = "darkgreen") +
  xlab("Trimestre") + ylab("Produção") +
  theme_minimal()


# 1.1 - Dados Treino e Teste ----------------------------------------------

## Tamanho da amostra
length(beer$Beer)

## Tamanho da amostra de treino
length(beer$Beer)*0.9

## Tamanho da amostra de teste
length(beer$Beer)-66

### Base de treino 
beer_train <- beer |>
  slice_head(n=66)

beer_train

tail(beer_train)

### Base de treino ate 2008 (ciclo completo)
beer_train <- beer |>
  slice_head(n=68)

tail(beer_train)

autoplot(beer_train ,Beer) + 
  geom_line(color = "darkblue") +
  xlab("Trimestre") + ylab("Produção") +
  theme_minimal()

## Teste estacionariedade ----
adf.test(beer_train$Beer)

kpss.test(beer_train$Beer)

unitroot_ndiffs(beer_train$Beer) ## Quantas diferenças?
install.packages("urca")
library(urca)
## Autocorrelaçoes -----

## Serie original
beer_train|>gg_tsdisplay(Beer,
                          plot_type='partial', lag=36) +
  labs(title="Serie original", y="")

### ACF e PACF serie diferenciada
beer_train|>gg_tsdisplay(difference(Beer),
                          plot_type='partial', lag=36) +
  labs(title="Serie diferenciada", y="")

### ACF e PACF serie diferenca sazonal
beer_train|>gg_tsdisplay(difference(Beer,4),
                          plot_type='partial', lag=36) +
  labs(title="Serie diferenca sazonal", y="")


beer_train|>gg_tsdisplay(difference(Beer,4) %>%
                            difference(),
                          plot_type='partial', lag=36) +
  labs(title="Serie primeira diferenca e sazonal", y="")


# Ajustando modelo da classe ARIMA ----------------------------------------

beer_fit <- beer_train |>
  model(sarima211_111 = ARIMA(Beer ~ pdq(2,1,1)+PDQ(1,1,1)),
        sarima101_210 = ARIMA(Beer ~ pdq(1,0,1)+PDQ(2,1,0)),
        sarima310_000 = ARIMA(Beer ~ pdq(3,1,0)+PDQ(0,0,0)),
        stepwise = ARIMA(Beer),# tome cuidado aqui,geralmente o R da direto, mas tem que avaliar no relatório, pelo menos 1 desse modelos de cima
        search = ARIMA(Beer, stepwise = FALSE, approx = FALSE))

beer_fit

beer_fit |> pivot_longer(everything(), 
                          names_to = "Model name",
                          values_to = "Orders")

## Acuracia dos modelos
accuracy(beer_fit)

## AIC, AICc e BIC
glance(beer_fit)

## AIC
glance(beer_fit) |> arrange(AIC) |>  
  mutate(AIC  = formatC(AIC, format = "f", digits = 6)) |>
  select(.model,AIC) 

## Valores dos parametros
beer_fit|> select(stepwise) |> report()

### Adequacao do modelo ====

beer_fit|> select(stepwise) |>
  gg_tsresiduals(lag=36)

augment(beer_fit)

## Teste Ljung–Box
# Dica: Não sazonais lag=10 e sazonais lag = 2s com s o periodo sazonal
augment(beer_fit) |>
  filter(.model == "stepwise") |>
  features(.innov, ljung_box, lag=8, dof=2) # dof = graus de liberdade

## Teste normalidade
sarimastep_res<-augment(beer_fit) |>
  filter(.model == "stepwise") |>
  select(.innov)
shapiro.test(sarimastep_res$.innov)

### Previsao pelo modelo adequado ====

beer_previsao <- beer_fit |>
  forecast(h = 6) # 6 tamanho dos dados de Teste

beer_previsao |>
  filter(.model=='stepwise')|>
  autoplot(beer,
           level=NULL) +
  labs(y = "Producao",
       title = "Previsao da produção trimestral de cerveja") +
  theme_minimal()

## Usando bootstrap (nao normalidade)

beer_previsao <- beer_fit |>
  forecast(h = 6,bootstrap = TRUE,times=1000)

beer_previsao |>
  hilo(level = c(80, 95)) |>
  as_tibble()

beer_previsao |>
  filter(.model=='stepwise')|>
  autoplot(beer,
           level=c(95)) +
  labs(y = "Producao",
       title = "Previsao da produção trimestral de cerveja") +
  theme_minimal()+
  theme(legend.position = "none")


## Previsao e valores ajustados  

autoplot(beer,.vars = Beer)+
  geom_line(aes(y = .mean,colour="Previsao"),
            linetype="dashed",
            data=beer_previsao |>
              filter(.model=='stepwise'))+
  geom_line(aes(y = Beer,colour="Original"),
            data=augment(beer_fit) %>%
              filter(.model == "stepwise"))+
  geom_line(aes(y = .fitted,colour="SARIMA(0,0,1)(0,1,1)[4]"),
            linetype="dashed",
            data=augment(beer_fit) %>%
              filter(.model == "stepwise"))+
  labs(x="Meses",y="Producao")+
  scale_color_manual(values=c("Previsao"="blue","Original"="black",
                              "SARIMA(0,0,1)(0,1,1)[4]"="red"
  ))+
  guides(colour=guide_legend(title="Serie"))+
  theme_minimal()+
  theme(legend.position = "bottom")

## Medindo acuracia das previsoes todos os modelos
accuracy(beer_previsao, beer) 

## Medindo acuracia das previsoes apenas stepwise
accuracy(beer_previsao, beer) %>%
  filter(.model=="stepwise")

# 1.2 - Validacao Cruzada -------------------------------------------------

length(beer_train$Beer)*0.9 

## Visualizar a validacao
validacao <-beer_train |>
  stretch_tsibble(.init = 61, .step = 1)

validacao

## Quantas validacoes?
table(validacao$.id)

          ## Validacao dos modelos 
validacao_fit <-beer_train |>
  stretch_tsibble(.init = 61, .step = 1)|>
  model(arima  = ARIMA(Beer),
        sarima310_000 = ARIMA(Beer ~ pdq(3,1,0)+PDQ(0,0,0)),# no trabalho da disciplina não usar esse modelo, apenas os outros 3
        naive  = NAIVE(Beer),
        snaive = SNAIVE(Beer))

validacao_fit 

## Previsao um passo a frente
validacao_fit |>
  forecast(h = 1) |>
  accuracy(beer_train)

## Ajuste final
fit_final <- beer_train |>
  model(
    arima  = ARIMA(Beer),
    sarima310_000 = ARIMA(Beer ~ pdq(3,1,0)+PDQ(0,0,0)),
    naive  = NAIVE(Beer),
    snaive = SNAIVE(Beer)
  )

## Previsoes 6 passos a frente
fc_test <- fit_final |>
  forecast(h = 6)

## Acuracia dos modelos 
accuracy(fc_test, beer) 

## Grafico comparando modelos 
autoplot(beer,.vars = Beer)+
  geom_line(aes(y = .mean,colour="Arima"),
            linetype="dashed",
            data=fc_test |>
              filter(.model=='arima'))+
  geom_line(aes(y = .mean,colour="Sarima310_000"),
            linetype="dashed",
            data=fc_test |>
              filter(.model=='sarima310_000'))+
  geom_line(aes(y = .mean,colour="Naive"),
            linetype="dashed",
            data=fc_test |>
              filter(.model=='naive'))+
  geom_line(aes(y = .mean,colour="SNaive"),
            linetype="dashed",
            data=fc_test |>
              filter(.model=='snaive'))+
  labs(x="Meses",y="Producao")+
  scale_color_manual(values=c("Arima"="blue",
                              "Sarima310_000"="darkred",
                              "Naive"="darkgreen",
                              "SNaive"="red"
  ))+
  guides(colour=guide_legend(title="Modelos"))+
  theme_minimal()+
  theme(legend.position = "bottom")

## Será o sarima310_000? 

fit_final|> select(sarima310_000) |>
  gg_tsresiduals(lag=36)

augment(fit_final) |>
      filter(.model == "sarima310_000") |>
      features(.innov, ljung_box, lag=8, dof=3) # dof = graus de liberdade
