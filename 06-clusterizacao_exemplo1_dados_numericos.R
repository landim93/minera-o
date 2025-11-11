#####################################################################
#            Clusterização de Dados Numéricos: Caso 1               #
#####################################################################

# Vamos usar dados do Kaggle para egmentar clientes de um shopping em grupos
# distintos com base em seus perfis de gastos e demografia para direcionar
# campanhas de marketing.
#
# Os dados estão disponíveis em:
# https://www.kaggle.com/datasets/vjchoudhary7/customer-segmentation-tutorial-in-python
#
# Vamos usar as variáveis:
# - Age (Numérica): idade do cliente
# - Annual Income (k$) (Numérica): renda anual em milhares de dólares
# - Spending Score (1-100) (Numérica): escore do cliente baseado em suas compras


# Parte 1: Carregando Pacotes 
# -------------------------------------------------------------------
library(tidyverse)  # Ecossistema para manipulação de dados e gráficos (ggplot2, dplyr...)
library(tidymodels) # Ecossistema para modelagem, incluindo o 'recipes'
library(factoextra) # Visualização de resultados de cluster (fviz_cluster, fviz_nbclust)
library(cluster)    # Algoritmos de cluster (pam, daisy)
library(NbClust)    # Para ajudar a determinar o número de clusters por vários métodos


# Parte 2: Carregando os Dados 
# -------------------------------------------------------------------
dados_mall <- read_csv("C:/Users/201220000417/Documents/minera-o/aula 5-mineração/Mall_Customers.csv")
glimpse(dados_mall)


# Selecionando apenas as colunas de interesse para clusterização
dados_mall_clusterizacao <- dados_mall |>
  select(Idade = Age,
         renda_anual = `Annual Income (k$)`,
         pontuacao_de_gastos = `Spending Score (1-100)`)

glimpse(dados_mall_clusterizacao) # Confirmar se todas as variáveis são numéricas


# Parte 3: Pré-processamento de Dados
# -------------------------------------------------------------------

# Pré-processamento com `recipes`

# Vamos padronizar os dados para poder usar um método de clusterização
receita_padronizacao <- recipe(~ ., data = dados_mall_clusterizacao) |>
  # step_normalize padroniza (z-score: média=0, dp=1)
  step_normalize(all_numeric_predictors())

# Preparando e aplicando a receita
receita_preparada <- prep(receita_padronizacao)
dados_mall_padronizados <- bake(receita_preparada, new_data = NULL)

# Verificando se se há NAs e as variáveis estão com média 0 e variância 1
skimr::skim(dados_mall_padronizados)


# Parte 4: Escolha do método de clusterização
# -------------------------------------------------------------------
# Boxplot dos dados
# Se observarmos muitos outliers extremos, temos um forte argumento para
# abandonar o K-Means e usar uma alternativa robusta, como o K-Medoids.
par(mfrow = c(2,2)) # dividir a área de plotagem em 4 quadrantes
dados_mall_padronizados |> select(Idade) |> boxplot()
dados_mall_padronizados |> select(renda_anual) |> boxplot()
dados_mall_padronizados |> select(pontuacao_de_gastos) |> boxplot()
par(mfrow = c(1,1)) # voltar a 1 único quadrante na área de plotagem

# Como há apenas um outlier para a variável renda_anual, vamos usar K-Means.


# Parte 5: K-Means - Encontrando o k ótimo
# -------------------------------------------------------------------

## 1. Método da Silhueta Média
# COMO LER: Procuramos o valor de 'k' que MAXIMIZA a silhueta média.
# É o método mais robusto e recomendado para K-Medoids.
set.seed(123)
fviz_nbclust(dados_mall_padronizados, kmeans, method = "silhouette", k.max = 10) +
  labs(subtitle = "Método da Silhueta para K-Medoids (PAM)")
# Este método está indicando 8 clusters


## 2. Método do Cotovelo (WCSS / Dissimilaridade Interna)
# COMO LER: Procuramos o "cotovelo" (elbow), o ponto onde 
# adicionar mais um cluster não traz uma redução significativa 
# na dissimilaridade total interna (WCSS).
set.seed(123)
fviz_nbclust(dados_mall_padronizados, kmeans, method = "wss", k.max = 10) +
  geom_vline(xintercept = 5, linetype = 2) + # Adicionando linha do resultado da Silhueta
  labs(subtitle = "Método do Cotovelo (WCSS) para K-Medoids (PAM)")
# Este método está indicando 6 clusters


## 3. Método da Estatística Gap
# COMO LER: Procuramos o valor de 'k' que MAXIMIZA a Estatística Gap.
# Este método compara a WCSS dos seus dados com a WCSS de dados 
# aleatórios (a "distribuição nula").
# NOTA: Este método pode ser BEM LENTO.
set.seed(123)
fviz_nbclust(dados_mall_padronizados, kmeans, method = "gap_stat", nboot = 100, k.max = 10) +
  labs(subtitle = "Método da Estatística Gap para K-Medoids (PAM)")
# Este método está indicando 6 clusters


# 4. Usando vários métodos simultâneos para determinar k
set.seed(123)
res.nbclust <- NbClust(dados_mall_padronizados, distance = "euclidean",
                       min.nc = 2, max.nc = 10, 
                       method = "complete", index ="all")
# De acordo com a maioria dos métodos, o melhor número de clusters é 4.


# Parte 6: K-meanss - Aplicação do Método
# -------------------------------------------------------------------
# Modelagem K-Means com 4 clusters
set.seed(123)
# Vamos colocar para testar 1000 pontos iniciais
modelo_kmeans <- kmeans(dados_mall_padronizados, centers = 4, nstart = 1000)

# Visualização dos clusters
fviz_cluster(modelo_kmeans, data = dados_mall_padronizados,
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_minimal()) +
  labs(title = "Clusters K-Means - Clientes do Shopping")

# Interpretação dos centróides (eles estão na escala padronizada)
# Para interpretar, podemos ver as médias originais por cluster
dados_mall_clusterizacao |>
  mutate(Cluster = modelo_kmeans$cluster) |>
  group_by(Cluster) |>
  summarise(
    Media_Idade = mean(Idade),
    Media_Renda_Anual = mean(renda_anual),
    Media_Gasto_Score = mean(pontuacao_de_gastos),
    Contagem = n()
  )

# Interpretação dos clusters:
# Cluster 1 - Jovens-adultos com alta renda e alto gasto (40 clientes)
# Cluster 2 - Adultos de alta renda, porém baixo gasto (38 clientes)
# Cluster 3 - Mais velhos, renda e gasto médios (65 clientes)
# Cluster 4 - Jovens com renda mais baixa, mas gasto relativamente alto

# Uma dica é jogar a saída com as médias das variáveis por cluster no ChatGPT e 
# pedir que ele caracterize os clusters encontrados.

