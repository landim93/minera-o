# Projeto- Preparação de Datasets para Análise de Risco
# Opção escolhida: 1 - Micro-Segmentos de Risco (Pessoa jurídica)
# UF escolhida: Alagoas (AL)

# Carregar as bibliotecas necessárias
#Definir mirror do CRAN
options(repos = c(CRAN = "https://cloud.r-project.org/"))
install.packages("tinytex")
install.packages("quarto")
tinytex::install_tinytex()
pkgbuild::has_build_tools(debug = TRUE)

install.packages("dplyr")
install.packages("tibble")
library(duckdb)
library(dplyr)
library(tibble)
quarto::quarto_install_tinytex()


# Configurar conexão com DuckDB (banco de dados em memória)
con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")

# Definir o caminho para o arquivo CSV baixado
# SUBSTITUA ESTE CAMINHO PELO CAMINHO REAL DO SEU ARQUIVO
caminho_csv <- "C:/Users/Katia/OneDrive/Documentos/marcelo/mineração de dados/minera-o/planilha_2025/planilha_202508.csv"  

# Consulta SQL para filtrar os dados
# Filtros aplicados: UF = 'AL' AND cliente = 'PJ'
consulta_sql <- "
SELECT 
    *
FROM 
    read_csv_auto(?) 
WHERE 
    UF = 'AL' 
    AND cliente = 'PJ'
"

# Executar a consulta e carregar os resultados em um dataframe
df_filtrado <- dbGetQuery(con, consulta_sql, list(caminho_csv))

# Fechar a conexão com o banco de dados
dbDisconnect(con)

# Análise exploratória do dataframe resultante
# Visualização rápida da estrutura dos dados
cat("=== ESTRUTURA DO DATAFRAME (glimpse) ===\n")
glimpse(df_filtrado)

# Estatísticas descritivas das variáveis numéricas
cat("\n=== ESTATÍSTICAS DESCRITIVAS (summary) ===\n")
summary(df_filtrado)

# Verificação adicional dos filtros aplicados
cat("\n=== VERIFICAÇÃO DOS FILTROS ===\n")
cat("Valores únicos na coluna UF:", unique(df_filtrado$UF), "\n")
cat("Valores únicos na coluna cliente:", unique(df_filtrado$cliente), "\n")
cat("Dimensões do dataframe (linhas x colunas):", dim(df_filtrado), "\n")
