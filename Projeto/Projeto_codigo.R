
# Projeto SAD 2024/2025 -  Como o clima afetaria a procura por partilha de bicicletas em áreas urbanas
# Grupo: Guilherme Carvalho 30010987, Hugo Pedro 30010791 e Mario Felix 300

#-------------------------------------------------------------------------------
# Bloco 1: Configuração Inicial e Bibliotecas
#-------------------------------------------------------------------------------
# Descrição:
# Este bloco inicial carrega todas as librarys necessárias para
# a execução do projeto. Cada biblioteca tem um propósito específico, como
# manipulação de dados (dplyr, data.table), leitura de ficheiros (readr),
# interação com APIs (httr, jsonlite), modelação (glmnet, caret) e
# visualização (ggplot2). Também são definidas variáveis para os diretórios
# onde os dados de entrada, os dados processados e os modelos serão guardados.
#-------------------------------------------------------------------------------


# Librarys necessárias
library(rvest)
library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(purrr)
library(data.table)
library(lubridate)
library(stringr)
library(fastDummies)
library(RSQLite)
library(DBI)
library(ggplot2)
library(caTools)
library(glmnet)
library(caret)

diretorio_dados_entrada <- "dados_entrada"
diretorio_dados_processados <- "dados_processados"
diretorio_modelos <- "modelos"

#-------------------------------------------------------------------------------
# Bloco 2: Função de Padronização de Nomes de Colunas
#-------------------------------------------------------------------------------
# Descrição:
# É definida uma função reutilizável chamada 'funcao_padronizar_nomes_colunas'.
# O objetivo desta função é limpar e padronizar os nomes das colunas de qualquer
# dataset que lhe seja passado. Ela converte todos os nomes para minúsculas,
# substitui caracteres especiais por underscores (_) e remove espaços.
#-------------------------------------------------------------------------------
funcao_padronizar_nomes_colunas <- function(dataframe_entrada) {
  if (is.null(dataframe_entrada) || ncol(dataframe_entrada) == 0) return(dataframe_entrada)
  nomes_colunas_originais <- colnames(dataframe_entrada)
  nomes_colunas_novos <- tolower(nomes_colunas_originais)
  nomes_colunas_novos <- gsub("[^a-zA-Z0-9_]", "_", nomes_colunas_novos)
  nomes_colunas_novos <- gsub("_+", "_", nomes_colunas_novos)
  nomes_colunas_novos <- gsub("^_|_$", "", nomes_colunas_novos)
  colnames(dataframe_entrada) <- nomes_colunas_novos
  return(dataframe_entrada)
}
nome_coluna_frota_numerica_gerada <- NA_character_


#-------------------------------------------------------------------------------
# Bloco 3: Leitura e Processamento dos Sistemas de Bicicletas
#-------------------------------------------------------------------------------
# Descrição:
# Este bloco lê o ficheiro "List_of_bicycle-sharing_systems.csv".
# O fluxo de dados é:
# 1. Verificar se o ficheiro existe.
# 2. Ler o ficheiro CSV, tratando de problemas de codificação de caracteres (UTF-8).
# 3. Aplicar a função de padronização aos nomes das colunas.
# 4. A coluna 'bicycles', que contém o tamanho da frota, está em formato de texto. 
#    O código extrai apenas os números desta coluna e cria
#    uma nova coluna numérica ('bicycles_numerica') para facilitar cálculos os calculos mais para a frente.
# 5. O dataframe processado é guardado como um novo ficheiro CSV nos "dados_processados".
#-------------------------------------------------------------------------------
# Ler e limpar Sistemas de Bicicletas

dados_sistemas_bicicletas_proc <- tibble() # Processado/Tratado
ficheiro_sistemas_bicicletas <- file.path(diretorio_dados_entrada, "List_of_bicycle-sharing_systems.csv")

if (file.exists(ficheiro_sistemas_bicicletas)) {
  tipos_colunas_sist_bicicletas <- cols(.default = col_guess())
  verificacao_cabecalho_sist_bicicletas <- read_delim(ficheiro_sistemas_bicicletas, delim = ";", n_max = 0, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
  if ("Bicycles" %in% colnames(verificacao_cabecalho_sist_bicicletas)) {
    tipos_colunas_sist_bicicletas$cols$Bicycles <- col_character()
  }
  
  dados_sistemas_bicicletas_proc <- read_delim(
    ficheiro_sistemas_bicicletas,
    delim = ";",
    locale = locale(encoding = "UTF-8"),
    guess_max = 10000,
    na = c("", "NA", "N/A", "N/D"),
    col_types = tipos_colunas_sist_bicicletas,
    show_col_types = FALSE
  ) %>%
    funcao_padronizar_nomes_colunas() %>%
    setNames(make.unique(colnames(.))) %>%
    as_tibble(.name_repair = "minimal") %>%
    mutate(across(where(is.character), ~ iconv(., from = "UTF-8", to = "UTF-8", sub = "")))
  
  if (nrow(dados_sistemas_bicicletas_proc) > 0) {
    nome_col_frota_padronizado_para_proc <- "bicycles" # Nome da coluna após padronização
    
    if (nome_col_frota_padronizado_para_proc %in% colnames(dados_sistemas_bicicletas_proc)) {
      nome_cand_col_frota_num <- paste0(nome_col_frota_padronizado_para_proc, "_numerica")
      dados_sistemas_bicicletas_proc <- dados_sistemas_bicicletas_proc %>%
        mutate(
          !!sym(nome_cand_col_frota_num) :=
            as.numeric(str_extract(gsub("[.,]", "", as.character(!!sym(nome_col_frota_padronizado_para_proc))), "\\d+"))
        )
      nome_coluna_frota_numerica_gerada <<- nome_cand_col_frota_num # Atribuição global
    } else {
      nome_coluna_frota_numerica_gerada <<- NA_character_
      warning(paste("Coluna padronizada '", nome_col_frota_padronizado_para_proc, "' não encontrada para processar a frota."))
    }
    write_csv(dados_sistemas_bicicletas_proc, file.path(diretorio_dados_processados, "sistemas_bicicletas_processado.csv"))
    print("Sistemas de bicicletas lidos, processados e salvos em dados_processados/.")
  } else {
    nome_coluna_frota_numerica_gerada <<- NA_character_
    print("Ficheiro de sistemas de bicicletas vazio ou não pôde ser lido.")
  }
} else {
  warning(paste("FICHEIRO NÃO ENCONTRADO:", ficheiro_sistemas_bicicletas))
  nome_coluna_frota_numerica_gerada <<- NA_character_
}


#-------------------------------------------------------------------------------
# Bloco 4: Obtenção de Dados da API OpenWeather
#-------------------------------------------------------------------------------
# Descrição:
# Este bloco é responsável por obter dados de previsão do tempo.
# 1. Define a chave da API (API key) e uma lista de cidades de interesse.
# 2. A 'funcao_obter_previsao_tempo' faz um pedido (request) à
#    API da OpenWeather para uma cidade específica. Ela extrai os dados relevantes
#    da resposta JSON e organiza-os
#    num formato de tabela (tibble).
# 3. O código utiliza 'map_dfr' para aplicar a função anterior a todas as cidades da lista,
#    juntando os resultados num único dataset.
# 4. Os dados de data/hora, que vêm como texto, são convertidos para um formato
#    de data/hora padrão (UTC).
# 5. Os dados processados são guardados num ficheiro CSV.
#-------------------------------------------------------------------------------
# API OpenWeather - Previsão do Tempo e Disputa

chave_api_openweathermap <- "db9a7ae8e54a25385c131a18856c3d11" # Mais específico
cidades_previsao <- c("Seoul", "New York", "Paris", "Suzhou", "London")

funcao_obter_previsao_tempo <- function(nome_cidade, chave_api) {
  url_base <- "https://api.openweathermap.org/data/2.5/forecast"
  Sys.sleep(0.5)
  resposta_http <- GET(url_base, query = list(q = nome_cidade, appid = chave_api, units = "metric", lang = "pt"))
  
  if (status_code(resposta_http) == 200) {
    dados_resposta <- content(resposta_http, "parsed", encoding = "UTF-8")
    lista_previsoes_api <- dados_resposta$list
    if (is.null(lista_previsoes_api) || length(lista_previsoes_api) == 0) return(tibble())
    tibble(
      cidade = nome_cidade, # Mantido 'cidade' para corresponder à coluna gerada pela API (após padronização)
      data_hora_texto = map_chr(lista_previsoes_api, ~ purrr::pluck(.x, "dt_txt", .default = NA_character_)),
      temperatura_celsius = map_dbl(lista_previsoes_api, ~ purrr::pluck(.x, "main", "temp", .default = NA_real_)),
      humidade_percentagem = map_dbl(lista_previsoes_api, ~ purrr::pluck(.x, "main", "humidity", .default = NA_real_)),
      tempo_condicao_principal = map_chr(lista_previsoes_api, ~ purrr::pluck(.x, "weather", 1, "main", .default = "Desconhecido")),
      tempo_descricao = map_chr(lista_previsoes_api, ~ purrr::pluck(.x, "weather", 1, "description", .default = "Desconhecido")),
      vento_velocidade_mps = map_dbl(lista_previsoes_api, ~ purrr::pluck(.x, "wind", "speed", .default = NA_real_)),
      pressao_hpa = map_dbl(lista_previsoes_api, ~ purrr::pluck(.x, "main", "pressure", .default = NA_real_)),
      precipitacao_probabilidade = map_dbl(lista_previsoes_api, ~ purrr::pluck(.x, "pop", .default = NA_real_))
    )
  } else {
    warning(paste("Falha API OpenWeather para", nome_cidade, "- Status:", status_code(resposta_http))); return(NULL)
  }
}

resultado_geral_previsoes <- safely(map_dfr)(cidades_previsao, funcao_obter_previsao_tempo, chave_api = chave_api_openweathermap)
dados_previsao_tempo_proc <- tibble() # Processado

if (!is.null(resultado_geral_previsoes$result) && nrow(resultado_geral_previsoes$result) > 0) {
  dados_previsao_tempo_proc <- resultado_geral_previsoes$result %>% funcao_padronizar_nomes_colunas()
  # Após padronização, os nomes de coluna serão, por exemplo, 'data_hora_texto', 'temperatura_celsius', etc.
  if ("data_hora_texto" %in% colnames(dados_previsao_tempo_proc)) {
    dados_previsao_tempo_proc <- dados_previsao_tempo_proc %>%
      mutate(data_hora_utc = ymd_hms(data_hora_texto, tz = "UTC", quiet = TRUE))
  } else if (!("data_hora_utc" %in% colnames(dados_previsao_tempo_proc))) {
    dados_previsao_tempo_proc$data_hora_utc <- NA_POSIXct_
  }
  write_csv(dados_previsao_tempo_proc, file.path(diretorio_dados_processados, "previsao_tempo_cidades_processado.csv"))
  print("Previsão do tempo obtida, processada e salva em dados_processados/.")
} else {
  if(!is.null(resultado_geral_previsoes$error)) warning(paste("Erro API OpenWeather:", resultado_geral_previsoes$error))
  else warning("Nenhum dado de previsão do tempo obtido.")
}

#-------------------------------------------------------------------------------
# Bloco 5: Leitura e Processamento dos Dados das Cidades do Mundo
#-------------------------------------------------------------------------------
# Descrição:
# Este bloco lê o ficheiro das cidades("worldcities.csv").
# 1. O ficheiro é lido e os nomes das colunas são padronizados.
# 2. As colunas 'lat', 'lng' e 'population' são convertidas para o tipo numérico.
# 3. As colunas 'lat' e 'lng' são renomeadas para 'latitude' e 'longitude' para melhor clareza de variaveis
# 4. O resultado final é guardado como um ficheiro CSV processado.
#-------------------------------------------------------------------------------
# Ler e Limpar Cidades do Mundo

dados_cidades_mundo_proc <- tibble() # Processado
ficheiro_cidades_mundo <- file.path(diretorio_dados_entrada, "worldcities.csv")

if (file.exists(ficheiro_cidades_mundo)) {
  dados_cidades_mundo_proc <- read_csv(ficheiro_cidades_mundo, locale = locale(encoding = "UTF-8"), guess_max = 50000, show_col_types = FALSE) %>%
    funcao_padronizar_nomes_colunas() %>%
    setNames(make.unique(colnames(.))) %>%
    as_tibble(.name_repair = "minimal") %>%
    mutate(across(where(is.character), ~ iconv(., from = "UTF-8", to = "UTF-8", sub = "")))
  
  if(nrow(dados_cidades_mundo_proc) > 0){
    colunas_a_converter_para_numerico <- c("lat", "lng", "population") # Nomes após padronização
    for (nome_coluna in colunas_a_converter_para_numerico) {
      if (nome_coluna %in% colnames(dados_cidades_mundo_proc) && !is.numeric(dados_cidades_mundo_proc[[nome_coluna]])) {
        dados_cidades_mundo_proc <- dados_cidades_mundo_proc %>% mutate(!!sym(nome_coluna) := as.numeric(!!sym(nome_coluna)))
      }
    }
    if ("lat" %in% colnames(dados_cidades_mundo_proc)) dados_cidades_mundo_proc <- rename(dados_cidades_mundo_proc, latitude = lat)
    if ("lng" %in% colnames(dados_cidades_mundo_proc)) dados_cidades_mundo_proc <- rename(dados_cidades_mundo_proc, longitude = lng)
    
    write_csv(dados_cidades_mundo_proc, file.path(diretorio_dados_processados, "cidades_mundo_processado.csv"))
    print("Cidades do mundo lidas, processadas e salvas em dados_processados/.")
  } else print(paste("Ficheiro '", ficheiro_cidades_mundo, "' vazio."))
} else warning(paste("FICHEIRO NÃO ENCONTRADO:", ficheiro_cidades_mundo))


#-------------------------------------------------------------------------------
# Bloco 6: Leitura e Processamento dos Dados de Aluguer de Bicicletas de Seul
#-------------------------------------------------------------------------------
# Descrição:
# lê e prepara o conjunto principal de dados para a análise, o ficheiro de Seoul("SeoulBikeData.csv").
# 1. É lido com a função 'fread' por ser mais rápida, e os nomes das colunas
#    são definidos manualmente para garantir consistência.
# 2. A coluna de data é convertida para o formato de data.
# 3. Os nomes das colunas são padronizados usando a função previamente definida.
# 4. As variáveis categóricas como "seasons", "holiday" e "functioning_day" são
#    transformadas em "dummy variables". Isto é feito para
#    que possam ser usadas nos modelos de regressão linear.
# 5. O dataframe final, completamente processado, é guardado num ficheiro CSV.
#-------------------------------------------------------------------------------
# Ler e limpar Dados do Seoul Bike Sharing

dados_bicicletas_seul_proc <- tibble() # Processado
ficheiro_bicicletas_seul <- file.path(diretorio_dados_entrada, "SeoulBikeData.csv")

if (file.exists(ficheiro_bicicletas_seul)) {
  nomes_colunas_seul_originais <- c("Date", "Rented_Bike_Count", "Hour", "Temperature_C", "Humidity_Percent", "Wind_speed_mps",
                                    "Visibility_10m", "Dew_point_temperature_C", "Solar_Radiation_MJ_m2",
                                    "Rainfall_mm", "Snowfall_cm", "Seasons", "Holiday", "Functioning_Day")
  
  dados_bicicletas_seul_inicial <- tryCatch({
    fread(ficheiro_bicicletas_seul, encoding = "UTF-8", showProgress = FALSE) %>%
      setNames(nomes_colunas_seul_originais) %>%
      as_tibble(.name_repair = "minimal") %>%
      mutate(Date = dmy(Date, quiet = TRUE)) # A coluna Date é convertida para o tipo Date
  }, error = function(e) { message(paste("Erro no processamento de SeoulBikeData.csv:", e$message)); return(tibble()) })
  
  if (nrow(dados_bicicletas_seul_inicial) > 0) {
    dados_bicicletas_seul_proc <- dados_bicicletas_seul_inicial
    if ("Seasons" %in% colnames(dados_bicicletas_seul_proc)) {
      dados_bicicletas_seul_proc$estacoes_texto_original <- dados_bicicletas_seul_proc$Seasons
    } else dados_bicicletas_seul_proc$estacoes_texto_original <- NA_character_
    
    dados_bicicletas_seul_proc <- funcao_padronizar_nomes_colunas(dados_bicicletas_seul_proc)
    
    colunas_para_dummies_seul <- c("seasons", "holiday", "functioning_day") # Nomes após padronização
    colunas_efetivas_para_dummies_seul <- intersect(colunas_para_dummies_seul, colnames(dados_bicicletas_seul_proc))
    
    if (length(colunas_efetivas_para_dummies_seul) > 0) {
      dados_bicicletas_seul_proc <- dados_bicicletas_seul_proc %>%
        mutate(across(all_of(colunas_efetivas_para_dummies_seul), as.factor))
      
      if ("seasons" %in% colunas_efetivas_para_dummies_seul) {
        niveis_estacoes_ordem_correta <- c("Winter", "Spring", "Summer", "Autumn")
        niveis_atuais_fator_seasons <- levels(dados_bicicletas_seul_proc$seasons)
        
        # Lógica de re-nivelamento (simplificada, assumindo que str_to_sentence funciona bem)
        levels(dados_bicicletas_seul_proc$seasons) <- str_to_sentence(levels(dados_bicicletas_seul_proc$seasons))
        dados_bicicletas_seul_proc$seasons <- factor(dados_bicicletas_seul_proc$seasons, levels = niveis_estacoes_ordem_correta, ordered = FALSE)
      }
      dados_bicicletas_seul_proc <- dummy_cols(dados_bicicletas_seul_proc,
                                               select_columns = colunas_efetivas_para_dummies_seul,
                                               remove_first_dummy = TRUE,
                                               remove_selected_columns = TRUE)
    }
    write_csv(dados_bicicletas_seul_proc, file.path(diretorio_dados_processados, "dados_bicicletas_seul_processado.csv"))
    print("Dados Seoul Bike Sharing lidos, processados e salvos em dados_processados/.")
  } else print(paste("Ficheiro", ficheiro_bicicletas_seul, "vazio ou erro ao ler."))
} else warning(paste("FICHEIRO NÃO ENCONTRADO:", ficheiro_bicicletas_seul))
print("Recolha e disputa de dados concluída.")


#-------------------------------------------------------------------------------
# Bloco 7: Criação e Carregamento da Base de Dados SQL
#-------------------------------------------------------------------------------
# Descrição:
# Esta secção utiliza SQL para fazer uma análise de dados (EDA).
# 1. É criada uma base de dados SQL
# 2. A 'funcao_carregar_tabela_bd' é definida para carregar os dataframes
#    processados nos blocos anteriores como tabelas na base de dados.
# 3. Cada dataframe é carregado na sua respetiva tabela.
# 4. São definidas funções auxiliares para executar queries SQL de forma segura
#    ('funcao_executar_consulta_sql') e para verificar a existência de tabelas e colunas.
#-------------------------------------------------------------------------------
# Análise Exploratória de Dados com SQL

print("--- Análise SQL ---")
ficheiro_bd <- file.path(diretorio_dados_processados, "projeto_sad_base_dados.db")
if (file.exists(ficheiro_bd)) file.remove(ficheiro_bd)
conexao_sqlite <- dbConnect(RSQLite::SQLite(), ficheiro_bd)

funcao_carregar_tabela_bd <- function(conexao, nome_da_tabela, dataframe_a_carregar, csv_alternativo) {
  df_existe_e_valido <- tryCatch({ !is.null(dataframe_a_carregar) && is.data.frame(dataframe_a_carregar) && nrow(dataframe_a_carregar) > 0 }, error = function(e) FALSE)
  
  if (df_existe_e_valido) {
    dbWriteTable(conexao, nome_da_tabela, dataframe_a_carregar, overwrite = TRUE, row.names = FALSE)
  } else if (file.exists(csv_alternativo)) {
    dataframe_temp_csv <- read_csv(csv_alternativo, guess_max = 50000, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
    if (nrow(dataframe_temp_csv) > 0) dbWriteTable(conexao, nome_da_tabela, dataframe_temp_csv, overwrite = TRUE, row.names = FALSE)
    else warning(paste("CSV alternativo '", csv_alternativo, "' vazio. Tabela '", nome_da_tabela, "' não carregada.", sep=""))
  } else warning(paste("Objeto Dataframe OU CSV alternativo '", csv_alternativo, "' não encontrado/vazio. Tabela '", nome_da_tabela, "' não carregada.", sep=""))
}

funcao_carregar_tabela_bd(conexao_sqlite, "sistemas_bicicletas", dados_sistemas_bicicletas_proc, file.path(diretorio_dados_processados, "sistemas_bicicletas_processado.csv"))
funcao_carregar_tabela_bd(conexao_sqlite, "previsao_tempo_cidades", dados_previsao_tempo_proc, file.path(diretorio_dados_processados, "previsao_tempo_cidades_processado.csv"))
funcao_carregar_tabela_bd(conexao_sqlite, "cidades_mundo", dados_cidades_mundo_proc, file.path(diretorio_dados_processados, "cidades_mundo_processado.csv"))
funcao_carregar_tabela_bd(conexao_sqlite, "aluguer_bicicletas_seul", dados_bicicletas_seul_proc, file.path(diretorio_dados_processados, "dados_bicicletas_seul_processado.csv"))
print(paste("Tabelas na Base de Dados:", paste(dbListTables(conexao_sqlite), collapse=", ")))

# Nomes de colunas (strings) para construir queries SQL - estes referem-se a nomes após padronização
sql_col_cidade <- "city"; sql_col_pais <- "country"; sql_col_lat <- "latitude"; sql_col_lon <- "longitude"; sql_col_pop <- "population"
sql_col_frota <- if (!is.na(nome_coluna_frota_numerica_gerada) && nzchar(nome_coluna_frota_numerica_gerada)) nome_coluna_frota_numerica_gerada else "bicycles_numeric"
sql_col_data <- "date"; sql_col_hora <- "hour"; sql_col_aluguer <- "rented_bike_count"
sql_col_estacoes_orig <- "estacoes_texto_original"; sql_col_temp <- "temperature_c" # Corresponde a 'temperature_c' em aluguer_bicicletas_seul

funcao_executar_consulta_sql <- function(conexao, consulta_sql, nome_tarefa_sql) {
  tryCatch({
    resultado_consulta <- dbGetQuery(conexao, consulta_sql)
    cat(paste("\nResultado SQL Tarefa:", nome_tarefa_sql, "\n"))
    print(resultado_consulta)
    return(invisible(resultado_consulta))
  }, error = function(e) {
    cat(paste("\nErro SQL Tarefa:", nome_tarefa_sql, "\n"))
    print(e$message)
    return(NULL)
  })
}
funcao_tabela_existe_bd <- function(conexao, nome_tabela_a_verificar) { nome_tabela_a_verificar %in% dbListTables(conexao) }
funcao_colunas_existem_tabela_bd <- function(conexao, nome_tabela, nomes_colunas) {
  if (!funcao_tabela_existe_bd(conexao, nome_tabela)) FALSE
  else all(nomes_colunas %in% dbListFields(conexao, nome_tabela))
}

#-------------------------------------------------------------------------------
# Bloco 8: Execução das Tarefas de Análise com SQL
#-------------------------------------------------------------------------------
# Descrição:
# Este bloco executa as 11 tarefas requeridas do Projeto à cerca de busca e filtração de dados em SQL
# Os resultados de cada query são impressos na consola.
#-------------------------------------------------------------------------------
# --- Início das Tarefas SQL Corrigidas ---

# Tarefa 1: Contagem total de registos na tabela de aluguer de bicicletas de Seul.
if (funcao_tabela_existe_bd(conexao_sqlite, "aluguer_bicicletas_seul")) {
  funcao_executar_consulta_sql(conexao_sqlite,"SELECT COUNT(*) AS total_registos FROM aluguer_bicicletas_seul;","1. Contagem total de registos em aluguer_bicicletas_seul")
}

# Tarefa 2: Contagem de horas onde houve alugueres (>0).
if (funcao_colunas_existem_tabela_bd(conexao_sqlite, "aluguer_bicicletas_seul", sql_col_aluguer)) {
  funcao_executar_consulta_sql(conexao_sqlite,sprintf("SELECT COUNT(*) AS horas_com_alugueres FROM aluguer_bicicletas_seul WHERE %s > 0;", sql_col_aluguer),"2. Contagem de horas com alugueres > 0")
}

# Tarefa 3: Obter a primeira previsão meteorológica para Seul da tabela de previsões.
# Colunas em 'previsao_tempo_cidades'
cols_previsao_seul <- c("cidade", "data_hora_utc", "temperatura_celsius")
if (funcao_colunas_existem_tabela_bd(conexao_sqlite, "previsao_tempo_cidades", cols_previsao_seul)) {
  funcao_executar_consulta_sql(conexao_sqlite,"SELECT cidade, data_hora_utc, temperatura_celsius FROM previsao_tempo_cidades WHERE LOWER(cidade) = 'seoul' AND data_hora_utc IS NOT NULL ORDER BY data_hora_utc ASC LIMIT 1;","3. Primeira previsão meteorológica para Seul")
}

# Tarefa 4: Listar as diferentes estações do ano presentes nos dados de Seul.
if (funcao_colunas_existem_tabela_bd(conexao_sqlite, "aluguer_bicicletas_seul", sql_col_estacoes_orig)) {
  funcao_executar_consulta_sql(conexao_sqlite,sprintf("SELECT DISTINCT %s FROM aluguer_bicicletas_seul WHERE %s IS NOT NULL;", sql_col_estacoes_orig, sql_col_estacoes_orig),"4. Estações do ano distintas nos dados de Seul")
}

# Tarefa 5: Determinar o período (primeira e última data) coberto pelos dados de aluguer de Seul.
if (funcao_colunas_existem_tabela_bd(conexao_sqlite, "aluguer_bicicletas_seul", sql_col_data)) {
  funcao_executar_consulta_sql(conexao_sqlite,sprintf("SELECT MIN(%s) AS primeira_data, MAX(%s) AS ultima_data FROM aluguer_bicicletas_seul;", sql_col_data, sql_col_data),"5. Período dos dados de Seul (primeira e última data)")
}

# Tarefa 6: Encontrar o registo (data, hora, contagem) com o maior número de alugueres.
cols_tarefa6 <- c(sql_col_data, sql_col_hora, sql_col_aluguer)
if (funcao_colunas_existem_tabela_bd(conexao_sqlite, "aluguer_bicicletas_seul", cols_tarefa6)) {
  funcao_executar_consulta_sql(conexao_sqlite,sprintf("SELECT %s, %s, %s FROM aluguer_bicicletas_seul ORDER BY %s DESC LIMIT 1;", sql_col_data, sql_col_hora, sql_col_aluguer, sql_col_aluguer),"6. Registo com o maior número de alugueres")
}

# Tarefa 7: Top 10 combinações de estação do ano e hora por média de temperatura e alugueres.
cols_tarefa7 <- c(sql_col_estacoes_orig, sql_col_hora, sql_col_temp, sql_col_aluguer)
if (funcao_colunas_existem_tabela_bd(conexao_sqlite, "aluguer_bicicletas_seul", cols_tarefa7)) {
  funcao_executar_consulta_sql(conexao_sqlite,
                               sprintf("SELECT %s, %s, AVG(%s) AS media_temperatura, AVG(%s) AS media_alugueres FROM aluguer_bicicletas_seul WHERE %s IS NOT NULL GROUP BY 1, 2 ORDER BY media_alugueres DESC LIMIT 10;",
                                       sql_col_estacoes_orig, sql_col_hora, sql_col_temp, sql_col_aluguer, sql_col_estacoes_orig),
                               "7. Top 10 estação-hora por média de alugueres (e temp. média)")
}

# Tarefa 8: Estatísticas de aluguer (média, min, max) por estação do ano. E cálculo do desvio padrão em R.
cols_tarefa8_sql <- c(sql_col_estacoes_orig, sql_col_aluguer)
if (funcao_colunas_existem_tabela_bd(conexao_sqlite, "aluguer_bicicletas_seul", cols_tarefa8_sql)) {
  funcao_executar_consulta_sql(conexao_sqlite,
                               sprintf("SELECT %s, AVG(%s) AS media_alugueres_por_hora, MIN(%s) AS min_alugueres_por_hora, MAX(%s) AS max_alugueres_por_hora FROM aluguer_bicicletas_seul WHERE %s IS NOT NULL GROUP BY 1;",
                                       sql_col_estacoes_orig, sql_col_aluguer, sql_col_aluguer, sql_col_aluguer, sql_col_estacoes_orig),
                               "8. Estatísticas de aluguer por estação (SQL)")
  
  # Calculo do desvio padrão com dplyr 
  if (exists("dados_bicicletas_seul_proc") && is.data.frame(dados_bicicletas_seul_proc) && nrow(dados_bicicletas_seul_proc) > 0 && all(cols_tarefa8_sql %in% names(dados_bicicletas_seul_proc))) {
    cat("\n--- Desvio Padrão dos Alugueres por Estação (R dplyr) ---\n")
    print(dados_bicicletas_seul_proc %>%
            filter(!is.na(.data[[sql_col_estacoes_orig]])) %>%
            group_by(.data[[sql_col_estacoes_orig]]) %>%
            summarise(desvio_padrao_alugueres = sd(.data[[sql_col_aluguer]], na.rm = TRUE), .groups = 'drop'))
  } else {
    cat("\nDataFrame 'dados_bicicletas_seul_proc' não disponível ou colunas em falta para cálculo do desvio padrão em R.\n")
  }
}

# Tarefa 9: Médias de várias condições climáticas e de alugueres, agrupadas por estação do ano.
colunas_climaticas_seul_sql <- c(sql_col_temp, "humidity_percent", "wind_speed_mps", "visibility_10m","dew_point_temperature_c", "solar_radiation_mj_m2", "rainfall_mm", "snowfall_cm")
cols_tarefa9 <- c(sql_col_estacoes_orig, colunas_climaticas_seul_sql, sql_col_aluguer)

if (funcao_colunas_existem_tabela_bd(conexao_sqlite, "aluguer_bicicletas_seul", cols_tarefa9)) {
  query_agregacao_clima <- paste0("AVG(", colunas_climaticas_seul_sql, ") AS media_",gsub("_c$|_percent$|_mps$|_10m$|_mj_m2$|_mm$|_cm$", "", colunas_climaticas_seul_sql),collapse = ", ")
  
  consulta_sql_tarefa9 <- sprintf("SELECT %s, %s, AVG(%s) AS media_alugueres FROM aluguer_bicicletas_seul WHERE %s IS NOT NULL GROUP BY 1 ORDER BY media_alugueres DESC;",
                                  sql_col_estacoes_orig,
                                  query_agregacao_clima,
                                  sql_col_aluguer,
                                  sql_col_estacoes_orig)
  funcao_executar_consulta_sql(conexao_sqlite, consulta_sql_tarefa9, "9. Médias de condições climáticas e alugueres por estação")
}

# Tarefa 10: Informação sobre Seul (cidade, país) da tabela de cidades do mundo, com uma estimativa de frota.
colunas_req_cidades_mundo <- c(sql_col_cidade, sql_col_pais, sql_col_lat, sql_col_lon, sql_col_pop)
if (funcao_colunas_existem_tabela_bd(conexao_sqlite, "cidades_mundo", colunas_req_cidades_mundo)) {
  consulta_sql_tarefa10 <- sprintf("SELECT %s, %s, 20000 AS frota_seul_estimada FROM cidades_mundo WHERE LOWER(%s) = 'seoul' GROUP BY 1, 2 LIMIT 1;",
                                   sql_col_cidade,
                                   sql_col_pais,
                                   sql_col_cidade)
  funcao_executar_consulta_sql(conexao_sqlite, consulta_sql_tarefa10, "10. Informação de Seul (tabela cidades_mundo)")
}

# Tarefa 11: Juntar dados de cidades do mundo com sistemas de bicicletas para encontrar cidades com frota entre 15k e 20k.
# Colunas em 'sistemas_bicicletas': sql_col_cidade ("city"), sql_col_frota (e.g., "bicycles_numerica")
colunas_req_sist_bicicletas <- c(sql_col_cidade, sql_col_frota)
if (funcao_colunas_existem_tabela_bd(conexao_sqlite, "cidades_mundo", colunas_req_cidades_mundo) &&
    funcao_colunas_existem_tabela_bd(conexao_sqlite, "sistemas_bicicletas", colunas_req_sist_bicicletas)) {
  
  consulta_sql_tarefa11 <- sprintf("SELECT wc.%s AS cidade, wc.%s AS pais, bs.%s AS num_bicicletas
                                     FROM cidades_mundo wc
                                     JOIN sistemas_bicicletas bs ON LOWER(wc.%s) = LOWER(bs.%s)
                                     WHERE bs.%s BETWEEN 15000 AND 20000;",
                                   sql_col_cidade,
                                   sql_col_pais,
                                   sql_col_frota,
                                   sql_col_cidade,
                                   sql_col_cidade,
                                   sql_col_frota)
  funcao_executar_consulta_sql(conexao_sqlite, consulta_sql_tarefa11, "11. Cidades com frota de bicicletas entre 15k e 20k")
}

# --- Fim das Tarefas SQL Corrigidas ---

dbDisconnect(conexao_sqlite); print(paste("Conexão SQLite fechada. Ficheiro BD guardado em:", ficheiro_bd,". Fim Fase 9."))

#-------------------------------------------------------------------------------
# Bloco 9: Análise Exploratória de Dados (EDA) com Visualização
#-------------------------------------------------------------------------------
# Descrição:
# Este bloco foca-se na criação de gráficos requeridos pelo documento do projeto e que
# servem para entender visualmente os dados.
# 1. São gerados vários gráficos com a library ggplot2:
#    - Gráficos de pontos para ver a tendência dos alugueres ao longo do tempo.
#    - Um histograma para ver a distribuição da contagem de alugueres.
#    - Gráficos de dispersão para analisar a relação entre alugueres e temperatura, divididos por estação do ano.
#    - Boxplots para mostrar a distribuição dos alugueres por hora do dia e estação.
# 2. Cada gráfico é impresso.
#-------------------------------------------------------------------------------
# EDA com Visualização

print("--- Iniciando Fase 10: Análise Exploratória Visual ---")
dados_seul_para_eda <- tibble()
ficheiro_seul_eda_orig <- file.path(diretorio_dados_entrada, "SeoulBikeData.csv")

if (file.exists(ficheiro_seul_eda_orig)) {
  dados_seul_para_eda <- read_csv(ficheiro_seul_eda_orig, locale=locale(encoding="UTF-8",asciify=TRUE), col_types=cols(Date=col_character()), show_col_types=FALSE)
  colnames(dados_seul_para_eda) <- c("Data_Original","Contagem_Aluguer_Bicicletas","Hora_Original","Temperatura_Celsius","Percentagem_Humidade","Velocidade_Vento_mps","Visibilidade_10m","Temperatura_Ponto_Orvalho_Celsius","Radiacao_Solar_MJ_m2","Precipitacao_mm","Nevao_cm","Estacoes_Ano_Original","Feriado_Original","Dia_Funcionamento_Original")
  
  dados_seul_para_eda <- dados_seul_para_eda %>%
    mutate(
      Data = dmy(Data_Original, quiet=TRUE),
      Hora = factor(Hora_Original, levels=0:23, ordered=TRUE),
      Estacao_Ano = factor(str_to_sentence(Estacoes_Ano_Original), levels=c("Winter","Spring","Summer","Autumn"), ordered=TRUE)
    )
  
  if(nrow(dados_seul_para_eda) > 0){
    print(paste("Feriados (contagem):", sum(dados_seul_para_eda$Feriado_Original=="Holiday",na.rm=T),"; Dia de Funcionamento 'Yes' (contagem):", sum(dados_seul_para_eda$Dia_Funcionamento_Original=="Yes",na.rm=T)))
    print(dados_seul_para_eda %>% group_by(Estacao_Ano) %>% summarize(Total_Precipitacao_mm=sum(Precipitacao_mm,na.rm=T),Total_Nevao_cm=sum(Nevao_cm,na.rm=T),.groups='drop'))
    
    grafico_aluguer_data <- ggplot(dados_seul_para_eda, aes(x=Data, y=Contagem_Aluguer_Bicicletas)) + geom_point(alpha=0.4,size=0.7,color="blue") + labs(title="Contagem de Alugueres vs. Data", x="Data", y="Nº de Alugueres") + theme_minimal()
    grafico_aluguer_data_cor_hora <- ggplot(dados_seul_para_eda, aes(x=Data, y=Contagem_Aluguer_Bicicletas, color=Hora)) + geom_point(alpha=0.5, size=0.7) + scale_color_viridis_d(name="Hora do Dia") + labs(title="Contagem de Alugueres vs. Data (cor por Hora)", x="Data", y="Nº de Alugueres") + theme_minimal()
    grafico_distribuicao_aluguer <- ggplot(dados_seul_para_eda, aes(x=Contagem_Aluguer_Bicicletas)) + geom_histogram(aes(y=after_stat(density)),bins=50,fill="skyblue",alpha=0.7) + geom_density(color="red") + labs(title="Distribuição da Contagem de Alugueres", x="Nº de Alugueres", y="Densidade") + theme_minimal()
    grafico_aluguer_temp_estacao <- ggplot(dados_seul_para_eda, aes(x=Temperatura_Celsius, y=Contagem_Aluguer_Bicicletas, color=Hora)) + geom_point(alpha=0.4,size=0.7) + facet_wrap(~Estacao_Ano) + scale_color_viridis_d(name="Hora do Dia") + labs(title="Alugueres vs. Temperatura (por Estação do Ano e Hora)", x="Temperatura (°C)", y="Nº de Alugueres") + theme_minimal()
    grafico_boxplot_aluguer_hora_estacao <- ggplot(dados_seul_para_eda, aes(x=Hora, y=Contagem_Aluguer_Bicicletas, fill=Hora)) + geom_boxplot(outlier.size=0.5) + facet_wrap(~Estacao_Ano) + scale_fill_viridis_d(guide="none") + labs(title="Boxplot de Alugueres por Hora (por Estação do Ano)", x="Hora do Dia", y="Nº de Alugueres") + theme_minimal()
    
    print(grafico_aluguer_data); print(grafico_aluguer_data_cor_hora); print(grafico_distribuicao_aluguer); print(grafico_aluguer_temp_estacao); print(grafico_boxplot_aluguer_hora_estacao)
    print(paste("Número de dias com neve:",dados_seul_para_eda %>% filter(Nevao_cm > 0) %>% distinct(Data) %>% nrow()))
  } else print("DataFrame 'dados_seul_para_eda' vazio para a Fase 10.")
} else warning(paste("FICHEIRO NÃO ENCONTRADO:", ficheiro_seul_eda_orig, "(Fase 10)"))
print("--- Fim Fase 10 ---")

#-------------------------------------------------------------------------------
# Bloco 10: Modelação com Regressão Linear
#-------------------------------------------------------------------------------
# Descrição:
# Este bloco é onde os modelos de regressão
# são construídos e avaliados.
# 1.  Preparação: Carrega os dados processados de Seoul e seleciona as variáveis
#     que serão usadas nos modelos garantindo que não há valores em falta.
# 2.  Divisão Treino/Teste: Os dados são divididos em dois conjuntos: um de
#     treino (70%) para construir os modelos e um de teste (30%) para avaliá-los.
# 3.  Modelos: Vários modelos de regressão linear são criados:
#     - `reg_modelo_meteorologico`: Usa apenas variáveis do clima.
#     - `reg_modelo_temporal`: Usa apenas variáveis de tempo (hora, estação, feriado).
#     - `reg_modelo_polinomial`: Adiciona termos quadráticos (ex: temperatura ao quadrado)
#       para capturar relações não-lineares.
#     - `reg_modelo_interacoes`: Adiciona termos de interação.
# 4.  Avaliação: Para cada modelo, são feitas previsões no conjunto de teste.
#     O desempenho é medido com o RMSE (Root Mean Square Error), que indica o
#     erro médio das previsões. O objetivo é encontrar o modelo com o menor RMSE.
# 5.  Gravação do Modelo Final: Ápos alguns testes verificamos que o melhor desempenhofoi do
#     modelo de interções('reg_modelo_interacoes'), logo este foi guardado num ficheiro .RData. Este ficheiro
#     será carregado pela aplicação Shiny para fazer previsões em tempo real.
#-------------------------------------------------------------------------------
# Modelos de Regressão

print("--- Iniciando Fase 11: Modelação de Regressão ---")
options(scipen = 999)

fallback_seul_proc <- file.path(diretorio_dados_processados, "dados_bicicletas_seul_processado.csv")

# Usar o dataframe já processado 'dados_bicicletas_seul_proc' se existir, senão carregar
if (!exists("dados_bicicletas_seul_proc") || !is.data.frame(dados_bicicletas_seul_proc) || nrow(dados_bicicletas_seul_proc) == 0) {
  warning(paste("DataFrame 'dados_bicicletas_seul_proc' não encontrado ou vazio. Tentando carregar de", fallback_seul_proc))
  if (file.exists(fallback_seul_proc)) {
    dados_bicicletas_seul_proc <- read_csv(fallback_seul_proc, show_col_types = FALSE)
    if ("hour" %in% names(dados_bicicletas_seul_proc) && !is.numeric(dados_bicicletas_seul_proc$hour)) {
      dados_bicicletas_seul_proc$hour <- as.numeric(as.character(dados_bicicletas_seul_proc$hour))
    }
    if ("estacoes_texto_original" %in% names(dados_bicicletas_seul_proc) && !is.character(dados_bicicletas_seul_proc$estacoes_texto_original)) {
      dados_bicicletas_seul_proc$estacoes_texto_original <- as.character(dados_bicicletas_seul_proc$estacoes_texto_original)
    }
  } else {
    stop(paste("Dados de Seul ('dados_bicicletas_seul_proc' ou", fallback_seul_proc, ") não disponíveis para modelação."))
  }
}

dados_para_modelo_base <- dados_bicicletas_seul_proc %>%
  mutate(
    humidity = ifelse("humidity_percent" %in% names(.), humidity_percent, NA_real_),
    wind_speed_m_s = ifelse("wind_speed_mps" %in% names(.), wind_speed_mps, NA_real_),
    is_holiday = if ("holiday_holiday" %in% names(.)) as.integer(holiday_holiday) else if ("holiday_Holiday" %in% names(.)) as.integer(holiday_Holiday) else 0L, # Considerar ambos os casos para robustez
    season_winter = ifelse(tolower(estacoes_texto_original) == "winter", 1L, 0L),
    season_spring = ifelse(tolower(estacoes_texto_original) == "spring", 1L, 0L),
    season_summer = ifelse(tolower(estacoes_texto_original) == "summer", 1L, 0L),
    season_autumn = ifelse(tolower(estacoes_texto_original) == "autumn", 1L, 0L)
  )

colunas_necessarias_modelo <- c(
  "rented_bike_count", "temperature_c", "humidity", "wind_speed_m_s",
  "solar_radiation_mj_m2", "rainfall_mm", "snowfall_cm", "hour",
  "is_holiday", "season_winter", "season_spring", "season_summer", "season_autumn"
)

colunas_em_falta <- setdiff(colunas_necessarias_modelo, names(dados_para_modelo_base))
if (length(colunas_em_falta) > 0) {
  stop(paste(
    "Faltam colunas para modelação em 'dados_para_modelo_base':",
    paste(colunas_em_falta, collapse = ", "),
    "\nVerifique os nomes e o processamento anterior."
  ))
}

dados_finais_modelo <- dados_para_modelo_base %>%
  select(all_of(colunas_necessarias_modelo)) %>%
  na.omit()

if (nrow(dados_finais_modelo) < 50) { # Aumentar o limite mínimo para dados de treino/teste robustos
  stop("Dataframe para modelação tem poucas linhas após preparação e remoção de NAs.")
}

set.seed(123)
indices_divisao_treino_teste <- createDataPartition(dados_finais_modelo$rented_bike_count, p = 0.7, list = FALSE)
conjunto_dados_treino <- dados_finais_modelo[indices_divisao_treino_teste, ]
conjunto_dados_teste <- dados_finais_modelo[-indices_divisao_treino_teste, ]

print(paste("Dimensões dos dados de treino:", nrow(conjunto_dados_treino), "x", ncol(conjunto_dados_treino)))
print(paste("Dimensões dos dados de teste:", nrow(conjunto_dados_teste), "x", ncol(conjunto_dados_teste)))

# --- Modelos de Regressão ---
reg_modelo_meteorologico <- lm(rented_bike_count ~ temperature_c + humidity + wind_speed_m_s +
                                 solar_radiation_mj_m2 + rainfall_mm + snowfall_cm,
                               data = conjunto_dados_treino)
print("--- Sumário Modelo Meteorológico ---"); summary(reg_modelo_meteorologico)

reg_modelo_temporal <- lm(rented_bike_count ~ hour + is_holiday +
                            season_winter + season_spring + season_summer + season_autumn,
                          data = conjunto_dados_treino)
print("--- Sumário Modelo Temporal ---"); summary(reg_modelo_temporal)

prev_meteorologico <- predict(reg_modelo_meteorologico, newdata = conjunto_dados_teste)
prev_temporal <- predict(reg_modelo_temporal, newdata = conjunto_dados_teste)

rmse_meteorologico <- RMSE(prev_meteorologico, conjunto_dados_teste$rented_bike_count)
rmse_temporal <- RMSE(prev_temporal, conjunto_dados_teste$rented_bike_count)
print(paste("RMSE Modelo Meteorológico:", round(rmse_meteorologico, 2)))
print(paste("RMSE Modelo Temporal:", round(rmse_temporal, 2)))

reg_modelo_polinomial <- lm(rented_bike_count ~ poly(temperature_c, 2) + humidity + wind_speed_m_s +
                              solar_radiation_mj_m2 + rainfall_mm + snowfall_cm +
                              hour + season_winter + season_spring + season_summer + season_autumn +
                              is_holiday,
                            data = conjunto_dados_treino)
print("--- Sumário Modelo Polinomial ---"); summary(reg_modelo_polinomial)

reg_modelo_interacoes <- lm(rented_bike_count ~ temperature_c * hour +
                              humidity + wind_speed_m_s + solar_radiation_mj_m2 +
                              rainfall_mm + snowfall_cm +
                              season_winter + season_spring + season_summer + season_autumn +
                              is_holiday,
                            data = conjunto_dados_treino)
print("--- Sumário Modelo com Interações ---"); summary(reg_modelo_interacoes)

prev_polinomial <- predict(reg_modelo_polinomial, newdata = conjunto_dados_teste)
rmse_polinomial <- RMSE(prev_polinomial, conjunto_dados_teste$rented_bike_count)

prev_interacoes <- predict(reg_modelo_interacoes, newdata = conjunto_dados_teste)
rmse_interacoes <- RMSE(prev_interacoes, conjunto_dados_teste$rented_bike_count)

print("--- Sumário Final dos RMSEs ---")
print(paste("RMSE - Modelo Meteorológico:", round(rmse_meteorologico, 2)))
print(paste("RMSE - Modelo Temporal:", round(rmse_temporal, 2)))
print(paste("RMSE - Modelo Polinomial:", round(rmse_polinomial, 2)))
print(paste("RMSE - Modelo com Interações:", round(rmse_interacoes, 2)))

ficheiro_modelo_final <- file.path(diretorio_modelos, "modelo_regressao_interacoes.RData")
save(reg_modelo_interacoes, file = ficheiro_modelo_final)
print(paste("Modelo 'reg_modelo_interacoes' salvo em", ficheiro_modelo_final))