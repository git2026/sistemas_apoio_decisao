#-------------------------------------------------------------------------------
# Bloco 1: Configuração Inicial da App
#-------------------------------------------------------------------------------
# Descrição:
# Este bloco carrega as librarys necessárias para a aplicação Shiny,
# como 'shiny' para a base, 'shinydashboard' para o layout,
# 'leaflet' para o mapa interativo, e 'ggplot2' para os gráficos.
# Também define a chave da API da OpenWeather, que será usada para obter
# dados de previsão do tempo em tempo real.
#-------------------------------------------------------------------------------
# app.R

# Carregar Librarys
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(lubridate)
library(httr)
library(jsonlite)
library(leaflet)
library(scales)
library(purrr)
library(DT)


options(scipen = 999)
CHAVE_API_OPENWEATHER <- "db9a7ae8e54a25385c131a18856c3d11"

#-------------------------------------------------------------------------------
# Bloco 2: Carregamento do Modelo de Regressão
#-------------------------------------------------------------------------------
# Descrição:
# Este bloco carrega o modelo de regressão ('reg_modelo_interacoes')
# que foi treinado e guardado pelo script 'Projeto_codigo.R'.
# O modelo é carregado de um ficheiro .RData e armazenado na variável
# 'modelo_predicao_bicicletas'..
#-------------------------------------------------------------------------------
# Carregar o modelo de regressão já treinado
modelo_predicao_bicicletas <- NULL
caminho_modelo <- file.path("modelos", "modelo_regressao_interacoes.RData")

if (file.exists(caminho_modelo)) {
  nome_objeto_no_ficheiro <- load(caminho_modelo)
  if ("reg_modelo_interacoes" %in% nome_objeto_no_ficheiro) {
    modelo_predicao_bicicletas <- get("reg_modelo_interacoes")
    print("Modelo 'modelo_regressao_interacoes.RData' carregado com sucesso.")
  } else {
    warning(paste("Ficheiro de modelo encontrado, mas o objeto 'reg_modelo_interacoes' não existe nele."))
  }
} else {
  warning(paste("ERRO CRÍTICO: Ficheiro do modelo '", caminho_modelo, "' não encontrado."))
}

#-------------------------------------------------------------------------------
# Bloco 3: Funções de API e Dados Estáticos
#-------------------------------------------------------------------------------
# Descrição:
# Este bloco define os componentes de dados para a aplicação.
# 1. 'funcao_obter_previsao_tempo_app': Uma versão da função de API do 
#     ficheiro de código principla("Projeto_codigo.R")
#    , otimizada para a app. Busca a previsão do tempo para uma cidade
#    e extrai os dados necessários para o modelo.
# 2. 'dados_cidades_app': Um dataframe que contém as informações
#    das cidades que o utilizador pode selecionar no dashboard (nome,
#    coordenadas geográficas, e o nome a ser usado na chamada da API).
#-------------------------------------------------------------------------------
# Função de Previsão meteorológica 
funcao_obter_previsao_tempo_app <- function(nome_cidade_api, chave_api) {
  url_base <- "https://api.openweathermap.org/data/2.5/forecast"
  resposta_http <- GET(url_base, query = list(q = nome_cidade_api, appid = chave_api, units = "metric", lang = "pt"))
  
  `%||%` <- function(a, b) if (!is.null(a) && length(a) == 1 && !is.na(a)) a else b
  
  if (status_code(resposta_http) == 200) {
    dados_resposta <- content(resposta_http, "parsed", encoding = "UTF-8")
    lista_previsoes_api <- dados_resposta$list
    if (is.null(lista_previsoes_api) || length(lista_previsoes_api) == 0) return(tibble())
    
    df_previsao <- tibble(
      data_hora_texto = purrr::map_chr(lista_previsoes_api, "dt_txt", .default = NA_character_),
      temperature_c = purrr::map_dbl(lista_previsoes_api, c("main", "temp"), .default = NA_real_),
      humidity_percent = purrr::map_dbl(lista_previsoes_api, c("main", "humidity"), .default = NA_real_),
      wind_speed_mps = purrr::map_dbl(lista_previsoes_api, c("wind", "speed"), .default = NA_real_),
      rainfall_mm = purrr::map_dbl(lista_previsoes_api, c("rain", "3h"), .default = 0),
      snowfall_cm = (purrr::map_dbl(lista_previsoes_api, c("snow", "3h"), .default = 0)) / 10,
      # Usar a sintaxe de fórmula com pluck para extrair corretamente de caminhos mistos
      descricao_tempo = purrr::map_chr(lista_previsoes_api, ~ purrr::pluck(.x, "weather", 1, "description", .default = "desconhecido")),
      
      nebulosidade = purrr::map_dbl(lista_previsoes_api, c("clouds", "all"), .default = 50)
    )
    df_previsao$data_hora_utc <- ymd_hms(df_previsao$data_hora_texto, tz = "UTC")
    df_previsao <- df_previsao %>%
      select(-data_hora_texto) %>%
      filter(!is.na(data_hora_utc))
    return(df_previsao)
  } else {
    warning(paste("Falha API para:", nome_cidade_api, "- Status:", status_code(resposta_http)))
    return(tibble())
  }
}


dados_cidades_app <- tibble(
  nome_exibicao = c("Nova York", "Paris", "Suzhou", "Londres"),
  nome_api = c("New York,US", "Paris,FR", "Suzhou,CN", "London,UK"),
  lat = c(40.7128, 48.8566, 31.3040, 51.5074),
  lon = c(-74.0060, 2.3522, 120.6164, -0.1278)
)

#-------------------------------------------------------------------------------
# Bloco 4: Definição da Interface do Utilizador (UI)
#-------------------------------------------------------------------------------
# Descrição:
# A variável 'ui' define a aparência do dashboard.
# - 'dashboardPage': Cria a estrutura com um cabeçalho e um corpo.
# - 'fluidRow' e 'box': Organizam os elementos em linhas e caixas.
# - 'selectInput': Cria o menu dropdown para o utilizador escolher uma cidade.
# - 'leafletOutput': Define um espaço onde o mapa interativo será renderizado.
# - 'tabBox', 'plotOutput', 'DTOutput': Criam uma caixa com tabs, uma para
#   o gráfico de previsão ao longo do tempo e outra para a tabela de dados detalhados.
# O layout foi desenhado para ser limpo e focado nos controlos e visualizações.
#-------------------------------------------------------------------------------
ui <- dashboardPage(
  skin = "purple", 
  dashboardHeader(title = "Procura de Bicicletas"),
  
  dashboardSidebar(disable = TRUE), 
  
  dashboardBody(
    tags$head(tags$style(HTML("
        .content-wrapper, .right-side { background-color: #f5f5f5; } 
        .shiny-notification { position:fixed; top:10%; left:calc(50% - 175px); width:350px; opacity:0.95; font-size:1.0em; z-index:9999; }
        "))),
    fluidRow(
      box(
        title = "Painel de Controlo",
        status = "primary", 
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        
        selectInput("cidade_selecionada_api", "Escolha uma Cidade para Análise:",
                    choices = setNames(dados_cidades_app$nome_api, dados_cidades_app$nome_exibicao),
                    selected = dados_cidades_app$nome_api[1],
                    width = "400px"),
        
        tags$p(HTML("<strong>Nota:</strong> As previsões são baseadas num modelo treinado com dados de Seul. A sua aplicabilidade a outras cidades é uma aproximação."))
      )
    ),
    fluidRow(
      tabBox(
        title = textOutput("titulo_detalhes_cidade"),
        id = "tabset_detalhes",
        width = 12,
        tabPanel("Gráfico de Previsão",
                 plotOutput("grafico_demanda_tempo", height = "450px")
        ),
        tabPanel("Tabela de Dados",
                 DTOutput("tabela_dados_previsao")
        )
      )
    ),
    fluidRow(
      box(title = "Mapa de Procura Máxima Prevista",
          width = 12, status = "primary", solidHeader = TRUE,
          leafletOutput("mapa_demanda", height = "450px")
      )
    )
  )
)

#-------------------------------------------------------------------------------
# Bloco 5: Lógica do Servidor (Server)
#-------------------------------------------------------------------------------
# Descrição:
# A função 'server' reage às ações do utilizador e atualiza as saídas (gráficos, mapas).
# 1. 'funcao_preparar_dados_modelo': Uma função interna que pega os dados brutos
#    da API do tempo e os transforma no formato exato que o modelo de regressão
#    espera, criando as variáveis 'dummy' para as estações do ano e estimando
#    a radiação solar com base na hora e nebulosidade.
# 2. 'dados_previsao_reativos': Este é o coração reativo da app. Sempre que o
#    utilizador muda a cidade no 'selectInput', este bloco executa:
#    a. Chama a API para obter a nova previsão do tempo.
#    b. Prepara os dados com a função acima.
#    c. Usa o 'modelo_predicao_bicicletas' carregado para prever a procura.
#    d. Retorna uma lista com os dados prontos para serem usados pelos outputs.
# 3. 'renderLeaflet', 'renderPlot', 'renderDT': Cada uma destas funções
#    usam os dados da parte reativa para construir e atualizar o mapa,
#    o gráfico e a tabela. O mapa faz uma chamada à API para cada cidade
#   para calcular a procura máxima e desenhar os círculos com tamanhos proporcionais.
#-------------------------------------------------------------------------------
# --- Server Logic ---
server <- function(input, output, session) {
  # (funcao_preparar_dados_modelo permanece a mesma)
  funcao_preparar_dados_modelo <- function(df_meteo_raw) {
    if (is.null(df_meteo_raw) || nrow(df_meteo_raw) == 0) return(tibble())
    df_meteo_proc <- df_meteo_raw %>%
      mutate(
        hora_numerica = as.integer(hour(data_hora_utc)),
        nebulosidade_perc = as.numeric(nebulosidade %||% 50)
      )
    df_features <- df_meteo_proc %>%
      mutate(
        hour = hora_numerica,
        humidity = humidity_percent,
        wind_speed_m_s = wind_speed_mps,
        solar_radiation_mj_m2 = if_else(hora_numerica >= 7 & hora_numerica <= 19,
                                        ((100 - nebulosidade_perc) / 100) * 1.5 + 0.2,
                                        0.1),
        solar_radiation_mj_m2 = pmax(0.05, solar_radiation_mj_m2),
        is_holiday = 0L,
        mes_num = month(data_hora_utc),
        season_winter = if_else(mes_num %in% c(12, 1, 2), 1L, 0L),
        season_spring = if_else(mes_num %in% c(3, 4, 5), 1L, 0L),
        season_summer = if_else(mes_num %in% c(6, 7, 8), 1L, 0L),
        season_autumn = if_else(mes_num %in% c(9, 10, 11), 1L, 0L)
      )
    colunas_requeridas_pelo_modelo <- c("temperature_c", "hour", "humidity", "wind_speed_m_s", "solar_radiation_mj_m2", "rainfall_mm", "snowfall_cm", "is_holiday", "season_winter", "season_spring", "season_summer", "season_autumn")
    colunas_em_falta <- setdiff(colunas_requeridas_pelo_modelo, names(df_features))
    if(length(colunas_em_falta) > 0) { return(tibble()) }
    df_features <- df_features %>% select(all_of(colunas_requeridas_pelo_modelo))
    return(df_features)
  }

  dados_previsao_reativos <- reactive({
    req(input$cidade_selecionada_api)
    nome_cidade_api_selecionada <- input$cidade_selecionada_api
    dados_meteo_brutos <- funcao_obter_previsao_tempo_app(nome_cidade_api_selecionada, CHAVE_API_OPENWEATHER)
    nome_cidade_exibicao_sel <- dados_cidades_app$nome_exibicao[dados_cidades_app$nome_api == nome_cidade_api_selecionada]
    if (length(nome_cidade_exibicao_sel) == 0) nome_cidade_exibicao_sel <- nome_cidade_api_selecionada
    if (nrow(dados_meteo_brutos) == 0) {
      showNotification(paste("Não foram obtidos dados meteorológicos para", nome_cidade_exibicao_sel), type="warning", duration=7)
      return(list(dados_plot = tibble(), coordenadas_cidade = NULL, nome_exibicao_cidade = nome_cidade_exibicao_sel))
    }
    df_features_para_predicao <- funcao_preparar_dados_modelo(dados_meteo_brutos)
    df_para_plot <- dados_meteo_brutos
    df_para_plot$Procura_Prevista <- NA_integer_
    if (!is.null(modelo_predicao_bicicletas) && nrow(df_features_para_predicao) > 0) {
      if (nrow(df_features_para_predicao) == nrow(dados_meteo_brutos)) {
        tryCatch({
          predicoes <- suppressWarnings(predict(modelo_predicao_bicicletas, newdata = df_features_para_predicao))
          predicoes[predicoes < 0] <- 0
          df_para_plot$Procura_Prevista <- round(predicoes)
        }, error = function(e) {
          msg_erro <- paste("Erro na predição para", nome_cidade_exibicao_sel, ":", e$message)
          print(msg_erro); showNotification(msg_erro, type="error", duration=10)
        })
      }
    } else {
      if(is.null(modelo_predicao_bicicletas)) showNotification("Modelo de predição não está carregado.", type="error", duration=NULL)
    }
    coordenadas_cidade_sel <- dados_cidades_app %>% filter(nome_api == nome_cidade_api_selecionada)
    return(list(dados_plot = df_para_plot, coordenadas_cidade = coordenadas_cidade_sel, nome_exibicao_cidade = nome_cidade_exibicao_sel))
  })
  output$titulo_detalhes_cidade <- renderText({
    res_reativo <- dados_previsao_reativos()
    paste("Detalhes da Previsão para", res_reativo$nome_exibicao_cidade)
  })
  
  output$grafico_demanda_tempo <- renderPlot({
    res_reativo <- dados_previsao_reativos()
    dados_para_plot <- res_reativo$dados_plot
    nome_cidade <- res_reativo$nome_exibicao_cidade
    req(nrow(dados_para_plot) > 0, "Procura_Prevista" %in% names(dados_para_plot))
    fator_escala_temp <- 1; range_procura <- range(dados_para_plot$Procura_Prevista, na.rm = TRUE); range_temp <- range(dados_para_plot$temperature_c, na.rm = TRUE)
    if (is.finite(diff(range_procura)) && diff(range_procura) > 0 && is.finite(diff(range_temp)) && diff(range_temp) != 0) {
      fator_escala_temp <- (diff(range_procura) / diff(range_temp)) * 0.6; fator_escala_temp <- max(1, fator_escala_temp); fator_escala_temp <- min(50, fator_escala_temp)
    } else if (is.finite(diff(range_procura)) && diff(range_procura) > 0) { fator_escala_temp <- 15 } else { fator_escala_temp <- 10 }
    if (!is.finite(fator_escala_temp) || fator_escala_temp == 0) fator_escala_temp <- 10
    ggplot(dados_para_plot, aes(x = data_hora_utc)) +
      geom_line(aes(y = Procura_Prevista, color = "Procura Prevista"), linewidth = 1.2) +
      geom_line(aes(y = temperature_c * fator_escala_temp, color = "Temperatura (°C)"), linewidth = 1) +
      scale_y_continuous(name = "Procura Prevista de Bicicletas (alugueres)", sec.axis = sec_axis(~ . / fator_escala_temp, name = "Temperatura (°C)")) +
      labs(x = "Data/Hora (UTC)", color = "Métrica:") +
      theme_minimal(base_size = 14) +
      scale_color_manual(values = c("Procura Prevista" = "darkgreen", "Temperatura (°C)" = "darkorange")) +
      theme(legend.position = "top", legend.title = element_blank(), plot.title = element_text(hjust=0.5, face="bold"))
  }, res = 96)
  
  output$mapa_demanda <- renderLeaflet({
    mapa_base <- leaflet(data = dados_cidades_app) %>% addTiles(attribution = 'Mapa © OpenStreetMap') %>% setView(lng = 10, lat = 45, zoom = 3.5)
    withProgress(message = 'Calculando previsões para o mapa...', value = 0, {
      lista_procuras_maximas <- sapply(1:nrow(dados_cidades_app), function(i) {
        incProgress(1/nrow(dados_cidades_app), detail = paste("Processando", dados_cidades_app$nome_exibicao[i]))
        info_cidade_mapa <- dados_cidades_app[i,]
        dados_meteo_cidade_mapa <- funcao_obter_previsao_tempo_app(info_cidade_mapa$nome_api, CHAVE_API_OPENWEATHER)
        procura_maxima_atual <- 0
        if (nrow(dados_meteo_cidade_mapa) > 0 && !is.null(modelo_predicao_bicicletas)) {
          features_cidade_mapa <- funcao_preparar_dados_modelo(dados_meteo_cidade_mapa)
          if (nrow(features_cidade_mapa) == nrow(dados_meteo_cidade_mapa)) {
            tryCatch({
              predicoes_cidade_mapa <- suppressWarnings(predict(modelo_predicao_bicicletas, newdata = features_cidade_mapa))
              procura_maxima_atual <- max(pmax(0, predicoes_cidade_mapa), na.rm = TRUE)
            }, error = function(e){ print(paste("Erro no mapa:", e$message)) })
          }
        }
        return(round(procura_maxima_atual))
      })
    })
    dados_mapa <- dados_cidades_app %>% mutate(procura_max_prevista = lista_procuras_maximas)
    raio_escalado <- if (length(unique(dados_mapa$procura_max_prevista[!is.na(dados_mapa$procura_max_prevista) & dados_mapa$procura_max_prevista > 0])) <= 1) {
      ifelse(dados_mapa$procura_max_prevista > 0, 10, 5)
    } else { scales::rescale(sqrt(pmax(dados_mapa$procura_max_prevista,1)), to = c(5, 25)) }
    dados_mapa$raio_circulo <- raio_escalado
    mapa_base %>% addCircleMarkers(data=dados_mapa, lng=~lon, lat=~lat, radius=~raio_circulo, popup=~paste(nome_exibicao,"<br>Procura Máx:",procura_max_prevista), label=~nome_exibicao, color="navy", fillColor="green", fillOpacity=0.7, stroke=TRUE, weight=1.5)
  })
  
  output$tabela_dados_previsao <- renderDT({
    res_reativo <- dados_previsao_reativos()
    req(nrow(res_reativo$dados_plot) > 0)
    tabela <- res_reativo$dados_plot %>%
      mutate(data_hora_utc = format(data_hora_utc, "%Y-%m-%d %H:%M")) %>%
      select(
        `Data e Hora (UTC)` = data_hora_utc,
        `Temperatura (°C)` = temperature_c,
        `Humidade (%)` = humidity_percent,
        `Vento (m/s)` = wind_speed_mps,
        `Precipitação (mm/3h)` = rainfall_mm,
        `Neve (cm/3h)` = snowfall_cm,
        `Descrição do Tempo` = descricao_tempo,
        `Procura Prevista` = Procura_Prevista
      )
    datatable(tabela, options = list(pageLength = 8, language = list(url = '//cdn.datatables.net/plug-ins/1.10.19/i18n/Portuguese.json'), searching = FALSE, lengthChange = FALSE), rownames = FALSE)
  })
}
#-------------------------------------------------------------------------------
# Bloco 6: Execução da Aplicação
#-------------------------------------------------------------------------------
# Descrição:
# A linha final, 'shinyApp(ui = ui, server = server)', é a que efetivamente
# inicia a aplicação Shiny, combinando a interface do utilizador (ui) com a
# lógica do servidor (server) para criar o dashboard interativo.
#-------------------------------------------------------------------------------
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))