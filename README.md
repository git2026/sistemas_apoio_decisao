Análise e Previsão da Procura de Partilha de Bicicletas
Este projeto analisa como as condições meteorológicas e temporais influenciam a procura por serviços de partilha de bicicletas em áreas populadas. Utiliza dados históricos para treinar um modelo de regressão e apresenta as previsões numa aplicação interativa construída com R e Shiny.


Pré-requisitos

R: Versão 4.0 ou superior.
RStudio: Versão 2022.07 ou superior.
Instalação de dependências
Abra a consola do RStudio e execute o seguinte comando para instalar todas as bibliotecas necessárias para o projeto:
install.packages(c(
    "rvest", "httr", "jsonlite", "dplyr", "readr", "purrr", "data.table",
    "lubridate", "stringr", "fastDummies", "RSQLite", "DBI", "ggplot2",
    "caTools", "glmnet", "caret", "shiny", "shinydashboard", "leaflet",
    "scales", "DT"
))


Como Executar: 
A execução do projeto é feita em duas fases principais.

Fase 1: 
1. Abrir o RStudio

2. Definir o diretório de execução na consola com o comando "setwd(pasta_onde_colocou_o_projeto/Projeto")

3. Executar o script principal (projeto_codigo) por inteiro que prepara tudo o que a aplicação interativa precisa.

4. No final da execução as pastas dados_processados/ e modelos/ terão sido criadas e preenchidas com os ficheiros necessários (se já existirem não há problema, o código está criado de maneira a nenhum problema de pastas ou falta de ficheiro acontecer).

Fase 2:

1. Abra o ficheiro app.R.

2. Clique no botão "Run App" que aparece no topo do editor de código.

3. A aplicação Shiny será lançada, permitindo-lhe selecionar cidades e ver as previsões de procura de bicicletas em tempo real.


Descrição Simples dos Scripts
Projeto_codigo.R: Responsável pelo trabalho de recolha, processamento e analise de dados. Este demostrar graficos e treino de modelos de regressão para fazer as previsões de aluguer em cidades expecificas.

app.R: É a interface do utilizador. Carrega o modelo treinado, comunica com a API da OpenWeather para obter dados meteorológicos em tempo real, e usa o modelo para gerar e apresentar as previsões de procura de bicicletas num mapa e em gráficos.

Autores
Guilherme Carvalho
Hugo Pedro
Mario Felix