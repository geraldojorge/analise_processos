# ==============================================================================
# app.R — Ponto de entrada do aplicativo Shiny
# Monitor de Processos de Aquisição de Medicamentos — SEI/SES-PE
# ==============================================================================
#
# COMO EXECUTAR:
#   1. Abra este projeto no RStudio
#   2. Instale os pacotes necessários (ver README.md ou rodar install_packages.R)
#   3. Coloque o arquivo CSV exportado do SEI na mesma pasta com o nome:
#      dados_sei.csv
#   4. Clique em "Run App" no RStudio, ou execute no console:
#      shiny::runApp(".")
#
# ESTRUTURA DO APP:
#   app.R           — este arquivo (launcher)
#   ui.R            — interface do usuário (4 abas)
#   server.R        — lógica do servidor (cálculos e gráficos)
#   bupar_engine.R  — pipeline de mineração de processos (conceitos bupaR)
#   dados_sei.csv   — dados exportados do SEI (substituir pelo real)
# ==============================================================================

library(shiny)

# Verificar pacotes necessários
pkgs_necessarios <- c(
  "shinydashboard", "dplyr", "ggplot2", "plotly", "DT",
  "lubridate", "stringr", "scales", "igraph", "tidyr", "shinyjs", "readr"
)
pkgs_faltando <- pkgs_necessarios[!sapply(pkgs_necessarios, requireNamespace, quietly = TRUE)]

if (length(pkgs_faltando) > 0) {
  stop(
    "Pacotes não instalados: ", paste(pkgs_faltando, collapse = ", "),
    "\nInstale com: install.packages(c('", paste(pkgs_faltando, collapse="','"), "'))"
  )
}

# Verificar arquivo de dados
if (!file.exists("dados_sei.csv")) {
  stop(
    "Arquivo 'dados_sei.csv' não encontrado na pasta do app.\n",
    "Exporte os dados do SEI e salve como 'dados_sei.csv' nesta pasta.\n",
    "Colunas esperadas: SEI, Data/Hora, Unidade, Usuário, Descrição, STATUS"
  )
}

source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
