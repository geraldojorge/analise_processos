# ==============================================================================
# install_packages.R
# Execute este script UMA VEZ para instalar todos os pacotes necessários.
# No RStudio: selecione tudo (Ctrl+A) e execute (Ctrl+Enter)
# ==============================================================================

pkgs <- c(
    "shiny",           # framework web
    "shinydashboard",  # layout de dashboard
    "shinyjs",         # interatividade JavaScript
    "dplyr",           # manipulação de dados
    "tidyr",           # transformação de dados
    "readr",           # leitura de CSV
    "lubridate",       # datas e horas
    "stringr",         # manipulação de texto
    "ggplot2",         # visualizações
    "plotly",          # gráficos interativos
    "DT",              # tabelas interativas
    "scales",          # formatação de escalas
    "igraph"           # análise de redes (betweenness, centralidade)
)

# Instalar apenas os que faltam
pkgs_faltando <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]

if (length(pkgs_faltando) == 0) {
    cat("Todos os pacotes já estão instalados!\n")
} else {
    cat("Instalando:", paste(pkgs_faltando, collapse = ", "), "\n")
    install.packages(pkgs_faltando, dependencies = TRUE)
    cat("Instalação concluída!\n")
}

# Verificação final
cat("\n=== Verificação ===\n")
for (p in pkgs) {
    ok <- requireNamespace(p, quietly = TRUE)
    cat(sprintf("  %-20s %s\n", p, ifelse(ok, "OK", "FALHOU")))
}