# ==============================================================================
# ui.R
# ==============================================================================
suppressPackageStartupMessages({
  library(shiny); library(shinydashboard); library(DT); library(plotly); library(shinyjs)
})

css_custom <- "
@import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@300;400;500;600&family=DM+Mono:wght@400;500&display=swap');
:root{--bg:#0f1117;--surface:#181c27;--surface2:#1e2334;--border:#2a3045;--text:#e8ecf4;--muted:#7a87a3;}
body,.content-wrapper,.right-side{background:var(--bg)!important;color:var(--text)!important;}
body,h1,h2,h3,h4,p,label,.box-title{font-family:'DM Sans',sans-serif!important;}

/* Fixed header */
.main-header{position:fixed!important;top:0;left:0;right:0;z-index:9999!important;}
.content-wrapper{margin-top:50px!important;}
.skin-blue .main-header .logo{background:#181c27!important;color:#e8ecf4!important;border-bottom:1px solid #2a3045!important;}
.skin-blue .main-header .navbar{background:#181c27!important;border-bottom:1px solid #2a3045!important;}
.skin-blue .main-header .logo{color:#e8ecf4!important;}

/* Sidebar */
.skin-blue .sidebar{background:#181c27!important;border-right:1px solid #2a3045;}
.skin-blue .sidebar-menu>li>a{color:#7a87a3!important;font-family:'DM Sans',sans-serif;font-size:13px;border-left:3px solid transparent;}
.skin-blue .sidebar-menu>li.active>a,.skin-blue .sidebar-menu>li>a:hover{color:#e8ecf4!important;background:#1e2334!important;border-left-color:#6366f1!important;}

/* Boxes */
.box{background:#181c27!important;border:1px solid #2a3045!important;border-top:3px solid #6366f1!important;border-radius:12px!important;box-shadow:none!important;}
.box.box-danger{border-top-color:#ef4444!important;}.box.box-warning{border-top-color:#f59e0b!important;}
.box.box-success{border-top-color:#22c55e!important;}.box.box-info{border-top-color:#06b6d4!important;}
.box.box-primary{border-top-color:#6366f1!important;}
.box-header{background:transparent!important;border-bottom:1px solid #2a3045!important;}
.box-title{color:#e8ecf4!important;font-size:14px!important;font-weight:600!important;}
.box-body{color:#e8ecf4!important;}

/* InfoBoxes */
.info-box{background:#181c27!important;border:1px solid #2a3045!important;border-radius:12px!important;box-shadow:none!important;}
.info-box-icon{border-radius:12px 0 0 12px!important;}
.info-box-content,.info-box-number{color:#e8ecf4!important;}
.info-box-number{font-family:'DM Mono',monospace!important;}
.info-box-text{color:#7a87a3!important;font-size:12px!important;}

/* Tables */
.dataTables_wrapper,.dataTables_filter input,.dataTables_length select,table.dataTable{
  color:#e8ecf4!important;background:#181c27!important;font-family:'DM Sans',sans-serif!important;font-size:13px!important;}
table.dataTable thead th{background:#1e2334!important;color:#7a87a3!important;font-size:11px!important;
  text-transform:uppercase;letter-spacing:.06em;border-bottom:1px solid #2a3045!important;}
table.dataTable tbody tr{background:#181c27!important;}
table.dataTable tbody tr:hover td{background:#1e2334!important;}
table.dataTable tbody td{border-bottom:1px solid #2a3045!important;color:#e8ecf4!important;}
.dataTables_info,.dataTables_paginate .paginate_button{color:#7a87a3!important;}
.dataTables_paginate .paginate_button.current{background:#6366f1!important;color:#fff!important;border-radius:6px!important;}

/* Select */
.selectize-input,.selectize-dropdown{background:#1e2334!important;border:1px solid #2a3045!important;
  color:#e8ecf4!important;border-radius:8px!important;font-family:'DM Sans',sans-serif!important;font-size:13px!important;}
.selectize-dropdown-content .option:hover{background:#2a3045!important;}

/* Badges */
.badge-alto{background:#2d0a0a;color:#ef4444;padding:3px 10px;border-radius:20px;font-size:11px;font-weight:700;}
.badge-medio{background:#2d1f05;color:#f59e0b;padding:3px 10px;border-radius:20px;font-size:11px;font-weight:700;}
.badge-baixo{background:#0a2e1a;color:#22c55e;padding:3px 10px;border-radius:20px;font-size:11px;font-weight:700;}

/* Explain box */
.explain-box{background:#151a2a;border:1px solid #2a3555;border-radius:10px;padding:12px 16px;
  font-size:13px;color:#7a87a3;line-height:1.6;margin-bottom:16px;}
.explain-box strong{color:#e8ecf4;}

/* Trace */
.trace-tag{display:inline-flex;align-items:center;gap:5px;border-radius:8px;padding:4px 10px;margin:3px;font-size:11px;color:#e8ecf4;}
.unit-legend{display:flex;flex-wrap:wrap;gap:12px;margin-bottom:14px;}
.unit-item{display:flex;align-items:center;gap:6px;font-size:12px;color:#7a87a3;}
.unit-dot{width:10px;height:10px;border-radius:50%;display:inline-block;}

/* Upload */
.upload-zone{border:2px solid #2a3045;border-radius:12px;padding:24px;background:#151a2a;}
.upload-status-ok{color:#22c55e;font-weight:600;}
.shiny-input-container input[type=file]{color:#e8ecf4;}

/* Outputs */
.json-output{background:#0a0d14;border:1px solid #2a3045;border-radius:8px;padding:14px;
  font-family:'DM Mono',monospace;font-size:11px;color:#a5b4fc;max-height:420px;overflow-y:auto;white-space:pre;line-height:1.5;}
.prompt-output{background:#0d1117;border:1px solid #2a3045;border-radius:8px;padding:16px;
  font-family:'DM Sans',sans-serif;font-size:12px;color:#cbd5e1;max-height:480px;overflow-y:auto;white-space:pre-wrap;line-height:1.7;}

/* Process map */
.process-map-svg{background:#f8fafc;border:1px solid #2a3045;border-radius:10px;padding:12px;overflow:hidden;min-height:300px;position:relative;}
.process-map-svg svg{width:100%;height:auto;display:block;transform-origin:top left;transition:transform .15s ease;cursor:grab;}
.process-map-svg svg:active{cursor:grabbing;}
/* Zoom controls */
.zoom-controls{position:absolute;top:8px;right:8px;z-index:10;display:flex;gap:4px;}
.zoom-btn{background:#181c27;border:1px solid #2a3045;color:#e8ecf4;border-radius:6px;
  width:30px;height:30px;font-size:16px;cursor:pointer;display:flex;align-items:center;
  justify-content:center;transition:background .15s;}
.zoom-btn:hover{background:#6366f1;border-color:#6366f1;}
.zoom-label{background:#181c27;border:1px solid #2a3045;color:#7a87a3;border-radius:6px;
  padding:0 8px;height:30px;font-size:11px;display:flex;align-items:center;
  font-family:'DM Mono',monospace;min-width:44px;justify-content:center;}
/* Sidebar freeze */
.skin-blue .main-sidebar{position:fixed!important;top:50px;bottom:0;height:calc(100vh - 50px)!important;overflow-y:auto;z-index:810;}
.main-sidebar::-webkit-scrollbar{width:4px;}
.main-sidebar::-webkit-scrollbar-track{background:transparent;}
.main-sidebar::-webkit-scrollbar-thumb{background:#2a3045;border-radius:2px;}
.animate-container{background:#0a0d14;border:1px solid #2a3045;border-radius:10px;overflow:hidden;}

/* Buttons */
.btn-export{background:#6366f1;color:#fff;border:none;border-radius:8px;padding:8px 18px;
  font-size:13px;font-weight:600;cursor:pointer;margin-right:8px;margin-bottom:8px;display:inline-flex;align-items:center;gap:6px;}
.btn-export:hover{background:#4f46e5;color:#fff;}
.btn-export.green{background:#10b981;}.btn-export.green:hover{background:#059669;}
.btn-export.red{background:#dc2626;}.btn-export.red:hover{background:#b91c1c;}
.btn-export-wrap{margin-bottom:16px;}
#btn_tema{background:transparent!important;border:1px solid #2a3045!important;color:#94a3b8!important;border-radius:8px!important;padding:4px 10px!important;}

/* Status */
.status-dot-live{width:8px;height:8px;border-radius:50%;background:#22c55e;animation:pulse 2s infinite;display:inline-block;}
.status-dot-idle{width:8px;height:8px;border-radius:50%;background:#7a87a3;display:inline-block;}
@keyframes pulse{0%,100%{opacity:1}50%{opacity:.4}}

/* Misc */
hr{border-color:#2a3045!important;}
label{color:#7a87a3!important;font-size:12px!important;}
.content-header h1{color:#e8ecf4!important;font-size:18px!important;}
.content-header .breadcrumb{display:none;}

/* ── Light mode ── */
body.light-mode,.light-mode .content-wrapper{background:#f1f5f9!important;color:#1e293b!important;}
.light-mode .skin-blue .main-header .logo,.light-mode .skin-blue .main-header .navbar{background:#6366f1!important;}
.light-mode .skin-blue .main-header .logo{color:#fff!important;}
.light-mode .skin-blue .sidebar{background:#fff!important;border-right:1px solid #e2e8f0;}
.light-mode .skin-blue .sidebar-menu>li>a{color:#64748b!important;}
.light-mode .skin-blue .sidebar-menu>li.active>a,.light-mode .skin-blue .sidebar-menu>li>a:hover{color:#1e293b!important;background:#f1f5f9!important;border-left-color:#6366f1!important;}
.light-mode .box{background:#fff!important;border-color:#e2e8f0!important;}
.light-mode .box-header{border-bottom:1px solid #e2e8f0!important;}
.light-mode .box-title,.light-mode .box-body{color:#1e293b!important;}
.light-mode .info-box{background:#fff!important;border-color:#e2e8f0!important;}
.light-mode .info-box-content,.light-mode .info-box-number{color:#1e293b!important;}
.light-mode .info-box-text{color:#64748b!important;}
.light-mode table.dataTable thead th{background:#f1f5f9!important;color:#64748b!important;border-bottom:1px solid #e2e8f0!important;}
.light-mode table.dataTable tbody tr{background:#fff!important;}
.light-mode table.dataTable tbody tr:hover td{background:#f8fafc!important;}
.light-mode table.dataTable tbody td{border-bottom:1px solid #f1f5f9!important;color:#1e293b!important;}
.light-mode .dataTables_wrapper,.light-mode .dataTables_filter input{color:#1e293b!important;background:#fff!important;}
.light-mode .dataTables_info,.light-mode .dataTables_paginate .paginate_button{color:#64748b!important;}
.light-mode .selectize-input,.light-mode .selectize-dropdown{background:#fff!important;border-color:#e2e8f0!important;color:#1e293b!important;}
.light-mode .explain-box{background:#f0f9ff;border-color:#bae6fd;color:#0369a1;}
.light-mode .explain-box strong{color:#0f172a;}
.light-mode .json-output{background:#f8fafc;color:#4f46e5;border-color:#e2e8f0;}
.light-mode .prompt-output{background:#fafafa;color:#374151;border-color:#e2e8f0;}
.light-mode .upload-zone{background:#f8fafc;border-color:#e2e8f0;}
.light-mode .process-map-svg{background:#fff;border-color:#e2e8f0;}
.light-mode .zoom-btn{background:#f1f5f9;border-color:#e2e8f0;color:#1e293b;}
.light-mode .zoom-btn:hover{background:#6366f1;color:#fff;}
.light-mode .zoom-label{background:#f1f5f9;border-color:#e2e8f0;color:#64748b;}
.light-mode .animate-container{background:#f8fafc;border-color:#e2e8f0;}
.light-mode label{color:#64748b!important;}
.light-mode .content-header h1{color:#1e293b!important;}
.light-mode hr{border-color:#e2e8f0!important;}
.light-mode .trace-tag{color:#1e293b!important;}
.light-mode .unit-item{color:#64748b;}
.light-mode #btn_tema{border-color:#e2e8f0!important;color:#64748b!important;}
.light-mode .box-body ol li{color:#1e293b!important;}
"

ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = tags$span(
      tags$span("💊", style="font-size:18px;margin-right:8px;"),
      tags$span("Monitor SEI — Medicamentos", style="font-size:14px;font-weight:600;")
    ),
    titleWidth = 280,
    tags$li(class="dropdown", style="padding:8px 14px;",
      actionButton("btn_tema", label=NULL, icon=icon("circle-half-stroke"),
                   title="Alternar modo claro/escuro"))
  ),

  dashboardSidebar(
    width = 240,
    useShinyjs(),
    tags$head(tags$style(HTML(css_custom))),
    sidebarMenu(
      id = "menu_ativo",
      menuItem("Dados & Upload",       tabName="upload",      icon=icon("upload")),
      menuItem("Visão Geral",          tabName="visao_geral", icon=icon("th-large")),
      menuItem("Processo Individual",  tabName="processo",    icon=icon("route")),
      menuItem("Fluxo entre Setores",  tabName="rede",        icon=icon("project-diagram")),
      menuItem("Mapa de Processo",     tabName="mapa",        icon=icon("diagram-project")),
      menuItem("Filtros & Exploração", tabName="filtros",     icon=icon("filter")),
      menuItem("Atividades",           tabName="atividades",  icon=icon("tasks")),
      menuItem("Desempenho por Setor", tabName="unidades",    icon=icon("building")),
      menuItem("Dados Brutos",         tabName="dados_brutos",icon=icon("table")),
      menuItem("Exportar",             tabName="exportar",    icon=icon("file-export"))
    ),
    tags$hr(),
    tags$div(style="padding:0 20px 12px;", uiOutput("sidebar_status"))
  ),

  dashboardBody(
    tabItems(

      # ══ UPLOAD ════════════════════════════════════════════════════════════
      tabItem(tabName="upload",
        fluidRow(box(width=12,
          tags$div(class="explain-box",
            tags$strong("Como usar:"),
            " Faça o upload do CSV exportado do SEI. O sistema não carrega dados automaticamente — ",
            "um arquivo precisa ser fornecido para que qualquer análise seja exibida.", tags$br(),
            "Colunas esperadas: ", tags$strong("SEI, Data/Hora, Unidade, Usuário, Descrição, STATUS"), "."
          )
        )),
        fluidRow(
          box(title="Carregar arquivo do SEI", width=6, solidHeader=TRUE, status="primary",
            tags$div(class="upload-zone",
              tags$div(style="display:flex;align-items:center;gap:16px;flex-wrap:wrap;",
                tags$div(style="flex:1;min-width:220px;",
                  fileInput("arquivo_csv", label=NULL,
                    accept = c("text/csv", "application/vnd.ms-excel", ".csv"),
                    width  = "100%",
                    buttonLabel = tags$span(
                      icon("folder-open"), " Selecionar arquivo CSV"
                    ),
                    placeholder = "Nenhum arquivo selecionado"
                  )
                )
              )
            ),
            tags$br(), uiOutput("upload_feedback")
          ),
          box(title="Status", width=6, solidHeader=TRUE, status="info",
            uiOutput("fonte_status"), tags$hr(),
            tags$div(class="explain-box",
              tags$strong("Formato esperado:"), tags$br(),
              tags$code("SEI,Data/Hora,Unidade,Usuário,Descrição,STATUS"), tags$br(), tags$br(),
              "• Data/Hora: dd/mm/aaaa HH:MM", tags$br(),
              "• STATUS: Aberto ou Concluído"
            )
          )
        ),
        fluidRow(box(title="Pré-visualização dos dados", width=12, solidHeader=TRUE, status="primary",
          DT::dataTableOutput("preview_csv")
        )),
        fluidRow(uiOutput("deps_aviso_ui"))
      ),

      # ══ VISÃO GERAL ═══════════════════════════════════════════════════════
      tabItem(tabName="visao_geral",
        fluidRow(infoBoxOutput("ib_alto",width=3),infoBoxOutput("ib_medio",width=3),
                 infoBoxOutput("ib_baixo",width=3),infoBoxOutput("ib_abertos",width=3)),
        fluidRow(box(width=12,
          tags$div(class="explain-box",
            tags$strong("Como ler:"), " O risco combina: prazo consumido, retrabalho, setores envolvidos e etapas lentas. ",
            "Clique em qualquer linha para ver os detalhes do processo."
          )
        )),
        fluidRow(box(title="Todos os Processos", width=12, solidHeader=TRUE, status="primary",
          DT::dataTableOutput("tabela_geral"))),
        fluidRow(
          box(title="Distribuição por Risco", width=6, plotlyOutput("plot_risco_bar",height="280px")),
          box(title="Duração × Movimentações", width=6, plotlyOutput("plot_scatter",height="280px"))
        )
      ),

      # ══ PROCESSO INDIVIDUAL ═══════════════════════════════════════════════
      tabItem(tabName="processo",
        # Seletor de processo DENTRO da aba
        fluidRow(box(width=12, solidHeader=FALSE,
          tags$div(style="display:flex;align-items:center;gap:16px;flex-wrap:wrap;",
            tags$span(style="color:#7a87a3;font-size:13px;font-weight:500;white-space:nowrap;","Processo analisado:"),
            tags$div(style="flex:1;min-width:300px;",
              selectInput("sei_selecionado", label=NULL, choices=NULL, width="100%")
            )
          )
        )),
        fluidRow(infoBoxOutput("ib_proc_risco",width=3),infoBoxOutput("ib_proc_dias",width=3),
                 infoBoxOutput("ib_proc_events",width=3),infoBoxOutput("ib_proc_units",width=3)),
        fluidRow(box(title="Linha do Tempo — em qual setor e por quanto tempo",
          width=12, solidHeader=TRUE, status="primary",
          tags$div(class="explain-box",
            "Cada barra mostra o tempo que o processo ficou em um setor. ",
            "Quando o mesmo setor aparece várias vezes, o processo retornou — sinal de retrabalho."
          ),
          uiOutput("legenda_gantt"),
          plotlyOutput("plot_gantt", height="320px")
        )),
        fluidRow(
          box(title="Indicadores de Risco", width=6, solidHeader=TRUE, status="primary",
            DT::dataTableOutput("tabela_indicadores")),
          box(title="Sequência de Setores (Trace)", width=6, solidHeader=TRUE, status="info",
            tags$div(class="explain-box","Sequência cronológica dos setores. Repetições compactadas = retornos."),
            uiOutput("trace_visual"))
        )
      ),

      # ══ FLUXO ENTRE SETORES ═══════════════════════════════════════════════
      tabItem(tabName="rede",
        fluidRow(box(width=12,
          tags$div(class="explain-box",
            tags$strong("Como ler:"), " Círculos = setores. Tamanho = centralidade. Espessura das setas = volume. Cor: ",
            tags$span("verde",style="color:#22c55e;font-weight:600;")," < 8h, ",
            tags$span("amarelo",style="color:#f59e0b;font-weight:600;")," 8-48h, ",
            tags$span("vermelho",style="color:#ef4444;font-weight:600;")," > 48h."
          )
        )),
        fluidRow(
          box(title="Rede de Fluxo", width=8, solidHeader=TRUE, status="primary",
            plotlyOutput("plot_rede",height="500px")),
          box(title="Legenda e Estatísticas", width=4, solidHeader=TRUE, status="info",
            tags$div(class="explain-box",
              tags$strong("Nós:"), " Tamanho ∝ centralidade.", tags$br(), tags$br(),
              tags$strong("Setas:"), tags$br(),
              tags$span(style="color:#22c55e;font-weight:700;","● Verde = < 8h"), tags$br(),
              tags$span(style="color:#f59e0b;font-weight:700;","● Amarelo = 8-48h"), tags$br(),
              tags$span(style="color:#ef4444;font-weight:700;","● Vermelho = > 48h")
            ),
            tags$hr(), uiOutput("mapa_stats"))
        ),
        fluidRow(box(title="Centralidade por Setor", width=12, solidHeader=TRUE, status="primary",
          plotlyOutput("plot_centralidade",height="260px"))),
        fluidRow(box(title="Passagens entre Setores", width=12, solidHeader=TRUE, status="primary",
          DT::dataTableOutput("tabela_handovers"))),
        fluidRow(
          box(title="Tabela de Nós", width=6, solidHeader=TRUE, status="info",
            DT::dataTableOutput("tabela_mapa_nos")),
          box(title="Tabela de Arestas (todas)", width=6, solidHeader=TRUE, status="info",
            DT::dataTableOutput("tabela_mapa_arestas"))
        )
      ),

      # ══ MAPA DE PROCESSO ══════════════════════════════════════════════════
      tabItem(tabName="mapa",
        fluidRow(box(width=12,
          tags$div(class="explain-box",
            tags$strong("Mapa de Processo (bupaR process_map):"),
            " Gerado com Graphviz — o mesmo motor visual que o bupaR usa internamente. ",
            "Selecione o tipo: Frequência (quantas vezes cada handover ocorreu), ",
            "Performance (tempos de espera), Risco (exposição de cada setor a processos ALTO), ",
            "Retrabalho (loops detectados) ou Animação (fluxo simulado ao longo do tempo)."
          )
        )),
        fluidRow(box(width=12,
          tags$div(style="display:flex;gap:8px;flex-wrap:wrap;margin-bottom:10px;",
            actionButton("map_freq",   tags$span(icon("hashtag"), " Frequência"),  style="background:#6366f1;color:#fff;border:none;border-radius:8px;padding:7px 14px;font-size:12px;font-weight:600;cursor:pointer;"),
            actionButton("map_perf",   tags$span(icon("clock"),   " Performance"), style="background:#06b6d4;color:#fff;border:none;border-radius:8px;padding:7px 14px;font-size:12px;font-weight:600;cursor:pointer;"),
            actionButton("map_risco",  tags$span(icon("shield-alt")," Risco"),     style="background:#ef4444;color:#fff;border:none;border-radius:8px;padding:7px 14px;font-size:12px;font-weight:600;cursor:pointer;"),
            actionButton("map_retrab", tags$span(icon("redo"),    " Retrabalho"),  style="background:#f59e0b;color:#fff;border:none;border-radius:8px;padding:7px 14px;font-size:12px;font-weight:600;cursor:pointer;"),
            actionButton("map_anim",   tags$span(icon("play"),    " Animação"),    style="background:#8b5cf6;color:#fff;border:none;border-radius:8px;padding:7px 14px;font-size:12px;font-weight:600;cursor:pointer;")
          ),
          uiOutput("mapa_tipo_label")
        )),

        # SVG maps
        uiOutput("mapa_ou_animacao"),

        fluidRow(
          box(title="Tabela de Nós", width=6, solidHeader=TRUE, status="primary",
            DT::dataTableOutput("tabela_mapa_nos2")),
          box(title="Tabela de Arestas", width=6, solidHeader=TRUE, status="primary",
            DT::dataTableOutput("tabela_mapa_arestas2"))
        )
      ),

      # ══ ATIVIDADES ════════════════════════════════════════════════════════
      tabItem(tabName="atividades",
        fluidRow(box(title="Presença de Atividades", width=12,
          tags$div(class="explain-box","100% = todos os processos passam por ela. Atividades raras = exceções."),
          plotlyOutput("plot_activity_presence",height="380px"))),
        fluidRow(
          box(title="Volume por Tipo", width=6, plotlyOutput("plot_activity_volume",height="300px")),
          box(title="Variantes (Traces)", width=6,
            tags$div(class="explain-box",tags$strong("Variante")," = sequência de setores. Muitas variantes = fluxo não padronizado."),
            DT::dataTableOutput("tabela_traces",height="260px"))
        )
      ),

      # ══ UNIDADES ══════════════════════════════════════════════════════════
      tabItem(tabName="unidades",
        fluidRow(box(width=12,
          tags$div(class="explain-box","Alta centralidade = ponto crítico. Se travar, afeta todos os processos."))),
        fluidRow(
          box(title="Centralidade dos Setores", width=7, solidHeader=TRUE, status="primary",
            plotlyOutput("plot_centralidade_barras",height="320px")),
          box(title="Envios e Recebimentos", width=5, solidHeader=TRUE, status="info",
            plotlyOutput("plot_env_rec",height="320px"))
        ),
        fluidRow(box(title="Detalhamento por Setor", width=12, solidHeader=TRUE, status="primary",
          DT::dataTableOutput("tabela_unidades")))
      ),

      # ══ DADOS BRUTOS ══════════════════════════════════════════════════════
      tabItem(tabName="dados_brutos",
        fluidRow(box(title="Event Log Completo", width=12, solidHeader=TRUE, status="primary",
          tags$div(class="explain-box","Cada linha = um evento: caso, atividade, momento e setor."),
          DT::dataTableOutput("tabela_eventlog")))
      ),

      # ══ EXPORTAR ══════════════════════════════════════════════════════════
      tabItem(tabName="exportar",
        fluidRow(box(width=12,
          tags$div(class="explain-box",
            tags$strong("Exportação:"),
            " Gere o JSON com resultados da análise, o prompt para LLM ou o relatório PDF. ",
            "Requer que um arquivo CSV tenha sido carregado na aba Dados & Upload."
          )
        )),
        fluidRow(
          box(title="1. JSON Estruturado", width=4, solidHeader=TRUE, status="primary",
            tags$div(class="explain-box","Contém: metadados, risco, handovers, rede, atividades, mapa de processo e metodologia."),
            tags$div(class="btn-export-wrap",
              downloadButton("btn_download_json", label=" Baixar JSON", class="btn-export"),
              actionButton("btn_preview_json", label=tags$span(icon("eye")," Pré-visualizar"),
                class="btn-export", style="background:#374151;")
            ),
            conditionalPanel("output.show_json_preview",
              tags$div(class="json-output", verbatimTextOutput("preview_json",placeholder=FALSE)))
          ),
          box(title="2. Prompt para LLM", width=4, solidHeader=TRUE, status="success",
            tags$div(class="explain-box","Prompt contextualizado para diagnóstico gerencial em 7 dimensões."),
            tags$div(class="btn-export-wrap",
              downloadButton("btn_download_prompt", label=" Baixar Prompt (.txt)", class="btn-export green"),
              actionButton("btn_copiar_prompt", label=tags$span(icon("eye")," Pré-visualizar"),
                class="btn-export", style="background:#374151;")
            ),
            conditionalPanel("output.show_prompt_preview",
              tags$div(class="prompt-output", verbatimTextOutput("preview_prompt",placeholder=FALSE)))
          ),
          box(title="3. Relatório PDF / HTML", width=4, solidHeader=TRUE, status="warning",
            tags$div(class="explain-box","PDF completo com capa, gráficos e tabelas. ",
              "Requer wkhtmltopdf instalado. Alternativa: baixe o relatório em HTML (funciona sem instalação adicional)."),
            uiOutput("pdf_path_ui"),
            tags$div(class="btn-export-wrap",
              downloadButton("btn_download_pdf",  label=" Baixar PDF",  class="btn-export red"),
              downloadButton("btn_download_html", label=" Baixar HTML", class="btn-export",
                             style="background:#374151;")
            )
          )
        ),
        fluidRow(
          box(title="Como usar JSON + Prompt com LLM", width=12,
            tags$ol(style="line-height:2.2;font-size:13px;padding-left:20px;",
              tags$li("Clique em ", tags$strong("Baixar JSON"), " e salve o arquivo."),
              tags$li("Clique em ", tags$strong("Baixar Prompt (.txt)"), " e salve o arquivo."),
              tags$li("Abra o prompt em qualquer editor de texto."),
              tags$li("Abra o JSON, selecione tudo e copie."),
              tags$li("No prompt, substitua ", tags$code("[SUBSTITUIR PELO CONTEÚDO DO ARQUIVO JSON EXPORTADO]"), " pelo JSON copiado."),
              tags$li("Cole o prompt completo no Claude, ChatGPT ou outro LLM."),
              tags$li("O modelo produzirá um diagnóstico gerencial completo.")
            )
          )
        )
      )

    ) # fim tabItems
  )   # fim dashboardBody
)
