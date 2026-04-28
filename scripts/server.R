# ==============================================================================
# server.R — Monitor SEI
# ==============================================================================
suppressPackageStartupMessages({
  library(shiny); library(shinydashboard); library(dplyr); library(ggplot2)
  library(plotly); library(DT); library(lubridate); library(stringr)
  library(scales); library(igraph); library(tidyr); library(shinyjs)
  library(readr); library(jsonlite)
})
source("bupar_engine.R")
source("relatorio.R")

CORES <- c("SES - GCJ_ARP"="#6366f1","SES - GCJ"="#06b6d4","SES - GFAJ_SA"="#10b981",
           "SES - NP_DGPO"="#f59e0b","SES - CSANS"="#ec4899","SES - GFAJ"="#8b5cf6",
           "SES - GORC"="#14b8a6","SES - NAJ"="#f97316")
COR_ALTO="#ef4444"; COR_MEDIO="#f59e0b"; COR_BAIXO="#22c55e"
BG="#0f1117"; BG2="#181c27"; BORDER="#2a3045"; TEXT="#e8ecf4"; MUTED="#7a87a3"

cor_u <- function(u){ c<-CORES[u]; if(is.na(c))"#888888" else c }

plotly_tema <- function(p,...) p %>% layout(
  paper_bgcolor=BG2, plot_bgcolor=BG2,
  font=list(family="DM Sans,sans-serif",color=TEXT,size=12),
  xaxis=list(gridcolor=BORDER,zerolinecolor=BORDER,tickfont=list(color=MUTED),title=list(font=list(color=MUTED))),
  yaxis=list(gridcolor=BORDER,zerolinecolor=BORDER,tickfont=list(color=MUTED),title=list(font=list(color=MUTED))),
  legend=list(bgcolor=BG2,bordercolor=BORDER,font=list(color=TEXT)),
  margin=list(l=10,r=10,t=30,b=10),...)

badge_html <- function(r){
  cls <- switch(as.character(r),"ALTO"="badge-alto","MÉDIO"="badge-medio","BAIXO"="badge-baixo","badge-baixo")
  sprintf('<span class="%s">%s</span>',cls,r)
}
fmt_h <- function(h) ifelse(h<1,"< 1h",ifelse(h<24,paste0(round(h),"h"),paste0(round(h/24,1),"d")))
fmt_horas <- function(h) ifelse(h<1,"< 1 hora",ifelse(h<24,paste0(round(h)," horas"),paste0(round(h/24,1)," dias")))

# ─────────────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Null-coalescing (must be defined first)
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

  # ── Upload & dados ──────────────────────────────────────────────────────
  # Dados só carregam após upload — sem dados pré-carregados
  caminho_csv <- reactive({
    arq <- input$arquivo_csv
    if (is.null(arq)) return(NULL)   # <-- NULL sem arquivo
    tryCatch({
      teste <- read_csv(arq$datapath, n_max=2, show_col_types=FALSE)
      nomes <- tolower(names(teste))
      falt  <- c("sei","unidade","status")[!c("sei","unidade","status") %in% nomes]
      if (length(falt)>0) {
        showNotification(paste0("CSV inválido. Colunas ausentes: ",paste(falt,collapse=", ")),
                         type="error",duration=8)
        return(NULL)
      }
      n <- nrow(read_csv(arq$datapath,show_col_types=FALSE))
      showNotification(paste0("Carregado: ",arq$name," (",n," linhas)"),type="message",duration=4)
      arq$datapath
    }, error=function(e){ showNotification(paste0("Erro: ",e$message),type="error",duration=8); NULL })
  })

  dados <- reactive({
    cam <- caminho_csv()
    if (is.null(cam)) return(NULL)   # <-- NULL sem dados
    withProgress(message="Processando event log...",value=0,{
      setProgress(0.2)
      d <- tryCatch(inicializar_dados(cam),
        error=function(e){ showNotification(paste0("Erro: ",e$message),type="error",duration=10); NULL })
      setProgress(1); d
    })
  })

  # Helper: TRUE se dados disponíveis
  tem_dados <- reactive({ !is.null(dados()) })

  # ── Populate selects ───────────────────────────────────────────────────
  observe({
    req(tem_dados())
    d  <- dados()
    df <- d$risco %>% arrange(risco,desc(score_normalizado)) %>%
      mutate(label=paste0(str_extract(case_id,"[^./]+/[^./]+-[^./]+$"),
                          "  [",risco," | ",score_normalizado,"pts]"))
    choices <- setNames(df$case_id,df$label)
    updateSelectInput(session,"sei_selecionado",choices=choices,selected=choices[1])
    updateSelectInput(session,"anim_sei",
      choices=setNames(df$case_id,str_extract(df$case_id,"[^./]+/[^./]+-[^./]+$")),
      selected=choices[1])
  })

  proc_sel <- reactive({
    req(tem_dados(), input$sei_selecionado)
    dados()$risco %>% filter(case_id==input$sei_selecionado)
  })

  # ── Tema ────────────────────────────────────────────────────────────────
  tema_claro <- reactiveVal(FALSE)
  observeEvent(input$btn_tema,      { tema_claro(!tema_claro()) })
  observe({
    if (tema_claro()) shinyjs::addClass(selector="body",class="light-mode")
    else              shinyjs::removeClass(selector="body",class="light-mode")
  })

  # ── Upload status ───────────────────────────────────────────────────────
  output$sidebar_status <- renderUI({
    if (!is.null(input$arquivo_csv))
      tags$div(tags$span(class="status-dot-live"),
               tags$span(str_trunc(input$arquivo_csv$name,22),
                          style="color:#7a87a3;font-size:11px;margin-left:6px;"))
    else
      tags$div(tags$span(class="status-dot-idle"),
               tags$span("Sem dados carregados",style="color:#7a87a3;font-size:11px;margin-left:6px;"))
  })

  output$fonte_status <- renderUI({
    if (tem_dados()) {
      d <- dados()
      tags$div(
        tags$p(tags$span(class="upload-status-ok",icon("check-circle")," Dados carregados")),
        tags$p(paste0("Arquivo: ",input$arquivo_csv$name), style="color:#7a87a3;font-size:12px;"),
        tags$p(paste0("Processos: ",n_distinct(d$el$case_id)),style="color:#e8ecf4;font-size:13px;"),
        tags$p(paste0("Eventos: ",  nrow(d$el)),              style="color:#e8ecf4;font-size:13px;"),
        tags$p(paste0("Unidades: ", n_distinct(d$el$resource)),style="color:#e8ecf4;font-size:13px;"),
        tags$p(paste0(format(min(d$el$timestamp),"%d/%m/%Y"),
                      " a ",format(max(d$el$timestamp),"%d/%m/%Y")),
               style="color:#7a87a3;font-size:12px;")
      )
    } else {
      tags$div(
        tags$p(tags$span(style="color:#f59e0b;font-weight:600;",
                         icon("exclamation-circle")," Nenhum arquivo carregado")),
        tags$p("Faça upload do CSV exportado do SEI para começar a análise.",
               style="color:#7a87a3;font-size:12px;line-height:1.5;")
      )
    }
  })

  output$upload_feedback <- renderUI({
    arq <- input$arquivo_csv
    if (!is.null(arq))
      tags$div(tags$span(class="upload-status-ok",icon("check-circle")),
               tags$span(paste0(" ",arq$name," — ",round(arq$size/1024,1)," KB"),
                          style="color:#7a87a3;font-size:12px;"))
    else
      tags$p("Nenhum arquivo selecionado.",style="color:#7a87a3;font-size:12px;")
  })

  output$preview_csv <- DT::renderDataTable({
    cam <- caminho_csv()
    shiny::validate(shiny::need(!is.null(cam),"Faça o upload de um arquivo CSV para visualizar os dados."))
    raw <- tryCatch(read_csv(cam,show_col_types=FALSE),
                    error=function(e) data.frame(Erro=as.character(e)))
    DT::datatable(head(raw,50),rownames=FALSE,
                  options=list(pageLength=10,dom="tp",scrollX=TRUE))
  })

  # ── Estado vazio: placeholder para abas sem dados ───────────────────────
  sem_dados_ui <- function() {
    tags$div(style="text-align:center;padding:60px 40px;",
      tags$i(class="fa fa-upload",style="font-size:48px;color:#2a3045;display:block;margin-bottom:20px;"),
      tags$h3("Nenhum dado carregado",style="color:#7a87a3;font-weight:500;margin-bottom:8px;"),
      tags$p("Vá até a aba ",tags$strong("Dados & Upload")," e carregue o CSV exportado do SEI.",
             style="color:#4a5568;font-size:13px;")
    )
  }


  # ── wkhtmltopdf path (manual override) ────────────────────────────────────
  wkhtmltopdf_path <- reactive({
    # Manual path takes priority (handle Windows backslashes)
    manual <- input$wkhtmltopdf_manual
    if (!is.null(manual) && nzchar(trimws(manual))) {
      p_norm <- normalizePath(trimws(manual), winslash="/", mustWork=FALSE)
      if (file.exists(p_norm)) return(p_norm)
      if (file.exists(manual)) return(manual)
    }
    # Sys.which (works if PATH is updated and RStudio was restarted)
    bin <- Sys.which("wkhtmltopdf")
    if (nzchar(bin)) return(normalizePath(bin, winslash="/", mustWork=FALSE))
    # Comprehensive path scan
    pf  <- Sys.getenv("ProgramFiles",      unset="C:/Program Files")
    pf86<- Sys.getenv("ProgramFiles(x86)", unset="C:/Program Files (x86)")
    pfw <- Sys.getenv("ProgramW6432",      unset=pf)
    loc <- Sys.getenv("LOCALAPPDATA",      unset="")
    all_paths <- c(
      # Standard Windows installs
      "C:/Program Files/wkhtmltopdf/bin/wkhtmltopdf.exe",
      "C:/Program Files (x86)/wkhtmltopdf/bin/wkhtmltopdf.exe",
      "C:/wkhtmltopdf/bin/wkhtmltopdf.exe",
      # Alt layout (some versions install here)
      "C:/Program Files/wkhtmltopdf/wkhtmltopdf.exe",
      "C:/Program Files (x86)/wkhtmltopdf/wkhtmltopdf.exe",
      # Via env vars
      file.path(pf,   "wkhtmltopdf/bin/wkhtmltopdf.exe"),
      file.path(pf86, "wkhtmltopdf/bin/wkhtmltopdf.exe"),
      file.path(pfw,  "wkhtmltopdf/bin/wkhtmltopdf.exe"),
      if (nzchar(loc)) file.path(loc, "Programs/wkhtmltopdf/bin/wkhtmltopdf.exe"),
      # Linux/Mac
      "/usr/bin/wkhtmltopdf", "/usr/local/bin/wkhtmltopdf",
      "/opt/wkhtmltopdf/bin/wkhtmltopdf",
      "/opt/homebrew/bin/wkhtmltopdf"
    )
    for (p in unique(all_paths[nzchar(all_paths)])) {
      p2 <- normalizePath(p, winslash="/", mustWork=FALSE)
      if (file.exists(p2)) return(p2)
    }
    return("")
  })

  output$pdf_path_ui <- renderUI({
    wk <- wkhtmltopdf_path()
    found <- nzchar(wk)

    path_input <- tags$div(
      tags$p(style="color:#7a87a3;font-size:12px;margin-bottom:4px;",
        "Informe o caminho do wkhtmltopdf.exe:"),
      textInput("wkhtmltopdf_manual", label=NULL,
        placeholder="Ex: C:/Program Files/wkhtmltopdf/bin/wkhtmltopdf.exe",
        width="100%"),
      tags$p(style="color:#7a87a3;font-size:11px;line-height:1.5;",
        icon("info-circle"), " Após instalar, reinicie o RStudio ou cole o caminho acima. ",
        tags$a("Baixar wkhtmltopdf", href="https://wkhtmltopdf.org/downloads.html",
               target="_blank", style="color:#6366f1;"))
    )

    if (found) {
      tagList(
        tags$div(class="explain-box",
          tags$span(style="color:#22c55e;", icon("check-circle")), " ",
          tags$strong("wkhtmltopdf encontrado:"), tags$br(),
          tags$code(style="font-size:11px;color:#7a87a3;word-break:break-all;", wk)
        )
      )
    } else {
      tagList(
        tags$div(class="explain-box",
          style="border-color:#f59e0b;background:#2d1f05;",
          tags$span(style="color:#f59e0b;", icon("exclamation-triangle")), " ",
          tags$strong(style="color:#f59e0b;", "wkhtmltopdf nao encontrado"), tags$br(),
          tags$span(style="color:#7a87a3;font-size:12px;",
            "Use o botao ", tags$strong("Baixar HTML"), " como alternativa sem instalacao.")
        ),
        path_input
      )
    }
  })

  # ── Verificação de dependências do sistema ─────────────────────────────
  output$system_deps_status <- renderUI({
    # Kept for backward compat — use deps_aviso_ui instead
    NULL
  })

  # deps_aviso_ui: compact warning shown on upload tab only when something missing
  output$deps_aviso_ui <- renderUI({
    dot_ok <- nzchar(encontrar_dot())
    wk_ok  <- nzchar(wkhtmltopdf_path())
    if (dot_ok && wk_ok) return(NULL)
    items <- list()
    if (!dot_ok) items[[length(items)+1]] <- tags$li(
      tags$strong("Graphviz (dot)"), " não encontrado — Mapa de Processo indisponível. ",
      tags$a("Instalar", href="https://graphviz.org/download/", target="_blank",
             style="color:#6366f1;"))
    if (!wk_ok) items[[length(items)+1]] <- tags$li(
      tags$strong("wkhtmltopdf"), " não encontrado — exportação PDF indisponível. ",
      tags$a("Instalar", href="https://wkhtmltopdf.org/downloads.html", target="_blank",
             style="color:#6366f1;"))
    tags$div(class="explain-box",style="border-color:#f59e0b;background:#2d1f05;margin:0;",
      tags$span(style="color:#f59e0b;font-weight:600;", icon("exclamation-triangle"),
                " Dependências opcionais ausentes:"),
      tags$ul(style="margin:6px 0 0 0;padding-left:20px;", items)
    )
  })

  # ── Visão Geral ─────────────────────────────────────────────────────────
  output$ib_alto    <- renderInfoBox(infoBox("Risco Alto",  if(tem_dados()) sum(dados()$risco$risco=="ALTO")  else "—",icon=icon("exclamation-triangle"),color="red",   subtitle="processos críticos",fill=TRUE))
  output$ib_medio   <- renderInfoBox(infoBox("Risco Médio", if(tem_dados()) sum(dados()$risco$risco=="MÉDIO") else "—",icon=icon("eye"),                  color="yellow",subtitle="em observação",fill=TRUE))
  output$ib_baixo   <- renderInfoBox(infoBox("Risco Baixo", if(tem_dados()) sum(dados()$risco$risco=="BAIXO") else "—",icon=icon("check-circle"),          color="green", subtitle="dentro do esperado",fill=TRUE))
  output$ib_abertos <- renderInfoBox(infoBox("Em Andamento",if(tem_dados()) sum(dados()$risco$status_final=="ABERTO") else "—",icon=icon("spinner"),       color="blue",  subtitle="processos abertos",fill=TRUE))

  output$tabela_geral <- DT::renderDataTable({
    if (!tem_dados()) return(DT::datatable(data.frame(Aviso="Faça upload de dados na aba Dados & Upload.")))
    df <- dados()$risco %>% arrange(risco,desc(score_normalizado)) %>%
      transmute(
        Processo       = str_extract(case_id,"[^./]+/[^./]+-[^./]+$"),
        Risco          = sapply(as.character(risco),badge_html),
        Status         = ifelse(status_final=="ABERTO","<span style='color:#f59e0b'>&#9679; Aberto</span>","<span style='color:#22c55e'>&#9679; Concluído</span>"),
        `Duração`      = ifelse(throughput_days<1,"< 1 dia",paste0(round(throughput_days)," dias")),
        `Movimentações`= n_eventos, Setores=n_unidades, Trocas=handovers,
        Retrabalho     = ifelse(retrabalho>0,paste0("<span style='color:#ef4444'>",retrabalho,"x</span>"),"<span style='color:#22c55e'>—</span>"),
        Score          = score_normalizado
      )
    DT::datatable(df,escape=FALSE,rownames=FALSE,selection="single",
      options=list(pageLength=15,dom="tp",ordering=TRUE,
        columnDefs=list(list(className="dt-center",targets=c(1,2,3,4,5,6,7,8)))))
  })
  observeEvent(input$tabela_geral_rows_selected,{
    idx <- input$tabela_geral_rows_selected
    if (!is.null(idx) && tem_dados()) {
      df <- dados()$risco %>% arrange(risco,desc(score_normalizado))
      updateSelectInput(session,"sei_selecionado",selected=df$case_id[idx])
      updateTabItems(session,"menu_ativo","processo")
    }
  })
  output$plot_risco_bar <- renderPlotly({
    if (!tem_dados()) return(plot_ly() %>% plotly_tema())
    df <- dados()$risco %>% count(risco) %>%
      mutate(risco=factor(as.character(risco),levels=c("ALTO","MÉDIO","BAIXO")),
             cor=c("ALTO"=COR_ALTO,"MÉDIO"=COR_MEDIO,"BAIXO"=COR_BAIXO)[as.character(risco)])
    plot_ly(df,x=~risco,y=~n,type="bar",
      marker=list(color=~cor,line=list(color=BG,width=1.5)),
      text=~paste0(n,"x"),textposition="outside",textfont=list(color=TEXT,size=13),
      hovertemplate="<b>%{x}</b>: %{y}<extra></extra>") %>%
      layout(xaxis=list(title="",tickfont=list(size=14,color=TEXT)),
             yaxis=list(title="N° processos",dtick=1,range=c(0,8)),showlegend=FALSE) %>% plotly_tema()
  })
  output$plot_scatter <- renderPlotly({
    if (!tem_dados()) return(plot_ly() %>% plotly_tema())
    df <- dados()$risco %>%
      mutate(cor=c("ALTO"=COR_ALTO,"MÉDIO"=COR_MEDIO,"BAIXO"=COR_BAIXO)[as.character(risco)],
             proc=str_extract(case_id,"[^./]+/[^./]+-[^./]+$"))
    plot_ly(df,x=~throughput_days,y=~n_eventos,type="scatter",mode="markers+text",
      marker=list(color=~cor,size=14,opacity=.85,line=list(color=BG,width=2)),
      text=~proc,textposition="top center",textfont=list(color=MUTED,size=9),
      hovertemplate="<b>%{text}</b><br>%{x:.0f} dias · %{y} eventos<extra></extra>") %>%
      layout(xaxis=list(title="Duração (dias)"),yaxis=list(title="N° movimentações"),showlegend=FALSE) %>% plotly_tema()
  })

  # ── Processo Individual ─────────────────────────────────────────────────
  output$ib_proc_risco  <- renderInfoBox({
    if (!tem_dados()) return(infoBox("Risco","—",icon=icon("shield-alt"),color="blue",fill=TRUE))
    p<-proc_sel(); cor<-switch(as.character(p$risco),"ALTO"="red","MÉDIO"="yellow","BAIXO"="green","blue")
    infoBox("Risco",as.character(p$risco),icon=icon("shield-alt"),color=cor,
            subtitle=paste0("Score: ",p$score_normalizado,"/100"),fill=TRUE)
  })
  output$ib_proc_dias <- renderInfoBox({
    if (!tem_dados()) return(infoBox("Duração","—",icon=icon("calendar-alt"),color="blue",fill=TRUE))
    p <- proc_sel()
    infoBox("Duração",ifelse(p$throughput_days<1,"< 1 dia",paste0(round(p$throughput_days)," dias")),
            icon=icon("calendar-alt"),color="blue",
            subtitle=paste0(format(p$inicio,"%d/%m/%Y")," → ",format(p$fim,"%d/%m/%Y")),fill=TRUE)
  })
  output$ib_proc_events <- renderInfoBox({
    if (!tem_dados()) return(infoBox("Movimentações","—",icon=icon("list-ul"),color="purple",fill=TRUE))
    infoBox("Movimentações",proc_sel()$n_eventos,icon=icon("list-ul"),color="purple",
            subtitle="eventos no SEI",fill=TRUE)
  })
  output$ib_proc_units <- renderInfoBox({
    if (!tem_dados()) return(infoBox("Setores","—",icon=icon("sitemap"),color="teal",fill=TRUE))
    infoBox("Setores",proc_sel()$n_unidades,icon=icon("sitemap"),color="teal",
            subtitle="unidades envolvidas",fill=TRUE)
  })

  output$legenda_gantt <- renderUI({
    if (!tem_dados()) return(NULL)
    segs  <- dados()$segmentos %>% filter(case_id==input$sei_selecionado)
    items <- lapply(unique(segs$resource),function(u){
      cor <- cor_u(u)
      tags$div(class="unit-item",
        tags$span(class="unit-dot",style=paste0("background:",cor,";")),
        tags$span(str_replace(u,"SES - ",""),style="color:#e8ecf4;font-size:12px;"))
    })
    tags$div(class="unit-legend",items)
  })

  output$plot_gantt <- renderPlotly({
    if (!tem_dados()) return(plotly::plot_ly() %>% plotly_tema())
    req(input$sei_selecionado)
    sei      <- input$sei_selecionado
    segs_raw <- dados()$segmentos %>% filter(case_id == sei)
    shiny::validate(shiny::need(nrow(segs_raw) > 0, "Sem dados de linha do tempo."))

    segs <- segs_raw %>%
      arrange(seg_inicio) %>%
      mutate(
        cor      = sapply(resource, cor_u),
        lbl      = str_replace(resource, "SES - ", ""),
        dur_fmt  = ifelse(duracao_horas < 24,
                     paste0(round(duracao_horas, 1), "h"),
                     paste0(round(duracao_horas / 24, 1), " dias")),
        tooltip  = paste0(lbl,
                     "\nEntrada: ", format(seg_inicio, "%d/%m/%Y %H:%M"),
                     "\nSaida: ",   format(seg_fim,    "%d/%m/%Y %H:%M"),
                     "\nTempo: ",   dur_fmt,
                     "\nEventos: ", n_eventos),
        seg_fim_v = pmax(seg_fim, seg_inicio + as.difftime(0.5, units="hours"))
      )

    unid_ord <- segs %>%
      group_by(lbl) %>% summarise(td=sum(duracao_horas),.groups="drop") %>%
      arrange(td) %>% pull(lbl)
    segs$lbl <- factor(segs$lbl, levels=unid_ord)

    p_gg <- ggplot2::ggplot(segs) +
      ggplot2::geom_segment(
        ggplot2::aes(x=seg_inicio, xend=seg_fim_v, y=lbl, yend=lbl,
                     color=resource, text=tooltip),
        linewidth=9, lineend="round", alpha=0.85
      ) +
      ggplot2::scale_color_manual(
        values=setNames(sapply(unique(segs$resource), cor_u), unique(segs$resource)),
        guide="none"
      ) +
      ggplot2::scale_x_datetime(date_labels="%d/%m", date_breaks="2 weeks") +
      ggplot2::labs(x=NULL, y=NULL) +
      ggplot2::theme_minimal(base_size=11) +
      ggplot2::theme(
        plot.background   = ggplot2::element_rect(fill=BG2, color=NA),
        panel.background  = ggplot2::element_rect(fill=BG2, color=NA),
        panel.grid.major.x= ggplot2::element_line(color=BORDER, linewidth=0.3),
        panel.grid.major.y= ggplot2::element_blank(),
        panel.grid.minor  = ggplot2::element_blank(),
        axis.text         = ggplot2::element_text(color=MUTED, size=9),
        axis.text.x       = ggplot2::element_text(angle=30, hjust=1)
      )

    plotly::ggplotly(p_gg, tooltip="text") %>%
      plotly_tema() %>%
      plotly::layout(margin=list(l=10,r=10,t=10,b=60))
  })

  output$tabela_indicadores <- DT::renderDataTable({
    if (!tem_dados()) return(DT::datatable(data.frame(Aviso="Faça upload de dados.")))
    p     <- proc_sel()
    prazo <- 90
    df <- data.frame(
      Indicador=c("1. Consumo de prazo (ref: 90 dias)","2. Retrabalho detectado",
        "3. Setores envolvidos","4. Passou por etapa lenta",
        "5. Aberto com duração alta","Pontuação composta (0-100)"),
      Valor=c(
        paste0(round(as.numeric(p$throughput_days)/prazo*100),"%"),
        ifelse(as.integer(p$retrabalho)>0,paste0(p$retrabalho," ocorrência(s)"),"Nenhum"),
        paste0(p$n_unidades," setor(es)"),
        ifelse(isTRUE(p$passou_gargalo),"Sim","Não"),
        ifelse(isTRUE(as.numeric(p$idx_aberto_longo)>0),"Sim","Não"),
        paste0(p$score_normalizado," / 100  [",as.character(p$risco),"]")
      ), stringsAsFactors=FALSE)
    DT::datatable(df,rownames=FALSE,selection="none",
      options=list(dom="t",pageLength=6,ordering=FALSE,
        columnDefs=list(list(className="dt-left",targets="_all"))))
  })

  output$trace_visual <- renderUI({
    if (!tem_dados()) return(NULL)
    tr <- dados()$traces %>% filter(case_id==input$sei_selecionado)
    if (nrow(tr)==0) return(tags$p("Trace não disponível.",style="color:#7a87a3;"))
    units <- str_split(tr$trace_units[1]," \u2192 ")[[1]]
    rle_u <- rle(units)
    items <- lapply(seq_along(rle_u$values),function(i){
      u   <- rle_u$values[i]; cor <- cor_u(u)
      lbl <- if(rle_u$lengths[i]>1) paste0(str_replace(u,"SES - ","")," (\u00d7",rle_u$lengths[i],")")
             else str_replace(u,"SES - ","")
      tagList(
        tags$div(class="trace-tag",
          style=paste0("background:",cor,"22;border:1px solid ",cor,";"),
          tags$span(style=paste0("width:7px;height:7px;border-radius:50%;background:",cor,";display:inline-block;")),
          tags$span(lbl,style="font-size:11px;")),
        if(i<length(rle_u$values))
          tags$span("\u2192",style="color:#7a87a3;font-size:13px;margin:0 1px;")
      )
    })
    tags$div(style="display:flex;flex-wrap:wrap;align-items:center;padding:8px 0;",
      tags$p(style="color:#7a87a3;font-size:11px;margin-bottom:6px;width:100%;",
        paste0(length(units)," passagens | ",length(rle_u$values)," blocos distintos")),
      items)
  })

  # ── Rede ────────────────────────────────────────────────────────────────
  node_df_r <- reactive({
    req(tem_dados())
    g  <- dados()$grafo; mu <- dados()$metricas_u
    coords <- tibble(
      name=c("SES - GCJ_ARP","SES - GCJ","SES - GFAJ_SA","SES - NP_DGPO",
              "SES - CSANS","SES - GFAJ","SES - GORC","SES - NAJ"),
      x=c(.50,.18,.18,.82,.82,.05,.95,.50), y=c(.50,.68,.32,.78,.22,.50,.50,.08))
    tibble(name=V(g)$name,betweenness=round(V(g)$betweenness*100,1),degree=V(g)$degree_tot) %>%
      left_join(coords,by="name") %>%
      left_join(mu%>%select(Unidade,centralidade),by=c("name"="Unidade")) %>%
      mutate(cor=sapply(name,cor_u),label=str_replace(name,"SES - ",""),
             sz=rescale(centralidade,to=c(28,65)),
             htxt=paste0("<b>",name,"</b><br>Centralidade: ",centralidade,
                         "<br>Intermediação: ",betweenness,"%<br>Conexões: ",degree))
  })

  output$plot_rede <- renderPlotly({
    if (!tem_dados()) return(plot_ly()%>%plotly_tema())
    node_df <- node_df_r(); ho <- dados()$handover_df
    p <- plot_ly()
    for (i in seq_len(nrow(ho))) {
      rw<-ho[i,]; nf<-node_df%>%filter(name==rw$from_unit); nt<-node_df%>%filter(name==rw$to_unit)
      if (nrow(nf)==0||nrow(nt)==0) next
      cor_a<-if(rw$avg_wait_hours>48)COR_ALTO else if(rw$avg_wait_hours>8)COR_MEDIO else COR_BAIXO
      w<-rescale(rw$n_handovers,from=c(1,max(ho$n_handovers)),to=c(1.5,6))
      ht<-paste0("<b>",str_replace(rw$from_unit,"SES - ","")," → ",
                 str_replace(rw$to_unit,"SES - ",""),"</b><br>Passagens: ",rw$n_handovers,
                 "<br>Espera: ",fmt_horas(rw$avg_wait_hours))
      mx<-(nf$x+nt$x)/2; my<-(nf$y+nt$y)/2
      p <- p %>%
        add_trace(type="scatter",mode="lines",x=c(nf$x,nt$x,NA),y=c(nf$y,nt$y,NA),
          line=list(color=cor_a,width=w),opacity=.75,hovertext=ht,hoverinfo="text",showlegend=FALSE) %>%
        add_trace(type="scatter",mode="text",x=mx,y=my,text=paste0(rw$n_handovers,"\u00d7"),
          textfont=list(color=MUTED,size=9,family="DM Mono"),hoverinfo="none",showlegend=FALSE)
    }
    p %>%
      add_trace(type="scatter",mode="markers+text",
        x=node_df$x,y=node_df$y,text=node_df$label,textposition="top center",
        textfont=list(color=TEXT,size=11,family="DM Sans"),
        marker=list(color=node_df$cor,size=node_df$sz,opacity=.92,line=list(color=BG,width=2.5)),
        hovertext=node_df$htxt,hoverinfo="text",showlegend=FALSE) %>%
      layout(xaxis=list(visible=FALSE,range=c(-.08,1.08)),
             yaxis=list(visible=FALSE,range=c(-.08,1.08)),
             margin=list(l=0,r=0,t=10,b=10)) %>% plotly_tema()
  })

  output$mapa_stats <- renderUI({
    if (!tem_dados()) return(NULL)
    mapa <- dados()$mapa
    tags$div(
      tags$p(paste0("Nós: ",nrow(mapa$nos)),style="color:#e8ecf4;font-size:13px;"),
      tags$p(paste0("Arestas: ",nrow(mapa$arestas)),style="color:#e8ecf4;font-size:13px;"),
      tags$p(paste0("Fluxos lentos (>48h): ",sum(mapa$arestas$wait_avg_h>48)),style="color:#ef4444;font-size:13px;font-weight:600;"),
      tags$p(paste0("Entradas: ",sum(mapa$nos$is_source)),style="color:#22c55e;font-size:13px;"),
      tags$p(paste0("Saídas: ",sum(mapa$nos$is_sink)),style="color:#94a3b8;font-size:13px;")
    )
  })

  output$plot_centralidade <- renderPlotly({
    if (!tem_dados()) return(plot_ly()%>%plotly_tema())
    mu <- dados()$metricas_u %>%
      mutate(label=str_replace(Unidade,"SES - ",""),cor=sapply(Unidade,cor_u)) %>%
      arrange(centralidade)
    plot_ly(mu,x=~centralidade,y=~reorder(label,centralidade),type="bar",orientation="h",
      marker=list(color=~cor,opacity=.85,line=list(color=BG,width=1)),
      text=~centralidade,textposition="outside",textfont=list(color=TEXT),
      hovertemplate="<b>%{y}</b><br>Centralidade: %{x}<extra></extra>") %>%
      layout(xaxis=list(title="Centralidade"),yaxis=list(title=""),showlegend=FALSE) %>% plotly_tema()
  })

  output$tabela_handovers <- DT::renderDataTable({
    if (!tem_dados()) return(DT::datatable(data.frame()))
    df <- dados()$handover_df %>% arrange(desc(n_handovers)) %>%
      transmute(De=str_replace(from_unit,"SES - ",""),Para=str_replace(to_unit,"SES - ",""),
        Passagens=n_handovers,`Espera media`=fmt_horas(avg_wait_hours),`Espera max`=fmt_horas(max_wait_hours),
        Velocidade=case_when(avg_wait_hours>48~"<span class='badge-alto'>Lento</span>",avg_wait_hours>8~"<span class='badge-medio'>Moderado</span>",TRUE~"<span class='badge-baixo'>Rapido</span>"))
    DT::datatable(df,escape=FALSE,rownames=FALSE,options=list(pageLength=21,dom="tip",columnDefs=list(list(className="dt-center",targets=c(2,3,4,5)))))
  })

  nos_tbl <- reactive({
    req(tem_dados())
    dados()$mapa$nos %>%
      transmute(Unidade=str_replace(id,"SES - ",""),Papel=papel,Centralidade=centralidade,
        Processos=processos,Entrada=ifelse(is_source,"Sim","Nao"),Saida=ifelse(is_sink,"Sim","Nao"))
  })
  arestas_tbl <- reactive({
    req(tem_dados())
    dados()$mapa$arestas %>%
      transmute(De=str_replace(source,"SES - ",""),Para=str_replace(target,"SES - ",""),
        Passagens=frequency,`Espera avg`=fmt_h(wait_avg_h),Performance=performance)
  })
  for (id in c("tabela_mapa_nos","tabela_mapa_nos2")) {
    local({ oid<-id; output[[oid]] <- DT::renderDataTable({
      if (!tem_dados()) return(DT::datatable(data.frame()))
      DT::datatable(nos_tbl(),rownames=FALSE,options=list(dom="t",pageLength=10))
    }) })
  }
  for (id in c("tabela_mapa_arestas","tabela_mapa_arestas2")) {
    local({ oid<-id; output[[oid]] <- DT::renderDataTable({
      if (!tem_dados()) return(DT::datatable(data.frame()))
      DT::datatable(arestas_tbl(),rownames=FALSE,options=list(dom="tip",pageLength=21))
    }) })
  }

  # ── Mapa de Processo (Graphviz) ──────────────────────────────────────────
  map_tipo <- reactiveVal("frequency")
  observeEvent(input$map_freq,   { map_tipo("frequency")   })
  observeEvent(input$map_perf,   { map_tipo("performance") })
  observeEvent(input$map_risco,  { map_tipo("risco")       })
  observeEvent(input$map_retrab, { map_tipo("retrabalho")  })
  observeEvent(input$map_anim,   { map_tipo("animation")   })

  output$mapa_tipo_label <- renderUI({
    labels <- list(
      frequency  =list("Frequência — n° de handovers por transição","#6366f1"),
      performance=list("Performance — tempo de espera por transição","#06b6d4"),
      risco      =list("Risco — exposição de cada setor a processos ALTO","#ef4444"),
      retrabalho =list("Retrabalho — loops e retornos (tracejado = caminho de volta)","#f59e0b"),
      animation  =list("Animação — simulação do fluxo ao longo do tempo","#8b5cf6")
    )
    info <- labels[[map_tipo()]]
    tags$div(style=paste0("background:",info[[2]],"22;border:1px solid ",info[[2]],
      ";border-radius:8px;padding:8px 14px;font-size:12px;color:",info[[2]],";"),
      tags$strong("Modo atual: "), info[[1]])
  })

  # SVG map — always rendered server-side, shown/hidden via CSS
  output$process_map_svg <- renderUI({
    if (!tem_dados()) return(sem_dados_ui())
    tipo <- map_tipo()
    if (tipo=="animation") return(NULL)
    d <- dados()
    dot_str <- tryCatch(switch(tipo,
      frequency  = dot_frequency(d),
      performance= dot_performance(d),
      risco      = dot_risco(d),
      retrabalho = dot_retrabalho(d)
    ), error=function(e){ message("DOT error: ",e$message); NULL })
    if (is.null(dot_str))
      return(tags$p("Erro ao gerar DOT.",style="color:#ef4444;padding:20px;"))
    svg_str <- tryCatch(dot_to_svg(dot_str), error=function(e){ e$message })
    if (is.null(svg_str) || !grepl("<svg", svg_str, fixed=TRUE)) {
      is_not_found <- grepl("nao encontrado|not found|Graphviz", svg_str, ignore.case=TRUE)
      if (is_not_found) {
        return(tags$div(style="padding:24px;",
          tags$div(class="explain-box",
            tags$strong(icon("exclamation-triangle"), " Graphviz nao encontrado"), tags$br(),
            tags$strong("Windows:"), " ", tags$a("graphviz.org/download", href="https://graphviz.org/download/", target="_blank"),
            " — marque 'Add to PATH' durante a instalacao, depois reinicie o RStudio.", tags$br(),
            tags$strong("Linux: "), tags$code("sudo apt-get install graphviz"), tags$br(),
            tags$strong("Mac: "), tags$code("brew install graphviz")
          )
        ))
      }
      return(tags$p(paste0("Erro: ", svg_str), style="color:#ef4444;padding:20px;"))
    }

    # Wrap SVG with zoom controls
    zoom_id <- paste0("zsvg_", sample(1e6, 1))
    tags$div(
      style = "position:relative;",
      # Zoom control bar
      tags$div(class="zoom-controls",
        tags$button("-", class="zoom-btn", id=paste0(zoom_id,"_out"),
                    onclick=paste0("zoomMap('",zoom_id,"',-0.2)")),
        tags$span("100%", class="zoom-label", id=paste0(zoom_id,"_lbl")),
        tags$button("+", class="zoom-btn", id=paste0(zoom_id,"_in"),
                    onclick=paste0("zoomMap('",zoom_id,"',0.2)")),
        tags$button("↺", class="zoom-btn", id=paste0(zoom_id,"_rst"),
                    onclick=paste0("zoomMap('",zoom_id,"',0,true)"),
                    title="Restaurar zoom")
      ),
      # SVG container
      tags$div(class="process-map-svg", id=zoom_id,
        style="overflow:hidden;",
        HTML(svg_str)
      ),
      # Zoom JS (scoped to this instance)
      tags$script(HTML(paste0("
        (function(){
          var scale=1, minS=0.3, maxS=4;
          var cont=document.getElementById('",zoom_id,"');
          var lbl =document.getElementById('",zoom_id,"_lbl');
          function apply(){
            var svg=cont&&cont.querySelector('svg');
            if(!svg)return;
            svg.style.transform='scale('+scale.toFixed(2)+')';
            svg.style.transformOrigin='top left';
            svg.style.width=(100/scale)+'%';
            lbl.textContent=Math.round(scale*100)+'%';
          }
          window.zoomMap=window.zoomMap||function(id,delta,reset){
            if(id!='",zoom_id,"')return;
            if(reset){scale=1;}else{scale=Math.min(maxS,Math.max(minS,scale+delta));}
            apply();
          };
          // Wheel zoom
          if(cont) cont.addEventListener('wheel',function(e){
            e.preventDefault();
            var delta=e.deltaY<0?0.15:-0.15;
            scale=Math.min(maxS,Math.max(minS,scale+delta));
            apply();
          },{passive:false});
          // Initial render
          setTimeout(apply,100);
        })();
      ")))
    )
  })

  output$process_map_legend <- renderUI({
    tipo <- map_tipo()
    content <- switch(tipo,
      frequency=tagList(tags$strong("Nós:"),tags$br(),
        tags$span(style="color:#6366f1;","◆ Diamante = entrada/saída"),tags$br(),
        tags$span(style="color:#06b6d4;","■ Caixa = intermediário"),tags$br(),tags$br(),
        tags$strong("Arestas:"),tags$br(),
        tags$span(style="color:#22c55e;","→ < 8h"),tags$br(),
        tags$span(style="color:#f59e0b;","→ 8-48h"),tags$br(),
        tags$span(style="color:#ef4444;","→ > 48h"),tags$br(),tags$br(),
        tags$strong("Espessura")," ∝ volume"),
      performance=tagList(tags$strong("Nós — espera p/ receber:"),tags$br(),
        tags$span(style="color:#22c55e;","■ Verde < 24h"),tags$br(),
        tags$span(style="color:#f59e0b;","■ Amarelo 24-100h"),tags$br(),
        tags$span(style="color:#ef4444;","■ Vermelho > 100h"),tags$br(),tags$br(),
        tags$strong("Arestas — espera média:"),tags$br(),"Etiqueta = duração. ",
        tags$strong("Espessura")," ∝ tempo"),
      risco=tagList(tags$strong("Nós — % processos ALTO:"),tags$br(),
        tags$span(style="color:#22c55e;","■ Verde < 40%"),tags$br(),
        tags$span(style="color:#f59e0b;","■ Amarelo 40-75%"),tags$br(),
        tags$span(style="color:#ef4444;","■ Vermelho > 75%")),
      retrabalho=tagList(tags$strong("Arestas:"),tags$br(),
        tags$span(style="color:#ef4444;","--- Tracejado = loop (retorno)"),tags$br(),
        tags$span(style="color:#94a3b8;","— Contínuo = fluxo normal")),
      animation=tagList(tags$p("Selecione velocidade e processo.",style="color:#7a87a3;"))
    )
    tags$div(class="explain-box",content)
  })

  # Animação
  output$process_animation <- renderUI({
    if (!tem_dados()) return(sem_dados_ui())
    d    <- dados()
    # Use first case if anim_sei not yet set
    sei  <- if (!is.null(input$anim_sei) && nzchar(input$anim_sei)) input$anim_sei else d$risco$case_id[1]
    speed_val <- if (!is.null(input$anim_speed)) input$anim_speed else 1.5
    segs <- d$segmentos %>% filter(case_id==sei) %>% arrange(seg_inicio) %>%
      mutate(unit=str_replace(resource,"SES - ",""), cor=sapply(resource,cor_u),
             start=as.numeric(difftime(seg_inicio,min(seg_inicio),units="hours")),
             end  =as.numeric(difftime(seg_fim,   min(seg_inicio),units="hours")),
             end  =pmax(end,start+0.5))
    segs_json <- toJSON(segs%>%select(unit,cor,start,end,n_eventos),auto_unbox=TRUE)
    speed <- speed_val; max_t <- max(segs$end)
    HTML(paste0('<style>
#aw{background:#0a0d14;border-radius:10px;padding:16px;height:440px;}
#atl{height:28px;background:#1e2334;border-radius:6px;margin-bottom:14px;position:relative;overflow:hidden;}
#ap{height:100%;background:#6366f1;border-radius:6px;width:0%;transition:width .1s;}
#atl-lbl{position:absolute;right:8px;top:5px;color:#7a87a3;font-size:11px;font-family:"DM Mono",monospace;}
.al{height:38px;margin-bottom:5px;display:flex;align-items:center;}
.al-lbl{width:85px;min-width:85px;font-size:11px;color:#7a87a3;text-align:right;padding-right:8px;}
.al-trk{flex:1;height:26px;background:#1e2334;border-radius:5px;position:relative;overflow:hidden;}
.at{position:absolute;height:24px;top:1px;border-radius:4px;opacity:.85;min-width:24px;display:flex;align-items:center;justify-content:center;font-size:10px;color:#fff;font-weight:600;}
#as{text-align:center;color:#7a87a3;font-size:12px;margin-top:8px;}
</style>
<div id="aw">
<div id="atl"><div id="ap"></div><div id="atl-lbl">0h / ',round(max_t,1),'h</div></div>
<div id="als"></div><div id="as">Clique Play para iniciar a animação</div>
</div>
<script>(function(){
var segs=',segs_json,';var maxT=',max_t,';var spd=',speed,'*1.5;
var units=[...new Set(segs.map(s=>s.unit))];
var alsDiv=document.getElementById("als");var ap=document.getElementById("ap");
var lbl=document.getElementById("atl-lbl");var st=document.getElementById("as");
var laneMap={};var curT=0,animId=null,running=false;
alsDiv.innerHTML="";
units.forEach(function(u){var l=document.createElement("div");l.className="al";
  l.innerHTML=\'<div class="al-lbl">\'+u+\'</div><div class="al-trk" id="lt-\'+u+\'"></div>\';
  alsDiv.appendChild(l);laneMap[u]=document.getElementById("lt-"+u);});
function render(t){curT=t;var pct=Math.min(t/maxT*100,100);
  ap.style.width=pct+"%";lbl.textContent=t.toFixed(1)+"h / "+maxT.toFixed(1)+"h";
  units.forEach(function(u){if(laneMap[u])laneMap[u].innerHTML="";});
  segs.forEach(function(s){if(t>=s.start&&t<=s.end){var trk=laneMap[s.unit];if(!trk)return;
    var tw=trk.getBoundingClientRect().width||200;
    var pos=(s.start/maxT)*tw;var wid=Math.max(((s.end-s.start)/maxT)*tw,24);
    var tok=document.createElement("div");tok.className="at";
    tok.style.left=pos+"px";tok.style.width=wid+"px";tok.style.background=s.cor;tok.textContent=s.n_eventos;
    trk.appendChild(tok);}});
  var active=segs.filter(s=>t>=s.start&&t<=s.end);
  st.textContent=active.length>0?"Em: "+active.map(s=>s.unit).join(", "):(t>=maxT?"Concluído":"Aguardando...");}
function step(){if(!running)return;curT=Math.min(curT+maxT/(spd*150),maxT);render(curT);
  if(curT<maxT){animId=requestAnimationFrame(step);}else{running=false;}}
window.animPlay=function(){if(curT>=maxT)curT=0;running=true;animId=requestAnimationFrame(step);};
window.animReset=function(){running=false;if(animId)cancelAnimationFrame(animId);curT=0;render(0);st.textContent="Reset.";};
render(0);}());
</script>'))
  })
  observeEvent(input$anim_play,  { shinyjs::runjs("if(typeof animPlay==='function')animPlay();") })
  observeEvent(input$anim_reset, { shinyjs::runjs("if(typeof animReset==='function')animReset();") })


  # ── mapa_ou_animacao: switch between SVG maps and SMIL animation ─────────
  output$mapa_ou_animacao <- renderUI({
    tipo <- map_tipo()
    if (tipo == "animation") {
      tagList(
        fluidRow(box(
          title = "Animação de Processo — processanimateR::animate_process()",
          width=12, solidHeader=TRUE, status="primary",
          tags$div(class="explain-box",
            tags$strong("Tokens SVG (SMIL) se movem pelo mapa:"),
            " cada círculo numerado é um processo. Cor = risco. ",
            "Arraste o slider ou clique Play. Os tokens seguem o caminho real ",
            "observado nos dados, atividade por atividade.",
            tags$br(), tags$br(),
            tags$div(style="display:flex;gap:12px;flex-wrap:wrap;align-items:center;",
              sliderInput("anim_velocidade", "Velocidade (dias/segundo):",
                min=0.5, max=30, value=2, step=0.5, width="260px"),
              tags$p(style="color:#7a87a3;font-size:11px;margin:0;",
                "Filtros da aba Filtros & Exploração são aplicados automaticamente.")
            )
          ),
          uiOutput("animate_process_ui")
        ))
      )
    } else {
      fluidRow(
        box(title="Mapa de Processo", width=9, solidHeader=TRUE, status="primary",
          tags$div(class="process-map-svg", uiOutput("process_map_svg"))
        ),
        box(title="Legenda", width=3, solidHeader=TRUE, status="info",
          uiOutput("process_map_legend")
        )
      )
    }
  })


  # ══════════════════════════════════════════════════════════════════════════
  # FILTROS & EXPLORAÇÃO (edeaR equivalents)
  # ══════════════════════════════════════════════════════════════════════════

  # Event log filtrado reativo — alimenta animação + mapa filtrado
  el_filtrado <- reactive({
    if (!tem_dados()) return(NULL)
    el  <- dados()$el
    max_dur <- max(as.numeric(difftime(
      tapply(el$timestamp, el$case_id, max),
      tapply(el$timestamp, el$case_id, min), units="days"
    )), na.rm=TRUE)

    # filter_throughput_time() — use slider or defaults
    dur_min <- input$filt_dur_min %||% 0
    dur_max_inp <- input$filt_dur_max %||% ceiling(max_dur)
    # Only filter if user actually changed the sliders
    if (dur_min > 0 || dur_max_inp < ceiling(max_dur))
      el <- filter_throughput_time(el, min_days=dur_min, max_days=dur_max_inp)

    # filter_activity_presence() — only if user selected activities
    ativ_sel <- input$filt_ativ_sel
    if (length(ativ_sel) > 0)
      el <- filter_activity_presence(el, ativ_sel, mode=input$filt_ativ_mode %||% "any")

    # filter_resource_presence() — only if user selected setores
    setor_sel <- input$filt_setor_sel
    if (length(setor_sel) > 0)
      el <- filter_resource_presence(el, setor_sel, mode=input$filt_setor_mode %||% "any")

    # filter_case_performance() — only if not all levels selected
    risco_sel <- input$filt_risco %||% c("ALTO","MÉDIO","BAIXO")
    if (length(risco_sel) < 3)
      el <- filter_case_performance(el, dados()$risco, niveis=risco_sel)

    el
  })

  # Null-coalescing operator
  # Update slider max based on actual data
  observe({
    if (!tem_dados()) return()
    el <- dados()$el
    max_dur <- ceiling(max(as.numeric(difftime(
      tapply(el$timestamp, el$case_id, max),
      tapply(el$timestamp, el$case_id, min), units="days"
    )), na.rm=TRUE))
    updateSliderInput(session, "filt_dur_max", max=max_dur, value=max_dur)
  })

  observeEvent(input$btn_resetar_filtros, {
    if (!tem_dados()) return()
    el <- dados()$el
    max_dur <- ceiling(max(as.numeric(difftime(
      tapply(el$timestamp, el$case_id, max),
      tapply(el$timestamp, el$case_id, min), units="days"
    )), na.rm=TRUE))
    updateSliderInput(session, "filt_dur_min", value=0)
    updateSliderInput(session, "filt_dur_max", value=max_dur)
    updateCheckboxGroupInput(session, "filt_risco", selected=c("ALTO","MÉDIO","BAIXO"))
    updateCheckboxGroupInput(session, "filt_ativ_sel",  selected=character(0))
    updateCheckboxGroupInput(session, "filt_setor_sel", selected=character(0))
  })

  output$filt_ativ_choices <- renderUI({
    if (!tem_dados()) return(NULL)
    ativs <- sort(unique(dados()$el$activity))
    checkboxGroupInput("filt_ativ_sel", NULL, choices=ativs, selected=NULL)
  })

  output$filt_setor_choices <- renderUI({
    if (!tem_dados()) return(NULL)
    setores <- sort(unique(dados()$el$resource))
    checkboxGroupInput("filt_setor_sel", NULL,
      choices=setNames(setores, str_replace(setores,"SES - ","")),
      selected=NULL)
  })

  output$filt_n_casos <- renderInfoBox({
    n <- if(!is.null(el_filtrado())) n_distinct(el_filtrado()$case_id) else 0
    infoBox("Processos", n, icon=icon("folder"), color="blue",
            subtitle="após filtros", fill=TRUE)
  })
  output$filt_n_eventos <- renderInfoBox({
    n <- if(!is.null(el_filtrado())) nrow(el_filtrado()) else 0
    infoBox("Eventos", n, icon=icon("list"), color="purple",
            subtitle="total filtrado", fill=TRUE)
  })
  output$filt_duracao_med <- renderInfoBox({
    if (is.null(el_filtrado()) || nrow(el_filtrado())==0) return(infoBox("Duração média","—",icon=icon("clock"),color="green",fill=TRUE))
    med <- el_filtrado() %>% group_by(case_id) %>%
      summarise(dur=as.numeric(difftime(max(timestamp),min(timestamp),units="days")),.groups="drop") %>%
      pull(dur) %>% mean(na.rm=TRUE)
    infoBox("Duração média", paste0(round(med)," dias"), icon=icon("clock"),
            color="green", subtitle="dos casos filtrados", fill=TRUE)
  })

  output$filt_codigo_r <- renderUI({
    lines <- c()
    dur_min <- input$filt_dur_min %||% 0
    dur_max <- input$filt_dur_max %||% 250
    if (dur_min > 0 || dur_max < 250)
      lines <- c(lines, paste0('filter_throughput_time(el, min_days=',dur_min,', max_days=',dur_max,')'))
    ativ_sel <- input$filt_ativ_sel
    if (!is.null(ativ_sel) && length(ativ_sel)>0) {
      mode_a <- input$filt_ativ_mode %||% "any"
      lines <- c(lines, sprintf('filter_activity_presence(el, c("%s"), mode="%s")',
                                paste(ativ_sel,collapse='","'), mode_a))
    }
    setor_sel <- input$filt_setor_sel
    if (!is.null(setor_sel) && length(setor_sel)>0) {
      mode_s <- input$filt_setor_mode %||% "any"
      lines <- c(lines, sprintf('filter_resource_presence(el, c("%s"), mode="%s")',
                                paste(setor_sel,collapse='","'), mode_s))
    }
    risco_sel <- input$filt_risco
    if (!is.null(risco_sel) && length(risco_sel)<3)
      lines <- c(lines, sprintf('filter_case_performance(el, risco, niveis=c("%s"))',
                                paste(risco_sel,collapse='","')))

    if (length(lines)==0) {
      tags$div(class="explain-box",
        icon("code"), " Código R equivalente aparecerá aqui conforme você ajusta os filtros.")
    } else {
      tags$div(class="explain-box",
        tags$strong("Código R equivalente (edeaR):"), tags$br(),
        tags$code(style="font-size:11px;color:#a5b4fc;white-space:pre-wrap;",
          paste(c("el_filtrado <- el",paste0("  |> ",lines)), collapse="
")))
    }
  })

  output$tabela_filtro_resultado <- DT::renderDataTable({
    el <- el_filtrado()
    if (is.null(el) || nrow(el)==0) return(DT::datatable(data.frame(Aviso="Nenhum caso corresponde aos filtros.")))
    df <- el %>% group_by(case_id) %>%
      summarise(
        Duracao_dias = round(as.numeric(difftime(max(timestamp),min(timestamp),units="days"))),
        Eventos = n(), Setores = n_distinct(resource),
        Inicio  = format(min(timestamp),"%d/%m/%Y"),
        .groups = "drop"
      ) %>%
      left_join(dados()$risco %>% mutate(risco=as.character(risco)) %>% select(case_id,risco,score_normalizado), by="case_id") %>%
      transmute(
        Processo = str_extract(case_id,"[^./]+/[^./]+-[^./]+$"),
        Risco    = sapply(risco, badge_html),
        `Duração (dias)` = Duracao_dias,
        Eventos, Setores, Inicio,
        Score = score_normalizado
      )
    DT::datatable(df, escape=FALSE, rownames=FALSE,
      options=list(pageLength=10, dom="tp",
        columnDefs=list(list(className="dt-center",targets=c(1,2,3,4,5)))))
  })

  output$mapa_filtrado <- renderUI({
    el <- el_filtrado()
    if (is.null(el) || n_distinct(el$case_id)==0)
      return(tags$p("Nenhum caso após os filtros.",style="color:#7a87a3;padding:20px;"))
    d_filt <- tryCatch({
      d_tmp <- dados()
      d_tmp$el <- el
      d_tmp$handover_df  <- calcular_handover_matrix(el)
      d_tmp$metricas_u   <- calcular_metricas_unidade(el, d_tmp$handover_df)
      d_tmp
    }, error=function(e) NULL)
    if (is.null(d_filt)) return(tags$p("Erro ao calcular mapa filtrado.",style="color:#ef4444;"))
    svg_str <- tryCatch(dot_to_svg(dot_frequency(d_filt)), error=function(e) NULL)
    if (is.null(svg_str)) return(tags$p("Graphviz não disponível.",style="color:#f59e0b;padding:20px;"))
    HTML(svg_str)
  })

  output$plot_duracao_filtrado <- renderPlotly({
    el <- el_filtrado()
    if (is.null(el) || nrow(el)==0) return(plot_ly() %>% plotly_tema())
    df <- el %>% group_by(case_id) %>%
      summarise(dur=as.numeric(difftime(max(timestamp),min(timestamp),units="days")),.groups="drop") %>%
      left_join(dados()$risco %>% mutate(risco=as.character(risco)) %>% select(case_id,risco),by="case_id") %>%
      mutate(cor=c("ALTO"=COR_ALTO,"MÉDIO"=COR_MEDIO,"BAIXO"=COR_BAIXO)[risco],
             proc=str_extract(case_id,"[^./]+/[^./]+-[^./]+$"))
    plot_ly(df, x=~reorder(proc,dur), y=~dur, type="bar",
      marker=list(color=~cor, opacity=.85, line=list(color=BG,width=1)),
      text=~paste0(round(dur),"d"), textposition="outside",
      textfont=list(color=TEXT, size=10),
      hovertemplate="<b>%{x}</b><br>%{y:.0f} dias<extra></extra>") %>%
      layout(xaxis=list(title="",tickangle=-30), yaxis=list(title="Dias"),
             showlegend=FALSE) %>% plotly_tema()
  })

  # ══════════════════════════════════════════════════════════════════════════
  # ANIMAÇÃO DE PROCESSO (animate_process equivalent)
  # ══════════════════════════════════════════════════════════════════════════

  output$animate_process_ui <- renderUI({
    if (!tem_dados()) return(sem_dados_ui())
    d   <- dados()
    el  <- el_filtrado()
    if (is.null(el) || nrow(el) == 0) el <- d$el
    vel <- input$anim_velocidade %||% 1
    withProgress(message="Gerando animação...", value=0.5, {
      html_str <- tryCatch(
        animate_process(d, el_filtrado=el, velocidade=vel),
        error=function(e) paste0("<p style='color:#ef4444;padding:20px;'>Erro: ",e$message,"</p>")
      )
      setProgress(1)
    })
    HTML(html_str)
  })

  # ── Atividades ────────────────────────────────────────────────────────────
  output$plot_activity_presence <- renderPlotly({
    if (!tem_dados()) return(plot_ly()%>%plotly_tema())
    ap <- dados()$act_presence %>% arrange(pct_presenca) %>%
      mutate(cor=case_when(pct_presenca==100~COR_BAIXO,pct_presenca>=50~"#06b6d4",pct_presenca>=20~COR_MEDIO,TRUE~COR_ALTO))
    plot_ly(ap,x=~pct_presenca,y=~reorder(activity,pct_presenca),type="bar",orientation="h",
      marker=list(color=~cor,opacity=.85),text=~paste0(pct_presenca,"%"),
      textposition="outside",textfont=list(color=TEXT),
      hovertemplate="<b>%{y}</b><br>%{x}% dos processos<extra></extra>") %>%
      layout(xaxis=list(title="% dos processos",range=c(0,120),ticksuffix="%"),
             yaxis=list(title=""),showlegend=FALSE) %>% plotly_tema()
  })
  output$plot_activity_volume <- renderPlotly({
    if (!tem_dados()) return(plot_ly()%>%plotly_tema())
    av <- dados()$el %>% count(activity,name="total") %>% arrange(desc(total)) %>% head(10)
    plot_ly(av,x=~reorder(activity,total),y=~total,type="bar",
      marker=list(color="#6366f1",opacity=.85,line=list(color=BG,width=1)),
      text=~total,textposition="outside",textfont=list(color=TEXT),
      hovertemplate="<b>%{x}</b><br>Total: %{y}<extra></extra>") %>%
      layout(xaxis=list(title="",tickangle=-35),yaxis=list(title="Total"),showlegend=FALSE) %>% plotly_tema()
  })
  output$tabela_traces <- DT::renderDataTable({
    if (!tem_dados()) return(DT::datatable(data.frame()))
    df <- dados()$traces %>% distinct(case_id,.keep_all=TRUE) %>%
      transmute(Processo=str_extract(case_id,"[^./]+/[^./]+-[^./]+$"),
        Sequencia=str_replace_all(trace_units,"SES - ",""),Eventos=n_eventos)
    DT::datatable(df,rownames=FALSE,options=list(pageLength=11,dom="tp",scrollX=TRUE))
  })

  # ── Unidades ────────────────────────────────────────────────────────────
  output$plot_centralidade_barras <- renderPlotly({
    if (!tem_dados()) return(plot_ly()%>%plotly_tema())
    mu <- dados()$metricas_u %>%
      mutate(label=str_replace(Unidade,"SES - ",""),cor=sapply(Unidade,cor_u),
        papel=case_when(centralidade>=70~"Hub central",centralidade>=20~"Intermediário",TRUE~"Especializado")) %>%
      arrange(centralidade)
    plot_ly(mu,x=~centralidade,y=~reorder(label,centralidade),type="bar",orientation="h",
      color=~papel,colors=c("Hub central"=COR_ALTO,"Intermediário"=COR_MEDIO,"Especializado"=COR_BAIXO),
      text=~centralidade,textposition="outside",textfont=list(color=TEXT),
      hovertemplate="<b>%{y}</b><br>Centralidade: %{x}<extra></extra>") %>%
      layout(xaxis=list(title="Centralidade"),yaxis=list(title=""),legend=list(title=list(text="Papel"))) %>% plotly_tema()
  })
  output$plot_env_rec <- renderPlotly({
    if (!tem_dados()) return(plot_ly()%>%plotly_tema())
    mu <- dados()$metricas_u %>% mutate(label=str_replace(Unidade,"SES - ","")) %>% arrange(desc(centralidade))
    plot_ly(mu,y=~reorder(label,centralidade)) %>%
      add_trace(type="bar",orientation="h",x=~enviou, name="Enviou", marker=list(color="#6366f1",opacity=.85),hovertemplate="<b>%{y}</b><br>Enviou: %{x}<extra></extra>") %>%
      add_trace(type="bar",orientation="h",x=~recebeu,name="Recebeu",marker=list(color="#06b6d4",opacity=.85),hovertemplate="<b>%{y}</b><br>Recebeu: %{x}<extra></extra>") %>%
      layout(barmode="group",xaxis=list(title="N° passagens"),yaxis=list(title="")) %>% plotly_tema()
  })
  output$tabela_unidades <- DT::renderDataTable({
    if (!tem_dados()) return(DT::datatable(data.frame()))
    df <- dados()$metricas_u %>%
      transmute(Setor=Unidade,Eventos=total_eventos,Processos=processos,Enviou=enviou,Recebeu=recebeu,
        Centralidade=centralidade,`Espera p/ entrar`=fmt_horas(wait_medio_receber),
        Papel=case_when(centralidade>=70~"<span class='badge-alto'>Hub</span>",centralidade>=20~"<span class='badge-medio'>Intermediário</span>",TRUE~"<span class='badge-baixo'>Especializado</span>"))
    DT::datatable(df,escape=FALSE,rownames=FALSE,options=list(pageLength=10,dom="t",scrollX=TRUE,columnDefs=list(list(className="dt-center",targets=c(1,2,3,4,5,6,7)))))
  })

  # ── Dados Brutos ────────────────────────────────────────────────────────
  output$tabela_eventlog <- DT::renderDataTable({
    if (!tem_dados()) return(DT::datatable(data.frame(Aviso="Faça upload de dados.")))
    df <- dados()$el %>%
      transmute(Processo=case_id,`N evento`=event_id,`Data e hora`=format(timestamp,"%d/%m/%Y %H:%M"),
        Setor=resource,Atividade=activity,Status=status) %>%
      arrange(Processo,`N evento`)
    DT::datatable(df,rownames=FALSE,filter="top",options=list(pageLength=20,scrollX=TRUE,dom="lfrtip"))
  })

  # ── Exportar ────────────────────────────────────────────────────────────
  json_rv   <- reactive({ req(tem_dados()); exportar_json(dados()) })
  prompt_rv <- reactive({ req(tem_dados()); gerar_prompt_llm(dados()) })

  show_json   <- reactiveVal(FALSE)
  show_prompt <- reactiveVal(FALSE)
  observeEvent(input$btn_preview_json,  { show_json(!show_json())     })
  observeEvent(input$btn_copiar_prompt, { show_prompt(!show_prompt()) })

  output$show_json_preview   <- reactive({ show_json()   })
  output$show_prompt_preview <- reactive({ show_prompt() })
  outputOptions(output,"show_json_preview",   suspendWhenHidden=FALSE)
  outputOptions(output,"show_prompt_preview", suspendWhenHidden=FALSE)

  output$preview_json   <- renderText({
    if (!tem_dados()) return("Faça upload de dados para gerar o JSON.")
    j <- json_rv()
    if (nchar(j)>3000) paste0(substr(j,1,3000),"\n\n... [baixe o arquivo para ver completo] ...") else j
  })
  output$preview_prompt <- renderText({
    if (!tem_dados()) return("Faça upload de dados para gerar o prompt.")
    prompt_rv()
  })

  output$btn_download_json <- downloadHandler(
    filename=function() paste0("sei_analise_",format(Sys.Date(),"%Y%m%d"),".json"),
    content=function(file){ req(tem_dados()); writeLines(json_rv(),file,useBytes=FALSE) },
    contentType="application/json")

  output$btn_download_prompt <- downloadHandler(
    filename=function() paste0("sei_prompt_llm_",format(Sys.Date(),"%Y%m%d"),".txt"),
    content=function(file){ req(tem_dados()); writeLines(prompt_rv(),file,useBytes=FALSE) },
    contentType="text/plain")

  output$btn_download_pdf <- downloadHandler(
    filename=function() paste0("sei_relatorio_",format(Sys.Date(),"%Y%m%d"),".pdf"),
    content=function(file){
      req(tem_dados())
      withProgress(message="Gerando PDF...",value=.5,{
        tryCatch(gerar_pdf_relatorio(dados(),file),
          error=function(e){
            msg <- e$message
            is_not_found <- grepl("nao encontrado|not found|wkhtmltopdf", msg, ignore.case=TRUE)
            notif_msg <- if (is_not_found) {
              paste0(
                "wkhtmltopdf não encontrado.\n",
                "Windows: https://wkhtmltopdf.org/downloads.html\n",
                "Linux: sudo apt-get install wkhtmltopdf"
              )
            } else paste0("Erro PDF: ", msg)
            showNotification(notif_msg, type="error", duration=15)
          })
        setProgress(1)
      })
    },
    contentType="application/pdf")

} # fim server
