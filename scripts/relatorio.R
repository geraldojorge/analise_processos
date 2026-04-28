# ==============================================================================
# relatorio.R — Geração de relatório PDF via HTML + wkhtmltopdf
# ==============================================================================
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(stringr)
  library(scales)
  library(igraph)
  library(lubridate)
})

# ── Paleta do relatório (modo claro para impressão) ──────────────────────────
RPT_BG      <- "#ffffff"
RPT_TEXT    <- "#1a1a2e"
RPT_MUTED   <- "#6b7280"
RPT_BORDER  <- "#e5e7eb"
RPT_ALTO    <- "#ef4444"
RPT_MEDIO   <- "#f59e0b"
RPT_BAIXO   <- "#22c55e"
RPT_ACCENT  <- "#6366f1"
CORES <- c("SES - GCJ_ARP"="#6366f1","SES - GCJ"="#06b6d4","SES - GFAJ_SA"="#10b981",
           "SES - NP_DGPO"="#f59e0b","SES - CSANS"="#ec4899","SES - GFAJ"="#8b5cf6",
           "SES - GORC"="#14b8a6","SES - NAJ"="#f97316")
cor_u <- function(u) { c <- CORES[u]; if (is.na(c)) "#888" else c }

tema_relatorio <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.background  = element_rect(fill = RPT_BG,     color = NA),
      panel.background = element_rect(fill = "#f8fafc",  color = NA),
      panel.grid.major = element_line(color = RPT_BORDER, linewidth = 0.4),
      panel.grid.minor = element_blank(),
      axis.text        = element_text(color = RPT_MUTED, size = 9),
      axis.title       = element_text(color = RPT_TEXT,  size = 10),
      plot.title       = element_text(color = RPT_TEXT,  size = 11, face = "bold"),
      legend.background= element_rect(fill = RPT_BG, color = NA),
      legend.text      = element_text(color = RPT_TEXT, size = 9),
      legend.title     = element_text(color = RPT_TEXT, size = 9, face = "bold")
    )
}

svg_b64 <- function(p, w = 6, h = 3) {
  tf <- tempfile(fileext = ".svg")
  ggsave(tf, p, width = w, height = h, device = "svg", bg = "white")
  raw_svg <- paste(readLines(tf, warn = FALSE), collapse = "\n")
  unlink(tf)
  raw_svg  # return inline SVG
}

# ── Gráfico 1: barras de risco ────────────────────────────────────────────────
graf_risco <- function(d) {
  df <- d$risco %>%
    mutate(risco = as.character(risco)) %>%
    count(risco) %>%
    mutate(risco = factor(risco, levels = c("ALTO","MÉDIO","BAIXO")),
           cor = c("ALTO"=RPT_ALTO,"MÉDIO"=RPT_MEDIO,"BAIXO"=RPT_BAIXO)[as.character(risco)])
  p <- ggplot(df, aes(risco, n, fill = risco)) +
    geom_col(width = .55, show.legend = FALSE) +
    geom_text(aes(label = n), vjust = -0.4, size = 4, color = RPT_TEXT, fontface = "bold") +
    scale_fill_manual(values = c("ALTO"=RPT_ALTO,"MÉDIO"=RPT_MEDIO,"BAIXO"=RPT_BAIXO)) +
    scale_y_continuous(breaks = scales::breaks_width(1), expand = expansion(mult=c(0,.25))) +
    labs(title = "Distribuição por Nível de Risco", x = NULL, y = "N° de processos") +
    tema_relatorio()
  svg_b64(p, 4, 3)
}

# ── Gráfico 2: scatter duração × eventos ─────────────────────────────────────
graf_scatter <- function(d) {
  df <- d$risco %>%
    mutate(risco = as.character(risco),
           cor = c("ALTO"=RPT_ALTO,"MÉDIO"=RPT_MEDIO,"BAIXO"=RPT_BAIXO)[risco],
           label = str_extract(case_id, "[^./]+/[^./]+-[^./]+$"))
  p <- ggplot(df, aes(throughput_days, n_eventos, color = risco, label = label)) +
    geom_point(size = 3.5, alpha = .85) +
    ggrepel_text_fallback(df) +
    scale_color_manual(values = c("ALTO"=RPT_ALTO,"MÉDIO"=RPT_MEDIO,"BAIXO"=RPT_BAIXO),
                       name = "Risco") +
    labs(title = "Duração × Volume de Movimentações", x = "Dias", y = "N° de eventos") +
    tema_relatorio()
  svg_b64(p, 6, 3.5)
}

ggrepel_text_fallback <- function(df) {
  # Use geom_text if ggrepel not available
  if (requireNamespace("ggrepel", quietly=TRUE)) {
    ggrepel::geom_text_repel(size=2.8, color=RPT_MUTED, max.overlaps=20)
  } else {
    geom_text(nudge_y=2, size=2.5, color=RPT_MUTED)
  }
}

# ── Gráfico 3: centralidade horizontal ───────────────────────────────────────
graf_centralidade <- function(d) {
  df <- d$metricas_u %>%
    mutate(label = str_replace(Unidade,"SES - ",""),
           cor   = sapply(Unidade, cor_u)) %>%
    arrange(centralidade)
  p <- ggplot(df, aes(centralidade, reorder(label,centralidade), fill=label)) +
    geom_col(show.legend=FALSE) +
    geom_text(aes(label=centralidade), hjust=-0.3, size=3.2, color=RPT_TEXT) +
    scale_fill_manual(values=setNames(df$cor, df$label)) +
    scale_x_continuous(expand=expansion(mult=c(0,.2))) +
    labs(title="Centralidade por Setor", x="Centralidade (envios+recebimentos)", y=NULL) +
    tema_relatorio()
  svg_b64(p, 6, 3.5)
}

# ── Gráfico 4: activity presence ─────────────────────────────────────────────
graf_activity <- function(d) {
  df <- d$act_presence %>%
    arrange(pct_presenca) %>%
    mutate(cor = case_when(pct_presenca==100~RPT_BAIXO, pct_presenca>=50~RPT_ACCENT,
                           pct_presenca>=20~RPT_MEDIO, TRUE~RPT_ALTO))
  p <- ggplot(df, aes(pct_presenca, reorder(activity,pct_presenca), fill=cor)) +
    geom_col(show.legend=FALSE) +
    geom_text(aes(label=paste0(pct_presenca,"%")), hjust=-0.2, size=3, color=RPT_TEXT) +
    scale_fill_identity() +
    scale_x_continuous(limits=c(0,120), labels=function(x) paste0(x,"%")) +
    labs(title="Presença de Atividades (%)", x="% dos processos", y=NULL) +
    tema_relatorio()
  svg_b64(p, 7, 4)
}

# ── Gráfico 5: Gantt de um processo ─────────────────────────────────────────
graf_gantt_processo <- function(d, case_id_sel) {
  segs <- d$segmentos %>%
    filter(case_id == case_id_sel) %>%
    arrange(seg_inicio) %>%
    mutate(
      cor        = sapply(resource, cor_u),
      unit_label = str_replace(resource, "SES - ", ""),
      dur_dias   = as.numeric(difftime(seg_fim, seg_inicio, units="days")),
      dur_dias   = pmax(dur_dias, 0.05),
      inicio_num = as.numeric(difftime(seg_inicio, min(seg_inicio), units="days")),
      fim_num    = inicio_num + dur_dias
    )
  if (nrow(segs) == 0) return(NULL)
  p <- ggplot(segs) +
    geom_segment(aes(x=seg_inicio, xend=seg_fim, y=unit_label, yend=unit_label,
                     color=resource), linewidth=8, lineend="round", alpha=.85) +
    scale_color_manual(values=setNames(segs$cor, segs$resource), guide="none") +
    scale_x_datetime(date_labels="%d/%m", date_breaks="2 weeks") +
    labs(title=paste0("Linha do Tempo: ", str_extract(case_id_sel,"[^./]+/[^./]+-[^./]+$")),
         x=NULL, y=NULL) +
    tema_relatorio() +
    theme(axis.text.x=element_text(angle=30,hjust=1))
  svg_b64(p, 8, max(2.5, n_distinct(segs$unit_label)*0.7))
}

# ── Gráfico 6: handover heatmap ──────────────────────────────────────────────
graf_handover_heat <- function(d) {
  df <- d$handover_df %>%
    mutate(from = str_replace(from_unit,"SES - ",""),
           to   = str_replace(to_unit,  "SES - ",""))
  p <- ggplot(df, aes(to, from, fill=avg_wait_hours)) +
    geom_tile(color="white", linewidth=.5) +
    geom_text(aes(label=paste0(n_handovers,"×\n",round(avg_wait_hours/24,1),"d")),
              size=2.5, color="white") +
    scale_fill_gradient2(low=RPT_BAIXO, mid=RPT_MEDIO, high=RPT_ALTO,
                         midpoint=24, name="Espera\n(horas)") +
    labs(title="Mapa de Handovers (nº passagens × espera média)", x="Para", y="De") +
    tema_relatorio() +
    theme(axis.text.x=element_text(angle=35,hjust=1))
  svg_b64(p, 7, 4)
}

# ── Tabela HTML helper ────────────────────────────────────────────────────────
html_table <- function(df, col_aligns = NULL) {
  ths <- paste0('<th>', names(df), '</th>', collapse='')
  rows <- apply(df, 1, function(r) {
    tds <- paste0('<td>', r, '</td>', collapse='')
    paste0('<tr>', tds, '</tr>')
  })
  paste0('<table class="rpt-table"><thead><tr>', ths,
         '</tr></thead><tbody>', paste(rows, collapse=''), '</tbody></table>')
}

badge_rpt <- function(r) {
  col <- switch(as.character(r), "ALTO"=RPT_ALTO, "MÉDIO"=RPT_MEDIO, "BAIXO"=RPT_BAIXO, "#888")
  sprintf('<span style="background:%s22;color:%s;padding:2px 8px;border-radius:12px;font-weight:700;font-size:11px;">%s</span>', col, col, r)
}

# ── GERAR HTML DO RELATÓRIO ───────────────────────────────────────────────────
gerar_html_relatorio <- function(d) {

  risco_df <- d$risco %>% mutate(risco = as.character(risco))
  n_alto   <- sum(risco_df$risco == "ALTO")
  n_medio  <- sum(risco_df$risco == "MÉDIO")
  n_baixo  <- sum(risco_df$risco == "BAIXO")
  periodo  <- paste0(format(min(d$el$timestamp),"%d/%m/%Y")," a ",format(max(d$el$timestamp),"%d/%m/%Y"))
  gerado   <- format(Sys.time(), "%d/%m/%Y %H:%M")

  svg_risco    <- graf_risco(d)
  svg_scatter  <- graf_scatter(d)
  svg_central  <- graf_centralidade(d)
  svg_activity <- graf_activity(d)
  svg_handover <- graf_handover_heat(d)

  # Gantts dos processos ALTO
  seis_alto <- risco_df %>% filter(risco=="ALTO") %>% arrange(desc(score_normalizado)) %>% pull(case_id)
  gantts_html <- ""
  for (sei in seis_alto) {
    svg_g <- graf_gantt_processo(d, sei)
    if (!is.null(svg_g)) {
      gantts_html <- paste0(gantts_html,
        '<div class="sub-section">', svg_g, '</div>')
    }
  }

  # Tabela de processos
  tbl_proc <- risco_df %>%
    arrange(risco, desc(score_normalizado)) %>%
    transmute(
      `Processo`    = str_extract(case_id,"[^./]+/[^./]+-[^./]+$"),
      `Risco`       = sapply(risco, badge_rpt),
      `Duração`     = ifelse(throughput_days<1,"< 1 dia",paste0(round(throughput_days)," dias")),
      `Eventos`     = as.character(n_eventos),
      `Setores`     = as.character(n_unidades),
      `Trocas`      = as.character(handovers),
      `Retrabalho`  = as.character(retrabalho),
      `Score`       = paste0(score_normalizado,"/100")
    )

  ths_p <- paste0('<th>',names(tbl_proc),'</th>',collapse='')
  rows_p <- apply(tbl_proc, 1, function(r) {
    paste0('<tr>',paste0('<td>',r,'</td>',collapse=''),'</tr>')
  })
  tabela_processos_html <- paste0(
    '<table class="rpt-table"><thead><tr>',ths_p,'</tr></thead><tbody>',
    paste(rows_p,collapse=''),'</tbody></table>'
  )

  # Tabela handovers
  tbl_ho <- d$handover_df %>%
    arrange(desc(avg_wait_hours)) %>%
    head(10) %>%
    transmute(
      De        = str_replace(from_unit,"SES - ",""),
      Para      = str_replace(to_unit,  "SES - ",""),
      Passagens = as.character(n_handovers),
      `Espera média` = ifelse(avg_wait_hours<24,paste0(round(avg_wait_hours),"h"),
                              paste0(round(avg_wait_hours/24,1)," dias")),
      Avaliacao = ifelse(avg_wait_hours>48,
        '<span style="color:#ef4444;font-weight:700;">Lento</span>',
        ifelse(avg_wait_hours>8,
          '<span style="color:#f59e0b;font-weight:700;">Moderado</span>',
          '<span style="color:#22c55e;font-weight:700;">Rápido</span>'))
    )
  ths_h <- paste0('<th>',names(tbl_ho),'</th>',collapse='')
  rows_h <- apply(tbl_ho,1,function(r) paste0('<tr>',paste0('<td>',r,'</td>',collapse=''),'</tr>'))
  tabela_handovers_html <- paste0('<table class="rpt-table"><thead><tr>',ths_h,'</tr></thead><tbody>',
                                  paste(rows_h,collapse=''),'</tbody></table>')

  # Tabela activity presence
  tbl_ap <- d$act_presence %>%
    transmute(
      Atividade   = activity,
      `Processos` = as.character(n_casos_presentes),
      `Total eventos` = as.character(total_ocorrencias),
      `Presença (%)`  = paste0(pct_presenca,"%")
    )
  tabela_ap_html <- html_table(tbl_ap)

  # ── CSS ──────────────────────────────────────────────────────────────────
  css <- "
  @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&family=JetBrains+Mono:wght@400;500&display=swap');
  * { box-sizing:border-box; margin:0; padding:0; }
  body { font-family:'Inter',sans-serif; color:#1a1a2e; background:#fff; font-size:12px; line-height:1.6; }
  .page { max-width:900px; margin:0 auto; padding:32px 40px; }
  /* Cover */
  .cover { background:linear-gradient(135deg,#0f1117 0%,#1e2334 100%); color:#fff; padding:48px 40px; border-radius:12px; margin-bottom:36px; page-break-after:always; }
  .cover h1 { font-size:26px; font-weight:700; margin-bottom:8px; }
  .cover .subtitle { color:#94a3b8; font-size:14px; margin-bottom:24px; }
  .cover-meta { display:flex; gap:32px; margin-top:28px; }
  .cover-stat { background:rgba(255,255,255,.08); border-radius:8px; padding:14px 20px; }
  .cover-stat .val { font-size:28px; font-weight:700; font-family:'JetBrains Mono'; }
  .cover-stat.alto  .val { color:#ef4444; }
  .cover-stat.medio .val { color:#f59e0b; }
  .cover-stat.baixo .val { color:#22c55e; }
  .cover-stat .lbl  { font-size:11px; color:#94a3b8; margin-top:2px; }
  /* Sections */
  h2 { font-size:15px; font-weight:700; color:#0f1117; border-bottom:2px solid #6366f1; padding-bottom:6px; margin:32px 0 14px; page-break-before:always; }
  h2:first-of-type { page-break-before:avoid; }
  h3 { font-size:13px; font-weight:600; color:#374151; margin:20px 0 8px; }
  .sub-section { margin-bottom:20px; }
  /* Tables */
  .rpt-table { width:100%; border-collapse:collapse; font-size:11px; margin-bottom:16px; }
  .rpt-table th { background:#f1f5f9; color:#475569; font-weight:600; text-transform:uppercase; font-size:10px; letter-spacing:.04em; padding:7px 10px; border-bottom:1px solid #e2e8f0; text-align:left; }
  .rpt-table td { padding:7px 10px; border-bottom:1px solid #f1f5f9; color:#1e293b; vertical-align:middle; }
  .rpt-table tr:hover td { background:#f8fafc; }
  /* KPI row */
  .kpi-row { display:flex; gap:12px; margin-bottom:20px; }
  .kpi { flex:1; background:#f8fafc; border:1px solid #e2e8f0; border-radius:8px; padding:12px 16px; }
  .kpi .k-val { font-size:22px; font-weight:700; font-family:'JetBrains Mono'; }
  .kpi .k-lbl { font-size:10px; color:#6b7280; text-transform:uppercase; letter-spacing:.06em; }
  .kpi.alto  .k-val { color:#ef4444; }
  .kpi.medio .k-val { color:#f59e0b; }
  .kpi.baixo .k-val { color:#22c55e; }
  .kpi.info  .k-val { color:#6366f1; }
  /* Info box */
  .info-box { background:#f0f9ff; border-left:4px solid #0ea5e9; border-radius:0 8px 8px 0; padding:10px 14px; margin:12px 0; font-size:11px; color:#0369a1; }
  .warn-box { background:#fffbeb; border-left:4px solid #f59e0b; border-radius:0 8px 8px 0; padding:10px 14px; margin:12px 0; font-size:11px; color:#92400e; }
  /* SVG */
  svg { max-width:100%; height:auto; }
  /* Grid 2col */
  .grid2 { display:grid; grid-template-columns:1fr 1fr; gap:16px; }
  /* Footer */
  .footer { margin-top:40px; padding-top:16px; border-top:1px solid #e2e8f0; font-size:10px; color:#9ca3af; display:flex; justify-content:space-between; }
  /* Print */
  @media print {
    body { font-size:11px; }
    h2 { page-break-before:always; }
    .grid2 { break-inside:avoid; }
  }
  "

  # ── HTML ─────────────────────────────────────────────────────────────────
  html <- paste0('<!DOCTYPE html><html lang="pt-BR"><head>
<meta charset="UTF-8">
<style>', css, '</style>
</head><body><div class="page">

<!-- CAPA -->
<div class="cover">
  <div style="font-size:11px;color:#64748b;margin-bottom:12px;text-transform:uppercase;letter-spacing:.1em;">Relatório de Mineração de Processos</div>
  <h1>Aquisição de Medicamentos — SEI</h1>
  <div class="subtitle">Secretaria Estadual de Saúde de Pernambuco · ', periodo, '</div>
  <div class="cover-meta">
    <div class="cover-stat alto"><div class="val">', n_alto, '</div><div class="lbl">Risco Alto</div></div>
    <div class="cover-stat medio"><div class="val">', n_medio, '</div><div class="lbl">Risco Médio</div></div>
    <div class="cover-stat baixo"><div class="val">', n_baixo, '</div><div class="lbl">Risco Baixo</div></div>
    <div class="cover-stat" style="border:1px solid rgba(255,255,255,.15);">
      <div class="val" style="color:#94a3b8;">', nrow(risco_df), '</div>
      <div class="lbl">Total de processos</div>
    </div>
  </div>
  <div style="margin-top:20px;font-size:11px;color:#64748b;">Gerado em: ', gerado, ' · Método: Mineração de Processos (bupaR-equivalente) + Análise de Redes (igraph)</div>
</div>

<!-- 1. SUMÁRIO EXECUTIVO -->
<h2>1. Sumário Executivo</h2>
<div class="kpi-row">
  <div class="kpi alto"><div class="k-val">', n_alto, '</div><div class="k-lbl">Processos risco alto</div></div>
  <div class="kpi medio"><div class="k-val">', n_medio, '</div><div class="k-lbl">Risco médio</div></div>
  <div class="kpi baixo"><div class="k-val">', n_baixo, '</div><div class="k-lbl">Risco baixo</div></div>
  <div class="kpi info"><div class="k-val">', sum(risco_df$status_final=="ABERTO"), '</div><div class="k-lbl">Em andamento</div></div>
  <div class="kpi info"><div class="k-val">', nrow(d$el), '</div><div class="k-lbl">Total de eventos</div></div>
</div>

<div class="grid2">
  <div>', svg_risco, '</div>
  <div>', svg_scatter, '</div>
</div>

<!-- 2. PROCESSOS -->
<h2>2. Classificação de Risco por Processo</h2>
<div class="info-box">O escore de risco combina 6 indicadores: consumo de prazo (30%), retrabalho (25%), dispersão organizacional (20%), passagem por gargalo (10%), processo aberto com alta duração (10%) e déficit de progressão (5%).</div>
', tabela_processos_html, '

<!-- 3. GARGALOS -->
<h2>3. Gargalos e Fluxo entre Setores</h2>
', svg_handover, '
<h3>Top-10 transições por tempo de espera</h3>
', tabela_handovers_html, '

<!-- 4. REDE ORGANIZACIONAL -->
<h2>4. Análise da Rede Organizacional</h2>
', svg_central, '

<!-- 5. ATIVIDADES -->
<h2>5. Presença de Atividades</h2>
', svg_activity, '
', tabela_ap_html, '

<!-- 6. LINHAS DO TEMPO -->
<h2>6. Linhas do Tempo — Processos de Risco Alto</h2>
', if (nchar(gantts_html) > 0) gantts_html else '<p style="color:#6b7280;">Nenhum processo de risco alto encontrado.</p>', '

<!-- 7. LIMITAÇÕES -->
<h2>7. Limitações da Análise</h2>
<div class="warn-box"><strong>Volume de dados:</strong> Esta análise é baseada em ', nrow(risco_df), ' processos. O escore de risco é heurístico (não estatístico). Com 100+ processos concluídos, recomenda-se substituir por regressão logística calibrada.</div>
<div class="warn-box"><strong>Prazo normativo:</strong> O prazo de referência de 90 dias é estimado. Ajustar conforme portaria interna do órgão para aumentar a precisão do indicador de consumo de prazo.</div>
<div class="warn-box"><strong>Lifecycle:</strong> As atividades foram inferidas a partir da descrição dos eventos do SEI, sem registro explícito de start/complete por atividade. O tempo medido é de espera entre eventos, não de execução.</div>

<div class="footer">
  <span>Monitor SEI — Aquisição de Medicamentos · SES-PE</span>
  <span>Gerado em ', gerado, '</span>
</div>

</div></body></html>')

  html
}

# ── FUNÇÃO PRINCIPAL: gerar PDF ───────────────────────────────────────────────
gerar_pdf_relatorio <- function(d, caminho_pdf = NULL, wkhtmltopdf_bin = NULL) {
  if (is.null(caminho_pdf))
    caminho_pdf <- tempfile(pattern = "sei_relatorio_", fileext = ".pdf")

  html_file <- tempfile(fileext = ".html")
  html_str  <- gerar_html_relatorio(d)
  writeLines(html_str, html_file, useBytes = FALSE)

  # Normalizar caminho (Windows backslashes -> forward slashes)
  if (!is.null(wkhtmltopdf_bin) && nzchar(wkhtmltopdf_bin))
    wkhtmltopdf_bin <- normalizePath(wkhtmltopdf_bin, winslash="/", mustWork=FALSE)

  if (is.null(wkhtmltopdf_bin) || !nzchar(wkhtmltopdf_bin) || !file.exists(wkhtmltopdf_bin)) {
    # Try auto-detect
    bin <- Sys.which("wkhtmltopdf")
    if (!nzchar(bin)) {
      pf <- Sys.getenv("ProgramFiles", unset="C:/Program Files")
      pf86 <- Sys.getenv("ProgramFiles(x86)", unset="C:/Program Files (x86)")
      for (p in unique(c(
        "/usr/bin/wkhtmltopdf", "/usr/local/bin/wkhtmltopdf",
        "/opt/homebrew/bin/wkhtmltopdf",
        "C:/Program Files/wkhtmltopdf/bin/wkhtmltopdf.exe",
        "C:/Program Files (x86)/wkhtmltopdf/bin/wkhtmltopdf.exe",
        "C:/Program Files/wkhtmltopdf/wkhtmltopdf.exe",
        "C:/wkhtmltopdf/bin/wkhtmltopdf.exe",
        file.path(pf,   "wkhtmltopdf/bin/wkhtmltopdf.exe"),
        file.path(pf86, "wkhtmltopdf/bin/wkhtmltopdf.exe")
      ))) { p2 <- normalizePath(p, winslash="/", mustWork=FALSE); if (file.exists(p2)) { bin <- p2; break } }
    } else {
      bin <- normalizePath(bin, winslash="/", mustWork=FALSE)
    }
    wkhtmltopdf_bin <- bin
  }
  if (!nzchar(wkhtmltopdf_bin) || !file.exists(wkhtmltopdf_bin)) {
    stop(paste0(
      "wkhtmltopdf nao encontrado.\n",
      "Windows: instale em https://wkhtmltopdf.org/downloads.html\n",
      "Apos instalar, reinicie o RStudio ou informe o caminho no painel Exportar."
    ))
  }
  ret <- system2(
    wkhtmltopdf_bin,
    args = c(
      "--page-size", "A4",
      "--margin-top",    "15mm",
      "--margin-bottom", "15mm",
      "--margin-left",   "12mm",
      "--margin-right",  "12mm",
      "--encoding",      "UTF-8",
      "--enable-local-file-access",
      "--quiet",
      html_file,
      caminho_pdf
    ),
    stdout = TRUE, stderr = TRUE
  )
  unlink(html_file)

  if (!file.exists(caminho_pdf))
    stop("Falha ao gerar PDF. wkhtmltopdf retornou: ", paste(ret, collapse=" "))

  message("PDF gerado: ", caminho_pdf, " (", round(file.size(caminho_pdf)/1024), " KB)")
  invisible(caminho_pdf)
}
