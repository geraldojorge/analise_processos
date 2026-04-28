# ==============================================================================
# bupar_engine.R
# Implementação dos conceitos do bupaR de forma nativa em R base + tidyverse.
# Reproduz: eventlog, traces, throughput_time, handover_matrix, 
#           activity_presence, process_map (via igraph), risco composto.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(readr)
  library(tidyr)
  library(igraph)
})

# ── 1. CATEGORIZAÇÃO DE ATIVIDADES (normalização terminológica) ─────────────
categorizar_atividade <- function(desc) {
  d <- tolower(iconv(desc, to = "ASCII//TRANSLIT"))
  case_when(
    str_detect(d, "atribu")                          ~ "Atribuicao",
    str_detect(d, "assinado|assinatura")              ~ "Assinatura de Documento",
    str_detect(d, "recebido na unidade")              ~ "Recebimento na Unidade",
    str_detect(d, "remetido pela unidade")            ~ "Remessa para Unidade",
    str_detect(d, "gerado documento restrito")        ~ "Geracao de Documento",
    str_detect(d, "gerado documento")                 ~ "Geracao de Documento",
    str_detect(d, "registro de documento externo")   ~ "Registro de Documento Externo",
    str_detect(d, "processo restrito gerado")         ~ "Processo Gerado",
    str_detect(d, "reabertura")                       ~ "Reabertura",
    str_detect(d, "conclusao|conclusão")              ~ "Conclusao",
    str_detect(d, "exclusao|exclusão")                ~ "Exclusao de Documento",
    str_detect(d, "bloco.*disponibilizado")           ~ "Bloco Disponibilizado",
    str_detect(d, "incluido em bloco|incluído")       ~ "Inclusao em Bloco",
    str_detect(d, "retirado do bloco")                ~ "Retirada de Bloco",
    str_detect(d, "alterada ordem")                   ~ "Reordenacao de Protocolo",
    TRUE                                              ~ "Outro"
  )
}

# ── 2. CARREGAR E CONSTRUIR O EVENT LOG (equivalente bupaR::eventlog) ────────
carregar_eventlog <- function(caminho) {
  
  raw <- read_csv(caminho, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
  
  # Renomear para nomenclatura XES / bupaR
  # Usar índice posicional para evitar problemas de encoding nos nomes
  names(raw) <- c("case_id","timestamp","resource","usuario","descricao","status")
  
  el <- raw %>%
    mutate(
      # Parse timestamp (formato dd/m/yyyy HH:MM)
      timestamp = dmy_hm(timestamp),
      # Normalizar activity
      activity  = categorizar_atividade(descricao),
      resource  = str_trim(resource),
      status    = toupper(status)
    ) %>%
    filter(!is.na(timestamp)) %>%
    # Remover duplicatas exatas
    distinct(case_id, timestamp, resource, activity, .keep_all = TRUE) %>%
    # Resolver timestamps idênticos dentro do mesmo caso (bupaR faz o mesmo)
    arrange(case_id, timestamp) %>%
    group_by(case_id) %>%
    mutate(
      event_id  = row_number(),
      # Desempate: adiciona microssegundos sequenciais
      timestamp = timestamp + microseconds(event_id)
    ) %>%
    ungroup()
  
  return(el)
}

# ── 3. MÉTRICAS DE CASO (equivalente bupaR::case_metrics) ───────────────────
calcular_metricas_caso <- function(el) {
  
  el %>%
    group_by(case_id) %>%
    summarise(
      # throughput_time (bupaR)
      inicio          = min(timestamp),
      fim             = max(timestamp),
      throughput_days = as.numeric(difftime(max(timestamp), min(timestamp), units = "days")),
      
      # n_activities (bupaR)
      n_eventos       = n(),
      
      # n_resources envolvidos
      n_unidades      = n_distinct(resource),
      unidades        = paste(unique(resource), collapse = " | "),
      
      # status final
      status_final    = last(status),
      
      # activity_presence equivalente: tipos distintos de atividade
      n_tipos_ativ    = n_distinct(activity),
      
      # Retrabalho: exclusões + reaberturas (bupaR number_of_repetitions proxy)
      retrabalho      = sum(activity %in% c("Exclusao de Documento", "Reabertura")),
      
      # Handovers: trocas de unidade (para tabela geral)
      handovers       = sum(resource != lag(resource, default = first(resource))),
      
      .groups = "drop"
    )
}

# ── 4. TRACES (equivalente bupaR::traces) ────────────────────────────────────
calcular_traces <- function(el) {
  
  el %>%
    group_by(case_id) %>%
    summarise(
      trace_ativ    = paste(activity, collapse = " → "),
      trace_units   = paste(resource,  collapse = " → "),
      n_eventos     = n(),
      .groups = "drop"
    ) %>%
    group_by(trace_units) %>%
    mutate(
      freq_variante = n(),
      pct_variante  = round(freq_variante / nrow(.) * 100, 1)
    ) %>%
    ungroup()
}

# ── 5. HANDOVER MATRIX (equivalente bupaR::handover_matrix / resource_matrix)
calcular_handover_matrix <- function(el) {
  
  el %>%
    arrange(case_id, timestamp) %>%
    group_by(case_id) %>%
    mutate(
      from_unit = resource,
      to_unit   = lead(resource),
      from_ts   = timestamp,
      to_ts     = lead(timestamp)
    ) %>%
    filter(!is.na(to_unit), from_unit != to_unit) %>%
    mutate(
      wait_hours = as.numeric(difftime(to_ts, from_ts, units = "hours"))
    ) %>%
    ungroup() %>%
    group_by(from_unit, to_unit) %>%
    summarise(
      n_handovers    = n(),
      avg_wait_hours = round(mean(wait_hours, na.rm = TRUE), 1),
      max_wait_hours = round(max(wait_hours,  na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    arrange(desc(n_handovers))
}

# ── 6. TRANSITIONS (todas, incluindo mesma unidade — para tempo de permanência)
calcular_transicoes <- function(el) {
  
  el %>%
    arrange(case_id, timestamp) %>%
    group_by(case_id) %>%
    mutate(
      from_unit    = resource,
      to_unit      = lead(resource),
      from_act     = activity,
      to_act       = lead(activity),
      delta_hours  = as.numeric(difftime(lead(timestamp), timestamp, units = "hours"))
    ) %>%
    filter(!is.na(to_unit), delta_hours > 0) %>%
    ungroup()
}

# ── 7. NETWORK GRAPH (equivalente bupaR process map via igraph) ──────────────
construir_grafo_processo <- function(handover_df, metricas_unidade) {
  
  edges <- handover_df %>%
    rename(from = from_unit, to = to_unit, weight = n_handovers) %>%
    mutate(avg_wait = avg_wait_hours)
  
  nodes <- metricas_unidade %>%
    rename(name = Unidade)
  
  g <- graph_from_data_frame(
    d        = edges[, c("from","to","weight","avg_wait")],
    vertices = nodes,
    directed = TRUE
  )
  
  # Adicionar betweenness centrality (bupaR mineração organizacional)
  V(g)$betweenness <- betweenness(g, directed = TRUE, normalized = TRUE)
  V(g)$degree_in   <- degree(g, mode = "in")
  V(g)$degree_out  <- degree(g, mode = "out")
  V(g)$degree_tot  <- degree(g, mode = "total")
  
  return(g)
}

# ── 8. MÉTRICAS POR UNIDADE (resource metrics bupaR) ────────────────────────
calcular_metricas_unidade <- function(el, handover_df) {
  
  eventos_por_unidade <- el %>%
    group_by(Unidade = resource) %>%
    summarise(
      total_eventos = n(),
      processos     = n_distinct(case_id),
      .groups = "drop"
    )
  
  enviou   <- handover_df %>% group_by(Unidade = from_unit) %>% summarise(enviou = sum(n_handovers), .groups="drop")
  recebeu  <- handover_df %>% group_by(Unidade = to_unit)   %>% summarise(recebeu = sum(n_handovers), .groups="drop")
  
  wait_como_destino <- handover_df %>%
    group_by(Unidade = to_unit) %>%
    summarise(wait_medio_receber = round(mean(avg_wait_hours), 1), .groups="drop")
  
  eventos_por_unidade %>%
    left_join(enviou,             by = "Unidade") %>%
    left_join(recebeu,            by = "Unidade") %>%
    left_join(wait_como_destino,  by = "Unidade") %>%
    mutate(
      enviou               = replace_na(enviou, 0),
      recebeu              = replace_na(recebeu, 0),
      centralidade         = enviou + recebeu,
      wait_medio_receber   = replace_na(wait_medio_receber, 0)
    ) %>%
    arrange(desc(centralidade))
}

# ── 9. ESCORE DE RISCO COMPOSTO ──────────────────────────────────────────────
# Proxy dos 6 indicadores propostos no TCC (sem regressão — dados insuficientes)
# Operacionaliza: consumo de prazo, déficit de progressão, retrabalho,
#                 tempo em transições críticas, variante e dispersão organizacional.

calcular_risco <- function(metricas_caso, handover_df, el) {
  
  # Prazo de referência por processo (assumido 90 dias para aquisição via pregão)
  prazo_ref <- 90
  
  # Tempo máximo de espera em transições críticas por caso
  wait_critico <- handover_df %>%
    filter(avg_wait_hours > 48) %>%
    group_by(from_unit) %>%
    summarise(
      tempo_critico_medio = mean(avg_wait_hours),
      .groups = "drop"
    )
  
  # Unidades que mais travam (gargalos detectados)
  gargalos <- handover_df %>%
    filter(avg_wait_hours > 24) %>%
    pull(from_unit) %>%
    unique()
  
  # Verificar se cada processo passou por gargalo
  passou_gargalo <- el %>%
    group_by(case_id) %>%
    summarise(
      passou_gargalo = any(resource %in% gargalos),
      .groups = "drop"
    )
  
  metricas_caso %>%
    left_join(passou_gargalo, by = "case_id") %>%
    mutate(
      passou_gargalo = replace_na(passou_gargalo, FALSE),
      
      # Indicador 1: Índice de consumo de prazo
      consumo_prazo = pmin(throughput_days / prazo_ref, 2),
      
      # Indicador 2: Déficit de progressão (eventos vs mediana histórica)
      mediana_eventos = median(n_eventos),
      deficit_prog    = pmax(0, (mediana_eventos - n_eventos) / mediana_eventos),
      
      # Indicador 3: Índice de retrabalho
      idx_retrabalho  = pmin(retrabalho / pmax(n_eventos, 1) * 10, 1),
      
      # Indicador 4: Dispersão organizacional
      dispersao_media = mean(n_unidades),
      idx_dispersao   = pmin(n_unidades / dispersao_media, 2) / 2,
      
      # Indicador 5: Passou por gargalo
      idx_gargalo     = as.numeric(passou_gargalo),
      
      # Indicador 6: Processo em aberto com alta duração
      idx_aberto_longo = as.numeric(status_final == "ABERTO" & throughput_days > 30),
      
      # Escore composto (pesos baseados na importância relativa)
      score = (consumo_prazo   * 0.30) +
              (idx_retrabalho  * 0.25) +
              (idx_dispersao   * 0.20) +
              (idx_gargalo     * 0.10) +
              (idx_aberto_longo* 0.10) +
              (deficit_prog    * 0.05),
      
      score_normalizado = pmin(round(score / max(score, na.rm = TRUE) * 100), 100),
      
      # Classificação em 3 níveis
      risco = case_when(
        score_normalizado >= 65 ~ "ALTO",
        score_normalizado >= 35 ~ "MÉDIO",
        TRUE                   ~ "BAIXO"
      ),
      
      risco = factor(risco, levels = c("ALTO", "MÉDIO", "BAIXO"))
    )
}

# ── 10. SEGMENTOS DE PERMANÊNCIA POR UNIDADE (para Gantt) ───────────────────
calcular_segmentos_gantt <- function(el) {
  
  el %>%
    arrange(case_id, timestamp) %>%
    group_by(case_id) %>%
    mutate(
      mudou_unidade = resource != lag(resource, default = first(resource)) | row_number() == 1,
      segmento_id   = cumsum(mudou_unidade)
    ) %>%
    group_by(case_id, segmento_id, resource) %>%
    summarise(
      seg_inicio  = min(timestamp),
      seg_fim     = max(timestamp),
      n_eventos   = n(),
      atividades  = paste(unique(activity), collapse = "; "),
      .groups     = "drop"
    ) %>%
    mutate(
      duracao_horas = as.numeric(difftime(seg_fim, seg_inicio, units = "hours")),
      duracao_horas = pmax(duracao_horas, 0.5)   # mínimo visual
    )
}

# ── 11. ACTIVITY PRESENCE (equivalente bupaR::activity_presence) ─────────────
calcular_activity_presence <- function(el) {
  
  n_casos <- n_distinct(el$case_id)
  
  el %>%
    group_by(activity) %>%
    summarise(
      n_casos_presentes = n_distinct(case_id),
      total_ocorrencias = n(),
      .groups = "drop"
    ) %>%
    mutate(
      pct_presenca = round(n_casos_presentes / n_casos * 100, 1)
    ) %>%
    arrange(desc(pct_presenca))
}

# ── 12. FUNÇÃO PRINCIPAL: carregar tudo de uma vez ───────────────────────────
inicializar_dados <- function(caminho) {
  
  el           <- carregar_eventlog(caminho)
  metricas_c   <- calcular_metricas_caso(el)
  handover_df  <- calcular_handover_matrix(el)
  transicoes   <- calcular_transicoes(el)
  metricas_u   <- calcular_metricas_unidade(el, handover_df)
  traces       <- calcular_traces(el)
  risco_df     <- calcular_risco(metricas_c, handover_df, el)
  segmentos    <- calcular_segmentos_gantt(el)
  act_presence <- calcular_activity_presence(el)
  grafo        <- construir_grafo_processo(handover_df, metricas_u)
  
  mapa <- gerar_mapa_processo(el, handover_df, metricas_u)

  list(
    el           = el,
    metricas_c   = metricas_c,
    handover_df  = handover_df,
    transicoes   = transicoes,
    metricas_u   = metricas_u,
    traces       = traces,
    risco        = risco_df,
    segmentos    = segmentos,
    act_presence = act_presence,
    grafo        = grafo,
    mapa         = mapa
  )
}

# ── 13. EXPORTAR JSON PARA ANÁLISE EXTERNA ───────────────────────────────────
# Gera um objeto JSON estruturado com todos os resultados da mineração de
# processos, pronto para consumo por ferramentas externas de LLM.

exportar_json <- function(dados, caminho_saida = NULL) {
  library(jsonlite)

  d        <- dados
  risco_df <- d$risco %>%
    mutate(
      inicio = format(inicio, "%Y-%m-%d"),
      fim    = format(fim,    "%Y-%m-%d"),
      risco  = as.character(risco)
    ) %>%
    select(
      case_id, risco, score_normalizado, throughput_days,
      n_eventos, n_unidades, handovers, retrabalho,
      status_final, inicio, fim, passou_gargalo
    )

  bet_df <- data.frame(
    unidade     = V(d$grafo)$name,
    betweenness = round(V(d$grafo)$betweenness * 100, 1),
    degree_in   = V(d$grafo)$degree_in,
    degree_out  = V(d$grafo)$degree_out,
    stringsAsFactors = FALSE
  )

  traces_df <- d$traces %>%
    distinct(case_id, .keep_all = TRUE) %>%
    select(case_id, trace_units, n_eventos)

  mu_df <- d$metricas_u %>%
    mutate(
      papel = case_when(
        centralidade >= 70 ~ "hub_central",
        centralidade >= 20 ~ "intermediario",
        TRUE               ~ "especializado"
      )
    )

  obj <- list(
    metadados = list(
      gerado_em             = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      total_processos       = n_distinct(d$el$case_id),
      total_eventos         = nrow(d$el),
      total_unidades        = n_distinct(d$el$resource),
      periodo_inicio        = format(min(d$el$timestamp), "%Y-%m-%d"),
      periodo_fim           = format(max(d$el$timestamp), "%Y-%m-%d"),
      prazo_referencia_dias = 90L,
      sistema               = "SEI - Secretaria Estadual de Saude PE"
    ),

    resumo_risco = list(
      alto  = sum(risco_df$risco == "ALTO"),
      medio = sum(risco_df$risco == "MÉDIO"),
      baixo = sum(risco_df$risco == "BAIXO"),
      abertos    = sum(risco_df$status_final == "ABERTO"),
      concluidos = sum(risco_df$status_final != "ABERTO")
    ),

    # Um objeto por processo
    processos = risco_df,

    # Sequência de setores por processo
    traces = traces_df,

    # Fluxo entre unidades com tempos de espera
    handovers = d$handover_df %>%
      select(from_unit, to_unit, n_handovers, avg_wait_hours, max_wait_hours),

    # Métricas de desempenho por unidade
    unidades = mu_df,

    # Análise de redes (betweenness = intermediação)
    centralidade_rede = bet_df,

    # Presença de atividades
    activity_presence = d$act_presence,

    # Mapa de processo (equivalente bupaR process map)
    mapa_processo = list(
      nos    = dados$mapa$nos,
      arestas= dados$mapa$arestas,
      descricao = "Grafo dirigido: nos=unidades organizacionais, arestas=handovers com frequencia e tempo de espera"
    ),

    # Metodologia do escore de risco
    metodologia_risco = list(
      descricao = "Escore composto por 6 indicadores ponderados",
      indicadores = list(
        list(nome = "consumo_prazo",    peso = 0.30, descricao = "Throughput vs prazo normativo de 90 dias"),
        list(nome = "idx_retrabalho",   peso = 0.25, descricao = "Proporcao de eventos de retrabalho (exclusoes + reaberturas)"),
        list(nome = "idx_dispersao",    peso = 0.20, descricao = "Numero de unidades vs media historica"),
        list(nome = "idx_gargalo",      peso = 0.10, descricao = "Passou por unidade com espera media > 24h"),
        list(nome = "idx_aberto_longo", peso = 0.10, descricao = "Status aberto com throughput > 30 dias"),
        list(nome = "deficit_prog",     peso = 0.05, descricao = "Deficit de eventos vs mediana historica")
      ),
      classificacao = list(
        alto  = "score_normalizado >= 65",
        medio = "score_normalizado >= 35 e < 65",
        baixo = "score_normalizado < 35"
      ),
      nota = "Com menos de 100 processos concluidos, o escore e heuristico (nao estatistico). Substituir por glm(family=binomial) quando houver volume suficiente."
    )
  )

  json_str <- toJSON(obj, pretty = TRUE, auto_unbox = TRUE, na = "null")

  if (!is.null(caminho_saida)) {
    writeLines(json_str, caminho_saida, useBytes = FALSE)
    message("JSON exportado para: ", caminho_saida)
  }

  invisible(json_str)
}

# ── 14. GERAR PROMPT PARA ANÁLISE EXTERNA POR LLM ────────────────────────────
gerar_prompt_llm <- function(dados) {
  d        <- dados
  risco_df <- d$risco %>% mutate(risco = as.character(risco))

  n_alto  <- sum(risco_df$risco == "ALTO")
  n_medio <- sum(risco_df$risco == "MÉDIO")
  n_baixo <- sum(risco_df$risco == "BAIXO")

  top_gargalo <- d$handover_df %>%
    arrange(desc(avg_wait_hours)) %>%
    slice(1)

  top_hub <- d$metricas_u %>%
    arrange(desc(centralidade)) %>%
    slice(1)

  proc_critico <- risco_df %>%
    filter(risco == "ALTO") %>%
    arrange(desc(score_normalizado)) %>%
    slice(1)

  atividades_risco <- d$act_presence %>%
    filter(pct_presenca < 50) %>%
    pull(activity) %>%
    paste(collapse = ", ")

  prompt <- paste0(
'Você é um especialista em gestão pública, processos administrativos e mineração de processos (process mining).

Analise os dados estruturados abaixo, gerados pelo sistema de monitoramento de processos de aquisição de medicamentos da Secretaria Estadual de Saúde de Pernambuco, processados a partir dos registros do Sistema Eletrônico de Informações (SEI).

Os dados foram produzidos com técnicas de mineração de processos equivalentes ao bupaR (event log, traces, handover matrix, activity presence, betweenness centrality via igraph) e um escore de risco composto por 6 indicadores heurísticos.

---

## CONTEXTO OPERACIONAL

- Sistema de origem: SEI (Sistema Eletrônico de Informações)
- Órgão: Secretaria Estadual de Saúde — Pernambuco
- Tipo de processo: Aquisição de medicamentos (pregão eletrônico / dispensa)
- Prazo normativo de referência: 90 dias
- Período analisado: ', format(min(d$el$timestamp), "%d/%m/%Y"), ' a ', format(max(d$el$timestamp), "%d/%m/%Y"), '
- Total de processos: ', n_distinct(d$el$case_id), '
- Total de eventos registrados: ', nrow(d$el), '
- Unidades organizacionais envolvidas: ', n_distinct(d$el$resource), '

---

## SUMÁRIO DE RISCO

- Processos em RISCO ALTO: ', n_alto, ' (requerem ação imediata)
- Processos em RISCO MÉDIO: ', n_medio, ' (requerem monitoramento)
- Processos em RISCO BAIXO: ', n_baixo, ' (dentro do esperado)

Processo mais crítico: ', proc_critico$case_id[1], '
  - Score: ', proc_critico$score_normalizado[1], '/100
  - Duração: ', round(proc_critico$throughput_days[1]), ' dias
  - Handovers: ', proc_critico$handovers[1], '
  - Retrabalho: ', proc_critico$retrabalho[1], ' ocorrência(s)

---

## GARGALO MAIS SEVERO (handover matrix)

Transição: ', top_gargalo$from_unit, ' → ', top_gargalo$to_unit, '
  - Espera média: ', round(top_gargalo$avg_wait_hours, 1), ' horas (', round(top_gargalo$avg_wait_hours/24, 1), ' dias)
  - Espera máxima: ', round(top_gargalo$max_wait_hours, 1), ' horas
  - Volume: ', top_gargalo$n_handovers, ' passagens

---

## UNIDADE MAIS CENTRAL (betweenness centrality)

Unidade: ', top_hub$Unidade, '
  - Centralidade: ', top_hub$centralidade, ' (soma de envios e recebimentos)
  - Processos que passaram por ela: ', top_hub$processos, ' de ', n_distinct(d$el$case_id), '

---

## DADOS COMPLETOS EM JSON

```json
[SUBSTITUIR PELO CONTEÚDO DO ARQUIVO JSON EXPORTADO]
```

---

## INSTRUÇÕES DE ANÁLISE

Com base nos dados acima, realize as seguintes análises em português, com linguagem adequada para gestores públicos (sem jargão técnico excessivo):

### 1. DIAGNÓSTICO GERAL
Descreva o estado atual da carteira de processos de aquisição. Quantos processos estão em situação preocupante? Qual é o padrão de comportamento dominante?

### 2. PROCESSOS CRÍTICOS
Para cada processo com risco ALTO, explique em linguagem simples:
- Por que ele foi classificado como alto risco
- Quais sinais concretos indicam problema (retrabalho, tempo excessivo, muitos setores)
- Qual seria a ação gerencial recomendada

### 3. GARGALOS ESTRUTURAIS
Identifique os pontos de lentidão no fluxo entre setores. Qual transição concentra mais tempo perdido? O gargalo é pontual (um processo específico) ou sistêmico (afeta todos)?

### 4. ANÁLISE DA REDE DE UNIDADES
Com base na centralidade e betweenness:
- Quais unidades são pontos críticos que, se sobrecarregadas, travam toda a cadeia?
- Existe concentração excessiva de passagens em poucas unidades?
- O fluxo organizacional parece equilibrado ou há desequilíbrio estrutural?

### 5. PADRÕES DE RETRABALHO
Identifique onde e com que frequência há documentos refeitos ou processos reabertos. O retrabalho parece ser um problema sistêmico ou isolado?

### 6. RECOMENDAÇÕES PRIORITÁRIAS
Liste de 3 a 5 ações concretas, ordenadas por impacto esperado, que o gestor deve tomar nos próximos 30 dias para reduzir o tempo médio de tramitação e o nível de risco da carteira.

### 7. LIMITAÇÕES DA ANÁLISE
Aponte honestamente o que esta análise NÃO pode concluir com os dados disponíveis (volume pequeno de processos, ausência de prazo normativo explícito, lifecycle inferido, etc.).

---

Seja direto, objetivo e prático. Priorize recomendações acionáveis. Evite repetir os números já apresentados no sumário — interprete-os, não os repita.
'
  )

  invisible(prompt)
}

# ── 15. MAPA DE PROCESSO (equivalente bupaR process map) ─────────────────────
# Retorna nós e arestas prontos para visualização e exportação JSON
gerar_mapa_processo <- function(el, handover_df, metricas_u) {

  # Nós: cada atividade/unidade é um nó
  nos <- metricas_u %>%
    rename(id = Unidade) %>%
    mutate(
      label      = str_replace(id, "SES - ", ""),
      freq_rel   = round(processos / max(processos) * 100, 1),
      papel      = case_when(
        centralidade >= 70 ~ "hub_central",
        centralidade >= 20 ~ "intermediario",
        TRUE               ~ "especializado"
      )
    )

  # Frequência de início (source nodes)
  freq_inicio <- el %>%
    group_by(case_id) %>%
    slice(1) %>%
    ungroup() %>%
    count(resource, name = "freq_inicio") %>%
    rename(id = resource)

  # Frequência de fim (sink nodes)
  freq_fim <- el %>%
    group_by(case_id) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    count(resource, name = "freq_fim") %>%
    rename(id = resource)

  nos <- nos %>%
    left_join(freq_inicio, by = "id") %>%
    left_join(freq_fim,    by = "id") %>%
    mutate(
      freq_inicio = replace_na(freq_inicio, 0),
      freq_fim    = replace_na(freq_fim, 0),
      is_source   = freq_inicio > 0,
      is_sink     = freq_fim    > 0
    )

  # Arestas: handovers com métricas completas
  arestas <- handover_df %>%
    rename(
      source      = from_unit,
      target      = to_unit,
      frequency   = n_handovers,
      wait_avg_h  = avg_wait_hours,
      wait_max_h  = max_wait_hours
    ) %>%
    mutate(
      wait_avg_days = round(wait_avg_h / 24, 2),
      wait_max_days = round(wait_max_h / 24, 2),
      performance   = case_when(
        wait_avg_h > 48 ~ "lento",
        wait_avg_h > 8  ~ "moderado",
        TRUE            ~ "rapido"
      )
    )

  list(nos = nos, arestas = arestas)
}

# ── 16. MAPAS DE PROCESSO via GRAPHVIZ DOT (equivalente bupaR process_map) ───

# Helper: ID seguro para nomes de setor
safe_id <- function(x) str_replace_all(str_replace(x,"SES - ",""), "[^A-Za-z0-9]", "_")

# 16a. Nós e arestas compartilhados para os mapas estilo bupaR ─────────────

# Retorna cor de fundo do nó baseada em frequência relativa (escala branco→azul)
freq_fill <- function(freq_rel) {
  # freq_rel entre 0 e 1
  r <- round(255 - freq_rel * 80)
  g <- round(255 - freq_rel * 80)
  b <- 255
  sprintf("#%02X%02X%02X", r, g, b)
}

# Cabeçalho comum do grafo DOT (estilo bupaR)
dot_header <- function(rankdir = "LR") {
  paste0(
    'digraph process_map {\n',
    '  graph [rankdir=', rankdir, ', bgcolor="#FFFFFF", ',
    'pad=0.5, nodesep=0.7, ranksep=1.0, fontname="Arial"];\n',
    '  node [fontname="Arial", fontsize=10];\n',
    '  edge [fontname="Arial", fontsize=9];\n'
  )
}

# Nó START (círculo preto cheio — padrão bupaR)
dot_start_node <- function(n_casos) {
  paste0('  START [shape=circle, style=filled, fillcolor="#000000", ',
    'label="", width=0.3, height=0.3, tooltip="Início\\n', n_casos, ' casos"];\n')
}

# Nó END (círculo duplo preto — padrão bupaR)
dot_end_node <- function(n_casos) {
  paste0('  END [shape=circle, style=filled, fillcolor="#000000", ',
    'peripheries=2, label="", width=0.3, height=0.3, tooltip="Fim\\n', n_casos, ' casos"];\n')
}

# Construir nó de atividade no estilo bupaR
dot_activity_node <- function(id, label, line2, fill_col="#FFF8DC", border_col="#999999", penwidth=1.5) {
  full_label <- paste0(label, "\\n", line2)
  paste0('  "', id, '" [shape=rectangle, style="rounded,filled", ',
    'fillcolor="', fill_col, '", color="', border_col, '", penwidth=', round(penwidth,1), ', ',
    'label="', full_label, '", tooltip="', label, '\\n', line2, '"];\n')
}

# Construir aresta no estilo bupaR
dot_edge <- function(from_id, to_id, label, penwidth=1.5, color="#555555") {
  paste0('  "', from_id, '" -> "', to_id, '" [',
    'label="', label, '", penwidth=', round(penwidth,1), ', ',
    'color="', color, '", fontcolor="#555555", arrowsize=0.7];\n')
}

# ─────────────────────────────────────────────────────────────────────────────
# 16a. Frequency map — process_map(type=frequency) estilo bupaR
dot_frequency <- function(d) {
  ho   <- d$handover_df
  mu   <- d$metricas_u
  el   <- d$el
  n_casos <- n_distinct(el$case_id)
  mx_f <- max(ho$n_handovers, 1)
  mx_e <- max(mu$total_eventos, 1)

  # Identificar start/end por caso
  starts <- el %>% group_by(case_id) %>% slice(1)         %>% ungroup() %>% count(resource, name="n_s")
  ends   <- el %>% group_by(case_id) %>% slice_tail(n=1)  %>% ungroup() %>% count(resource, name="n_e")

  nodes <- mu %>%
    left_join(starts, by=c("Unidade"="resource")) %>%
    left_join(ends,   by=c("Unidade"="resource")) %>%
    replace_na(list(n_s=0L, n_e=0L)) %>%
    mutate(
      nid       = safe_id(Unidade),
      lbl       = str_replace(Unidade,"SES - ",""),
      freq_rel  = total_eventos / mx_e,
      fill      = freq_fill(freq_rel),
      border    = "#666666",
      pw        = round(0.8 + freq_rel * 2.5, 1),
      line2     = paste0(total_eventos, " eventos (", round(processos/n_casos*100), "%)")
    )

  # Arestas START→primeiro, último→END por frequência
  start_edges <- starts %>%
    mutate(nid=safe_id(resource), pct=round(n_s/n_casos*100)) %>%
    mutate(lbl=paste0(n_s, " (", pct, "%)"),
           pw=round(0.8 + (n_s/n_casos)*4, 1))
  end_edges <- ends %>%
    mutate(nid=safe_id(resource), pct=round(n_e/n_casos*100)) %>%
    mutate(lbl=paste0(n_e, " (", pct, "%)"),
           pw=round(0.8 + (n_e/n_casos)*4, 1))

  dot <- dot_header()
  dot <- paste0(dot, dot_start_node(n_casos))
  dot <- paste0(dot, dot_end_node(n_casos))

  for (i in seq_len(nrow(nodes))) {
    n <- nodes[i,]
    dot <- paste0(dot, dot_activity_node(n$nid, n$lbl, n$line2, n$fill, n$border, n$pw))
  }

  # START edges
  for (i in seq_len(nrow(start_edges))) {
    e <- start_edges[i,]
    dot <- paste0(dot, '  START -> "', e$nid, '" [label="', e$lbl,
      '", penwidth=', e$pw, ', color="#555555", fontcolor="#555555", arrowsize=0.7];\n')
  }

  # Activity edges
  edges <- ho %>% mutate(
    fid = safe_id(from_unit), tid = safe_id(to_unit),
    pw  = round(0.8 + (n_handovers/mx_f) * 5, 1),
    col = "#555555",
    lbl = paste0(n_handovers, "x")
  )
  for (i in seq_len(nrow(edges))) {
    e <- edges[i,]
    dot <- paste0(dot, dot_edge(e$fid, e$tid, e$lbl, e$pw, e$col))
  }

  # END edges
  for (i in seq_len(nrow(end_edges))) {
    e <- end_edges[i,]
    dot <- paste0(dot, '  "', e$nid, '" -> END [label="', e$lbl,
      '", penwidth=', e$pw, ', color="#555555", fontcolor="#555555", arrowsize=0.7];\n')
  }

  paste0(dot, "}\n")
}

# ─────────────────────────────────────────────────────────────────────────────
# 16b. Performance map — process_map(performance(mean, "hours")) estilo bupaR
dot_performance <- function(d) {
  ho   <- d$handover_df
  mu   <- d$metricas_u
  el   <- d$el
  n_casos <- n_distinct(el$case_id)
  mx_w <- max(ho$avg_wait_hours, 0.1)

  nodes <- mu %>%
    left_join(
      ho %>% group_by(Unidade=to_unit) %>%
        summarise(wait_in=mean(avg_wait_hours, na.rm=TRUE), .groups="drop"),
      by="Unidade"
    ) %>%
    replace_na(list(wait_in=0)) %>%
    mutate(
      nid      = safe_id(Unidade),
      lbl      = str_replace(Unidade,"SES - ",""),
      freq_rel = pmin(wait_in / 200, 1),  # scale: 0h=white, 200h+=full blue
      fill     = case_when(
        wait_in > 100 ~ "#FFCCCC",   # red tint = slow
        wait_in > 24  ~ "#FFF3CC",   # yellow tint = moderate
        TRUE          ~ "#CCFFCC"    # green tint = fast
      ),
      border   = case_when(
        wait_in > 100 ~ "#CC0000",
        wait_in > 24  ~ "#CC8800",
        TRUE          ~ "#006600"
      ),
      pw       = 1.5,
      wait_lbl = case_when(
        wait_in < 1   ~ "< 1h",
        wait_in < 24  ~ paste0(round(wait_in), "h espera"),
        TRUE          ~ paste0(round(wait_in/24, 1), "d espera")
      ),
      line2    = paste0(total_eventos, " ev | ", wait_lbl)
    )

  edges <- ho %>%
    mutate(
      fid  = safe_id(from_unit), tid = safe_id(to_unit),
      pw   = round(0.8 + (avg_wait_hours/mx_w)*5, 1),
      col  = case_when(avg_wait_hours>48~"#CC0000", avg_wait_hours>8~"#CC8800", TRUE~"#006600"),
      lbl  = case_when(
        avg_wait_hours < 1  ~ "< 1h",
        avg_wait_hours < 24 ~ paste0(round(avg_wait_hours), "h"),
        TRUE                ~ paste0(round(avg_wait_hours/24, 1), "d")
      )
    )

  starts <- el %>% group_by(case_id) %>% slice(1) %>% ungroup() %>% count(resource, name="n_s")
  ends   <- el %>% group_by(case_id) %>% slice_tail(n=1) %>% ungroup() %>% count(resource, name="n_e")

  dot <- dot_header()
  dot <- paste0(dot, dot_start_node(n_casos))
  dot <- paste0(dot, dot_end_node(n_casos))
  for (i in seq_len(nrow(nodes))) {
    n <- nodes[i,]
    dot <- paste0(dot, dot_activity_node(n$nid, n$lbl, n$line2, n$fill, n$border, n$pw))
  }
  for (i in seq_len(nrow(starts))) {
    e <- starts[i,]
    dot <- paste0(dot, '  START -> "', safe_id(e$resource), '" [label="', e$n_s,
      '", penwidth=1.5, color="#555555", arrowsize=0.7];\n')
  }
  for (i in seq_len(nrow(edges))) {
    e <- edges[i,]
    dot <- paste0(dot, dot_edge(e$fid, e$tid, e$lbl, e$pw, e$col))
  }
  for (i in seq_len(nrow(ends))) {
    e <- ends[i,]
    dot <- paste0(dot, '  "', safe_id(e$resource), '" -> END [label="', e$n_e,
      '", penwidth=1.5, color="#555555", arrowsize=0.7];\n')
  }
  paste0(dot, "}\n")
}

# ─────────────────────────────────────────────────────────────────────────────
# 16c. Risco map — custom: exposição a processos de risco alto
dot_risco <- function(d) {
  ho      <- d$handover_df
  mu      <- d$metricas_u
  el      <- d$el
  n_casos <- n_distinct(el$case_id)
  mx_f    <- max(ho$n_handovers, 1)

  alto_seis <- d$risco %>% filter(as.character(risco)=="ALTO") %>% pull(case_id)
  risco_u   <- el %>%
    group_by(resource) %>%
    summarise(pct_alto=round(mean(case_id %in% alto_seis)*100), .groups="drop")

  nodes <- mu %>%
    left_join(risco_u, by=c("Unidade"="resource")) %>%
    replace_na(list(pct_alto=0)) %>%
    mutate(
      nid    = safe_id(Unidade),
      lbl    = str_replace(Unidade,"SES - ",""),
      fill   = case_when(pct_alto>=75~"#FFCCCC", pct_alto>=40~"#FFF3CC", TRUE~"#CCFFEE"),
      border = case_when(pct_alto>=75~"#CC0000", pct_alto>=40~"#CC8800", TRUE~"#006600"),
      pw     = 1.5,
      line2  = paste0(pct_alto, "% em risco alto")
    )

  starts <- el %>% group_by(case_id) %>% slice(1) %>% ungroup() %>% count(resource,name="n_s")
  ends   <- el %>% group_by(case_id) %>% slice_tail(n=1) %>% ungroup() %>% count(resource,name="n_e")
  edges  <- ho %>% mutate(fid=safe_id(from_unit),tid=safe_id(to_unit),
    pw=round(0.8+(n_handovers/mx_f)*5,1),col="#888888",lbl=paste0(n_handovers,"x"))

  dot <- dot_header()
  dot <- paste0(dot, dot_start_node(n_casos))
  dot <- paste0(dot, dot_end_node(n_casos))
  for (i in seq_len(nrow(nodes))) { n<-nodes[i,]; dot<-paste0(dot,dot_activity_node(n$nid,n$lbl,n$line2,n$fill,n$border,n$pw)) }
  for (i in seq_len(nrow(starts))) { e<-starts[i,]; dot<-paste0(dot,'  START -> "',safe_id(e$resource),'" [label="',e$n_s,'", penwidth=1.5, color="#555555", arrowsize=0.7];\n') }
  for (i in seq_len(nrow(edges)))  { e<-edges[i,];  dot<-paste0(dot,dot_edge(e$fid,e$tid,e$lbl,e$pw,e$col)) }
  for (i in seq_len(nrow(ends)))   { e<-ends[i,];   dot<-paste0(dot,'  "',safe_id(e$resource),'" -> END [label="',e$n_e,'", penwidth=1.5, color="#555555", arrowsize=0.7];\n') }
  paste0(dot,"}\n")
}

# ─────────────────────────────────────────────────────────────────────────────
# 16d. Retrabalho map — loops/retornos destacados
dot_retrabalho <- function(d) {
  ho      <- d$handover_df
  mu      <- d$metricas_u
  el      <- d$el
  n_casos <- n_distinct(el$case_id)
  mx_f    <- max(ho$n_handovers, 1)

  # Detectar loops (A→B existe e B→A existe)
  loop_pairs <- ho %>%
    inner_join(ho %>% select(from_unit, to_unit),
               by=c("from_unit"="to_unit","to_unit"="from_unit")) %>%
    mutate(pair_key = paste0(from_unit,"->",to_unit)) %>%
    pull(pair_key)

  retrabalho_u <- d$risco %>%
    group_by() %>% summarise() # placeholder
  
  nodes <- mu %>%
    mutate(
      nid   = safe_id(Unidade),
      lbl   = str_replace(Unidade,"SES - ",""),
      fill  = "#EEF2FF",
      border= "#6366f1",
      pw    = 1.5,
      line2 = paste0(total_eventos," eventos")
    )

  starts <- el %>% group_by(case_id) %>% slice(1) %>% ungroup() %>% count(resource,name="n_s")
  ends   <- el %>% group_by(case_id) %>% slice_tail(n=1) %>% ungroup() %>% count(resource,name="n_e")
  edges  <- ho %>%
    mutate(
      fid     = safe_id(from_unit), tid = safe_id(to_unit),
      pair_id = paste0(from_unit,"->",to_unit)
    ) %>%
    mutate(
      is_loop = pair_id %in% loop_pairs,
      pw      = round(0.8+(n_handovers/mx_f)*5,1),
      col     = ifelse(is_loop,"#CC0000","#888888"),
      style_e = ifelse(is_loop,"dashed","solid"),
      lbl     = ifelse(is_loop, paste0(n_handovers,"x ↩"), paste0(n_handovers,"x"))
    )

  dot <- dot_header()
  dot <- paste0(dot, dot_start_node(n_casos))
  dot <- paste0(dot, dot_end_node(n_casos))
  for (i in seq_len(nrow(nodes))) { n<-nodes[i,]; dot<-paste0(dot,dot_activity_node(n$nid,n$lbl,n$line2,n$fill,n$border,n$pw)) }
  for (i in seq_len(nrow(starts))) { e<-starts[i,]; dot<-paste0(dot,'  START -> "',safe_id(e$resource),'" [label="',e$n_s,'", penwidth=1.5, color="#555555", arrowsize=0.7];\n') }
  for (i in seq_len(nrow(edges))) {
    e <- edges[i,]
    dot <- paste0(dot, '  "', e$fid, '" -> "', e$tid, '" [label="', e$lbl,
      '", penwidth=', e$pw, ', color="', e$col, '", style=', e$style_e,
      ', fontcolor="#555555", arrowsize=0.7];\n')
  }
  for (i in seq_len(nrow(ends))) { e<-ends[i,]; dot<-paste0(dot,'  "',safe_id(e$resource),'" -> END [label="',e$n_e,'", penwidth=1.5, color="#555555", arrowsize=0.7];\n') }
  paste0(dot,"}\n")
}


# 16e. Render DOT string -> SVG string — cross-platform (Linux + Windows)
encontrar_dot <- function() {
  # 1. Tentar via PATH do sistema
  bin <- Sys.which("dot")
  if (nzchar(bin)) return(bin)

  # 2. Caminhos comuns no Linux
  linux_paths <- c("/usr/bin/dot", "/usr/local/bin/dot")
  for (p in linux_paths) if (file.exists(p)) return(p)

  # 3. Caminhos comuns no Windows
  win_paths <- c(
    "C:/Program Files/Graphviz/bin/dot.exe",
    "C:/Program Files (x86)/Graphviz/bin/dot.exe",
    "C:/Graphviz/bin/dot.exe",
    file.path(Sys.getenv("ProgramFiles"), "Graphviz/bin/dot.exe"),
    file.path(Sys.getenv("APPDATA"), "../Local/Programs/Graphviz/bin/dot.exe")
  )
  for (p in win_paths) if (file.exists(p)) return(p)

  return("")
}

dot_to_svg <- function(dot_str, layout="dot") {
  dot_bin <- encontrar_dot()

  if (!nzchar(dot_bin)) {
    msg <- paste0(
      "Graphviz nao encontrado.\n",
      "Instale em: https://graphviz.org/download/\n",
      "Linux: sudo apt-get install graphviz\n",
      "Windows: baixe o instalador em graphviz.org e adicione ao PATH"
    )
    stop(msg)
  }

  tf_in  <- tempfile(fileext=".dot")
  tf_out <- tempfile(fileext=".svg")
  on.exit(unlink(c(tf_in, tf_out)), add=TRUE)

  writeLines(dot_str, tf_in, useBytes=FALSE)

  suppressWarnings(
    system2(dot_bin,
      args   = c("-Tsvg", paste0("-K", layout), tf_in, "-o", tf_out),
      stdout = FALSE, stderr = FALSE
    )
  )

  if (!file.exists(tf_out) || file.size(tf_out) < 50)
    stop("dot falhou ao gerar SVG. Verifique se o Graphviz esta corretamente instalado.")

  raw <- paste(readLines(tf_out, warn=FALSE, encoding="UTF-8"), collapse="\n")

  # Extrair a partir da tag <svg (ignora cabecalho XML/DOCTYPE)
  m <- regexpr("<svg[ \t>]", raw, perl=TRUE)
  if (m < 1) stop("Tag <svg nao encontrada no output do Graphviz")
  substr(raw, as.integer(m), nchar(raw))
}

# ==============================================================================
# MÓDULO DE FILTROS — equivalente edeaR
# ==============================================================================

#' filter_throughput_time() — equivalente edeaR::filter_throughput_time()
#' Filtra casos com throughput_time dentro do intervalo [min_days, max_days]
filter_throughput_time <- function(el, min_days = 0, max_days = Inf,
                                   units = "days") {
  casos_validos <- el %>%
    group_by(case_id) %>%
    summarise(
      throughput = as.numeric(difftime(max(timestamp), min(timestamp), units=units)),
      .groups = "drop"
    ) %>%
    filter(throughput >= min_days, throughput <= max_days) %>%
    pull(case_id)
  el %>% filter(case_id %in% casos_validos)
}

#' filter_activity_presence() — equivalente edeaR::filter_activity_presence()
#' Filtra casos que CONTÊM pelo menos uma das atividades especificadas
#' Se mode = "all", o caso precisa conter TODAS as atividades.
filter_activity_presence <- function(el, activities, mode = "any") {
  casos_com <- el %>%
    filter(activity %in% activities) %>%
    group_by(case_id) %>%
    summarise(
      n_ativ_presentes = n_distinct(activity[activity %in% activities]),
      .groups = "drop"
    )
  if (mode == "all") {
    casos_validos <- casos_com %>%
      filter(n_ativ_presentes == length(unique(activities))) %>%
      pull(case_id)
  } else {
    casos_validos <- casos_com$case_id
  }
  el %>% filter(case_id %in% casos_validos)
}

#' filter_resource_presence() — equivalente edeaR::filter_resource()
#' Filtra casos que passaram por determinado(s) setor(es)
filter_resource_presence <- function(el, resources, mode = "any") {
  casos_com <- el %>%
    filter(resource %in% resources) %>%
    group_by(case_id) %>%
    summarise(n_res = n_distinct(resource), .groups = "drop")
  if (mode == "all") {
    casos_validos <- casos_com %>% filter(n_res == length(unique(resources))) %>% pull(case_id)
  } else {
    casos_validos <- casos_com$case_id
  }
  el %>% filter(case_id %in% casos_validos)
}

#' filter_case_performance() — equivalente edeaR: filtra por nível de risco
filter_case_performance <- function(el, risco_df, niveis = c("ALTO","MÉDIO","BAIXO")) {
  casos_validos <- risco_df %>%
    filter(as.character(risco) %in% niveis) %>%
    pull(case_id)
  el %>% filter(case_id %in% casos_validos)
}

# ==============================================================================
# ANIMATE PROCESS — equivalente processanimateR::animate_process()
# Usa SMIL (SVG animation) nativo — exatamente como o processanimateR
# Tokens SVG <circle> com <animateMotion> se movem entre nós do mapa
# ==============================================================================

animate_process <- function(d, el_filtrado = NULL, velocidade = 1) {
  library(jsonlite)

  el_use <- if (!is.null(el_filtrado) && nrow(el_filtrado) > 0) el_filtrado else d$el
  n_casos <- n_distinct(el_use$case_id)
  if (n_casos == 0) return("<p style='color:#ef4444;padding:20px;'>Nenhum caso disponível para animação.</p>")

  # ── Gerar SVG do mapa de processo ─────────────────────────────────────────
  d_anim <- d
  if (!is.null(el_filtrado) && nrow(el_filtrado) > 0) {
    d_anim$el         <- el_filtrado
    d_anim$handover_df<- calcular_handover_matrix(el_filtrado)
    d_anim$metricas_u <- calcular_metricas_unidade(el_filtrado, d_anim$handover_df)
  }
  svg_map <- tryCatch(dot_to_svg(dot_frequency(d_anim)),
    error = function(e) NULL)
  if (is.null(svg_map))
    return("<p style='color:#ef4444;padding:20px;'>Graphviz não encontrado. Instale em graphviz.org</p>")

  # ── Extrair viewBox e posições dos nós ────────────────────────────────────
  vb_str  <- regmatches(svg_map, regexpr('viewBox="[^"]+"', svg_map))
  vb_vals <- as.numeric(strsplit(gsub('viewBox="([^"]+)"','\\1',vb_str),' ')[[1]])
  vb_w <- vb_vals[3]; vb_h <- vb_vals[4]

  texts_svg <- regmatches(svg_map, gregexpr('<text[^>]+>[^<]+</text>', svg_map))[[1]]
  t_labels  <- regmatches(texts_svg, regexpr('(?<=>)[^<]+(?=</text>)', texts_svg, perl=TRUE))
  t_x       <- as.numeric(regmatches(texts_svg, regexpr('(?<=x=")[^"]+', texts_svg, perl=TRUE)))
  t_y       <- as.numeric(regmatches(texts_svg, regexpr('(?<=y=")[^"]+', texts_svg, perl=TRUE)))

  unit_ids  <- c("GCJ_ARP","GCJ","GFAJ_SA","NP_DGPO","CSANS","GFAJ","GORC","NAJ","START","END")
  keep      <- t_labels %in% unit_ids
  node_df   <- data.frame(
    label = t_labels[keep],
    cx    = t_x[keep],
    cy    = vb_h + t_y[keep],   # graphviz SVG: y flipped; real SVG y = height + text_y
    stringsAsFactors = FALSE
  )

  # ── Segmentos temporais por caso ──────────────────────────────────────────
  t0    <- min(el_use$timestamp)
  max_t <- as.numeric(difftime(max(el_use$timestamp), t0, units = "days"))
  # Animation total duration in seconds (scaled by velocidade)
  anim_dur_sec <- max(10, round(max_t / velocidade))
  scale <- anim_dur_sec / max_t   # seconds per day

  segs_all <- el_use %>%
    arrange(case_id, timestamp) %>%
    group_by(case_id) %>%
    mutate(
      mudou  = resource != lag(resource, default = first(resource)) | row_number() == 1,
      seg_id = cumsum(mudou)
    ) %>%
    group_by(case_id, seg_id, resource) %>%
    summarise(
      seg_inicio = min(timestamp),
      seg_fim    = max(timestamp),
      n_eventos  = n(),
      .groups    = "drop"
    ) %>%
    mutate(
      unit_id = str_replace(resource, "SES - ", ""),
      t_s     = as.numeric(difftime(seg_inicio, t0, units = "days")) * scale,
      t_e     = pmax(as.numeric(difftime(seg_fim, t0, units = "days")) * scale, t_s + 0.3)
    )

  riscos <- d$risco %>%
    mutate(risco = as.character(risco)) %>%
    select(case_id, risco, score_normalizado)

  cases_df <- segs_all %>%
    distinct(case_id) %>%
    left_join(riscos, by = "case_id") %>%
    mutate(
      risco       = replace_na(risco, "BAIXO"),
      token_color = case_when(risco=="ALTO"~"#ef4444", risco=="MÉDIO"~"#f59e0b", TRUE~"#22c55e"),
      case_short  = str_extract(case_id, "[^./]+/[^./]+-[^./]+$"),
      tok_idx     = row_number()
    )

  # ── Construir elementos SMIL para cada token ──────────────────────────────
  node_lookup <- setNames(as.list(as.data.frame(t(node_df[,c("cx","cy")]))), node_df$label)

  build_token_smil <- function(cid, color, label, idx) {
    case_segs <- segs_all %>% filter(case_id == cid) %>% arrange(t_s)
    if (nrow(case_segs) == 0) return("")

    r <- 8  # token radius

    # Start position = first unit
    first_unit <- case_segs$unit_id[1]
    pos0 <- node_lookup[[first_unit]]
    if (is.null(pos0)) return("")
    cx0 <- pos0[1]; cy0 <- pos0[2]

    # Build animateMotion elements
    motions <- character(nrow(case_segs))
    for (i in seq_len(nrow(case_segs))) {
      seg   <- case_segs[i,]
      unit  <- seg$unit_id
      pos   <- node_lookup[[unit]]
      if (is.null(pos)) next
      cx_n <- pos[1]; cy_n <- pos[2]

      # Offset slightly so multiple tokens on same node don't fully overlap
      jitter_x <- ((idx - 1) %% 3 - 1) * (r * 2.5)
      jitter_y <- ((idx - 1) %/% 3 %% 3 - 1) * (r * 2.5)

      target_x <- cx_n + jitter_x - cx0
      target_y <- cy_n + jitter_y - cy0

      t_begin <- round(seg$t_s, 3)
      t_dur   <- round(seg$t_e - seg$t_s, 3)
      t_dur   <- max(t_dur, 0.2)

      # Use animateTransform to move the token group
      motions[i] <- sprintf(
        '<animateTransform attributeName="transform" type="translate" from="%s,%s" to="%s,%s" begin="%.3fs" dur="%.3fs" fill="freeze" calcMode="linear"/>',
        if(i==1) 0 else {
          prev_pos <- node_lookup[[case_segs$unit_id[i-1]]]
          if(!is.null(prev_pos)) round(prev_pos[1]+jitter_x-cx0,1) else 0
        },
        if(i==1) 0 else {
          prev_pos <- node_lookup[[case_segs$unit_id[i-1]]]
          if(!is.null(prev_pos)) round(prev_pos[2]+jitter_y-cy0,1) else 0
        },
        round(target_x,1), round(target_y,1),
        t_begin, t_dur
      )
    }

    # Token: circle group with id for JS control
    paste0(
      '<g id="tok_', idx, '" data-case="', htmltools::htmlEscape(label), '" data-risco="', color, '">',
      '<circle cx="', round(cx0,1), '" cy="', round(cy0,1), '" r="', r, '" ',
        'fill="', color, '" opacity="0.9" stroke="white" stroke-width="1.5">',
        '<title>', htmltools::htmlEscape(label), '</title>',
      '</circle>',
      '<text x="', round(cx0,1), '" y="', round(cy0+3.5,1), '" text-anchor="middle" ',
        'font-size="7" font-family="DM Mono,monospace" fill="white" pointer-events="none">',
        idx,
      '</text>',
      paste(motions[nchar(motions)>0], collapse=""),
      '</g>'
    )
  }

  token_svgs <- mapply(build_token_smil,
    cid   = cases_df$case_id,
    color = cases_df$token_color,
    label = cases_df$case_short,
    idx   = cases_df$tok_idx,
    SIMPLIFY = TRUE
  )

  # ── Injetar tokens no SVG ─────────────────────────────────────────────────
  tokens_block <- paste(
    paste0('<g id="pam-tokens-layer">'),
    paste(token_svgs, collapse="\n"),
    '</g>',
    sep="\n"
  )

  # Remove closing </svg> and re-insert with tokens
  svg_with_tokens <- sub("</svg>\\s*$",
    paste0(tokens_block, "\n</svg>"),
    svg_map)

  # ── Build legend + controls HTML ──────────────────────────────────────────
  legend_items <- cases_df %>%
    arrange(tok_idx) %>%
    mutate(html = paste0(
      '<div style="display:flex;align-items:center;gap:5px;margin-right:10px;">',
      '<svg width="18" height="18"><circle cx="9" cy="9" r="7" fill="', token_color,
      '" stroke="white" stroke-width="1.5"/><text x="9" y="13" text-anchor="middle" ',
      'font-size="7" font-family=\'DM Mono\' fill="white">', tok_idx, '</text></svg>',
      '<span style="font-size:10px;color:#7a87a3;">', htmltools::htmlEscape(case_short), '</span>',
      '</div>'
    ))

  legend_html <- paste(legend_items$html, collapse="")

  # ── Assemble final HTML ───────────────────────────────────────────────────
  uid <- paste0("pam", sample(1e5, 1))
  paste0('
<style>
#', uid, '-wrap{background:#0f1117;border-radius:10px;overflow:hidden;}
#', uid, '-ctrl{background:#181c27;border-bottom:1px solid #2a3045;padding:10px 14px;
  display:flex;align-items:center;gap:10px;flex-wrap:wrap;}
.', uid, '-btn{background:#1e2334;border:1px solid #2a3045;color:#e8ecf4;border-radius:6px;
  padding:5px 14px;font-size:12px;cursor:pointer;font-family:"DM Sans",sans-serif;
  transition:background .15s;}
.', uid, '-btn:hover{background:#6366f1;border-color:#6366f1;}
#', uid, '-slider{flex:1;min-width:120px;accent-color:#6366f1;}
#', uid, '-t{color:#e8ecf4;font-size:12px;font-family:"DM Mono",monospace;min-width:60px;}
#', uid, '-svg-wrap{overflow:auto;background:#f8fafc;padding:12px;}
#', uid, '-svg-wrap svg{width:100%;height:auto;max-height:560px;}
#', uid, '-legend{display:flex;flex-wrap:wrap;gap:4px;margin-top:8px;padding:0 14px 10px;}
.', uid, '-speed{background:#1e2334;border:1px solid #2a3045;color:#e8ecf4;border-radius:6px;
  padding:4px 8px;font-size:11px;font-family:"DM Mono",monospace;width:64px;}
</style>

<div id="', uid, '-wrap">
  <div id="', uid, '-ctrl">
    <button class="', uid, '-btn" id="', uid, '-playbtn" onclick="', uid, '_toggle()">&#9654; Play</button>
    <button class="', uid, '-btn" onclick="', uid, '_reset()">&#8635; Reset</button>
    <input type="range" id="', uid, '-slider" min="0" max="',anim_dur_sec,'" value="0" step="0.1"
           oninput="', uid, '_seek(parseFloat(this.value))">
    <span id="', uid, '-t">0 / ',anim_dur_sec,'s</span>
    <label style="color:#7a87a3;font-size:11px;">Vel:
      <input type="number" class="', uid, '-speed" id="', uid, '-vel"
        value="1" min="0.1" max="10" step="0.5"
        onchange="', uid, '_setSpeed(parseFloat(this.value))">x
    </label>
    <span style="color:#7a87a3;font-size:10px;">',anim_dur_sec,'s = ',round(max_t),' dias reais</span>
  </div>
  <div id="', uid, '-svg-wrap">', svg_with_tokens, '</div>
  <div id="', uid, '-legend">
    <span style="color:#7a87a3;font-size:10px;margin-right:6px;align-self:center;">Processos:</span>
    ', legend_html, '
  </div>
</div>

<script>
(function(){
  var uid="', uid, '";
  var svgEl=document.querySelector("#"+uid+"-svg-wrap svg");
  var slider=document.getElementById(uid+"-slider");
  var tLabel=document.getElementById(uid+"-t");
  var playBtn=document.getElementById(uid+"-playbtn");
  var maxT=', anim_dur_sec, ';
  var curT=0, speed=1, playing=false, animId=null, lastTs=null;

  // Pause SVG SMIL initially
  function svgPause(){ if(svgEl&&svgEl.pauseAnimations) svgEl.pauseAnimations(); }
  function svgPlay(){  if(svgEl&&svgEl.unpauseAnimations) svgEl.unpauseAnimations(); }
  function svgSeek(t){ if(svgEl&&svgEl.setCurrentTime) svgEl.setCurrentTime(t); }

  function render(t){
    curT=Math.max(0,Math.min(t,maxT));
    slider.value=curT.toFixed(2);
    tLabel.textContent=curT.toFixed(1)+"s / "+maxT+"s";
    svgSeek(curT);
  }

  function step(ts){
    if(!playing)return;
    if(lastTs){var dt=(ts-lastTs)/1000*speed; curT=Math.min(curT+dt,maxT);}
    lastTs=ts;
    render(curT);
    if(curT<maxT){ animId=requestAnimationFrame(step); }
    else{ playing=false;lastTs=null;playBtn.textContent="\u25B6 Play"; svgPause(); }
  }

  window[uid+"_toggle"]=function(){
    if(playing){
      playing=false;lastTs=null;if(animId)cancelAnimationFrame(animId);
      playBtn.textContent="\u25B6 Play"; svgPause();
    } else {
      if(curT>=maxT)curT=0;
      playing=true; playBtn.textContent="\u23F8 Pausa";
      svgPlay(); svgSeek(curT);
      animId=requestAnimationFrame(step);
    }
  };
  window[uid+"_reset"]=function(){
    playing=false;lastTs=null;if(animId)cancelAnimationFrame(animId);
    playBtn.textContent="\u25B6 Play";
    svgPause(); render(0);
  };
  window[uid+"_seek"]=function(v){
    playing=false;lastTs=null;if(animId)cancelAnimationFrame(animId);
    playBtn.textContent="\u25B6 Play";
    svgPause(); render(parseFloat(v));
  };
  window[uid+"_setSpeed"]=function(v){ speed=v||1; };

  // Init: pause and go to t=0
  setTimeout(function(){
    svgPause(); svgSeek(0);
    render(0);
  }, 200);
})();
</script>
')
}
