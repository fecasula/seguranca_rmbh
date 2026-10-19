#===============================================================================
# FREQUÊNCIAS REVISADAS - SEXO, RAÇA E GRUPO ETÁRIO
# source("R/analise_frequencia.R") a partir da raiz do projeto.
# Oito gráficos: quatro painéis dia/noite + quatro gráficos de desordem social.
# Não executa nem altera os modelos logísticos.
#===============================================================================
source("R/funcoes_frequencia.R", local = TRUE)
source("R/renda_quartis.R", local = TRUE)
renda_preparada <- classificar_quartis(anexar_renda(preparar_frequencias(carregar_base())))
base_analise <- renda_preparada$base
cortes_renda <- renda_preparada$cortes
pasta <- file.path("resultados", "frequencias")
dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
frequencias <- prevalencias <- testes <- ausencias <- disponibilidade <- list()

analisar_recorte <- function(des, variavel, respostas, evento, ano, bloco, recorte,
                            periodo = NA_character_, item = NA_character_) {
  d <- des$variables
  # Cada recorte usa sua própria população elegível. Não filtra sexo nas
  # análises de raça/idade; raça excluída não remove respondentes de idade/sexo.
  grupos <- levels(d[[recorte]])
  for (g in grupos) {
    dom <- des[!is.na(d[[recorte]]) & d[[recorte]] == g, ]
    chave <- tibble(ano_rodada = ano, bloco = bloco, variavel = variavel,
      periodo = periodo, item = item, recorte = recorte, grupo = g)
    n <- nrow(dom$variables)
    ausencias[[length(ausencias) + 1L]] <<- bind_cols(chave,
      tibble(n_entrevistados = n, n_validos = sum(!is.na(dom$variables[[variavel]])),
             n_ausentes = sum(is.na(dom$variables[[variavel]]))))
    for (r in respostas) {
      frequencias[[length(frequencias) + 1L]] <<- bind_cols(chave,
        tibble(resposta = r), estimar_proporcao(dom, variavel, r))
    }
    if (!is.na(evento)) prevalencias[[length(prevalencias) + 1L]] <<- bind_cols(chave,
      tibble(evento = evento), estimar_proporcao(dom, variavel, evento))
  }
  if (!is.na(evento)) {
    chave <- tibble(ano_rodada = ano, bloco = bloco, variavel = variavel,
      periodo = periodo, item = item, recorte = recorte)
    testes[[length(testes) + 1L]] <<- bind_cols(chave, teste_grupos(des, variavel, recorte))
  }
}

for (ano in sort(unique(base_analise$ano_rodada))) {
  message("Frequências revisadas: rodada ", ano)
  d <- filter(base_analise, ano_rodada == ano)
  des <- criar_desenho(d)
  for (recorte in names(recortes_frequencia)) {
    for (periodo in c("dia", "noite")) {
      v <- paste0("inseguranca_", periodo)
      analisar_recorte(des, v, c("0", "1"), "1", ano, "Insegurança", recorte,
                       periodo = periodo)
      original <- paste0("andar_", periodo)
      respostas <- if (ano == 2002) setdiff(niveis_seguranca,
        "Nem seguro(a) nem inseguro(a)") else niveis_seguranca
      analisar_recorte(des, original, respostas, NA_character_, ano,
        "Respostas originais de segurança", recorte, periodo = periodo)
    }
    for (v in names(rotulos_desordem)) {
      aplicado <- ano %in% c(2005, 2008) &&
        (v != "desordem_prostituicao_ocorrencia" || ano == 2008)
      if (recorte == "sexo") disponibilidade[[length(disponibilidade) + 1L]] <-
        tibble(ano_rodada = ano, variavel = v, item = unname(rotulos_desordem[v]),
          aplicado = aplicado, motivo = if (aplicado) "Aplicada na rodada" else
            "Não aplicada na rodada")
      if (!aplicado) next
      analisar_recorte(des, v, c("Não", "Sim"), "Sim", ano,
        "Ocorrência de desordem social", recorte, item = unname(rotulos_desordem[v]))
    }
  }
}

frequencias <- bind_rows(frequencias)
testes <- ajustar_testes(bind_rows(testes), c("ano_rodada", "bloco", "recorte")) |>
  preservar_testes_sexo()
prevalencias <- bind_rows(prevalencias) |>
  left_join(testes, by = c("ano_rodada", "bloco", "variavel", "periodo", "item", "recorte"))
frequencias <- mutate(frequencias, percentual = proporcao * 100,
  ic_inferior_percentual = ic_inferior * 100, ic_superior_percentual = ic_superior * 100)
prevalencias <- mutate(prevalencias, percentual = proporcao * 100,
  ic_inferior_percentual = ic_inferior * 100, ic_superior_percentual = ic_superior * 100)
ausencias <- bind_rows(ausencias)
disponibilidade <- bind_rows(disponibilidade)
elegibilidade <- bind_rows(lapply(sort(unique(base_analise$ano_rodada)), function(ano) {
  d <- filter(base_analise, ano_rodada == ano)
  bind_rows(lapply(names(recortes_frequencia), function(r) {
    tibble(ano_rodada = ano, recorte = r, n_rodada = nrow(d),
      n_elegiveis = sum(!is.na(d[[r]])), n_excluidos = sum(is.na(d[[r]])),
      criterio = if (r == "raca_grupo") "Somente branca, amarela, preta, parda e indígena" else
        if (r == "idade_grupo") "Idade válida: 18–34, 35–59 ou 60+" else
        if (r == "quartil_renda") "Renda individual registrada válida" else "Sexo informado")
  }))
}))
mapa_raca <- base_analise |>
  count(ano_rodada, raca_original = raca_cor, grupo = raca_grupo, name = "n") |>
  mutate(incluida = !is.na(grupo))
composicao <- bind_rows(lapply(names(recortes_frequencia), function(r) {
  base_analise |>
    filter(!is.na(.data[[r]])) |>
    group_by(ano_rodada, grupo = .data[[r]]) |>
    summarise(n = n(), soma_pesos = sum(peso_analise), .groups = "drop") |>
    mutate(recorte = r, grupo = as.character(grupo))
}))
salvar_csv(frequencias, file.path(pasta, "frequencias_completas.csv"))
salvar_csv(prevalencias, file.path(pasta, "prevalencias_e_testes.csv"))
salvar_csv(testes, file.path(pasta, "testes_diferencas_entre_grupos.csv"))
salvar_csv(filter(testes, significativo_nominal), file.path(pasta, "diferencas_significativas.csv"))
salvar_csv(ausencias, file.path(pasta, "respostas_validas_e_ausentes.csv"))
salvar_csv(elegibilidade, file.path(pasta, "elegibilidade_por_recorte.csv"))
salvar_csv(mapa_raca, file.path(pasta, "recodificacao_raca.csv"))
salvar_csv(composicao, file.path(pasta, "composicao_grupos.csv"))
salvar_csv(disponibilidade, file.path(pasta, "disponibilidade_desordem_por_ano.csv"))
for (r in names(recortes_frequencia)) {
  nome <- switch(r, sexo = "sexo", raca_grupo = "raca", idade_grupo = "grupo_etario", quartil_renda = "renda")
  salvar_csv(filter(prevalencias, bloco == "Insegurança", recorte == r),
              file.path(pasta, paste0("inseguranca_por_", nome, ".csv")))
  salvar_csv(filter(prevalencias, bloco == "Ocorrência de desordem social", recorte == r),
              file.path(pasta, paste0("desordem_por_", nome, ".csv")))
}

# Oito gráficos de frequência, distribuídos nos dois blocos temáticos.
catalogo <- list(inseguranca = character(), desordem = character())
for (tipo in c("inseguranca", "desordem")) {
  bloco <- if (tipo == "inseguranca") "Insegurança" else "Ocorrência de desordem social"
  subpasta <- if (tipo == "inseguranca") "1_seguranca_sociodemograficas" else "2_desordem_social"
  for (r in names(recortes_frequencia)) {
    d <- filter(prevalencias, .data$bloco == .env$bloco, recorte == r)
    t <- filter(testes, .data$bloco == .env$bloco, recorte == r)
    p <- plotar_frequencias_revisadas(d, t, r, tipo)
    nome_recorte <- switch(r, sexo = "sexo", raca_grupo = "raca", idade_grupo = "grupo_etario", quartil_renda = "renda")
    nome <- paste0(if (tipo == "inseguranca") "painel_inseguranca_" else "ocorrencia_", nome_recorte)
    salvar_grafico(p, file.path(pasta, "graficos", subpasta), nome,
                   altura = if (tipo == "inseguranca") 6.4 else 8.2,
                   largura = if (tipo == "inseguranca") 14 else 12)
    titulo <- paste(if (tipo == "inseguranca") "Insegurança - dia e noite" else
      "Percepção de desordem social", "por", recortes_frequencia[[r]])
    catalogo[[tipo]] <- c(catalogo[[tipo]], paste0("<article><h3>", escape_html(titulo),
      "</h3><img loading='lazy' src='graficos/", subpasta, "/", nome, ".png' alt='",
      escape_html(titulo), "'><a href='graficos/", subpasta, "/", nome,
      ".png'>PNG</a> · <a href='graficos/", subpasta, "/", nome, ".pdf'>PDF</a></article>"))
  }
}
source("R/matriz_covariancia.R", local = TRUE)
covariancias <- calcular_covariancias(base_analise, pasta)
source("R/relatorio_interativo.R", local = TRUE)
salvar_csv(cortes_renda, file.path(pasta, "cortes_quartis_renda.csv"))
salvar_csv(select(base_analise, ano_rodada, id_linha, renda_trabalho_principal,
  renda_adicional_casa, renda_outras_fontes, renda_individual, quartil_renda,
  conflito_resposta_2002, fonte_renda), file.path(pasta, "renda_entrevistados.csv"))
gerar_relatorio(prevalencias, covariancias, cortes_renda, elegibilidade, pasta)
saveRDS(list(frequencias = frequencias, prevalencias = prevalencias, testes = testes,
  ausencias = ausencias, elegibilidade = elegibilidade, disponibilidade = disponibilidade,
  composicao = composicao, mapa_raca = mapa_raca, cortes_renda = cortes_renda, config = config),
  file.path(pasta, "resultados_frequencias.rds"))
saveRDS(base_analise, file.path(pasta, "base_frequencias.rds"))
capture.output(sessionInfo(), file = file.path(pasta, "sessionInfo.txt"))
message("Revisão concluída: oito gráficos e matrizes em resultados/frequencias/indice_graficos.html.")
