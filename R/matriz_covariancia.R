# Matriz descritiva entre os desfechos e indicadores das categorias.
# Codificação nominal: uma referência por variável; não atribui ordens artificiais.
calcular_covariancias <- function(base, pasta) {
  referencias <- c(sexo = "Homem", idade_grupo = "18–34", quartil_renda = "Q1",
                   raca_grupo = "Brancos")
  cod_desordem <- c("D_BARULHO", "D_PICHACOES", "D_RUA", "D_ARRUACA",
    "D_VANDALISMO", "D_GANGUES", "D_TRAFICO", "D_PROSTITUICAO")
  vars_desordem <- names(rotulos_desordem)
  codigos <- c("I_DIA", "I_NOITE", "MULHER", "IDADE35_59", "IDADE60", "RENDA_Q2",
    "RENDA_Q3", "RENDA_Q4", "NEGROS", cod_desordem)
  descricoes <- c("Insegurança de dia", "Insegurança à noite", "Mulher", "Idade 35-59",
    "Idade 60+", "Renda Q2", "Renda Q3", "Renda Q4", "Negros", unname(rotulos_desordem))
  dicionario <- tibble(Codigo = codigos, Indicador_1 = descricoes,
    Referencia = c("Demais respostas válidas", "Demais respostas válidas", "Homem",
      "18-34", "18-34", "Q1", "Q1", "Q1", "Brancos", rep("Não", 8)),
    Variavel_fonte = c("inseguranca_dia", "inseguranca_noite", "sexo",
      rep("idade_grupo", 2), rep("quartil_renda", 3), "raca_grupo", vars_desordem))
  pasta_cov <- file.path(pasta, "covariancia")
  dir.create(pasta_cov, recursive = TRUE, showWarnings = FALSE)
  salvar_csv(dicionario, file.path(pasta_cov, "codificacao_matriz.csv"))
  saidas <- list()
  amostras <- disponibilidade <- list()
  for (ano in sort(unique(base$ano_rodada))) {
    d <- filter(base, ano_rodada == ano)
    aplicado <- ano %in% c(2005, 2008) &
      (vars_desordem != "desordem_prostituicao_ocorrencia" | ano == 2008)
    cod_ativos <- c(setdiff(codigos, cod_desordem), cod_desordem[aplicado])
    vars <- c("inseguranca_dia", "inseguranca_noite", names(referencias),
      vars_desordem[aplicado])
    disp <- tibble(ano_rodada = ano, Codigo = codigos,
      aplicado = c(rep(TRUE, 9), aplicado)) |>
      mutate(Status = ifelse(aplicado, "Aplicado", "Não aplicado na rodada"))
    disponibilidade[[as.character(ano)]] <- disp
    des <- criar_desenho(d)
    ok <- complete.cases(d[vars])
    dm <- des[ok, ]
    x <- dm$variables
    if (nrow(x) < 3L) stop("Covariância: poucos casos completos em ", ano)
    indicadores <- data.frame(I_DIA = x$inseguranca_dia, I_NOITE = x$inseguranca_noite,
      MULHER = as.integer(x$sexo == "Mulher"),
      IDADE35_59 = as.integer(x$idade_grupo == "35–59"), IDADE60 = as.integer(x$idade_grupo == "60+"),
      RENDA_Q2 = as.integer(x$quartil_renda == "Q2"), RENDA_Q3 = as.integer(x$quartil_renda == "Q3"),
      RENDA_Q4 = as.integer(x$quartil_renda == "Q4"), NEGROS = as.integer(x$raca_grupo == "Negros"))
    for (i in which(aplicado)) indicadores[[cod_desordem[i]]] <-
      as.integer(x[[vars_desordem[i]]] == "Sim")
    dm$variables[cod_ativos] <- indicadores
    formula <- reformulate(cod_ativos)
    environment(formula) <- baseenv()
    estimativa <- survey::svyvar(formula, dm)
    # Layout comum: itens não aplicados são NA, não zero, e não excluem casos.
    m <- ep <- matrix(NA_real_, length(codigos), length(codigos),
      dimnames = list(codigos, codigos))
    m[cod_ativos, cod_ativos] <- matrix(as.numeric(estimativa), length(cod_ativos))
    ep[cod_ativos, cod_ativos] <- matrix(sqrt(pmax(0, diag(attr(estimativa, "var")))),
      length(cod_ativos))
    tabela <- bind_cols(tibble(Variavel = rownames(m)), as_tibble(m, .name_repair = "minimal"))
    salvar_csv(tabela, file.path(pasta_cov, paste0("matriz_covariancia_", ano, ".csv")))
    longo <- as_tibble(as.table(m), .name_repair = "minimal")
    names(longo) <- c("Variavel_1", "Variavel_2", "Covariancia")
    longo$Erro_padrao <- as.numeric(ep)
    longo$ano_rodada <- ano
    longo$Status <- ifelse(is.na(longo$Covariancia), "Não aplicado na rodada", "Estimado")
    limite <- max(abs(m), na.rm = TRUE)
    nota <- if (ano == 2002) "Ocorrência de desordem não aplicada em 2002 (células cinza)." else
      if (ano == 2005) "Prostituição não aplicada em 2005 (células cinza)." else
      "Todos os oito itens de ocorrência foram aplicados."
    p <- ggplot(longo, aes(x = factor(Variavel_2, levels = codigos),
      y = factor(Variavel_1, levels = rev(codigos)), fill = Covariancia)) +
      geom_tile(color = "white", linewidth = .3) +
      scale_fill_gradient2(low = "#6c3483", mid = "#ffffff", high = "#148f77",
                           midpoint = 0, limits = c(-limite, limite), na.value = "#d9dde1") +
      coord_fixed() + labs(title = paste("Matriz de covariância -", ano),
        subtitle = paste("Insegurança, dados sociodemográficos e desordem; n =", nrow(x), "casos completos."),
        x = NULL, y = NULL, fill = "Covariância",
        caption = paste(nota, "\nIndicadores 0/1; pesos amostrais. Diagonal = variância. Codificação no relatório.")) +
      tema_grafico() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
                             axis.text.y = element_text(size = 9), legend.position = "right")
    salvar_grafico(p, pasta_cov, paste0("matriz_covariancia_", ano), altura = 11, largura = 13)
    saidas[[as.character(ano)]] <- list(matriz = m, erro_padrao = ep, tabela = tabela,
      longo = longo, indicadores = indicadores, pesos = x$peso_analise,
      ids_incluidos = x$id_linha, codigos_aplicados = cod_ativos, nota = nota)
    amostras[[as.character(ano)]] <- tibble(ano_rodada = ano, n_rodada = nrow(d),
      n_casos_completos = nrow(x), n_excluidos = sum(!ok),
      n_itens_desordem_aplicados = sum(aplicado),
      criterio = "Ambos os desfechos, sexo, idade, renda, raça e todos os itens de ocorrência aplicados na rodada")
  }
  amostras <- bind_rows(amostras)
  disponibilidade <- bind_rows(disponibilidade)
  salvar_csv(disponibilidade, file.path(pasta_cov, "disponibilidade_indicadores.csv"))
  salvar_csv(amostras, file.path(pasta_cov, "amostras_matriz.csv"))
  salvar_csv(bind_rows(lapply(saidas, function(x) x$longo)), file.path(pasta_cov, "covariancias_formato_longo.csv"))
  saveRDS(list(matrizes = saidas, dicionario = dicionario, amostras = amostras,
              disponibilidade = disponibilidade),
          file.path(pasta_cov, "matrizes_covariancia.rds"))
  list(matrizes = saidas, dicionario = dicionario, amostras = amostras,
       disponibilidade = disponibilidade)
}
