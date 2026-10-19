# Funções da revisão das frequências. Não modifica as variáveis dos modelos.
source("R/funcoes_analise.R", local = TRUE)

recortes_frequencia <- c(sexo = "Sexo", raca_grupo = "Raça", idade_grupo = "Grupo etário", quartil_renda = "Quartil de renda")
cores_frequencia <- list(
  sexo = c(Homem = "#2980b9", Mulher = "#c0392b"),
  raca_grupo = c(Brancos = "#6c3483", Negros = "#148f77"),
  idade_grupo = c(`18–34` = "#2980b9", `35–59` = "#b9770e", `60+` = "#6c3483"),
  quartil_renda = c(Q1 = "#6c3483", Q2 = "#148f77", Q3 = "#b9770e", Q4 = "#495867")
)

preparar_frequencias <- function(d) {
  # Agrupamento operacional solicitado: somente estes cinco rótulos entram.
  # A opção original "Negro(a)" e todas as outras opções NÃO listadas são NA.
  raca <- as.character(d$raca_cor)
  d$raca_grupo <- factor(case_when(
    raca %in% c("Indígena", "Preto(a)", "Pardo(a)") ~ "Negros",
    raca %in% c("Amarelo(a)", "Branco(a)") ~ "Brancos",
    TRUE ~ NA_character_), levels = c("Brancos", "Negros"))
  d$idade_grupo <- cut(d$idade, breaks = c(18, 35, 60, Inf), right = FALSE,
                       labels = c("18–34", "35–59", "60+"))
  d
}

teste_grupos <- function(des, resposta, recorte) {
  d <- des$variables
  ok <- !is.na(d[[resposta]]) & !is.na(d[[recorte]])
  des <- des[ok, ]
  des$variables$grupo_teste <- droplevels(des$variables[[recorte]])
  des$variables$resposta_teste <- factor(des$variables[[resposta]])
  n <- table(des$variables$grupo_teste)
  esperado <- nlevels(d[[recorte]])
  p <- F <- NA_real_
  status <- "OK"
  if (length(n) != esperado || any(n < config$minimo_n_sexo_teste)) {
    status <- "Menos de 5 respostas válidas em pelo menos um grupo"
  } else if (nlevels(des$variables$resposta_teste) < 2L) {
    status <- "Sem variação na resposta"
  } else if (survey::degf(des) < 2L) {
    status <- "Poucas UPAs para o teste"
  } else {
    t <- tryCatch(suppressWarnings(survey::svychisq(~resposta_teste + grupo_teste,
      design = des, statistic = "F")), error = function(e) e)
    if (inherits(t, "error")) status <- conditionMessage(t) else {
      p <- as.numeric(t$p.value)
      F <- as.numeric(t$statistic)
      if (!is.finite(p)) {p <- NA_real_; status <- "Teste não estimável"}
    }
  }
  tibble(p_valor = p, estatistica_F = F, n_grupos = length(n),
    n_grupo_1 = if (length(n) >= 1L) as.integer(n[1]) else 0L,
    n_grupo_2 = if (length(n) >= 2L) as.integer(n[2]) else 0L,
    n_grupo_3 = if (length(n) >= 3L) as.integer(n[3]) else NA_integer_,
    upas_dominio = nlevels(droplevels(des$variables$upa)),
    metodo = "Rao–Scott de segunda ordem (F)",
    interpretacao = if (recorte == "idade_grupo") "Global entre os três grupos etários" else if (recorte == "quartil_renda") "Global entre os quatro quartis" else
      "Comparação entre os dois grupos", status_teste = status)
}

preservar_testes_sexo <- function(testes) {
  # Mantém as marcações dos anexos: o BH original da segurança incluía os
  # recortes sociodemográficos da primeira versão, além do recorte por sexo.
  # Usa a referência apenas se p nominal e contagens continuam correspondendo.
  caminho <- "dados/tratados/referencia_testes_sexo_v1.csv"
  testes$familia_BH <- "Rodada × bloco × recorte (versão atual)"
  if (!file.exists(caminho)) {
    message("Referência de sexo ausente: utilizando ajuste BH da versão atual.")
    return(testes)
  }
  ref <- readr::read_csv2(caminho, show_col_types = FALSE, progress = FALSE)
  ref <- filter(ref, recorte == "sexo", bloco != "Distribuição original de segurança")
  i <- which(testes$recorte == "sexo")
  chave <- function(x) paste(x$ano_rodada, x$variavel, sep = "|")
  j <- match(chave(testes[i, ]), chave(ref))
  correspondem <- !anyNA(j) && all(abs(testes$p_valor[i] - ref$p_valor[j]) < 1e-10) &&
    all(testes$n_grupo_1[i] == ref$n_homem[j]) &&
    all(testes$n_grupo_2[i] == ref$n_mulher[j])
  if (isTRUE(correspondem)) {
    for (coluna in c("p_ajustado_BH", "significativo_nominal", "significativo_BH", "marca")) {
      testes[[coluna]][i] <- ref[[coluna]][j]
    }
    testes$marca[is.na(testes$marca)] <- ""
    testes$familia_BH[i] <- "Rodada × bloco da primeira versão (sexo preservado)"
  } else {
    message("Dados/testes de sexo mudaram: utilizando o ajuste BH da versão atual.")
  }
  testes
}

plotar_frequencias_revisadas <- function(d, t, recorte, tipo) {
  cores <- cores_frequencia[[recorte]]
  d$grupo <- factor(d$grupo, levels = names(cores))
  painel <- tipo == "inseguranca"
  if (painel) {
    d$categoria <- factor("Todos", levels = "Todos")
    t$categoria <- factor("Todos", levels = "Todos")
    d$periodo_plot <- factor(d$periodo, levels = c("dia", "noite"),
                            labels = c("De dia", "À noite"))
    t$periodo_plot <- factor(t$periodo, levels = c("dia", "noite"),
                            labels = c("De dia", "À noite"))
    titulo <- paste("Insegurança ao andar na rua -", recortes_frequencia[[recorte]])
    subtitulo <- "Inseguro(a) + muito inseguro(a); de dia e à noite, por rodada."
  } else {
    niveis <- rev(unname(rotulos_desordem))
    d$categoria <- factor(d$item, levels = niveis)
    t$categoria <- factor(t$item, levels = niveis)
    titulo <- paste("Percepção de desordem social -", recortes_frequencia[[recorte]])
    subtitulo <- "Percentual de respostas 'Sim'. Prostituição foi perguntada apenas em 2008."
  }
  if (recorte != "sexo") subtitulo <- paste0(subtitulo, " Sem divisão por sexo.")
  dodge <- position_dodge(width = .60)
  familia <- if (recorte == "sexo")
    "Sexo: ajuste BH da família original, quando a referência continua válida." else
    "Ajuste BH por rodada, bloco e recorte."
  comparacao <- if (recorte == "idade_grupo")
    "Marcas: teste global entre 18–34, 35–59 e 60+; não são comparações par a par." else if (recorte == "quartil_renda")
    "Marcas: teste global entre Q1-Q4. Quartis por rodada; Q1 = menor renda. Sem comparações par a par." else
    "Marcas: comparação entre os dois grupos."
  p <- ggplot(d, aes(x = proporcao, y = categoria, color = grupo, group = grupo)) +
    geom_errorbar(aes(xmin = ic_inferior, xmax = ic_superior), orientation = "y",
                  width = .16, position = dodge, na.rm = TRUE) +
    geom_point(aes(alpha = n_validos >= config$minimo_n_sexo_teste),
               size = 3, position = dodge, na.rm = TRUE) +
    geom_text(data = t, aes(x = 1.04, y = categoria, label = marca),
              inherit.aes = FALSE, color = "#111827", fontface = "bold", size = 5) +
    scale_color_manual(values = cores, drop = FALSE) +
    scale_alpha_manual(values = c(`FALSE` = .35, `TRUE` = 1), guide = "none") +
    scale_x_continuous(labels = function(x) paste0(round(x * 100), "%"),
                       breaks = seq(0, 1, .25), limits = c(0, 1.10)) +
    scale_y_discrete(labels = envolver, drop = FALSE) +
    labs(x = "Percentual ponderado entre respostas válidas", y = NULL,
      color = recortes_frequencia[[recorte]], title = titulo, subtitle = subtitulo,
      caption = paste0("Pontos: percentuais; barras: IC95%. * p ajustado BH < 0,05; † apenas p nominal < 0,05; ? teste indisponível.\n",
        familia, " ", comparacao, "\nPontos claros: n < 5. IC ausente: 0/1 ou não estimável.")) +
    tema_grafico()
  if (painel) {
    p <- p + facet_grid(periodo_plot ~ ano_rodada, switch = "y") +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        strip.placement = "outside", strip.text.y.left = element_text(angle = 0),
        panel.spacing.y = grid::unit(.7, "lines"))
  } else p <- p + facet_wrap(~ano_rodada, nrow = 1)
  p
}
