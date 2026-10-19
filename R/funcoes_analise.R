#===============================================================================
# FUNÇÕES COMPARTILHADAS - BH SURVEY
# Os scripts principais devem ser executados a partir da raiz do projeto.
#===============================================================================
pacotes <- c("dplyr", "readr", "survey", "ggplot2")
ausentes <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(ausentes)) {
  stop("Instale os pacotes antes de executar: install.packages(c(",
       paste(sprintf('"%s"', ausentes), collapse = ", "), "))", call. = FALSE)
}
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

# O município não é tratado como estrato sem documentação do plano original.
# Informe nomes de colunas do plano oficial aqui, se estiverem na base tratada.
config <- list(
  peso = "peso_pos_estratificacao",
  estratos = character(0),
  confianca = 0.95,
  alpha = 0.05,
  minimo_n_sexo_teste = 5L,
  cor_sexo = c(Homem = "#2980b9", Mulher = "#c0392b")
)

niveis_seguranca <- c("Muito seguro(a)", "Seguro(a)",
  "Nem seguro(a) nem inseguro(a)", "Inseguro(a)", "Muito inseguro(a)")
niveis_escolaridade <- c("Alfabetização de adultos", "Educação de jovens e adultos",
  "Ensino fundamental", "Ensino médio", "Curso técnico", "Pré-vestibular",
  "Curso sequencial", "Ensino superior de tecnologia", "Normal superior",
  "Ensino superior", "Especialização", "Mestrado ou doutorado")
niveis_raca <- c("Branco(a)", "Preto(a)", "Pardo(a)", "Amarelo(a)", "Indígena",
  "Negro(a)", "Moreno(a)", "Pálido(a)", "Claro(a)",
  "Moreno(a) claro", "Moreno(a) escuro", "Outra")
niveis_civil <- c("Casado(a)", "Solteiro(a)", "União consensual",
  "Separado(a) / divorciado(a)", "Viúvo(a)")
rotulos_desordem <- c(
  desordem_barulho_ocorrencia = "Barulho",
  desordem_pichacoes_ocorrencia = "Pichações",
  desordem_pessoas_em_situacao_de_rua_ocorrencia = "Pessoas em situação de rua",
  desordem_arruaca_ocorrencia = "Arruaça",
  desordem_vandalismo_ocorrencia = "Vandalismo",
  desordem_gangues_ocorrencia = "Gangues",
  desordem_trafico_drogas_ocorrencia = "Tráfico de drogas",
  desordem_prostituicao_ocorrencia = "Prostituição"
)

fator_validado <- function(x, niveis, nome) {
  extras <- setdiff(unique(as.character(x[!is.na(x)])), niveis)
  if (length(extras)) stop("Categorias não previstas em ", nome, ": ",
                            paste(extras, collapse = ", "))
  # Fator nominal: levels fixam a apresentação, sem contrastes polinomiais.
  factor(x, levels = niveis, ordered = FALSE)
}

carregar_base <- function() {
  arquivo <- file.path("dados", "tratados", "bh_svy_2002_2005_2008.rds")
  if (!file.exists(arquivo)) stop("Execute primeiro source('R/preparar_dados.R').")
  d <- readRDS(arquivo)
  obrigatorias <- c("ano_rodada", "id_linha", "sexo", "municipio", "idade",
    "raca_cor", "escolaridade_grau_frequentado", "estado_civil", "andar_dia",
    "andar_noite", "setor_censitario", config$peso, config$estratos,
    names(rotulos_desordem))
  faltam <- setdiff(obrigatorias, names(d))
  if (length(faltam)) stop("Colunas ausentes: ", paste(faltam, collapse = ", "))
  if (anyDuplicated(d[c("ano_rodada", "id_linha")])) stop("Identificadores duplicados.")
  if (any(!is.na(d$idade) & (d$idade < 18 | d$idade >= 110))) {
    stop("Idade fora do intervalo adulto previsto [18,110). Revise a base.")
  }
  d$sexo <- fator_validado(d$sexo, c("Homem", "Mulher"), "sexo")
  d$municipio_grupo <- factor(ifelse(is.na(d$municipio), NA_character_,
    ifelse(d$municipio %in% c("Belo Horizonte", "Contagem", "Betim"),
           d$municipio, "Outros")),
    levels = c("Belo Horizonte", "Contagem", "Betim", "Outros"))
  d$grupo_etario <- cut(d$idade, breaks = c(18, 20, seq(30, 110, 10)),
    right = FALSE, labels = c("18–19", paste0(seq(20, 100, 10), "–", seq(29, 109, 10))))
  d$raca_cor <- fator_validado(d$raca_cor, niveis_raca, "raça/cor")
  d$escolaridade <- fator_validado(d$escolaridade_grau_frequentado,
                                 niveis_escolaridade, "escolaridade")
  d$estado_civil <- fator_validado(d$estado_civil, niveis_civil, "estado civil")
  for (periodo in c("dia", "noite")) {
    coluna <- paste0("andar_", periodo)
    d[[coluna]] <- fator_validado(d[[coluna]], niveis_seguranca, coluna)
    x <- as.character(d[[coluna]])
    # 1 = inseguro/muito inseguro; 0 = todas as outras respostas válidas.
    d[[paste0("inseguranca_", periodo)]] <- ifelse(is.na(x), NA_integer_,
      as.integer(x %in% c("Inseguro(a)", "Muito inseguro(a)")))
    # Sensibilidade: retira a resposta neutra, mantendo seguros versus inseguros.
    d[[paste0("inseguranca_", periodo, "_sem_neutros")]] <- ifelse(
      !is.na(x) & x == "Nem seguro(a) nem inseguro(a)", NA_integer_,
      d[[paste0("inseguranca_", periodo)]])
  }
  for (v in names(rotulos_desordem)) {
    d[[v]] <- fator_validado(d[[v]], c("Não", "Sim"), v)
  }
  # Escolaridade mede grau frequentado, e não conclusão ou anos de estudo.
  # Agrupamentos explícitos para evitar dezenas de parâmetros em células raras.
  e <- as.character(d$escolaridade)
  d$escolaridade_modelo <- factor(case_when(
    e %in% c("Alfabetização de adultos", "Educação de jovens e adultos",
              "Ensino fundamental") ~ "Fundamental / alfabetização / EJA",
    e %in% c("Ensino médio", "Curso técnico", "Pré-vestibular") ~
      "Médio / técnico / pré-vestibular",
    e %in% c("Curso sequencial", "Ensino superior de tecnologia",
              "Normal superior", "Ensino superior") ~ "Superior / sequencial / normal",
    e %in% c("Especialização", "Mestrado ou doutorado") ~ "Pós-graduação",
    TRUE ~ NA_character_),
    levels = c("Fundamental / alfabetização / EJA", "Médio / técnico / pré-vestibular",
               "Superior / sequencial / normal", "Pós-graduação"))
  r <- as.character(d$raca_cor)
  principais <- c("Branco(a)", "Preto(a)", "Pardo(a)", "Amarelo(a)", "Indígena")
  d$raca_modelo <- factor(ifelse(is.na(r), NA_character_,
    ifelse(r %in% principais, r, "Outras respostas")),
    levels = c(principais, "Outras respostas"))
  # OR por dez anos; evita estimar separadamente os dois casos de 90–99 anos.
  d$idade_10 <- d$idade / 10
  d$peso_analise <- as.numeric(d[[config$peso]])
  d$upa <- interaction(d$ano_rodada, d$municipio, d$setor_censitario,
                       drop = TRUE, lex.order = TRUE)
  if (any(!is.finite(d$peso_analise) | d$peso_analise <= 0) || anyNA(d$upa)) {
    stop("Peso inválido ou município/setor ausente. Corrija a base antes da análise.")
  }
  if (length(config$estratos) && anyNA(d[config$estratos])) stop("Estrato ausente.")
  d
}

criar_desenho <- function(d) {
  # Desenho de trabalho: peso final + UPA por setor; sem FPC ou estratos presumidos.
  # Município apenas compõe o ID para evitar colisões de códigos de setor.
  args <- list(ids = ~upa, weights = ~peso_analise, data = d, nest = TRUE)
  if (length(config$estratos)) args$strata <- reformulate(config$estratos)
  do.call(survey::svydesign, args)
}

salvar_csv <- function(d, arquivo) {
  dir.create(dirname(arquivo), recursive = TRUE, showWarnings = FALSE)
  # BOM UTF-8 para os rótulos aparecerem corretamente no Excel do Windows.
  readr::write_excel_csv2(d, arquivo, na = "")
}

estimar_proporcao <- function(des, variavel, categoria) {
  x <- des$variables[[variavel]]
  n <- sum(!is.na(x))
  if (n == 0L) return(tibble(n_validos = 0L, n_categoria = 0L,
    soma_pesos_validos = NA_real_, soma_pesos_categoria = NA_real_,
    proporcao = NA_real_, erro_padrao = NA_real_, ic_inferior = NA_real_,
    ic_superior = NA_real_, status_ic = "Sem resposta válida"))
  des <- des[!is.na(x), ]
  des$variables$indicador <- as.numeric(as.character(des$variables[[variavel]]) == categoria)
  y <- des$variables$indicador
  w <- des$variables$peso_analise
  m <- survey::svymean(~indicador, des)
  p <- as.numeric(coef(m)[1])
  ep <- as.numeric(survey::SE(m)[1])
  # Detecta limites pelos dados, evitando que 1 - erro de arredondamento
  # entre no ajuste logit e produza um IC quase degenerado.
  limite <- all(y == 0) || all(y == 1)
  if (limite) {p <- y[1]; ep <- 0}
  ci <- c(NA_real_, NA_real_)
  status <- "Limite 0/1: IC não estimável pelo método logit"
  if (!limite && p > 0 && p < 1 && survey::degf(des) > 0) {
    estimativa <- tryCatch(survey::svyciprop(~indicador, des, method = "logit",
      level = config$confianca, df = survey::degf(des)), error = function(e) e)
    if (!inherits(estimativa, "error")) {
      ci <- as.numeric(confint(estimativa))
      status <- if (all(is.finite(ci))) "OK" else "IC não estimável"
      if (!all(is.finite(ci))) ci <- c(NA_real_, NA_real_)
    } else status <- conditionMessage(estimativa)
  }
  tibble(n_validos = n, n_categoria = sum(y), soma_pesos_validos = sum(w),
    soma_pesos_categoria = sum(w * y), proporcao = p, erro_padrao = ep,
    ic_inferior = ci[1], ic_superior = ci[2], status_ic = status)
}

teste_sexo <- function(des, variavel) {
  v <- des$variables
  ok <- !is.na(v$sexo) & !is.na(v[[variavel]])
  des <- des[ok, ]
  des$variables$sexo <- droplevels(des$variables$sexo)
  des$variables$resposta_teste <- factor(des$variables[[variavel]])
  cont <- table(des$variables$sexo)
  status <- "OK"
  p <- NA_real_
  estatistica <- NA_real_
  if (length(cont) != 2L || any(cont < config$minimo_n_sexo_teste)) {
    status <- "Menos de 5 respostas válidas em pelo menos um sexo"
  } else if (nlevels(des$variables$resposta_teste) < 2L) {
    status <- "Sem variação na resposta"
  } else if (survey::degf(des) < 2L) {
    status <- "Poucas UPAs para o teste"
  } else {
    t <- tryCatch(suppressWarnings(survey::svychisq(~resposta_teste + sexo,
      des, statistic = "F")), error = function(e) e)
    if (inherits(t, "error")) status <- conditionMessage(t) else {
      p <- as.numeric(t$p.value)
      estatistica <- as.numeric(t$statistic)
      if (!is.finite(p)) {p <- NA_real_; status <- "Teste não estimável"}
    }
  }
  tibble(p_valor = p, estatistica_F = estatistica,
    n_homem = sum(des$variables$sexo == "Homem"),
    n_mulher = sum(des$variables$sexo == "Mulher"),
    upas_dominio = nlevels(droplevels(des$variables$upa)),
    metodo = "Rao–Scott de segunda ordem (F)", status_teste = status)
}

ajustar_testes <- function(tabela, grupos) {
  tabela |>
    group_by(across(all_of(grupos))) |>
    mutate(p_ajustado_BH = p.adjust(p_valor, method = "BH"),
      significativo_nominal = !is.na(p_valor) & p_valor < config$alpha,
      significativo_BH = !is.na(p_ajustado_BH) & p_ajustado_BH < config$alpha,
      marca = case_when(significativo_BH ~ "*", significativo_nominal ~ "†",
                         is.na(p_valor) ~ "?", TRUE ~ "")) |>
    ungroup()
}

tema_grafico <- function() {
  theme_minimal(base_size = 12) + theme(
    panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
    legend.position = "bottom", plot.title = element_text(face = "bold", size = 15),
    strip.text = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0, size = 9),
    plot.margin = margin(12, 16, 12, 12))
}

envolver <- function(x, largura = 30) {
  vapply(as.character(x), function(y) paste(strwrap(y, width = largura), collapse = "\n"),
         character(1))
}

plotar_comparacao <- function(d, testes, niveis, titulo, subtitulo) {
  d$categoria <- factor(d$categoria, levels = rev(niveis))
  testes$categoria <- factor(testes$categoria, levels = rev(niveis))
  desvio <- position_dodge(width = 0.55)
  ggplot(d, aes(x = proporcao, y = categoria, color = sexo, group = sexo)) +
    geom_errorbar(aes(xmin = ic_inferior, xmax = ic_superior), orientation = "y",
                  width = 0.16, position = desvio, na.rm = TRUE) +
    geom_point(aes(alpha = n_validos >= config$minimo_n_sexo_teste),
               size = 2.8, position = desvio, na.rm = TRUE) +
    geom_text(data = testes, aes(x = 1.04, y = categoria, label = marca),
              inherit.aes = FALSE, color = "#111827", size = 5, fontface = "bold") +
    facet_wrap(~ano_rodada, nrow = 1, drop = FALSE) +
    scale_color_manual(values = config$cor_sexo) +
    scale_alpha_manual(values = c(`FALSE` = 0.35, `TRUE` = 1), guide = "none") +
    scale_x_continuous(labels = function(x) paste0(round(x * 100), "%"),
      breaks = seq(0, 1, .25), limits = c(0, 1.10)) +
    scale_y_discrete(labels = envolver) +
    labs(x = "Percentual ponderado entre respostas válidas", y = NULL, color = "Sexo",
      title = titulo, subtitle = subtitulo,
      caption = paste0("Pontos: percentuais; barras: IC ", config$confianca * 100,
        "%. * p ajustado BH < 0,05; † apenas p nominal < 0,05; ? teste indisponível.\n",
        "Ajuste BH por rodada e bloco. Pontos claros: n < 5. IC ausente: 0/1 ou não estimável.")) +
    tema_grafico()
}

salvar_grafico <- function(p, pasta, nome, altura = 6.5, largura = 14) {
  dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(pasta, paste0(nome, ".png")), p, width = largura,
         height = altura, dpi = 180, bg = "white")
  ggsave(file.path(pasta, paste0(nome, ".pdf")), p, width = largura,
         height = altura, device = grDevices::cairo_pdf, bg = "white")
}

escape_html <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

tabela_html <- function(d) {
  linhas <- apply(d, 1, function(x) paste0("<tr><td>",
    paste(escape_html(x), collapse = "</td><td>"), "</td></tr>"))
  paste0("<div class='rolagem'><table><thead><tr><th>",
    paste(escape_html(names(d)), collapse = "</th><th>"),
    "</th></tr></thead><tbody>", paste(linhas, collapse = "\n"),
    "</tbody></table></div>")
}

pagina_html <- function(titulo, conteudo, arquivo) {
  html <- paste0("<!doctype html><html lang='pt-BR'><meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width,initial-scale=1'><title>",
    escape_html(titulo), "</title><style>",
    "body{font-family:Arial,sans-serif;margin:36px auto;padding:0 24px;max-width:1500px;",
    "color:#172b40;background:#f5f7fa;line-height:1.6}h1,h2{line-height:1.25}",
    "article{background:white;padding:20px;margin:20px 0;border-radius:10px;}",
    "img{width:100%;height:auto}a{color:#195e98}table{border-collapse:collapse;",
    "background:white;white-space:nowrap}th,td{padding:8px 12px;text-align:left;",
    "border-bottom:1px solid #dde3ea}th{background:#e8eef5}.rolagem{overflow:auto}",
    ".sig{font-weight:bold;color:#9a3412}nav a{margin-right:20px}</style><body>",
    "<h1>", escape_html(titulo), "</h1><p>Gerado em ",
    format(Sys.time(), "%d/%m/%Y %H:%M:%S %Z"), "</p>",
    paste(conteudo, collapse = "\n"), "</body></html>")
  writeLines(html, arquivo, useBytes = TRUE)
}
