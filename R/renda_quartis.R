# Renda individual registrada nos questionários. A base de regressão não muda.
limpar_valor_renda <- function(x) {
  v <- suppressWarnings(as.numeric(haven::zap_labels(x)))
  # Cinco e seis dígitos aparecem nos dicionários e questionários de 2002.
  codigos <- c(66666, 77777, 88888, 99999, 666666, 777777, 888888, 999999)
  labels <- attr(x, "labels", exact = TRUE)
  if (length(labels)) codigos <- unique(c(codigos,
    unname(labels[trimws(toupper(names(labels))) %in% c("SI", "NR", "NA", "NS")])))
  v[v %in% codigos | !is.finite(v) | v < 0] <- NA_real_
  v
}

zero_por_salto <- function(valor, sem_fonte) {
  # Zero somente quando a ausência da fonte foi declarada no formulário.
  valor[is.na(valor) & !is.na(sem_fonte) & sem_fonte] <- 0
  valor
}

anexar_renda <- function(base) {
  if (!requireNamespace("haven", quietly = TRUE)) stop("Instale o pacote haven.")
  fontes <- list()
  for (ano in sort(unique(base$ano_rodada))) {
    bruto <- haven::read_sav(file.path("dados", "brutos", paste0("PRMBH_", ano, ".sav")))
    codigo <- function(nome) as.numeric(haven::zap_labels(bruto[[nome]]))
    conflito <- rep(FALSE, nrow(bruto))
    if (ano == 2002) {
      a <- limpar_valor_renda(bruto$T9A)
      b <- limpar_valor_renda(bruto$T9B)
      conflito <- !is.na(a) & !is.na(b) & a != b
      principal <- coalesce(a, b)
      principal[conflito] <- NA_real_
      trabalho <- c("T2.A", "T2.B", "T2.C", "T2.D1", "T2.D2", "T2.D3", "T2.D4", "T2.E", "T2.J")
      ocupado <- vapply(trabalho, codigo, numeric(nrow(bruto)))
      sem_principal <- apply(ocupado, 1, function(z) all(!is.na(z) & z == 2))
      principal <- zero_por_salto(principal, sem_principal & !conflito)
      principal[conflito] <- NA_real_
      nomes_outras <- paste0("T20.", LETTERS[1:6])
      perguntas <- vapply(nomes_outras, codigo, numeric(nrow(bruto)))
      outras <- limpar_valor_renda(bruto$T21)
      outras <- zero_por_salto(outras, apply(perguntas, 1, function(z) all(!is.na(z) & z == 2)))
      # T14 especifica um SEGUNDO trabalho quando o principal é em casa.
      adicional <- zero_por_salto(limpar_valor_renda(bruto$T16), codigo("T14") == 2)
      fonte <- "T9A/T9B + T16 (segundo trabalho em casa) + T21"
    } else {
      principal <- limpar_valor_renda(bruto$T10)
      ocupacao <- codigo("T1A")
      # O salto exclui ocupações principais remuneradas; estágio não é zero.
      nao_remunerado <- codigo(if (ano == 2005) "T3A" else "T3") == 6
      principal <- zero_por_salto(principal, ocupacao %in% 1:4 | nao_remunerado)
      nomes_outras <- if (ano == 2005) paste0("T23", LETTERS[1:5]) else paste0("T15", LETTERS[1:5])
      perguntas <- vapply(nomes_outras, codigo, numeric(nrow(bruto)))
      nome_valor <- if (ano == 2005) "T24" else "T16"
      outras <- zero_por_salto(limpar_valor_renda(bruto[[nome_valor]]),
        apply(perguntas, 1, function(z) all(!is.na(z) & z == 2)))
      adicional <- rep(0, nrow(bruto)) # não há componente separado nessas rodadas
      fonte <- paste0("T10 + ", nome_valor)
    }
    total <- principal + adicional + outras # componente desconhecido mantém NA
    fontes[[as.character(ano)]] <- tibble(ano_rodada = ano, id_linha = seq_len(nrow(bruto)),
      renda_trabalho_principal = principal, renda_adicional_casa = adicional,
      renda_outras_fontes = outras, renda_individual = total,
      conflito_resposta_2002 = conflito, fonte_renda = fonte)
  }
  renda <- bind_rows(fontes)
  if (anyDuplicated(renda[c("ano_rodada", "id_linha")])) stop("Renda: chave duplicada.")
  anti <- anti_join(base, renda, by = c("ano_rodada", "id_linha"))
  if (nrow(anti)) stop("Entrevistas sem correspondência no arquivo bruto.")
  left_join(base, renda, by = c("ano_rodada", "id_linha"))
}

classificar_quartis <- function(base) {
  base$quartil_renda <- factor(NA_character_, levels = paste0("Q", 1:4))
  cortes <- list()
  for (ano in sort(unique(base$ano_rodada))) {
    des <- criar_desenho(filter(base, ano_rodada == ano))
    valido <- !is.na(des$variables$renda_individual)
    if (sum(valido) < 4L) stop("Poucas rendas válidas na rodada ", ano)
    dv <- des[valido, ]
    q <- as.numeric(coef(survey::svyquantile(~renda_individual, dv,
      quantiles = c(.25, .50, .75), ci = FALSE, qrule = "math")))
    if (any(!is.finite(q))) stop("Quartis não estimáveis na rodada ", ano)
    # Mesma renda sempre no mesmo grupo, sem desempatar pelo número da linha.
    # Empates podem produzir grupos desiguais ou vazios.
    idx <- which(base$ano_rodada == ano & !is.na(base$renda_individual))
    cod <- 1L + as.integer(base$renda_individual[idx] > q[1]) +
      as.integer(base$renda_individual[idx] > q[2]) + as.integer(base$renda_individual[idx] > q[3])
    base$quartil_renda[idx] <- paste0("Q", cod)
    cortes[[as.character(ano)]] <- tibble(ano_rodada = ano, n_rodada = sum(base$ano_rodada == ano),
      n_renda_valida = sum(valido), n_renda_ausente = sum(!valido),
      corte_25_reais = q[1], corte_50_reais = q[2], corte_75_reais = q[3],
      metodo = "Quantis ponderados: svyquantile, qrule=math; empates preservados")
  }
  list(base = base, cortes = bind_rows(cortes))
}
