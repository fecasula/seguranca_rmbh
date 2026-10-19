# Validação das saídas e de escolhas que afetam os resultados estatísticos.
# Execute após as análises, na raiz: source("validacao/validar_resultados.R")
source("R/funcoes_analise.R", local = TRUE)
base <- carregar_base()
f <- readRDS("resultados/frequencias/resultados_frequencias.rds")
r <- readRDS("resultados/regressoes/modelos_logisticos.rds")
validacoes <- character()
verificar <- function(condicao, descricao) {
  if (!isTRUE(condicao)) stop("FALHOU: ", descricao)
  validacoes <<- c(validacoes, paste0("OK — ", descricao))
}

# Comparação independente com a fórmula do estimador ponderado e os n brutos.
for (ano in sort(unique(base$ano_rodada))) {
  for (sexo in c("Homem", "Mulher")) {
    d <- base[base$ano_rodada == ano & base$sexo == sexo, ]
    for (periodo in c("dia", "noite")) {
      v <- paste0("inseguranca_", periodo)
      ok <- !is.na(d[[v]])
      p <- sum(d$peso_analise[ok] * d[[v]][ok]) / sum(d$peso_analise[ok])
      linha <- f$prevalencias[f$prevalencias$ano_rodada == ano &
        f$prevalencias$sexo == sexo & f$prevalencias$variavel == v &
        f$prevalencias$recorte == "sexo", ]
      verificar(nrow(linha) == 1L && abs(linha$proporcao - p) < 1e-10 &&
        linha$n_validos == sum(ok) && linha$n_categoria == sum(d[[v]][ok]),
        paste("Percentual e denominador:", ano, sexo, periodo))
    }
  }
}

somatorios <- f$frequencias |>
  group_by(ano_rodada, bloco, variavel, periodo, recorte, categoria, sexo) |>
  summarise(soma = sum(proporcao), n = first(n_validos), .groups = "drop")
verificar(all(abs(somatorios$soma[somatorios$n > 0] - 1) < 1e-10),
          "Categorias válidas somam 100% dentro de cada domínio")
verificar(all(is.na(f$frequencias$proporcao[f$frequencias$n_validos == 0])),
          "Ausência de respostas não foi convertida em zero")
ci <- f$frequencias
verificar(all(is.na(ci$ic_inferior) == is.na(ci$ic_superior)),
          "IC não estimável não conserva apenas um limite")
ok <- ci$status_ic == "OK"
verificar(all(ci$ic_inferior[ok] >= 0 & ci$ic_superior[ok] <= 1 &
  ci$ic_inferior[ok] <= ci$proporcao[ok] & ci$ic_superior[ok] >= ci$proporcao[ok]),
  "IC válidos contêm a estimativa e ficam entre 0 e 1")

# Verifica ajuste por famílias e tratamento de p indisponível.
familias <- split(f$testes, interaction(f$testes$ano_rodada, f$testes$bloco, drop = TRUE))
for (t in familias) {
  ok <- !is.na(t$p_valor)
  esperado <- p.adjust(t$p_valor[ok], method = "BH")
  verificar(isTRUE(all.equal(t$p_ajustado_BH[ok], esperado)) &&
    all(!t$significativo_BH[!ok]),
    paste("BH por família:", t$ano_rodada[1], t$bloco[1]))
}
verificar(!any(f$prevalencias$ano_rodada == 2002 &
                f$prevalencias$bloco == "Ocorrência de desordem social"),
          "Gravidade de 2002 não foi convertida em ocorrência")
verificar(!any(f$prevalencias$ano_rodada == 2005 &
                f$prevalencias$variavel == "desordem_prostituicao_ocorrencia"),
          "Prostituição não foi estimada para 2005")
verificar(is.factor(base$escolaridade) && !is.ordered(base$escolaridade) &&
            identical(levels(base$escolaridade), niveis_escolaridade),
          "Escolaridade nominal mantém levels explícitos")
verificar(all(is.na(base$inseguranca_dia[is.na(base$andar_dia)])),
          "Segurança ausente não recebe zero no desfecho")
neutro <- !is.na(base$andar_dia) & base$andar_dia == "Nem seguro(a) nem inseguro(a)"
verificar(all(base$inseguranca_dia[neutro] == 0) &&
            all(is.na(base$inseguranca_dia_sem_neutros[neutro])),
          "Definições principal e de sensibilidade preservam neutralidade")

# Invariância ao reescalonamento de todos os pesos dentro de uma rodada.
d <- base[base$ano_rodada == 2005, ]
des1 <- criar_desenho(d)
d$peso_analise <- d$peso_analise * 100
des2 <- criar_desenho(d)
z1 <- estimar_proporcao(des1, "inseguranca_dia", "1")
z2 <- estimar_proporcao(des2, "inseguranca_dia", "1")
verificar(abs(z1$proporcao - z2$proporcao) < 1e-10 &&
            abs(z1$erro_padrao - z2$erro_padrao) < 1e-10,
          "Reescalonar pesos não altera a proporção ou seu erro padrão")

verificar(length(r$modelos) == 10L && sum(r$diagnosticos$especificacao == "principal") == 6L,
          "Seis modelos principais e quatro sensibilidades foram estimados")
for (nome in names(r$modelos)) {
  m <- r$modelos[[nome]]
  ano <- as.integer(substr(nome, 1, 4))
  verificar(all(m$survey.design$variables$ano_rodada == ano),
            paste("Modelo contém apenas sua rodada:", nome))
  z <- r$coeficientes[r$coeficientes$ano_rodada == ano &
    r$coeficientes$periodo == ifelse(grepl("_noite_", nome), "noite", "dia") &
    r$coeficientes$especificacao == ifelse(grepl("sem_neutros", nome), "sem_neutros", "principal") &
    !r$coeficientes$referencia, ]
  z <- z[match(names(coef(m)), z$termo), ]
  verificar(max(abs(z$OR - exp(coef(m)))) < 1e-10,
            paste("OR corresponde a exp(beta):", nome))
  resumo <- summary(m)$coefficients
  verificar(max(abs(z$p_valor - resumo[, ncol(resumo)])) < 1e-10,
            paste("p e IC usam os graus de liberdade do modelo:", nome))
  cimo <- exp(confint(m, level = config$confianca))
  verificar(max(abs(z$ic_inferior - cimo[, 1])) < 1e-8 &&
            max(abs(z$ic_superior - cimo[, 2])) < 1e-8,
            paste("IC corresponde a confint(svyglm):", nome))
}
verificar(all(r$diagnosticos$convergiu) && all(r$diagnosticos$status == "OK"),
          "Modelos convergiram sem alerta de instabilidade")
graficos <- list.files("resultados/frequencias/graficos", recursive = TRUE, pattern = "[.]png$")
pdfs <- list.files("resultados/frequencias/graficos", recursive = TRUE, pattern = "[.]pdf$")
verificar(length(graficos) == 23L && length(pdfs) == 23L,
          "Os 23 gráficos foram exportados em PNG e PDF")
dir.create("resultados/validacao", showWarnings = FALSE, recursive = TRUE)
writeLines(c(paste("Validação:", format(Sys.time(), "%d/%m/%Y %H:%M:%S %Z")),
             validacoes), "resultados/validacao/verificacoes.txt", useBytes = TRUE)
message(length(validacoes), " verificações concluídas sem falhas.")
