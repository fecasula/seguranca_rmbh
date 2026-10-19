#===============================================================================
# REGRESSÕES LOGÍSTICAS - SEXO, RAÇA E QUARTIL DE RENDA
# Execute: source("R/regressoes_logisticas.R") a partir da raiz do projeto.
# Desfechos: insegurança de dia e à noite; modelos separados por rodada.
#===============================================================================
source("R/funcoes_frequencia.R", local = TRUE)
source("R/renda_quartis.R", local = TRUE)
# Cortes definidos na renda válida da rodada, antes da seleção de casos do modelo.
renda_preparada <- classificar_quartis(anexar_renda(preparar_frequencias(carregar_base())))
base_analise <- renda_preparada$base
pasta <- file.path("resultados", "regressoes")
dir.create(pasta, recursive = TRUE, showWarnings = FALSE)

# Ajuste mútuo, sem interação. Renda é fator nominal; referência Q1.
preditores <- c("sexo", "raca_grupo", "quartil_renda")
rotulos_preditores <- c(sexo = "Sexo", raca_grupo = "Raça", quartil_renda = "Quartil de renda")
modelos <- list()
coeficientes <- list()
diagnosticos <- list()
testes_globais <- list()
fluxo_amostra <- list()
celulas_modelo <- list()

ajustar_modelo <- function(des, desfecho, ano, periodo, especificacao) {
  d <- des$variables
  chave <- tibble(ano_rodada = ano, periodo = periodo, especificacao = especificacao,
                  desfecho = desfecho)
  variaveis <- c(desfecho, preditores)
  completos <- complete.cases(d[variaveis])
  # Fluxo de perdas por casos completos, na ordem declarada dos preditores.
  restantes <- rep(TRUE, nrow(d))
  fluxo <- list(bind_cols(chave, tibble(etapa = "Amostra da rodada",
    n_perdidos_etapa = 0L, n_restantes = nrow(d))))
  for (v in variaveis) {
    novo <- restantes & !is.na(d[[v]])
    fluxo[[length(fluxo) + 1L]] <- bind_cols(chave, tibble(etapa = v,
      n_perdidos_etapa = sum(restantes) - sum(novo), n_restantes = sum(novo)))
    restantes <- novo
  }
  fluxo_amostra[[length(fluxo_amostra) + 1L]] <<- bind_rows(fluxo)
  # Subpopulação do desenho da rodada: não recria o desenho após remover NAs.
  dm <- des[completos, ]
  for (v in preditores) if (is.factor(dm$variables[[v]])) {
    dm$variables[[v]] <- droplevels(dm$variables[[v]])
  }
  x <- dm$variables
  n <- nrow(x)
  motivo <- character()
  if (n < 50L || length(unique(x[[desfecho]])) != 2L) motivo <- "Amostra/desfecho insuficiente"
  sem_variacao <- preditores[vapply(x[preditores], function(z) length(unique(z)) < 2L,
                                    logical(1))]
  if (length(sem_variacao)) motivo <- c(motivo,
    paste("Preditores sem variação:", paste(sem_variacao, collapse = ", ")))
  n_eventos <- sum(x[[desfecho]] == 1)
  n_nao_eventos <- sum(x[[desfecho]] == 0)
  if (length(motivo)) {
    diagnosticos[[length(diagnosticos) + 1L]] <<- bind_cols(chave,
      tibble(n_rodada = nrow(d), n_modelo = n, n_excluidos = nrow(d) - n,
        n_eventos = n_eventos, n_nao_eventos = n_nao_eventos,
        status = paste(motivo, collapse = "; ")))
    return(invisible(NULL))
  }
  # Conteúdo das células categóricas: auditável antes de interpretar OR raros.
  categorias <- preditores[vapply(x[preditores], is.factor, logical(1))]
  for (v in categorias) {
    t <- as.data.frame(table(categoria = x[[v]], evento = x[[desfecho]]),
                       stringsAsFactors = FALSE)
    celulas_modelo[[length(celulas_modelo) + 1L]] <<- bind_cols(chave,
      tibble(variavel = v), as_tibble(t))
  }
  formula <- reformulate(preditores, response = desfecho)
  # Todas as variáveis estão no desenho; não retenha o ambiente de execução
  # inteiro (e os demais modelos) no ambiente da fórmula salva em RDS.
  environment(formula) <- baseenv()
  avisos <- character()
  modelo <- tryCatch(withCallingHandlers(
    survey::svyglm(formula, design = dm, family = quasibinomial(link = "logit"),
                  control = glm.control(maxit = 100)),
    warning = function(w) {avisos <<- c(avisos, conditionMessage(w));
                          invokeRestart("muffleWarning")}), error = function(e) e)
  if (inherits(modelo, "error")) {
    diagnosticos[[length(diagnosticos) + 1L]] <<- bind_cols(chave,
      tibble(n_rodada = nrow(d), n_modelo = n, n_excluidos = nrow(d) - n,
        n_eventos = n_eventos, n_nao_eventos = n_nao_eventos,
        status = conditionMessage(modelo)))
    return(invisible(NULL))
  }
  nome <- paste(ano, periodo, especificacao, sep = "_")
  modelos[[nome]] <<- modelo
  b <- coef(modelo)
  ep <- sqrt(diag(vcov(modelo)))
  graus <- modelo$df.residual
  # IC e p usam os mesmos graus de liberdade residuais do desenho/modelo.
  critico <- if (is.finite(graus) && graus > 0) qt(1 - (1 - config$confianca) / 2,
                                                 df = graus) else NA_real_
  p <- if (is.finite(graus) && graus > 0) 2 * pt(-abs(b / ep), df = graus) else
    rep(NA_real_, length(b))
  termo_variavel <- vapply(names(b), function(termo) {
    if (termo == "(Intercept)") return("Intercepto")
    acertos <- preditores[startsWith(termo, preditores)]
    if (length(acertos)) acertos[which.max(nchar(acertos))] else termo
  }, character(1))
  categoria_coef <- vapply(seq_along(b), function(i) {
    v <- termo_variavel[i]
    if (v == "Intercepto") "Intercepto" else
      substring(names(b)[i], nchar(v) + 1L)
  }, character(1))
  suspeita <- !is.finite(b) | !is.finite(ep) | abs(b) > 10 | ep > 5
  instabilidade_global <- !isTRUE(modelo$converged) || any(suspeita) ||
    any(grepl("0 or 1|did not converge", avisos))
  diag <- bind_cols(chave, tibble(n_rodada = nrow(d), n_modelo = n,
    n_excluidos = nrow(d) - n, n_eventos = n_eventos, n_nao_eventos = n_nao_eventos,
    n_upas = nlevels(droplevels(x$upa)), gl_desenho = survey::degf(dm),
    gl_residuais = graus, parametros_sem_intercepto = length(b) - 1L,
    eventos_por_parametro = n_eventos / (length(b) - 1L),
    convergiu = isTRUE(modelo$converged),
    probabilidades_extremas = sum(fitted(modelo) < 1e-6 | fitted(modelo) > 1 - 1e-6),
    diagnostico_instabilidade = instabilidade_global,
    avisos = paste(unique(avisos), collapse = " | "),
    status = if (instabilidade_global) "Revisar instabilidade/separação" else "OK"))
  diagnosticos[[length(diagnosticos) + 1L]] <<- diag
  tb <- bind_cols(chave, tibble(termo = names(b), variavel = termo_variavel,
    categoria = categoria_coef, referencia = FALSE,
    beta = as.numeric(b), erro_padrao = as.numeric(ep),
    OR = exp(as.numeric(b)), ic_inferior = exp(as.numeric(b - critico * ep)),
    ic_superior = exp(as.numeric(b + critico * ep)), p_valor = as.numeric(p),
    significativo = !is.na(p) & p < config$alpha & !instabilidade_global,
    alerta_coeficiente = suspeita, modelo_instavel = instabilidade_global,
    n_modelo = n))
  # Inclui a categoria de referência na tabela para tornar cada OR interpretável.
  refs <- bind_rows(lapply(categorias, function(v) bind_cols(chave,
    tibble(termo = paste0(v, levels(x[[v]])[1]), variavel = v,
      categoria = levels(x[[v]])[1], referencia = TRUE, beta = 0,
      erro_padrao = NA_real_, OR = 1, ic_inferior = NA_real_, ic_superior = NA_real_,
      p_valor = NA_real_, significativo = FALSE, alerta_coeficiente = FALSE,
      modelo_instavel = instabilidade_global, n_modelo = n))))
  coeficientes[[length(coeficientes) + 1L]] <<- bind_rows(tb, refs)
  for (v in preditores) {
    t <- tryCatch(survey::regTermTest(modelo, reformulate(v), method = "Wald",
                                    df = graus), error = function(e) e)
    testes_globais[[length(testes_globais) + 1L]] <<- bind_cols(chave,
      tibble(variavel = v, p_global = if (inherits(t, "error")) NA_real_ else
        as.numeric(t$p), status = if (inherits(t, "error")) conditionMessage(t) else "OK"))
  }
  invisible(modelo)
}

for (ano in sort(unique(base_analise$ano_rodada))) {
  message("Regressões: rodada ", ano)
  d <- filter(base_analise, ano_rodada == ano)
  des <- criar_desenho(d)
  for (periodo in c("dia", "noite")) {
    ajustar_modelo(des, paste0("inseguranca_", periodo), ano, periodo, "principal")
    if (ano %in% c(2005, 2008)) {
      ajustar_modelo(des, paste0("inseguranca_", periodo, "_sem_neutros"),
                    ano, periodo, "sem_neutros")
    }
  }
}

coeficientes <- bind_rows(coeficientes)
diagnosticos <- bind_rows(diagnosticos)
testes_globais <- bind_rows(testes_globais)
fluxo_amostra <- bind_rows(fluxo_amostra)
celulas_modelo <- bind_rows(celulas_modelo)
if (!nrow(coeficientes)) {
  salvar_csv(diagnosticos, file.path(pasta, "diagnosticos_modelos.csv"))
  stop("Nenhum modelo estimado. Consulte diagnosticos_modelos.csv.")
}
coeficientes <- coeficientes |>
  left_join(select(testes_globais, ano_rodada, periodo, especificacao, variavel, p_global),
            by = c("ano_rodada", "periodo", "especificacao", "variavel")) |>
  mutate(limite_menos_1EP = ifelse(referencia, NA_real_, exp(beta - erro_padrao)),
         limite_mais_1EP = ifelse(referencia, NA_real_, exp(beta + erro_padrao)),
         erro_padrao_OR_delta = ifelse(referencia, NA_real_, OR * erro_padrao),
         variavel_rotulo = ifelse(variavel == "Intercepto", "Intercepto",
                                 unname(rotulos_preditores[variavel])))
salvar_csv(coeficientes, file.path(pasta, "coeficientes_todos_modelos.csv"))
salvar_csv(diagnosticos, file.path(pasta, "diagnosticos_modelos.csv"))
salvar_csv(testes_globais, file.path(pasta, "testes_globais_preditores.csv"))
salvar_csv(fluxo_amostra, file.path(pasta, "fluxo_perdas_amostra.csv"))
salvar_csv(celulas_modelo, file.path(pasta, "contagens_categorias_desfechos.csv"))
salvar_csv(renda_preparada$cortes, file.path(pasta, "cortes_quartis_renda_modelos.csv"))

# Tabela resumida: uma coluna por modelo (ano × período), OR ajustados e IC95%.
formatar_p <- function(x) ifelse(is.na(x), "-",
  ifelse(x < .001, "<0,001", formatC(x, format = "f", digits = 3, decimal.mark = ",")))
resumir_tabela <- function(especificacao) {
  z <- filter(coeficientes, .data$especificacao == .env$especificacao,
              variavel != "Intercepto")
  z$modelo_rotulo <- paste(z$ano_rodada, ifelse(z$periodo == "dia", "Dia", "Noite"))
  z$texto <- ifelse(z$referencia, "1,00 (referência)",
    paste0(formatC(z$OR, format = "f", digits = 2, decimal.mark = ","), " [",
      formatC(z$ic_inferior, format = "f", digits = 2, decimal.mark = ","), "; ",
      formatC(z$ic_superior, format = "f", digits = 2, decimal.mark = ","), "]",
      ifelse(z$significativo, " *", ""), "; p=", formatar_p(z$p_valor),
      ifelse(z$modelo_instavel, " [revisar modelo]", "")))
  ordem <- c(preditores)
  z <- arrange(z, match(variavel, ordem), !referencia, categoria)
  linhas <- distinct(z, variavel, variavel_rotulo, categoria)
  out <- select(linhas, Variavel = variavel_rotulo, Categoria = categoria)
  for (m in unique(z$modelo_rotulo)) {
    zm <- filter(z, modelo_rotulo == m)
    indices <- match(paste(linhas$variavel, linhas$categoria), paste(zm$variavel, zm$categoria))
    out[[m]] <- ifelse(is.na(indices), "Não estimado na rodada", zm$texto[indices])
  }
  out
}
tabela_principal <- resumir_tabela("principal")
tabela_sensibilidade <- resumir_tabela("sem_neutros")
salvar_csv(tabela_principal, file.path(pasta, "tabela_modelos_principais.csv"))
salvar_csv(tabela_sensibilidade, file.path(pasta, "tabela_sensibilidade_sem_neutros.csv"))
conteudo <- c(
  "<p><a href='../frequencias/indice_graficos.html'>Abrir gráficos de frequência</a></p>",
  "<p>Regressões logísticas ponderadas (survey::svyglm, quasibinomial/logit), com erros padrão por setor e casos completos por modelo. Desfecho principal: inseguro/muito inseguro = 1; seguro/muito seguro/neutro = 0. Cada modelo usa somente sua rodada.</p>",
  "<p>Cada modelo inclui sexo, raça (Brancos/Negros) e quartil de renda, com ajuste mútuo e sem interação. Referências: Homem, Brancos e Q1. OR &gt; 1 indica maior chance de declarar insegurança. Ausências nos campos do modelo e respostas raciais fora dos grupos definidos são excluídas por modelo.</p>",
  "<h2>Modelo - sexo, raça e quartil de renda</h2>",
  "<p>OR ajustado [IC95%]; p do coeficiente. * p &lt; 0,05, sem ajuste de multiplicidade nas regressões. Consulte os testes globais e o diagnóstico antes de interpretar categorias raras.</p>",
  tabela_html(tabela_principal),
  "<h2>Sensibilidade - excluindo respostas neutras</h2>",
  "<p>Compara exclusivamente seguros com inseguros. Em 2002 não havia resposta neutra, portanto o modelo principal já corresponde a esta definição.</p>",
  tabela_html(tabela_sensibilidade),
  "<h2>Diagnósticos e tamanho das amostras</h2>", tabela_html(diagnosticos),
  "<p>O desenho utiliza peso final e setores disponíveis. Não foram presumidos estratos oficiais. Os resultados são associações observacionais; a OR não é uma razão de prevalências ou um efeito causal.</p>")
pagina_html("BH Survey - regressões logísticas", conteudo, file.path(pasta, "tabela_modelos.html"))
resultado_regressao <- list(modelos = modelos, coeficientes = coeficientes,
  diagnosticos = diagnosticos, testes_globais = testes_globais, config = config,
  preditores = preditores, tabela_principal = tabela_principal,
  tabela_sensibilidade = tabela_sensibilidade, cortes_renda = renda_preparada$cortes)
saveRDS(resultado_regressao,
        file.path(pasta, "modelos_logisticos.rds"))
source("R/plotagens_odds.R", local = TRUE)
plotar_odds(coeficientes, pasta)
source("R/relatorio_interativo.R", local = TRUE)
gerar_relatorio_regressao(resultado_regressao, pasta)
# Atualiza o relatório integrado quando as frequências já estão disponíveis.
arquivo_f <- "resultados/frequencias/resultados_frequencias.rds"
arquivo_c <- "resultados/frequencias/covariancia/matrizes_covariancia.rds"
if (file.exists(arquivo_f) && file.exists(arquivo_c)) {
  freq <- readRDS(arquivo_f)
  gerar_relatorio(freq$prevalencias, readRDS(arquivo_c), freq$cortes_renda,
    freq$elegibilidade, "resultados/frequencias", regressao = resultado_regressao)
}
capture.output(sessionInfo(), file = file.path(pasta, "sessionInfo.txt"))
if (any(diagnosticos$status != "OK")) {
  warning("Há modelos que precisam de revisão; consulte diagnosticos_modelos.csv.", call. = FALSE)
}
message("Regressões concluídas. Abra resultados/regressoes/tabela_modelos.html.")
