#===============================================================================
# ANÁLISE DE FREQUÊNCIA — BH SURVEY (2002, 2005, 2008)
# Execute: source("R/analise_frequencia.R") a partir da raiz do projeto.
# Saídas: resultados/frequencias/ (CSV, RDS, HTML e gráficos PNG/PDF).
#===============================================================================
source("R/funcoes_analise.R", local = TRUE)
base_analise <- carregar_base()
pasta <- file.path("resultados", "frequencias")
dir.create(pasta, recursive = TRUE, showWarnings = FALSE)

variaveis_socio <- c(sexo = "Sexo", municipio_grupo = "Município",
  grupo_etario = "Grupo etário", raca_cor = "Raça/cor", escolaridade = "Escolaridade",
  estado_civil = "Estado civil")
frequencias <- list()
prevalencias <- list()
testes <- list()
ausencias <- list()
disponibilidade <- list()

# Tabela completa e proporção do evento: denominador específico de cada célula
# ano × categoria sociodemográfica × sexo; nunca o total das três rodadas.
analisar_celula <- function(des, variavel, categorias_resposta, evento,
                           ano, bloco, recorte, categoria, periodo = NA_character_) {
  v <- des$variables
  for (s in c("Homem", "Mulher")) {
    ds <- des[!is.na(v$sexo) & v$sexo == s, ]
    if (!nrow(ds$variables)) next
    chave <- tibble(ano_rodada = ano, bloco = bloco, variavel = variavel,
      periodo = periodo, recorte = recorte, categoria = categoria, sexo = s)
    ausencias[[length(ausencias) + 1L]] <<- bind_cols(chave,
      tibble(n_entrevistados = nrow(ds$variables),
        n_validos = sum(!is.na(ds$variables[[variavel]])),
        n_ausentes = sum(is.na(ds$variables[[variavel]]))))
    for (resposta in categorias_resposta) {
      linha <- bind_cols(chave, tibble(resposta = resposta),
        estimar_proporcao(ds, variavel, resposta))
      frequencias[[length(frequencias) + 1L]] <<- linha
    }
    prev <- bind_cols(chave, tibble(evento = evento),
      estimar_proporcao(ds, variavel, evento))
    prevalencias[[length(prevalencias) + 1L]] <<- prev
  }
  chave_teste <- tibble(ano_rodada = ano, bloco = bloco, variavel = variavel,
    periodo = periodo, recorte = recorte, categoria = categoria)
  testes[[length(testes) + 1L]] <<- bind_cols(chave_teste, teste_sexo(des, variavel))
}

for (ano in sort(unique(base_analise$ano_rodada))) {
  message("Frequências: rodada ", ano)
  d <- filter(base_analise, ano_rodada == ano)
  des <- criar_desenho(d)
  for (periodo in c("dia", "noite")) {
    original <- paste0("andar_", periodo)
    binaria <- paste0("inseguranca_", periodo)
    for (recorte in names(variaveis_socio)) {
      categorias <- if (recorte == "sexo") "Todos" else
        levels(droplevels(d[[recorte]]))
      for (categoria in categorias) {
        dom <- if (recorte == "sexo") des else des[
          !is.na(d[[recorte]]) & d[[recorte]] == categoria, ]
        # Variável binária para a comparação do percentual de insegurança.
        analisar_celula(dom, binaria, c("0", "1"), "1", ano,
          "Segurança X Variáveis Sociodemográficas", recorte, categoria, periodo)
        # Todas as respostas originais, preservando a opção neutra de 2005/2008.
        respostas <- if (ano == 2002) niveis_seguranca[
          niveis_seguranca != "Nem seguro(a) nem inseguro(a)"] else niveis_seguranca
        analisar_celula(dom, original, respostas, "Inseguro(a)", ano,
          "Distribuição original de segurança", recorte, categoria, periodo)
      }
    }
  }
  for (v in names(rotulos_desordem)) {
    aplicado <- ano %in% c(2005, 2008) &&
      (v != "desordem_prostituicao_ocorrencia" || ano == 2008)
    disponibilidade[[length(disponibilidade) + 1L]] <- tibble(
      ano_rodada = ano, variavel = v, item = unname(rotulos_desordem[v]),
      aplicado = aplicado,
      motivo = if (aplicado) "Ocorrência aplicada na rodada" else "Não aplicada na rodada")
    if (!aplicado) next
    analisar_celula(des, v, c("Não", "Sim"), "Sim", ano,
      "Ocorrência de desordem social", "sexo", unname(rotulos_desordem[v]))
  }
}

frequencias <- bind_rows(frequencias)
prevalencias <- bind_rows(prevalencias)
testes <- ajustar_testes(bind_rows(testes), c("ano_rodada", "bloco"))
ausencias <- bind_rows(ausencias)
disponibilidade <- bind_rows(disponibilidade)
prevalencias <- left_join(prevalencias, testes,
  by = c("ano_rodada", "bloco", "variavel", "periodo", "recorte", "categoria"))
frequencias <- mutate(frequencias, percentual = 100 * proporcao,
  ic_inferior_percentual = 100 * ic_inferior, ic_superior_percentual = 100 * ic_superior)
prevalencias <- mutate(prevalencias, percentual = 100 * proporcao,
  ic_inferior_percentual = 100 * ic_inferior, ic_superior_percentual = 100 * ic_superior)

salvar_csv(frequencias, file.path(pasta, "frequencias_completas.csv"))
salvar_csv(filter(prevalencias, bloco == "Segurança X Variáveis Sociodemográficas"),
  file.path(pasta, "inseguranca_sociodemograficas.csv"))
salvar_csv(filter(frequencias, bloco == "Distribuição original de segurança"),
  file.path(pasta, "seguranca_respostas_originais.csv"))
salvar_csv(filter(prevalencias, bloco == "Ocorrência de desordem social"),
  file.path(pasta, "desordem_ocorrencia_por_sexo.csv"))
salvar_csv(testes, file.path(pasta, "testes_diferencas_por_sexo.csv"))
salvar_csv(filter(testes, significativo_nominal),
  file.path(pasta, "diferencas_significativas.csv"))
salvar_csv(ausencias, file.path(pasta, "respostas_validas_e_ausentes.csv"))
salvar_csv(disponibilidade, file.path(pasta, "disponibilidade_desordem_por_ano.csv"))
faltantes_socio <- bind_rows(lapply(sort(unique(base_analise$ano_rodada)), function(ano) {
  d <- filter(base_analise, ano_rodada == ano)
  bind_rows(lapply(names(variaveis_socio), function(v) tibble(ano_rodada = ano,
    variavel = v, n_total = nrow(d), n_ausentes = sum(is.na(d[[v]])))))
}))
salvar_csv(faltantes_socio, file.path(pasta, "ausencias_sociodemograficas.csv"))

# GRUPO 1 — SEGURANÇA X VARIÁVEIS SOCIODEMOGRÁFICAS
pasta_socio <- file.path(pasta, "graficos", "1_seguranca_sociodemograficas")
catalogo_socio <- character()
for (recorte in names(variaveis_socio)) {
  niveis <- if (recorte == "sexo") "Todos" else
    levels(droplevels(base_analise[[recorte]]))
  for (periodo in c("dia", "noite")) {
    dados_plot <- filter(prevalencias,
      bloco == "Segurança X Variáveis Sociodemográficas",
      .data$recorte == .env$recorte, .data$periodo == .env$periodo)
    testes_plot <- filter(testes,
      bloco == "Segurança X Variáveis Sociodemográficas",
      .data$recorte == .env$recorte, .data$periodo == .env$periodo)
    periodo_legenda <- if (periodo == "dia") "de dia" else "à noite"
    titulo <- paste("Insegurança ao andar", periodo_legenda, "—", variaveis_socio[[recorte]])
    p <- plotar_comparacao(dados_plot, testes_plot, niveis, titulo,
      "Inseguro(a) + muito inseguro(a); comparação entre homens e mulheres em cada categoria.")
    nome <- paste0("inseguranca_", periodo, "_", recorte)
    salvar_grafico(p, pasta_socio, nome, altura = max(4.8, 2.8 + length(niveis) * .55))
    catalogo_socio <- c(catalogo_socio, paste0("<article><h3>", escape_html(titulo),
      "</h3><img loading='lazy' src='graficos/1_seguranca_sociodemograficas/", nome,
      ".png' alt='", escape_html(titulo), "'><a href='graficos/1_seguranca_sociodemograficas/",
      nome, ".pdf'>Baixar PDF</a></article>"))
  }
}

# Distribuição completa das cinco respostas por sexo, inclusive neutralidade.
# O destaque vem do teste global da distribuição, não da soma de insegurança.
for (periodo in c("dia", "noite")) {
  original <- paste0("andar_", periodo)
  ds <- filter(frequencias, variavel == original, recorte == "sexo") |>
    mutate(categoria = resposta)
  ts <- filter(testes, variavel == original, recorte == "sexo")
  ts <- bind_rows(lapply(niveis_seguranca, function(r) mutate(ts, categoria = r)))
  p <- plotar_comparacao(ds, ts, niveis_seguranca,
    paste("Segurança", if (periodo == "dia") "de dia" else "à noite", "— respostas originais"),
    "A mesma marca em todas as respostas indica o teste global da distribuição por sexo.")
  nome <- paste0("respostas_originais_", periodo)
  salvar_grafico(p, pasta_socio, nome, altura = 6.2)
  catalogo_socio <- c(catalogo_socio, paste0("<article><h3>Respostas originais — ",
    periodo, "</h3><img loading='lazy' src='graficos/1_seguranca_sociodemograficas/",
    nome, ".png' alt='Distribuição original de segurança'></article>"))
}

# GRUPO 2 — OCORRÊNCIA DE DESORDEM SOCIAL
pasta_desordem <- file.path(pasta, "graficos", "2_desordem_social")
dd <- filter(prevalencias, bloco == "Ocorrência de desordem social")
td <- filter(testes, bloco == "Ocorrência de desordem social")
p <- plotar_comparacao(dd, td, unname(rotulos_desordem),
  "Ocorrência de desordem social por sexo",
  "Percentual de respostas 'Sim'. Prostituição foi perguntada apenas em 2008.")
salvar_grafico(p, pasta_desordem, "ocorrencia_todos_itens", altura = 8, largura = 12)
catalogo_desordem <- paste0("<article><h3>Todos os itens</h3><img src='graficos/",
  "2_desordem_social/ocorrencia_todos_itens.png' alt='Ocorrência de desordem social'>",
  "<a href='graficos/2_desordem_social/ocorrencia_todos_itens.pdf'>Baixar PDF</a></article>")
for (v in names(rotulos_desordem)) {
  d_item <- filter(dd, variavel == v)
  t_item <- filter(td, variavel == v)
  titulo <- unname(rotulos_desordem[v])
  p <- plotar_comparacao(d_item, t_item, titulo,
    paste("Ocorrência —", titulo), "Percentual de respostas 'Sim' por sexo e rodada.")
  nome <- sub("^desordem_", "", sub("_ocorrencia$", "", v))
  salvar_grafico(p, pasta_desordem, nome, altura = 4.8, largura = 10)
  catalogo_desordem <- c(catalogo_desordem, paste0("<article><h3>", escape_html(titulo),
    "</h3><img loading='lazy' src='graficos/2_desordem_social/", nome,
    ".png' alt='", escape_html(titulo), "'></article>"))
}

pagina_html("BH Survey — análise de frequência", c(
  "<nav><a href='#seguranca'>1. Segurança X Variáveis Sociodemográficas</a><a href='#desordem'>2. Ocorrência de desordem social</a><a href='../regressoes/tabela_modelos.html'>Regressões</a></nav>",
  "<p>Percentuais ponderados pelo peso final de pós-estratificação e variância por setor censitário. Resultados separados por rodada. Não respostas excluídas do denominador específico de cada estimativa.</p>",
  "<p><strong>*</strong> Diferença por sexo com p ajustado por Benjamini–Hochberg &lt; 0,05. <strong>†</strong> Significativa apenas pelo p nominal. <strong>?</strong> Teste indisponível, sem conclusão de igualdade. Os testes não são inferidos pela sobreposição dos intervalos.</p>",
  "<p>O denominador de insegurança inclui respostas neutras em 2005/2008. Em 2002, desordem foi medida como gravidade: essa medida não é convertida em ocorrência. Em 2008, 'Não' pode representar 'não mencionou' na lista de problemas, conforme o questionário.</p>",
  "<p>Sem estratos oficiais identificados, o desenho usa os pesos e os setores disponíveis. Veja a documentação metodológica e os CSV de ausências e testes para os limites de cada estimativa.</p>",
  "<h2 id='seguranca'>1. Segurança X Variáveis Sociodemográficas</h2>", catalogo_socio,
  "<h2 id='desordem'>2. Ocorrência de desordem social</h2>", catalogo_desordem),
  file.path(pasta, "indice_graficos.html"))
saveRDS(list(frequencias = frequencias, prevalencias = prevalencias, testes = testes,
             ausencias = ausencias, disponibilidade = disponibilidade, config = config),
        file.path(pasta, "resultados_frequencias.rds"))
capture.output(sessionInfo(), file = file.path(pasta, "sessionInfo.txt"))
message("Análise concluída. Abra resultados/frequencias/indice_graficos.html.")
