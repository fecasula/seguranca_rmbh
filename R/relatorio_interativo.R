# Tabelas HTML nativas: retráteis, busca, ordenação e exportação sem CDN.
attr_html <- function(x) gsub('"', "&quot;", escape_html(x), fixed = TRUE)

tabela_retratil <- function(d, id, titulo, nota = "") {
  linhas <- vapply(seq_len(nrow(d)), function(i) {
    celulas <- vapply(seq_along(d), function(j) {
      valor <- d[[j]][i]
      tipo <- if (is.numeric(d[[j]])) "number" else "string"
      raw <- if (is.na(valor)) "" else if (tipo == "number")
        format(valor, digits = 15, scientific = FALSE, trim = TRUE, decimal.mark = ".") else as.character(valor)
      mostrar <- if (is.na(valor)) "-" else if (tipo == "number") {
        if (names(d)[j] %in% c("Ano", "ano_rodada")) format(valor, trim = TRUE, scientific = FALSE) else
        if (grepl("^p_", names(d)[j]) && valor < .001) "<0,001" else
        if (grepl("^p_|Covari|Erro_padrao|^I_|MULHER|IDADE|RENDA|^D_|NEGROS|^OR|^IC95|^EP_|^Limite_", names(d)[j]))
          formatC(valor, digits = 4, format = "f", decimal.mark = ",") else
          if (grepl("Percentual|IC_", names(d)[j])) formatC(valor, digits = 1, format = "f", decimal.mark = ",") else
            format(valor, trim = TRUE, decimal.mark = ",", big.mark = ".", scientific = FALSE)
      } else as.character(valor)
      paste0("<td data-type='", tipo, "' data-value=\"", attr_html(raw), "\">",
        escape_html(mostrar), "</td>")
    }, character(1))
    paste0("<tr>", paste(celulas, collapse = ""), "</tr>")
  }, character(1))
  cab <- vapply(seq_along(d), function(j) paste0("<th scope='col' tabindex='0' data-col='", j - 1L,
    "' aria-sort='none'>", escape_html(names(d)[j]), "</th>"), character(1))
  paste0("<details class='dados' id='", id, "'><summary>", escape_html(titulo),
    " (", nrow(d), " linhas)</summary><p>", escape_html(nota),
    "</p><div class='acoes'><label>Buscar <input type='search' class='busca' placeholder='Filtrar esta tabela'></label>",
    "<button type='button' data-action='csv'>CSV</button><button type='button' data-action='tsv'>TSV</button>",
    "<button type='button' data-action='json'>JSON</button><button type='button' data-action='copy'>Copiar</button>",
    "<button type='button' data-action='print'>Imprimir / PDF</button>",
    "<span class='contagem' aria-live='polite'></span></div><div class='rolagem'><table data-export='", id,
    "'><thead><tr>", paste(cab, collapse = ""), "</tr></thead><tbody>",
    paste(linhas, collapse = "\n"), "</tbody></table></div></details>")
}

dados_tabela_grafico <- function(d) {
  d |>
    transmute(Ano = ano_rodada, Periodo = case_when(periodo == "dia" ~ "Dia",
      periodo == "noite" ~ "Noite", TRUE ~ ""), Item = ifelse(is.na(item), "", item),
      Grupo = grupo, n_validos, n_eventos = n_categoria,
      Percentual = percentual, IC_inferior = ic_inferior_percentual,
      IC_superior = ic_superior_percentual, p_nominal = p_valor, p_BH = p_ajustado_BH,
      Significancia = marca, Teste = interpretacao, Status = status_teste)
}

gerar_relatorio <- function(prevalencias, covariancias, cortes, elegibilidade, pasta,
                            regressao = NULL) {
  if (is.null(regressao) && file.exists("resultados/regressoes/modelos_logisticos.rds")) {
    anterior <- readRDS("resultados/regressoes/modelos_logisticos.rds")
    if (identical(anterior$preditores, c("sexo", "raca_grupo", "quartil_renda"))) regressao <- anterior
  }
  link_excel <- "__LINK_EXCEL__"
  conteudo <- c("<nav><a href='#seguranca'>Insegurança</a><a href='#desordem'>Desordem social</a><a href='#renda'>Quartis de renda</a><a href='#covariancia'>Covariância</a><a href='#regressoes'>Regressões de sexo, raça e renda</a></nav>",
    "<p>Resultados por sexo, raça, grupo etário e quartil de renda, separados por rodada. Clique em <strong>Ver tabela</strong> abaixo de cada gráfico. As tabelas permitem busca, ordenação e exportação das linhas visíveis.</p>",
    link_excel,
    "<p>Insegurança: inseguro(a) + muito inseguro(a). Pontos ponderados; barras IC95%. * p ajustado BH &lt; 0,05; † apenas p nominal &lt; 0,05. Em idade e renda, os testes são globais entre grupos. Sem divisão por sexo nos demais recortes.</p>",
    "<p>Raça: Negros = pretos, pardos e indígenas; Brancos = brancos e amarelos. Outras opções são excluídas apenas do recorte racial. Renda: componentes individuais declarados; quartis ponderados por rodada, com empates preservados. Não respostas não recebem zero.</p>")
  exportacoes <- list()
  for (tipo in c("inseguranca", "desordem")) {
    bloco <- if (tipo == "inseguranca") "Insegurança" else "Ocorrência de desordem social"
    subpasta <- if (tipo == "inseguranca") "1_seguranca_sociodemograficas" else "2_desordem_social"
    conteudo <- c(conteudo, if (tipo == "inseguranca")
      "<h2 id='seguranca'>1. Segurança X Variáveis Sociodemográficas</h2>" else
      "<h2 id='desordem'>2. Ocorrência de desordem social</h2>")
    for (r in names(recortes_frequencia)) {
      dados <- filter(prevalencias, .data$bloco == .env$bloco, recorte == r)
      nome_r <- switch(r, sexo = "sexo", raca_grupo = "raca", idade_grupo = "grupo_etario", quartil_renda = "renda")
      nome <- paste0(if (tipo == "inseguranca") "painel_inseguranca_" else "ocorrencia_", nome_r)
      titulo <- paste(if (tipo == "inseguranca") "Insegurança - dia e noite" else "Percepção de desordem social",
                       "por", recortes_frequencia[[r]])
      tabela <- dados_tabela_grafico(dados)
      exportacoes[[nome]] <- tabela
      salvar_csv(tabela, file.path(pasta, "exportacoes", paste0(nome, ".csv")))
      conteudo <- c(conteudo, paste0("<article><h3>", escape_html(titulo), "</h3><img loading='lazy' src='graficos/",
        subpasta, "/", nome, ".png' alt='", escape_html(titulo), "'><p><a href='graficos/", subpasta,
        "/", nome, ".png'>Gráfico PNG</a> · <a href='graficos/", subpasta, "/", nome, ".pdf'>Gráfico PDF</a></p>",
        tabela_retratil(tabela, nome, "Ver tabela de resultados",
          "Percentuais e IC em pontos percentuais. CSV, TSV, JSON, copiar e imprimir usam as linhas visíveis na ordem atual."), "</article>"))
    }
  }
  exportacoes$quartis_renda <- cortes
  exportacoes$elegibilidade <- elegibilidade
  conteudo <- c(conteudo, "<h2 id='renda'>3. Quartis de renda</h2>",
    "<p>Q1 contém as menores rendas; Q4, as maiores. Os cortes são específicos de cada ano, em reais nominais. Valores iguais não são separados por ordem da entrevista; por isso cada grupo pode ter tamanho diferente de 25%.</p>",
    tabela_retratil(cortes, "quartis_renda", "Ver cortes e disponibilidade da renda"),
    tabela_retratil(elegibilidade, "elegibilidade", "Ver elegibilidade e exclusões por recorte"),
    "<h2 id='covariancia'>4. Matriz de covariância</h2>",
    "<p>Matrizes ponderadas entre insegurança de dia/noite e indicadores 0/1 de sexo, grupo etário, quartil de renda, raça e os oito itens de ocorrência de desordem social. Estado civil foi retirado. A diagonal contém variâncias. Covariância não é correlação nem efeito ajustado.</p>",
    "<p>Referências: Homem, idade 18-34, Q1, Brancos e Não para ocorrência. Cada rodada usa uma única amostra completa em todos os indicadores aplicáveis. Ocorrência não foi aplicada em 2002; prostituição, apenas em 2008. Esses itens ficam como ausentes (células cinza) e não excluem entrevistas naquelas rodadas. Em 2002, apenas o bloco de insegurança e dados sociodemográficos pode ser estimado.</p>",
    tabela_retratil(covariancias$dicionario, "codificacao_matriz", "Ver codificação dos indicadores"),
    tabela_retratil(covariancias$amostras, "amostras_matriz", "Ver amostras da matriz"),
    tabela_retratil(covariancias$disponibilidade, "disponibilidade_indicadores", "Ver aplicação dos indicadores por rodada"))
  exportacoes$codificacao_matriz <- covariancias$dicionario
  exportacoes$amostras_matriz <- covariancias$amostras
  exportacoes$disponibilidade_indicadores <- covariancias$disponibilidade
  for (ano in names(covariancias$matrizes)) {
    nome <- paste0("matriz_covariancia_", ano)
    tabela <- covariancias$matrizes[[ano]]$tabela
    exportacoes[[nome]] <- tabela
    conteudo <- c(conteudo, paste0("<article><h3>Matriz de covariância - ", ano,
      "</h3><img loading='lazy' src='covariancia/", nome, ".png' alt='Matriz de covariância de ", ano,
      "'><p><a href='covariancia/", nome, ".pdf'>Gráfico PDF</a></p>",
      tabela_retratil(tabela, nome, "Ver matriz numérica", paste(covariancias$matrizes[[ano]]$nota,
        "Valores com quatro casas; exportações preservam maior precisão. Ausências não são zero.")), "</article>"))
  }
  if (!is.null(regressao)) {
    conteudo <- c(conteudo, "<h2 id='regressoes'>5. Regressões logísticas - sexo, raça e renda</h2>",
      bloco_regressao(regressao, "../regressoes/"))
    exportacoes$regressoes_principais <- dados_tabela_odds(regressao$coeficientes, "principal")
    exportacoes$regressoes_sensibilidade <- dados_tabela_odds(regressao$coeficientes, "sem_neutros")
    exportacoes$diagnosticos_regressao <- regressao$diagnosticos
  }
  conteudo <- c(conteudo, "<p>Os pesos e setores disponíveis compõem o desenho de trabalho, sem estratos oficiais presumidos. A análise de covariância é descritiva, com exclusão conjunta de ausências nos itens aplicáveis de cada rodada.</p>")
  excel_ok <- FALSE
  assinatura <- file.path(pasta, "exportacoes", "assinatura_excel.csv")
  if (file.exists(assinatura) && file.exists(file.path(pasta, "exportacoes", "tabelas_resultados.xlsx"))) {
    registro <- read.csv(assinatura, sep = ";", stringsAsFactors = FALSE)
    caminhos <- file.path(pasta, registro$arquivo)
    excel_ok <- all(file.exists(caminhos)) && all(unname(tools::md5sum(caminhos)) == registro$md5)
  }
  link_excel <- if (isTRUE(excel_ok))
    "<p><a class='download-excel' href='exportacoes/tabelas_resultados.xlsx'>Baixar todas as tabelas em Excel (.xlsx)</a></p>" else
    "<p>Planilha Excel ainda não corresponde a esta execução. As exportações das tabelas abaixo usam os resultados atuais.</p>"
  conteudo[conteudo == "__LINK_EXCEL__"] <- link_excel
  arquivo <- file.path(pasta, "indice_graficos.html")
  pagina_html("BH Survey - frequências, covariância e regressões", conteudo, arquivo)
  adicionar_interatividade(arquivo)
  saveRDS(exportacoes, file.path(pasta, "exportacoes", "tabelas_exportacao.rds"))
  invisible(exportacoes)
}

adicionar_interatividade <- function(arquivo) {
  html <- paste(readLines(arquivo, warn = FALSE), collapse = "\n")
  css <- paste(readLines("R/relatorio_tabelas.css", warn = FALSE), collapse = "\n")
  js <- paste(readLines("R/relatorio_tabelas.js", warn = FALSE), collapse = "\n")
  html <- paste0(sub("</body></html>", "", html, fixed = TRUE), "<style>", css, "</style><script>", js, "</script></body></html>")
  writeLines(html, arquivo, useBytes = TRUE)
}

dados_tabela_odds <- function(coeficientes, especificacao) {
  coeficientes |>
    filter(.data$especificacao == .env$especificacao, variavel != "Intercepto") |>
    transmute(Ano = ano_rodada, Periodo = ifelse(periodo == "dia", "Dia", "Noite"),
      Variavel = variavel_rotulo, Categoria = categoria,
      Referencia = referencia, OR = OR, EP_log_OR = erro_padrao,
      Limite_menos_1EP = limite_menos_1EP, Limite_mais_1EP = limite_mais_1EP,
      EP_OR_delta = erro_padrao_OR_delta, IC95_inferior = ic_inferior,
      IC95_superior = ic_superior, p_valor = p_valor, n_modelo = n_modelo)
}

bloco_regressao <- function(regressao, prefixo = "") {
  conteudo <- c(
    "<p>Modelo: logit[P(insegurança)] = intercepto + sexo + raça + quartil de renda. Ajuste mútuo, sem interação, separado por ano e período. Referências: Homem, Brancos e Q1. Entram somente entrevistas com desfecho, sexo, raça e renda válidos; idade e desordem não são critérios de inclusão da regressão.</p>",
    "<p>Renda é um fator com quatro categorias: Q1 (menores rendas), Q2, Q3 e Q4 (maiores rendas). Cortes ponderados por rodada, calculados em toda renda válida antes da seleção dos casos do modelo, idênticos aos das frequências. Empates são preservados. As OR de renda comparam cada quartil com Q1, controlando por sexo e raça. Os gráficos de sexo e raça também foram reestimados com controle por renda.</p>",
    "<p>Negros = pretos, pardos e indígenas; Brancos = brancos e amarelos, conforme a definição operacional do projeto. OR é a razão de chances, não uma razão de prevalências. A tabela apresenta IC95% e p nominal de Wald com os graus de liberdade residuais do desenho.</p>",
    "<p>Nos gráficos, as barras representam <strong>±1 erro padrão (EP) de log(OR)</strong>, transformado por exp(beta ± EP). O EP quantifica a incerteza do coeficiente estimado; não é o desvio padrão das respostas individuais. Essas barras não são IC95% e não indicam significância a 5%. A tabela também exporta EP da OR por aproximação delta (OR × EP de log(OR)).</p>")
  for (v in c("sexo", "raca", "renda")) {
    var <- c(sexo = "Sexo", raca = "Raça", renda = "Quartil de renda")[[v]]
    tabela <- dados_tabela_odds(regressao$coeficientes, "principal") |>
      filter(Variavel == var)
    nome <- paste0("odds_", v)
    conteudo <- c(conteudo, paste0("<article><h3>Odds ratios por ", tolower(var),
      " - dia e noite</h3><img loading='lazy' src='", prefixo, "graficos/", nome,
      ".png' alt='Odds ratios de insegurança por ", tolower(var),
      "'><p><a href='", prefixo, "graficos/", nome, ".png'>Gráfico PNG</a> · <a href='",
      prefixo, "graficos/", nome, ".pdf'>Gráfico PDF</a></p>",
      tabela_retratil(tabela, nome, "Ver tabela de odds ratios",
        "Referências têm OR = 1 por definição, sem EP estimado. Barras dos gráficos: exp(beta ± 1 EP). IC95% estão em colunas separadas."), "</article>"))
  }
  c(conteudo,
    tabela_retratil(regressao$tabela_principal, "resumo_modelos", "Ver resumo dos seis modelos principais",
      "OR ajustada [IC95%]; p nominal. * p < 0,05."),
    tabela_retratil(dados_tabela_odds(regressao$coeficientes, "sem_neutros"),
      "sensibilidade_modelos", "Ver sensibilidade sem respostas neutras",
      "2005 e 2008. Em 2002 não havia opção neutra; o modelo principal já usa essa definição."),
    tabela_retratil(regressao$testes_globais, "testes_globais_modelos", "Ver testes globais dos preditores",
      "Wald pelo desenho. Quartil de renda: teste conjunto de Q2, Q3 e Q4 em relação a Q1, sem presumir tendência linear."),
    tabela_retratil(regressao$diagnosticos, "diagnosticos_modelos", "Ver diagnósticos e amostras"))
}

gerar_relatorio_regressao <- function(regressao, pasta) {
  conteudo <- c("<p><a href='../frequencias/indice_graficos.html'>Abrir relatório integrado</a></p>",
    bloco_regressao(regressao))
  arquivo <- file.path(pasta, "tabela_modelos.html")
  pagina_html("BH Survey - regressões logísticas de sexo, raça e renda", conteudo, arquivo)
  adicionar_interatividade(arquivo)
}
