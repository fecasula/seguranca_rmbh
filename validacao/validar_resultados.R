# Verifica a preservação das frequências e as novas matrizes/regressões.
# Execute na raiz: source("validacao/validar_resultados.R")
source("R/funcoes_frequencia.R", local = TRUE)
d <- readRDS("resultados/frequencias/base_frequencias.rds")
f <- readRDS("resultados/frequencias/resultados_frequencias.rds")
verificacoes <- character()
verificar <- function(condicao, texto) {
  if (!isTRUE(condicao)) stop("FALHOU: ", texto)
  verificacoes <<- c(verificacoes, paste0("OK - ", texto))
}
ler_csv <- function(arquivo) readr::read_delim(arquivo, delim = ";",
  locale = readr::locale(decimal_mark = ","), show_col_types = FALSE, progress = FALSE)

# Casos de fronteira e todas as opções raciais que mudam a elegibilidade.
artificial <- tibble(raca_cor = c("Preto(a)", "Pardo(a)", "Indígena", "Amarelo(a)",
  "Branco(a)", "Negro(a)", "Moreno(a)", "Outra", NA_character_),
  idade = c(18, 34, 35, 59, 60, 95, NA, 18, 60))
x <- preparar_frequencias(artificial)
verificar(identical(as.character(x$raca_grupo),
  c("Negros", "Negros", "Negros", "Brancos", "Brancos", NA, NA, NA, NA)),
  "Somente os cinco rótulos raciais indicados entram nos dois grupos")
verificar(identical(as.character(x$idade_grupo),
  c("18–34", "18–34", "35–59", "35–59", "60+", "60+", NA, "18–34", "60+")),
  "Fronteiras de 18, 34, 35, 59 e 60 anos classificadas corretamente")
verificar(identical(levels(d$raca_grupo), c("Brancos", "Negros")) &&
  identical(levels(d$idade_grupo), c("18–34", "35–59", "60+")),
  "Níveis dos fatores raciais e etários fixados")

# Cada estimativa é confrontada com n bruto e média ponderada direta.
# O recorte por raça/idade não usa sexo como filtro.
for (i in seq_len(nrow(f$prevalencias))) {
  linha <- f$prevalencias[i, ]
  r <- linha$recorte
  sel <- d$ano_rodada == linha$ano_rodada & !is.na(d[[r]]) & d[[r]] == linha$grupo
  dom <- d[sel, ]
  y <- dom[[linha$variavel]]
  ok <- !is.na(y)
  evento <- as.character(y[ok]) == linha$evento
  p <- sum(dom$peso_analise[ok] * evento) / sum(dom$peso_analise[ok])
  verificar(nrow(dom) >= sum(ok) && linha$n_validos == sum(ok) &&
    linha$n_categoria == sum(evento) && abs(linha$proporcao - p) < 1e-10,
    paste("Estimativa e denominador sem filtro adicional:", linha$ano_rodada,
      linha$variavel, r, linha$grupo))
}
e <- f$elegibilidade
for (ano in unique(d$ano_rodada)) {
  linhas <- e[e$ano_rodada == ano, ]
  verificar(all(linhas$n_elegiveis + linhas$n_excluidos == linhas$n_rodada),
    paste("Elegibilidade e exclusões por recorte:", ano))
  idade <- linhas[linhas$recorte == "idade_grupo", ]
  verificar(idade$n_elegiveis == sum(d$ano_rodada == ano & !is.na(d$idade)),
    paste("Raça excluída não remove entrevistas do recorte etário:", ano))
}
somatorios <- f$frequencias |>
  group_by(ano_rodada, variavel, recorte, grupo) |>
  summarise(soma = sum(proporcao), n = first(n_validos), .groups = "drop")
verificar(all(abs(somatorios$soma[somatorios$n > 0] - 1) < 1e-10),
  "Categorias de resposta somam 100% dentro de cada grupo")
ci <- f$prevalencias
ok <- ci$status_ic == "OK"
verificar(all(is.na(ci$ic_inferior) == is.na(ci$ic_superior)) &&
  all(ci$ic_inferior[ok] >= 0 & ci$ic_superior[ok] <= 1 &
    ci$ic_inferior[ok] <= ci$proporcao[ok] & ci$ic_superior[ok] >= ci$proporcao[ok]),
  "IC válidos contêm a proporção; IC indisponíveis não conservam um único limite")
verificar(!any(f$prevalencias$ano_rodada == 2002 &
  f$prevalencias$bloco == "Ocorrência de desordem social"),
  "Ocorrência não foi inventada a partir da gravidade de 2002")
verificar(!any(f$prevalencias$ano_rodada == 2005 &
  f$prevalencias$variavel == "desordem_prostituicao_ocorrencia"),
  "Prostituição não foi estimada para 2005")

# Reprodução numérica dos anexos por sexo.
ref <- ler_csv("dados/tratados/referencia_percentuais_sexo_v1.csv")
sexo <- filter(f$prevalencias, recorte == "sexo")
chave <- function(x) paste(x$ano_rodada, x$variavel, x$grupo, sep = "|")
j <- match(chave(sexo), chave(ref))
verificar(!anyNA(j) && nrow(sexo) == nrow(ref), "Todas as estimativas de sexo foram mantidas")
for (c in c("n_validos", "n_categoria", "proporcao", "ic_inferior", "ic_superior")) {
  verificar(isTRUE(all.equal(as.numeric(sexo[[c]]), as.numeric(ref[[c]][j]),
                           tolerance = 1e-10)), paste("Sexo preservado:", c))
}
antigos <- ler_csv("dados/tratados/referencia_testes_sexo_v1.csv") |>
  filter(recorte == "sexo", bloco != "Distribuição original de segurança")
ts <- filter(f$testes, recorte == "sexo")
chave_t <- function(x) paste(x$ano_rodada, x$variavel, sep = "|")
j <- match(chave_t(ts), chave_t(antigos))
verificar(!anyNA(j) && all(abs(ts$p_valor - antigos$p_valor[j]) < 1e-10) &&
  all(abs(ts$p_ajustado_BH - antigos$p_ajustado_BH[j]) < 1e-10),
  "Testes e ajustes BH dos anexos por sexo foram preservados")
marcas <- antigos$marca[j]
marcas[is.na(marcas)] <- ""
verificar(identical(ts$marca, marcas), "Marcações de significância por sexo foram mantidas")

novos <- filter(f$testes, recorte != "sexo")
familias <- split(novos, interaction(novos$ano_rodada, novos$bloco, novos$recorte, drop = TRUE))
for (t in familias) {
  ok <- !is.na(t$p_valor)
  verificar(isTRUE(all.equal(t$p_ajustado_BH[ok], p.adjust(t$p_valor[ok], method = "BH"))),
    paste("BH dos novos recortes:", t$ano_rodada[1], t$bloco[1], t$recorte[1]))
}
verificar(all(f$testes$n_grupos[f$testes$recorte == "idade_grupo"] == 3) &&
  all(f$testes$interpretacao[f$testes$recorte == "idade_grupo"] == "Global entre os três grupos etários"),
  "Testes etários são globais entre três grupos")

# Comparação dos arquivos fora do escopo das alterações com a versão de entrada.
preservados <- ler_csv("validacao/arquivos_preservados.csv")
for (i in seq_len(nrow(preservados))) {
  arquivo <- preservados$arquivo[i]
  verificar(file.exists(arquivo) && unname(tools::md5sum(arquivo)) == preservados$md5[i],
    paste("Arquivo preservado nesta revisão:", arquivo))
}
verificar(grepl('source("R/regressoes_logisticas.R"',
  paste(readLines("executar_analises.R"), collapse = "\n"), fixed = TRUE),
  "Executor inclui as regressões de sexo e raça")
png <- list.files("resultados/frequencias/graficos", pattern = "[.]png$", recursive = TRUE)
pdf <- list.files("resultados/frequencias/graficos", pattern = "[.]pdf$", recursive = TRUE)
verificar(length(png) == 8L && length(pdf) == 8L,
  "Os oito gráficos de frequência foram exportados em PNG e PDF")
dir.create("resultados/validacao", recursive = TRUE, showWarnings = FALSE)
writeLines(c(paste("Validação:", format(Sys.time(), "%d/%m/%Y %H:%M:%S %Z")), verificacoes),
  "resultados/validacao/verificacoes.txt", useBytes = TRUE)
salvar_csv(preservados, "resultados/validacao/arquivos_preservados.csv")


# Quartis: CDF ponderada e estabilidade dos empates.
for (ano in unique(d$ano_rodada)) {
 z <- d[d$ano_rodada == ano & !is.na(d$renda_individual), ]
 q <- f$cortes_renda[f$cortes_renda$ano_rodada == ano, c("corte_25_reais","corte_50_reais","corte_75_reais")]
 q <- as.numeric(q[1, ])
 for (j in 1:3) {
   cdf <- sum(z$peso_analise[z$renda_individual <= q[j]]) / sum(z$peso_analise)
   antes <- sum(z$peso_analise[z$renda_individual < q[j]]) / sum(z$peso_analise)
   verificar(cdf >= j / 4 - 1e-10 && antes <= j / 4 + 1e-10, paste("Corte ponderado:",ano,j))
 }
 por_valor <- split(as.character(z$quartil_renda), z$renda_individual)
 verificar(all(vapply(por_valor, function(x) length(unique(x)) == 1L, logical(1))), paste("Empates preservados:",ano))
}
verificar(all(is.na(d$quartil_renda[is.na(d$renda_individual)])), "Renda desconhecida não recebe quartil")
verificar(all(abs(d$renda_individual - (d$renda_trabalho_principal + d$renda_adicional_casa + d$renda_outras_fontes)) < 1e-10, na.rm=TRUE), "Renda corresponde aos componentes registrados")
covs <- readRDS("resultados/frequencias/covariancia/matrizes_covariancia.rds")
for (ano in names(covs$matrizes)) {
 z <- covs$matrizes[[ano]]
 mat <- as.matrix(z$indicadores)
 ativos <- z$codigos_aplicados
 bloco <- z$matriz[ativos,ativos,drop=FALSE]
 w <- z$pesos / sum(z$pesos)
 medias <- colSums(mat * w)
 central <- sweep(mat, 2, medias)
 esperado <- crossprod(central, central * w) * nrow(mat)/(nrow(mat)-1)
 verificar(max(abs(bloco - esperado)) < 1e-10,
   paste("Covariância confrontada com cálculo ponderado direto:",ano))
 verificar(max(abs(bloco - t(bloco))) < 1e-10 &&
   min(eigen(bloco,symmetric=TRUE)$values) >= -1e-10,
   paste("Bloco aplicado simétrico e sem autovalores negativos:",ano))
 verificar(all(mat %in% c(0,1)), paste("Categorias nominais são indicadores 0/1:",ano))
 verificar(nrow(z$matriz)==17L && !any(grepl("CIV_",rownames(z$matriz))),
   paste("Estado civil removido e oito indicadores de desordem incluídos:",ano))
 indisponiveis <- setdiff(rownames(z$matriz),ativos)
 verificar(all(is.na(z$matriz[indisponiveis,,drop=FALSE])) &&
   all(is.na(z$matriz[,indisponiveis,drop=FALSE])) && all(is.finite(bloco)),
   paste("Itens não aplicados são NA; bloco medido permanece estimável:",ano))
 n_itens <- sum(startsWith(ativos,"D_"))
 verificar(n_itens == c('2002'=0L,'2005'=7L,'2008'=8L)[ano],
   paste("Aplicação de ocorrência conforme rodada:",ano))
 vars <- unique(covs$dicionario$Variavel_fonte[match(ativos,covs$dicionario$Codigo)])
 da <- d[d$ano_rodada==as.integer(ano),]
 ids <- da$id_linha[complete.cases(da[vars])]
 verificar(identical(z$ids_incluidos,ids),
   paste("Casos completos somente em indicadores aplicados:",ano))
}

reg <- readRDS("resultados/regressoes/modelos_logisticos.rds")
verificar(identical(reg$preditores,c("sexo","raca_grupo","quartil_renda")) && length(reg$modelos)==10L,
  "Dez modelos estimados com sexo, raça e quartil de renda")
verificar(isTRUE(all.equal(reg$cortes_renda,f$cortes_renda,tolerance=1e-10)),
  "Regressões usam os mesmos cortes ponderados das frequências, antes da seleção dos casos")
verificar(!is.ordered(d$quartil_renda) && identical(levels(d$quartil_renda),paste0("Q",1:4)),
  "Renda é fator nominal com quatro níveis; sem contraste polinomial ou escore contínuo")
verificar(all(reg$diagnosticos$status=="OK") && all(reg$diagnosticos$convergiu),
  "Todos os modelos convergiram sem alertas de instabilidade")
for (nome in names(reg$modelos)) {
 mod <- reg$modelos[[nome]]
 termos <- attr(terms(mod),"term.labels")
 verificar(identical(termos,c("sexo","raca_grupo","quartil_renda")) && length(coef(mod))==6L,
   paste("Fórmula contém intercepto, sexo, raça e quartil de renda:",nome))
 partes <- strsplit(nome,"_",fixed=TRUE)[[1]]
 ano <- as.integer(partes[1]); periodo <- partes[2]
 spec <- paste(partes[-c(1,2)],collapse="_")
 var_y <- paste0("inseguranca_",periodo,if(spec=="sem_neutros") "_sem_neutros" else "")
 da <- d[d$ano_rodada==ano,]
 completo <- complete.cases(da[c(var_y,"sexo","raca_grupo","quartil_renda")])
 verificar(nrow(model.frame(mod))==sum(completo),
   paste("Amostra exige renda válida e independe de idade e desordem:",nome))
 verificar(identical(mod$xlevels$sexo,c("Homem","Mulher")) &&
   identical(mod$xlevels$raca_grupo,c("Brancos","Negros")) &&
   identical(mod$xlevels$quartil_renda,paste0("Q",1:4)),
   paste("Referências Homem, Brancos e Q1:",nome))
 # Equações de escore da verossimilhança logística ponderada.
 X <- model.matrix(mod)
 pesos <- as.numeric(weights(mod,type="prior"))
 score <- colSums(X*(model.response(model.frame(mod))-fitted(mod))*pesos)
 verificar(max(abs(score))/sum(pesos)<1e-7,
   paste("Coeficientes satisfazem escore logístico ponderado:",nome))
 z <- reg$coeficientes |> filter(ano_rodada==ano,.data$periodo==.env$periodo,
   especificacao==spec,!referencia)
 idx <- match(z$termo,names(coef(mod)))
 ep <- sqrt(diag(vcov(mod)))[idx]
 verificar(max(abs(z$OR-exp(coef(mod)[idx])))<1e-10 &&
   max(abs(z$erro_padrao-ep))<1e-10 &&
   max(abs(z$limite_menos_1EP-exp(z$beta-ep)))<1e-10 &&
   max(abs(z$limite_mais_1EP-exp(z$beta+ep)))<1e-10 &&
   max(abs(z$erro_padrao_OR_delta-z$OR*ep))<1e-10,
   paste("OR, EP do desenho e barras exp(beta ± EP) conferidos:",nome))
}
refs <- reg$coeficientes |> filter(referencia)
verificar(all(refs$OR==1) && all(is.na(refs$erro_padrao)) &&
  all(is.na(refs$limite_menos_1EP)), "Referências sem incerteza artificial")
plots <- list.files("resultados/regressoes/graficos",full.names=TRUE)
verificar(all(file.exists(file.path("resultados/regressoes/graficos",
  c("odds_sexo.png","odds_sexo.pdf","odds_raca.png","odds_raca.pdf","odds_renda.png","odds_renda.pdf")))),
  "Três gráficos de OR exportados em PNG e PDF")
verificar(all(file.info(plots)$size > 1000),
  "Arquivos dos gráficos de OR têm conteúdo, sem exportações vazias")
for (arquivo in plots[grepl("[.]png$",plots)]) {
 verificar(identical(readBin(arquivo,what="raw",n=8),
   as.raw(c(137,80,78,71,13,10,26,10))),
   paste("Assinatura PNG válida:",basename(arquivo)))
}
renda_plot <- ler_csv("resultados/regressoes/odds_quartil_renda.csv")
verificar(nrow(renda_plot)==18L && setequal(renda_plot$categoria,c("Q2","Q3","Q4")) &&
  all(renda_plot$especificacao=="principal"),
  "Gráfico de renda contém 18 OR: três quartis versus Q1, em seis modelos ajustados")
html <- paste(readLines("resultados/frequencias/indice_graficos.html",warn=FALSE),collapse="\n")
verificar(!grepl("—",html,fixed=TRUE), "Relatório usa hífen no lugar do travessão")
verificar(all(!cores_frequencia$raca_grupo %in% c("#2980b9","#c0392b")), "Raça usa cores diferentes de vermelho e azul")
writeLines(c(paste("Validação:",format(Sys.time(),"%d/%m/%Y %H:%M:%S %Z")), verificacoes), "resultados/validacao/verificacoes.txt",useBytes=TRUE)
message(length(verificacoes), " verificações estatísticas e estruturais concluídas sem falhas.")
