#===============================================================================
# PREPARAÇÃO DE DADOS BH SURVEY (2002, 2005, 2008)
# Executar a partir da raiz do projeto seguranca_rmbh.
#===============================================================================
# install.packages(c('dplyr', 'haven', 'readr'))
library(dplyr)
library(haven)
library(readr)

arquivos <- file.path("dados", "brutos", paste0("PRMBH_", c(2002, 2005, 2008), ".sav"))
if (!all(file.exists(arquivos))) {
  stop("Arquivos ausentes: ", paste(arquivos[!file.exists(arquivos)], collapse = ", "))
}
svy_2002 <- read_sav(arquivos[[1]])
svy_2005 <- read_sav(arquivos[[2]])
svy_2008 <- read_sav(arquivos[[3]])

# Os nomes originais do SPSS são usados para evitar confundir questionários.
coluna <- function(d, nome) {
  if (!nome %in% names(d)) stop("Variável ausente: ", nome)
  haven::zap_labels(d[[nome]])
}
codigo <- function(d, nome) suppressWarnings(as.numeric(coluna(d, nome)))
identificador <- function(d, nome) {
  x <- sub("\\.0$", "", as.character(coluna(d, nome)))
  x[is.na(x) | trimws(x) == ""] <- NA_character_
  x
}
traduzir <- function(x, mapa, faltantes = c(0, 7, 8, 9, 77, 88, 99)) {
  inesperados <- setdiff(unique(x[!is.na(x)]), c(as.numeric(names(mapa)), faltantes))
  if (length(inesperados)) stop("Código não previsto: ", paste(inesperados, collapse = ", "))
  unname(mapa[match(as.character(x), names(mapa))])
}
municipio <- function(d, nome) {
  x <- as.character(haven::as_factor(d[[nome]], levels = "labels"))
  x[x %in% c("SI", "NR", "NA", "NS")] <- NA_character_
  x[!is.na(x) & x == "Bruamdinho"] <- "Brumadinho" # erro no rótulo do SPSS 2002
  x
}

sexo <- c(`1` = "Homem", `2` = "Mulher")
seg02 <- c(`1` = "Muito seguro(a)", `2` = "Seguro(a)",
           `3` = "Inseguro(a)", `4` = "Muito inseguro(a)")
seg05 <- c(`1` = "Muito seguro(a)", `2` = "Seguro(a)",
           `3` = "Nem seguro(a) nem inseguro(a)",
           `4` = "Inseguro(a)", `5` = "Muito inseguro(a)")
civil02 <- c(`1` = "Solteiro(a)", `2` = "Casado(a)", `3` = "Casado(a)",
             `4` = "Casado(a)", `5` = "União consensual",
             `6` = "Separado(a) / divorciado(a)", `7` = "Viúvo(a)")
civil05 <- c(civil02[names(civil02) != "7"],
             `7` = "Separado(a) / divorciado(a)", `8` = "Viúvo(a)")
escola02 <- c(`1` = "Ensino fundamental", `2` = "Ensino médio",
              `3` = "Ensino fundamental", `4` = "Ensino médio",
              `5` = "Curso técnico", `6` = "Ensino superior",
              `7` = "Alfabetização de adultos", `8` = "Pré-vestibular",
              `9` = "Especialização", `10` = "Mestrado ou doutorado")
escola05 <- c(escola02[names(escola02) %in% as.character(1:6)],
              `7` = "Curso sequencial", `8` = "Alfabetização de adultos",
              `9` = "Pré-vestibular", `10` = "Especialização",
              `11` = "Mestrado ou doutorado")
escola08 <- c(escola02[names(escola02) %in% as.character(1:6)],
              `7` = "Curso sequencial", `8` = "Ensino superior de tecnologia",
              `9` = "Educação de jovens e adultos", `10` = "Pré-vestibular",
              `11` = "Normal superior", `12` = "Especialização",
              `13` = "Mestrado ou doutorado")
raca02a <- c(`1` = "Branco(a)", `2` = "Preto(a)", `3` = "Pardo(a)",
             `4` = "Amarelo(a)", `5` = "Indígena", `6` = "Negro(a)",
             `7` = "Moreno(a)", `8` = "Pálido(a)")
raca02b <- c(raca02a[names(raca02a) %in% as.character(1:6)],
             `7` = "Pardo(a)", `8` = "Claro(a)",
             `9` = "Moreno(a) claro", `10` = "Moreno(a) escuro")
raca05 <- c(raca02a[names(raca02a) %in% as.character(1:5)], `6` = "Outra")
ocorre <- c(`1` = "Sim", `2` = "Não")
freq <- c(`1` = "Frequentemente", `2` = "Algumas vezes")
gravidade <- c(`1` = "Muito grave", `2` = "Grave",
               `3` = "Pouco grave", `4` = "Não é problema")
procura <- c(`1` = "Órgão da prefeitura",
             `2` = "Liderança comunitária ou associação local",
             `3` = "Vereador ou outro político", `4` = "Polícia",
             `5` = "Pessoa influente", `6` = "Parente ou vizinho",
             `7` = "Resolve por conta própria", `8` = "Não procura ninguém",
             `9` = "Outro")

# Em 2002, RC4A é aplicado no questionário A e RC4B no B.
# A categoria "moreno" permanece separada de "pardo".
grau_frequentado <- function(d, atual, anterior, cursa, mapa) {
  a <- codigo(d, atual)
  b <- codigo(d, anterior)
  e <- codigo(d, cursa)
  a_valido <- !is.na(traduzir(a, mapa))
  escolhido <- ifelse(!is.na(e) & e == 1 & a_valido, a, b)
  traduzir(escolhido, mapa)
}

df_2002 <- tibble(
  ano_rodada = 2002L, id_linha = seq_len(nrow(svy_2002)),
  nquest = identificador(svy_2002, "F5"), id_individuo = NA_character_,
  tipo_questionario = traduzir(codigo(svy_2002, "F4"), c(`1` = "A", `2` = "B", `3` = "C")),
  setor_censitario = identificador(svy_2002, "F3"),
  municipio = municipio(svy_2002, "F2"), bairro = NA_character_,
  distrito = identificador(svy_2002, "DISTRITO"),
  regional = identificador(svy_2002, "REGIONAL"),
  fator_expansao = codigo(svy_2002, "FATOREXP"),
  peso_pos_estratificacao = codigo(svy_2002, "PESO"),
  sexo = traduzir(codigo(svy_2002, "E1"), sexo),
  idade = replace(codigo(svy_2002, "E2"), codigo(svy_2002, "E2") %in% c(0, 777), NA_real_),
  raca_cor = coalesce(traduzir(codigo(svy_2002, "RC4A"), raca02a),
                      traduzir(codigo(svy_2002, "RC4B"), raca02b)),
  escolaridade_grau_frequentado = grau_frequentado(svy_2002, "E7", "E12", "E6", escola02),
  estado_civil = traduzir(codigo(svy_2002, "E4"), civil02,
                         faltantes = c(0, 77, 88, 99)),
  andar_dia = traduzir(codigo(svy_2002, "C1"), seg02),
  andar_noite = traduzir(codigo(svy_2002, "C2"), seg02),
  desordem_meninos_de_rua_gravidade = traduzir(codigo(svy_2002, "C3.A"), gravidade),
  desordem_gangues_gravidade = traduzir(codigo(svy_2002, "C3.B"), gravidade),
  desordem_trafico_drogas_gravidade = traduzir(codigo(svy_2002, "C3.C"), gravidade)
)

# 2005: módulo C4 (ocorrência, frequência e primeira pessoa/instituição procurada).
# 2008: módulo DS1/DS2/DS3, com prostituição adicional.
itens_2005 <- c(barulho = "1", pichacoes = "2", pessoas_em_situacao_de_rua = "3",
                arruaca = "4", vandalismo = "5", gangues = "6", trafico_drogas = "7")
itens_2008 <- c(barulho = "A", pichacoes = "B", pessoas_em_situacao_de_rua = "C",
                arruaca = "D", vandalismo = "E", prostituicao = "F",
                gangues = "G", trafico_drogas = "H")
adicionar_desordem <- function(df, d, ano) {
  itens <- if (ano == 2005) itens_2005 else itens_2008
  for (item in names(itens)) {
    s <- unname(itens[[item]])
    fontes <- if (ano == 2005) paste0("C4", c("A", "B", "C"), s) else
      paste0("DS", 1:3, s)
    pre <- paste0("desordem_", item, "_")
    df[[paste0(pre, "ocorrencia")]] <- traduzir(codigo(d, fontes[[1]]), ocorre)
    df[[paste0(pre, "frequencia")]] <- traduzir(codigo(d, fontes[[2]]), freq)
    # Nesta questão, 7, 8 e 9 são categorias substantivas.
    df[[paste0(pre, "procura")]] <- traduzir(codigo(d, fontes[[3]]), procura,
                                            faltantes = c(0, 77, 88, 99))
  }
  df
}

df_2005 <- tibble(
  ano_rodada = 2005L, id_linha = seq_len(nrow(svy_2005)),
  nquest = identificador(svy_2005, "NQUEST"), id_individuo = NA_character_,
  tipo_questionario = traduzir(codigo(svy_2005, "TIPOQUESTIONARIO"),
                               c(`1` = "A", `2` = "B")),
  setor_censitario = identificador(svy_2005, "NºSETOR"),
  municipio = municipio(svy_2005, "CIDADE"),
  bairro = identificador(svy_2005, "BAIRRO"),
  distrito = identificador(svy_2005, "distrito2"),
  regional = identificador(svy_2005, "REGIONAL"),
  fator_expansao = codigo(svy_2005, "fatorexp"),
  peso_pos_estratificacao = codigo(svy_2005, "pesofinal"),
  sexo = traduzir(codigo(svy_2005, "I1"), sexo),
  idade = replace(codigo(svy_2005, "I13"), codigo(svy_2005, "I13") %in% c(0, 99), NA_real_),
  raca_cor = traduzir(codigo(svy_2005, "RC4"), raca05),
  escolaridade_grau_frequentado = grau_frequentado(svy_2005, "I5", "I10", "I4", escola05),
  estado_civil = traduzir(codigo(svy_2005, "I2"), civil05,
                         faltantes = c(0, 77, 88, 99)),
  andar_dia = traduzir(codigo(svy_2005, "C1"), seg05),
  andar_noite = traduzir(codigo(svy_2005, "C2"), seg05)
) |>
  adicionar_desordem(svy_2005, 2005)

df_2008 <- tibble(
  ano_rodada = 2008L, id_linha = seq_len(nrow(svy_2008)),
  nquest = identificador(svy_2008, "F2"),
  id_individuo = identificador(svy_2008, "ID"),
  tipo_questionario = NA_character_,
  setor_censitario = identificador(svy_2008, "F6"),
  municipio = municipio(svy_2008, "F3"),
  bairro = identificador(svy_2008, "F4"),
  distrito = identificador(svy_2008, "F5"), regional = NA_character_,
  fator_expansao = codigo(svy_2008, "fatorexp"),
  peso_pos_estratificacao = codigo(svy_2008, "pesofinal"),
  sexo = traduzir(codigo(svy_2008, "CD1"), sexo),
  idade = replace(codigo(svy_2008, "CD13"), codigo(svy_2008, "CD13") %in% c(0, 99), NA_real_),
  raca_cor = traduzir(codigo(svy_2008, "RC17"), raca05),
  escolaridade_grau_frequentado = grau_frequentado(svy_2008, "CD5", "CD10", "CD4", escola08),
  estado_civil = traduzir(codigo(svy_2008, "CD2"), civil05,
                         faltantes = c(0, 77, 88, 99)),
  andar_dia = traduzir(codigo(svy_2008, "CS16"), seg05),
  andar_noite = traduzir(codigo(svy_2008, "CS17"), seg05)
) |>
  adicionar_desordem(svy_2008, 2008)

bh_svy <- bind_rows(df_2002, df_2005, df_2008) |>
  select(ano_rodada, id_linha, nquest, id_individuo, tipo_questionario,
         setor_censitario, municipio, bairro, distrito, regional,
         fator_expansao, peso_pos_estratificacao, sexo, idade, raca_cor,
         escolaridade_grau_frequentado, estado_civil, andar_dia, andar_noite,
         starts_with("desordem_"))

stopifnot(identical(as.integer(table(bh_svy$ano_rodada)),
                    as.integer(c(nrow(svy_2002), nrow(svy_2005), nrow(svy_2008)))))
stopifnot(!anyDuplicated(bh_svy[c("ano_rodada", "id_linha")]))
dir.create(file.path("dados", "tratados"), recursive = TRUE, showWarnings = FALSE)
saveRDS(bh_svy, file.path("dados", "tratados", "bh_svy_2002_2005_2008.rds"))
readr::write_csv(bh_svy, file.path("dados", "tratados", "bh_svy_2002_2005_2008.csv"), na = "")
message("Base salva: ", nrow(bh_svy), " entrevistas; ", ncol(bh_svy), " variáveis.")
