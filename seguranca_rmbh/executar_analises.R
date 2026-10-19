# Execute a partir da raiz do projeto seguranca_rmbh:
# source("executar_analises.R")
# As duas análises também podem ser executadas separadamente.
if (!file.exists("seguranca_rmbh.Rproj")) {
  stop("Abra seguranca_rmbh.Rproj ou defina setwd() para a raiz do projeto.")
}
if (!file.exists("dados/tratados/bh_svy_2002_2005_2008.rds")) {
  source("R/preparar_dados.R", local = new.env(parent = globalenv()))
}
source("R/analise_frequencia.R", local = new.env(parent = globalenv()))
source("R/regressoes_logisticas.R", local = new.env(parent = globalenv()))
message("Concluído: consulte a pasta resultados/ e o README.md.")
