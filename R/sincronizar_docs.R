# Relatórios destinados à estrutura existente do GitHub Pages.
for (origem in c("resultados/frequencias", "resultados/regressoes")) {
  arquivos <- list.files(origem, pattern = "[.](html|png|pdf|xlsx)$", recursive = TRUE, full.names = TRUE)
  for (arquivo in arquivos) {
    destino <- sub("^resultados/", "docs/", arquivo)
    dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(arquivo, destino, overwrite = TRUE)) stop("Falha ao copiar: ", arquivo)
  }
}
message("Relatórios e gráficos sincronizados em docs/.")
