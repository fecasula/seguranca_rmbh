# No Console do RStudio, execute source("atualizar_projeto.R") na raiz do projeto.
local({
  git <- Sys.which("git")
  if (!nzchar(git)) stop("Instale o Git e reinicie o RStudio.", call. = FALSE)
  if (!file.exists("seguranca_rmbh.Rproj") || !file.exists(".git")) {
    stop("Abra o arquivo seguranca_rmbh.Rproj dentro do clone Git.", call. = FALSE)
  }

  branch <- system2(git, c("branch", "--show-current"), stdout = TRUE)
  if (!identical(branch, "main")) {
    stop("Abra a branch main no Git antes de atualizar.", call. = FALSE)
  }

  status <- system2(git, c("pull", "--ff-only", "origin", "main"))
  if (status != 0L) {
    stop("O Git não concluiu a atualização. Consulte a mensagem acima.", call. = FALSE)
  }
})
