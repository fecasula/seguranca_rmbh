# A incerteza de um coeficiente é seu erro padrão (EP), não o desvio padrão
# das observações. As barras solicitadas usam exp(beta - EP), exp(beta + EP).
plotar_odds <- function(coeficientes, pasta) {
  dados <- coeficientes |>
    filter(especificacao == "principal", !referencia,
      variavel %in% c("sexo", "raca_grupo", "quartil_renda")) |>
    mutate(Periodo = factor(periodo, levels = c("noite", "dia"),
      labels = c("Noite", "Dia")),
      contraste = case_when(variavel == "sexo" ~ "Mulher / Homem",
        variavel == "raca_grupo" ~ "Negros / Brancos",
        TRUE ~ paste(categoria, "/ Q1")))
  salvar_csv(dados, file.path(pasta, "odds_graficos.csv"))
  for (v in c("sexo", "raca_grupo")) {
    z <- filter(dados, variavel == v)
    nome <- if (v == "sexo") "sexo" else "raca"
    cor <- if (v == "sexo") "#495867" else "#148f77"
    p <- ggplot(z, aes(x = OR, y = Periodo)) +
      geom_vline(xintercept = 1, linetype = "dashed", color = "#858e96") +
      geom_errorbarh(aes(xmin = limite_menos_1EP, xmax = limite_mais_1EP),
        height = .13, linewidth = .9, color = cor) +
      geom_point(size = 3.4, color = cor) +
      geom_text(aes(label = formatC(OR, digits = 2, format = "f", decimal.mark = ",")), vjust = -1.05, size = 3.7,
        color = "#263238") +
      facet_wrap(~ano_rodada, nrow = 1) +
      scale_x_log10(expand = expansion(mult = c(.18, .18)),
        labels = function(x) format(x, decimal.mark = ",", trim = TRUE)) +
      scale_y_discrete(expand = expansion(add = .65)) +
      labs(title = paste("Razão de chances de insegurança por", if (v == "sexo") "sexo" else "raça"),
        subtitle = paste(unique(z$contraste), "- ajustada por", if (v == "sexo") "raça e quartil de renda" else "sexo e quartil de renda"),
        x = "Odds ratio (OR) - escala logarítmica", y = NULL,
        caption = paste("Barras: ±1 erro padrão de log(OR), transformado em OR = exp(beta ± EP). Não são IC95%.",
          "\nLinha tracejada: OR = 1. Modelos ponderados por rodada; erro padrão pelo desenho amostral.")) +
      tema_grafico() + theme(legend.position = "none", panel.grid.minor = element_blank(),
        plot.caption = element_text(size = 10, hjust = 0))
    salvar_grafico(p, file.path(pasta, "graficos"), paste0("odds_", nome),
      altura = 5, largura = 12)
  }
  # Três comparações com Q1; efeito de renda ajustado por sexo e raça.
  z <- filter(dados, variavel == "quartil_renda") |>
    mutate(Contraste = factor(contraste, levels = c("Q4 / Q1", "Q3 / Q1", "Q2 / Q1")),
      Periodo = factor(periodo, levels = c("dia", "noite"), labels = c("Dia", "Noite")))
  salvar_csv(z, file.path(pasta, "odds_quartil_renda.csv"))
  p <- ggplot(z, aes(x = OR, y = Contraste, color = categoria)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "#858e96") +
    geom_errorbarh(aes(xmin = limite_menos_1EP, xmax = limite_mais_1EP),
      height = .15, linewidth = .9) +
    geom_point(size = 3.2) +
    geom_text(aes(label = formatC(OR, digits = 2, format = "f", decimal.mark = ",")),
      vjust = -1.05, size = 3.5, color = "#263238") +
    facet_grid(Periodo ~ ano_rodada) +
    scale_color_manual(values = cores_frequencia$quartil_renda) +
    scale_x_log10(expand = expansion(mult = c(.16, .20)),
      labels = function(x) format(x, decimal.mark = ",", trim = TRUE)) +
    scale_y_discrete(expand = expansion(add = .7)) +
    labs(title = "Razão de chances de insegurança por quartil de renda",
      subtitle = "Q2, Q3 e Q4 em relação a Q1 - ajustadas por sexo e raça",
      x = "Odds ratio (OR) - escala logarítmica", y = NULL,
      caption = paste("Barras: ±1 erro padrão de log(OR), transformado em OR = exp(beta ± EP). Não são IC95%.",
        "\nQ1 = menores rendas; cortes ponderados por rodada. Linha tracejada: OR = 1.")) +
    tema_grafico() + theme(legend.position = "none", panel.grid.minor = element_blank(),
      plot.caption = element_text(size = 10, hjust = 0),
      panel.spacing.x = grid::unit(1.2, "lines"))
  salvar_grafico(p, file.path(pasta, "graficos"), "odds_renda", altura = 7.2, largura = 12)
  invisible(dados)
}
