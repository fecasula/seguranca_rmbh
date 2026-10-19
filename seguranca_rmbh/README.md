# Segurança e desordem social na RMBH

Projeto ampliado para as rodadas 2002, 2005 e 2008 da PRMBH/BH Survey.
A base preparada possui 2.893 entrevistas: 1.029, 1.122 e 742, respectivamente.

## Como executar

Extraia todo o ZIP e abra `seguranca_rmbh.Rproj` no RStudio.
Instale as dependências uma vez, no console:

```r
install.packages(c("dplyr", "haven", "readr", "survey", "ggplot2"))
```

Execute as duas análises:

```r
source("executar_analises.R")
```

Cada etapa também pode ser executada separadamente, na raiz do projeto:

```r
source("R/preparar_dados.R")         # opcional: refaz a base a partir dos SAV
source("R/analise_frequencia.R")
source("R/regressoes_logisticas.R")
```

`R/regrecoes_logisticas.R`, nome existente no projeto original, continua
funcionando como um atalho para o novo script com a grafia corrigida.

## Resultados incluídos

Os resultados já calculados estão em `resultados/`. Abra no navegador:

- `resultados/frequencias/indice_graficos.html`: gráficos nos dois grupos pedidos.
- `resultados/regressoes/tabela_modelos.html`: tabela de OR ajustados, IC95%,
  valores de p, análises de sensibilidade e diagnósticos.

Gráficos em **PNG e PDF**. Tabelas CSV com separador `;`, decimal `,` e BOM UTF-8
para uso no Excel em português. RDS preservam resultados e objetos de modelos.

### Frequências

- Município: Belo Horizonte, Contagem, Betim, Outros; cada categoria por sexo.
- Sexo: comparação geral dentro de cada rodada.
- Grupos etários: 18–19, 20–29, 30–39 etc.; cada grupo por sexo.
- Raça/cor: respostas originais preservadas; cada categoria por sexo.
- Escolaridade: fator nominal com `levels` explícitos; cada categoria por sexo.
- Estado civil: cada categoria por sexo.
- Ocorrência de desordem social: Sim/Não por sexo, nas rodadas aplicáveis.

Há distribuições completas das respostas de segurança em CSV, além dos gráficos
do percentual agregado de insegurança. Dois gráficos adicionais de respostas
originais por sexo mostram também a categoria neutra.

**Marcas:** `*` = diferença por sexo com p ajustado BH < 0,05; `†` = diferença
apenas pelo p nominal < 0,05; `?` = teste indisponível. Ausência de marca, com
teste disponível, não demonstra igualdade. Pontos claros indicam menos de
cinco respostas válidas. CSV de testes e ausências documentam suporte e falhas.

### Regressões

Seis modelos principais: dia e noite em cada rodada. Incluem simultaneamente
sexo, município, idade (OR por dez anos), raça/cor, grau frequentado agrupado e
estado civil. Quatro modelos adicionais retiram respostas neutras de 2005/2008.
Nenhum modelo combina as rodadas. Desordem social não entra nesta etapa inicial.

As tabelas apresentam referências, OR, IC95%, p dos coeficientes, testes globais
por preditor e n de entrevistas. Há fluxo das exclusões por dados ausentes e
contagens dos eventos por categoria.

### Validação

`validacao/validar_resultados.R` confere denominadores e percentuais contra
cálculos independentes, soma das frequências, IC, ajuste BH, ausência estrutural
de itens, recodificações e correspondência dos OR/IC/p com os objetos estimados.
O registro fica em `resultados/validacao/verificacoes.txt`. Para repetir:

```r
source("validacao/validar_resultados.R")
```

## Organização

| Arquivo/pasta | Conteúdo |
|---|---|
| `dados/brutos/` | SAV originais |
| `dados/tratados/` | Base harmonizada de referência (CSV e RDS) |
| `doc/` | Questionários e `metodologia_analises.md` |
| `R/preparar_dados.R` | Preparação de referência |
| `R/funcoes_analise.R` | Recodificações, desenho, estimadores e exportação |
| `R/analise_frequencia.R` | Frequências, testes e gráficos |
| `R/regressoes_logisticas.R` | Modelos e tabelas |
| `executar_analises.R` | Execução das análises |
| `resultados/` | Saídas já geradas |

## Escolhas metodológicas

Leia `doc/metodologia_analises.md`. A análise usa o peso final de
pós-estratificação e conglomerados por setor. A base não identifica estrato
oficial: município e regional não foram presumidos como estratos. O desenho
de trabalho está em `R/funcoes_analise.R`; revise-o com o plano amostral original
antes de tratar a inferência como definitiva.

Insegurança reúne **Inseguro(a) + Muito inseguro(a)**; as outras respostas
válidas, inclusive neutras, compõem o complemento. Ausências não recebem zero.
Ausência estrutural de desordem em 2002 não vira “Não”.

O `renv.lock` original registra somente `renv`, sem as dependências analíticas.
Portanto, `renv::restore()` sozinho não instala os pacotes desta ampliação.
Use a instalação acima; para atualizar o lockfile no seu ambiente, execute
`renv::snapshot()` após instalar os pacotes.

A distribuição omite metadados Git, histórico de console e bibliotecas binárias
Windows do ambiente `renv`; preserva scripts, dados, questionários e configurações.
