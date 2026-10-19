# Segurança e desordem social na RMBH - sexo, raça e renda

Esta versão inclui quartil de renda na regressão logística, junto com sexo e
raça, e acrescenta um gráfico das OR de Q2, Q3 e Q4 em relação a Q1, ajustadas
por sexo e raça. Os gráficos de sexo e raça também são reestimados com renda.
As frequências e matrizes de covariância da versão anterior são preservadas.

## Gráficos

| Recorte | Insegurança ao andar na rua | Percepção de desordem social |
|---|---|---|
| Sexo | Um painel com dia e noite; percentuais e marcações dos anexos preservados | Gráfico dos oito itens por sexo mantido |
| Raça | Um painel com dia e noite para Brancos e Negros, sem divisão por sexo | Gráfico dos itens para Brancos e Negros, sem divisão por sexo |
| Grupo etário | Um painel com dia e noite para 18–34, 35–59 e 60+, sem divisão por sexo | Gráfico dos itens pelos três grupos, sem divisão por sexo |
| Quartil de renda | Um painel com dia e noite por Q1-Q4, sem divisão por sexo | Gráfico dos itens por Q1-Q4, sem divisão por sexo |

Os resultados são apresentados por rodada (2002, 2005, 2008), com pesos,
IC95% e os testes entre os grupos. Ocorrência de desordem foi medida apenas
em 2005/2008; prostituição foi medida apenas em 2008.

### Agrupamentos solicitados

- **Negros:** respostas originais preto(a), pardo(a) e indígena.
- **Brancos:** respostas originais branco(a) e amarelo(a).
- Todas as demais opções de raça são excluídas **apenas do recorte por raça**.
  Isso inclui a resposta original “Negro(a)”, que não consta da lista indicada
  para inclusão. Não respostas também permanecem excluídas desse recorte.
- **Idade:** 18–34 (18 ≤ idade < 35), 35–59 (35 ≤ idade < 60), 60+ (idade ≥ 60).

As análises de raça, idade e renda não fazem cruzamento ou ajuste por sexo. Pessoas com
raça excluída continuam elegíveis nas análises por idade/sexo. O agrupamento
racial é uma definição operacional específica desta análise.

## Renda e quartis

A renda individual registrada é recuperada dos SPSS originais:

- 2002: T9A/T9B (trabalho principal) + T16 (segundo trabalho em casa) + T21 (outras fontes).
- 2005: T10 (trabalho principal) + T24 (outras fontes).
- 2008: T10 (trabalho principal) + T16 (outras fontes).

Só há zero por salto quando o formulário indica ausência daquela fonte.
NR/NS/NA e componentes desconhecidos não viram zero. Quartis são ponderados
pelo peso final e calculados **por rodada**, usando a renda válida, antes dos
recortes de sexo/raça/idade. Q1 é a menor renda; Q4, a maior. Empates são
preservados, portanto os grupos não precisam conter exatamente 25%.
Cortes e disponibilidade estão no relatório e em `cortes_quartis_renda.csv`.

## Matriz de covariância

Há uma matriz para cada rodada, entre insegurança de dia/noite e indicadores
0/1 de sexo, idade, renda, raça e os oito itens de ocorrência de desordem social.
Estado civil foi retirado. Referências: Homem, 18-34, Q1, Brancos e Não (ocorrência).
As 17 linhas/colunas têm a mesma ordem nos três anos. Cada bloco estimável usa
uma única amostra completa nos indicadores aplicados naquela rodada.

Em 2002, ocorrência não foi aplicada: as oito linhas/colunas correspondentes
ficam ausentes. Em 2005, apenas prostituição não foi aplicada; em 2008, os oito
itens foram aplicados. Células cinza ou vazias significam não aplicado, nunca zero.
Os itens não aplicados não removem entrevistas. N, aplicação e codificação estão
no relatório. CSV, RDS e mapas de cores ficam em `covariancia/`.

## Regressões e odds ratios

Modelos ponderados por ano e período (dia/noite), com a fórmula:

```r
inseguranca ~ sexo + raca_grupo + quartil_renda
```

São seis modelos principais e quatro de sensibilidade sem respostas neutras
(2005/2008). Sexo, raça e renda ajustam-se mutuamente, sem interação. Referências:
Homem, Brancos e Q1. A raça usa a mesma definição das frequências. A amostra de cada
modelo exige desfecho, sexo, raça e renda válidos; não exige idade ou
desordem preenchidas. Quartil é fator nominal, sem supor tendência linear.
Os cortes ponderados por rodada são os mesmos das frequências, calculados
antes de selecionar os casos completos do modelo. Renda ausente exclui o caso
do modelo; não se atribuem zero ou um quartil à renda desconhecida. O desenho mantém peso final e agrupamento por setor.

Três gráficos em `resultados/regressoes/graficos/` comparam Mulher/Homem,
Negros/Brancos e os quartis Q2/Q3/Q4 em relação a Q1, por ano e período.
Cada OR é controlada pelos outros dois preditores. `odds_renda.png` e
`odds_renda.pdf` reúnem os resultados de renda em um painel de dia/noite. Pontos são OR ajustadas; barras
são `exp(beta ± EP)`, com EP pelo desenho amostral. Trata-se de ±1 erro padrão
de log(OR), não de desvio padrão das respostas nem de IC95%. O EP da OR por
aproximação delta (`OR * EP`) também é exportado. IC95% e p nominal ficam nas
tabelas retráteis; não se deve usar essas barras para concluir significância a 5%.

## Abrir os resultados

Abra `resultados/frequencias/indice_graficos.html` no navegador para acessar
os oito gráficos. Eles também estão em PNG e PDF em:

- `resultados/frequencias/graficos/1_seguranca_sociodemograficas/`
- `resultados/frequencias/graficos/2_desordem_social/`

Cada gráfico tem uma tabela retrátil, com busca e ordenação por coluna.
Botões CSV, TSV, JSON, copiar e imprimir/PDF exportam as linhas visíveis na
ordem atual. A planilha `exportacoes/tabelas_resultados.xlsx` reúne os resultados
desta execução nas abas Frequências, Covariâncias, Renda e Regressões.
Se os dados forem recalculados e mudarem, o relatório oculta o link do Excel
anterior para evitar uma exportação desatualizada; CSV/TSV/JSON continuam atuais.

Tabelas CSV (separador `;`, decimal `,`, UTF-8) incluem percentuais,
intervalos, denominadores, testes, recodificação da raça e exclusões por recorte.
`resultados_frequencias.rds` preserva as tabelas; `base_frequencias.rds` contém
uma cópia com as duas novas variáveis, sem substituir a base de referência.

`resultados/regressoes/tabela_modelos.html` apresenta as tabelas exportáveis, os três gráficos e os diagnósticos dos novos modelos.

## Executar novamente

Abra `seguranca_rmbh.Rproj` no RStudio. Se necessário, instale os pacotes:

```r
install.packages(c("dplyr", "haven", "readr", "survey", "ggplot2"))
```

Execute:

```r
source("executar_analises.R")
# ou apenas:
source("R/analise_frequencia.R")
```

O executor roda frequências, covariâncias e regressões. Para executar apenas
os modelos, use `source("R/regressoes_logisticas.R")`. Os relatórios estão
prontos para consulta, sem necessidade de executar R. A planilha Excel é um
retrato exportado desta execução; os scripts R atualizam CSV e HTML.

## Significância e validação

`*` = p ajustado BH < 0,05; `†` = apenas p nominal < 0,05;
`?` = teste indisponível. Nas análises de idade e renda, o teste é global entre os três/quatro
grupos, não uma identificação dos pares que diferem.

Os novos recortes usam ajuste BH por rodada, bloco e recorte. Por sexo,
conservam-se os valores ajustados da primeira versão para reproduzir os anexos.
O script verifica p nominal e contagens antes de usar essa referência;
se não corresponderem, recalcula BH para a família atual.

Para verificar matrizes, modelos e a preservação das frequências:

```r
source("validacao/validar_resultados.R")
```

O relatório de verificação fica em `resultados/validacao/verificacoes.txt`.
Leia `doc/metodologia_analises.md` para os denominadores, desenho de trabalho,
regras de exclusão e testes. Não foram presumidos estratos oficiais.

O `renv.lock` foi preservado e registra somente `renv`; para as dependências
analíticas, use a instalação acima. Os anexos de referência estão em
`doc/referencias_graficos/`.
