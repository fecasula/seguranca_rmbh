# Segurança e desordem social na RMBH

Análises ponderadas da insegurança ao andar na rua e da desordem social
relatada no bairro, por rodada do survey (2002, 2005, 2008).

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

### Grupos de raça/cor e idade

- **Brancos** inclui somente a resposta Branco(a).
- **Negros** reúne as respostas Preto(a) e Pardo(a).
- As demais respostas, inclusive a opção original Negro(a) de 2002, Amarelo(a),
  Indígena e as não respostas, ficam fora das frequências por raça/cor, das
  matrizes de covariância e das regressões.
- Os grupos etários são 18–34, 35–59 e 60+. As frequências por sexo, idade e
  renda mantêm seus próprios critérios de elegibilidade.

Os recortes descritivos de raça/cor, idade e renda não são estratificados por
sexo nem ajustados por sexo. O desenho amostral, as funções de `survey` e a
comparação dos questionários estão na [metodologia](doc/metodologia_analises.md).

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

Os modelos ponderados são ajustados por rodada e período (dia/noite). O
modelo principal inclui sexo, raça/cor e quartil de renda. Em 2005 e 2008
inclui também se a pessoa relatou tráfico de drogas no bairro. Em 2002 a
pergunta mediu gravidade. Para 2005 e 2008 também há uma análise de
sensibilidade que exclui as respostas neutras de insegurança.

O modelo adicional usa o total de itens de desordem relatados, que já inclui
tráfico. O total vai de 0 a 7 em 2005 e de 0 a 8 em 2008, quando a lista
passou a incluir prostituição. Cada entrevista precisa ter resposta válida
para todos os itens da rodada. A OR indica a variação multiplicativa na
chance de declarar insegurança por item adicional relatado, com sexo,
raça/cor e quartil de renda fixos.

As referências são Homem, Brancos e Q1. Todos os ajustes exigem desfecho
e preditores válidos. Os quartis usam os mesmos cortes ponderados das
frequências. O desenho usa o peso final e o agrupamento por setor.

Os três gráficos em `resultados/regressoes/graficos/` mostram os contrastes
do modelo principal por sexo, raça/cor e quartil de renda. As barras mostram
`exp(beta ± EP)`, com EP pelo desenho amostral. As tabelas exportam o IC95% e
o p nominal, incluindo os resultados de tráfico e do total de itens.
O resumo do modelo de contagem também está em
[`tabela_modelos_total_desordem.csv`](resultados/regressoes/tabela_modelos_total_desordem.csv).

## Abrir os resultados

Abra [o relatório integrado](resultados/frequencias/indice_graficos.html) no navegador para acessar
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

## Atualizar um clone pelo RStudio

Abra `seguranca_rmbh.Rproj` no seu clone, na branch `main`. Para buscar o
script pela primeira vez, execute no Console:

```r
system2(Sys.which("git"), c("pull", "--ff-only", "origin", "main"))
```

Nas próximas atualizações, use `source("atualizar_projeto.R")`. O Git só
atualiza a branch por avanço direto e avisa se houver alterações locais
conflitantes. Para consultar a versão publicada, abra
[resultados/frequencias/indice_graficos.html](resultados/frequencias/indice_graficos.html).

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
Leia `doc/metodologia_analises.md` para os questionários, desenho amostral, funções de estimação,
denominadores e regras de exclusão. Não foram presumidos estratos oficiais.

O `renv.lock` foi preservado e registra somente `renv`; para as dependências
analíticas, use a instalação acima. Os anexos de referência estão em
`doc/referencias_graficos/`.
