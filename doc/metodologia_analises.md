# Metodologia - segurança e desordem social na RMBH

## Rodadas e desenho amostral

As rodadas de 2002, 2005 e 2008 são analisadas separadamente, sem presumir acompanhamento dos mesmos indivíduos. A base harmonizada é construída em `R/preparar_dados.R`; o desenho é declarado em `R/funcoes_analise.R`.

| Componente | 2002: variável SPSS | 2005: variável SPSS | 2008: variável SPSS | Variável harmonizada |
|---|---|---|---|---|
| Peso final | `PESO` | `pesofinal` | `pesofinal` | `peso_pos_estratificacao`, convertido em `peso_analise` |
| Município | `F2` | `CIDADE` | `F3` | `municipio` |
| Setor censitário | `F3` | `NºSETOR` | `F6` | `setor_censitario` |
| Rodada | Ano do arquivo | Ano do arquivo | Ano do arquivo | `ano_rodada` |
| Fator de expansão disponível | `FATOREXP` | `fatorexp` | `fatorexp` | `fator_expansao`, não utilizado como peso |

A UPA de trabalho é criada por:

```r
d$upa <- interaction(d$ano_rodada, d$municipio, d$setor_censitario,
                     drop = TRUE, lex.order = TRUE)
survey::svydesign(ids = ~upa, weights = ~peso_analise, data = d, nest = TRUE)
```

Município e rodada compõem o identificador do setor para evitar colisões de códigos. Município não é tratado como estrato. `config$estratos = character(0)`: não é enviado argumento `strata`. Também não são informados `fpc` ou probabilidades dos demais estágios, pois não foram identificados no material disponível. `nest = TRUE` não cria estratos. A pós-estratificação incorporada ao peso final não equivale a declarar estratos amostrais.

Trata-se de um desenho de trabalho que considera os pesos finais e a dependência por setor, sem afirmar reproduzir integralmente o plano original. Não se multiplica o peso final pelo fator de expansão, nem se interpreta sua soma como total populacional. O desenho é criado com toda a rodada antes da seleção de domínios e casos completos, feita por `des[ok, ]`, preservando as informações do desenho original.

## Funções do pacote survey e indicadores

| Etapa | Função e configuração utilizadas | Resultado |
|---|---|---|
| Desenho | `survey::svydesign(ids = ~upa, weights = ~peso_analise, data = d, nest = TRUE)` | Objeto amostral por rodada |
| Proporções | `survey::svymean(~indicador, des)` e `survey::SE()` | Média de indicador 0/1 e erro padrão; percentual = 100 × média |
| IC das proporções | `survey::svyciprop(~indicador, des, method = "logit", level = .95, df = survey::degf(des))`, seguido de `confint()` | IC95% logit entre respostas válidas |
| Associação entre grupos | `survey::svychisq(~resposta_teste + grupo_teste, design = des, statistic = "F")` | Rao-Scott de segunda ordem; global para idade e renda |
| Quartis da renda | `survey::svyquantile(..., quantiles = c(.25,.5,.75), qrule = "math", ci = FALSE)` | Cortes ponderados específicos da rodada |
| Covariâncias | `survey::svyvar(formula, dm)` | Covariâncias descritivas ponderadas e seus erros padrão |
| Regressão logística | `survey::svyglm(formula, design = dm, family = quasibinomial(link = "logit"))` | Coeficientes ajustados e variâncias pelo desenho |
| Testes de preditores | `survey::regTermTest(modelo, reformulate(v), method = "Wald", df = modelo$df.residual)` | Wald global por preditor |
| Graus de liberdade | `survey::degf(des)` | Informação utilizada em intervalos, testes e diagnósticos |

`coef()`, `vcov()` e `confint()` são genéricos do R com métodos para os objetos estimados. Nas regressões, EP = `sqrt(diag(vcov(modelo)))`, OR = `exp(beta)`, IC95% = `exp(beta ± qt(.975, df.residual) × EP)` e p bilateral pela distribuição t com os mesmos graus de liberdade residuais.

Cada percentual é a soma dos pesos dos eventos dividida pela soma dos pesos das respostas válidas dentro do grupo e da rodada. São exportados n válido, n de eventos, somas dos pesos, estimativa, EP e IC. Ausências não viram zero. Proporções exatamente 0 ou 1 mantêm o ponto e deixam o IC logit ausente, com status explícito.

## Raça/cor

| Grupo analítico | Respostas originais incluídas |
|---|---|
| Brancos | Somente `Branco(a)` |
| Negros | `Preto(a)` e `Pardo(a)` |
| Excluídas das análises com raça | `Amarelo(a)`, `Indígena`, demais respostas e ausências |

A inclusão se restringe aos três rótulos originais indicados. A resposta original separada `Negro(a)`, presente em 2002, também fica fora: não é convertida em preto ou pardo. As demais opções incluem Moreno(a), Claro(a), Pálido(a) e Outra.

A variável `raca_grupo` é fator nominal Brancos/Negros, com Brancos como referência. A recodificação é compartilhada por frequências, covariâncias e regressões. Nas frequências, a exclusão afeta apenas o recorte por raça; os mesmos entrevistados podem integrar as análises por sexo, idade e renda. Nas matrizes e regressões, raça válida é requisito porque integra essas análises. As exclusões constam de `recodificacao_raca.csv` e `elegibilidade_por_recorte.csv`.

## Insegurança ao andar na rua

Insegurança = Inseguro(a) ou Muito inseguro(a), codificada como 1; as demais respostas válidas recebem 0. Em 2002, `C1`/`C2` têm quatro categorias; em 2005, `C1`/`C2`, e em 2008, `CS16`/`CS17`, incluem resposta neutra. Nos modelos principais, neutros integram o complemento; em 2005/2008 há sensibilidade que os exclui. Não respostas permanecem ausentes.

## Comparação dos questionários de desordem social

Foram comparados `Questionário A 2005 FINAL_FOR TRANSLATION.doc` e `Questionário PRMBH 2008 CORRIGIDO (1).pdf`, disponíveis em `doc/`. Em 2005, C4a/C4b/C4c integram o módulo 3 (Criminalidade). Em 2008, DS1/DS2/DS3 formam o módulo 4 (Desordem Social), páginas impressas 13-14. As referências C.R. p.6 e p.12 se referem aos cartões de resposta.

| Aspecto | 2005 | 2008 | Implicação |
|---|---|---|---|
| Ocorrência | C4a: ler itens 1-7 e marcar uma ou mais opções | DS1: ler itens a-h e marcar uma ou mais opções | Mesmo enunciado central sobre quais problemas ocorrem no bairro |
| Respostas da ocorrência | 1 = Sim; 2 = Não | 1 = Sim (mencionou); 2 = Não mencionou | A negativa de 2008 explicita não menção; mede relato/percepção, não ocorrência objetiva |
| Frequência | C4b: 1 = Frequentemente; 2 = Algumas vezes | DS2: 1 = Frequentemente; 2 = Às vezes | Mesma estrutura de duas categorias; pequena mudança no rótulo |
| Fluxo | C4b e C4c somente para cada problema mencionado | DS2 e DS3 somente para cada problema mencionado | Frequência e procura são condicionais ao relato; salto não é frequência zero |
| Lista de problemas | Sete itens | Oito, acrescentando prostituição na rua no item f | Prostituição só é estimada em 2008 |
| Vandalismo | Item 5: vandalismos e danos intencionais a propriedades | Item e: quebra-quebra ou estragos em prédios, carros e casas da vizinhança | Harmonizado como Vandalismo, com redação mais concreta em 2008 |
| Gangues | Item 6: presença de gangues | Item g: acrescenta explicação sobre adolescentes envolvidos em brigas, venda de drogas e pequenos furtos | Ampliação explicativa pode influenciar respostas |
| Tráfico | Item 7: `C4A7` (ocorrência), `C4B7` (frequência) | Item h: `DS1H` (ocorrência), `DS2H` (frequência) | Mesmo rótulo do problema, em posições/contextos diferentes na lista |

Os demais itens comuns são vizinhos ou festas barulhentas, pichações, pessoas em situação de rua (rótulo analítico para o item original) e adolescentes fazendo arruaça. Nas duas rodadas, ocorrência usa 7 = NR e 9 = NS. Frequência usa 7 = NS, 8 = NA e 9 = NR. Esses códigos ficam ausentes. C4c/DS3 perguntam a primeira pessoa ou instituição procurada para resolver o problema, uma medida distinta de ocorrência e frequência.

As análises usam `_ocorrencia`, não `_frequencia`. Sim representa relato do problema; Não é a categoria negativa harmonizada e, em 2008, significa não menção. NR/NS não são ausência do problema. O enunciado central não fixa janela temporal: trata-se da percepção sobre o bairro, não de vitimização pessoal. As diferenças de lista e redação impedem presumir equivalência perfeita entre rodadas.

Em 2002, não existe essa lista de ocorrência. Há perguntas de gravidade sobre meninos de rua, gangues e tráfico (`C3.A`, `C3.B`, `C3.C`). Gravidade não é convertida em ocorrência ou em ausência de tráfico.

## Renda e idade

Renda individual registrada: 2002 T9A/T9B + T16 + T21; 2005 T10 + T24; 2008 T10 + T16. T9A/T9B são alternativas da mesma pergunta, não rendas a somar; respostas válidas discordantes produzem ausência da renda principal. Zero por salto exige ausência explícita da fonte. Componentes aplicáveis desconhecidos mantêm a soma ausente. T16/2002 admite valor recebido ou a receber e as fontes variam entre rodadas: não há mensuração perfeitamente uniforme de todos os rendimentos, nem renda domiciliar per capita.

Cortes ponderados são calculados em toda renda válida da rodada antes de selecionar sexo, raça ou casos completos. Q1: renda <= P25; Q2: P25 < renda <= P50; Q3: P50 < renda <= P75; Q4: renda > P75. Empates são preservados, podendo gerar grupos diferentes de 25%. Cortes não são recalculados nos modelos e sua incerteza não é propagada nas análises condicionais aos quartis. Quartis expressam posição relativa no ano, não poder de compra entre anos. Componentes e cortes são exportados.

Idade é agrupada em 18-34, 35-59 e 60+, correspondendo a [18,35), [35,60) e [60,+infinito). Idade ausente não é uma categoria. Os percentuais por raça, idade e renda não são ajustados por sexo.

## Matrizes de covariância: seção 4 do relatório

São 17 indicadores 0/1 na mesma ordem em todas as rodadas: insegurança de dia/noite, Mulher, 35-59, 60+, Q2, Q3, Q4, Negros e os oito itens de ocorrência de desordem. Referências: Homem, 18-34, Q1, Brancos e Não. Estado civil não integra a matriz.

Usa-se uma única amostra completa em todos os indicadores aplicáveis da rodada, sem exclusão par a par. Os oito itens de ocorrência em 2002 e prostituição em 2005 são não aplicados: ficam NA, em cinza, e não excluem entrevistas nessas rodadas. Há sete itens disponíveis em 2005 e oito em 2008; as matrizes diferem em composição e amostra.

`svyvar` estima covariâncias descritivas ponderadas e seus EP, com correção n/(n-1). Não se trata de `vcov(svymean)`, correlação ou efeito ajustado. A diagonal contém variâncias. Indicadores mutuamente exclusivos podem ter covariância negativa pela codificação. CSV/RDS registram n, aplicação e EP.

## Regressões logísticas

Modelos por ano e período (dia/noite), com ajuste mútuo e sem interação:

```r
# 2005 e 2008
inseguranca ~ sexo + raca_grupo + quartil_renda + desordem_trafico_drogas_ocorrencia

# 2002: ocorrência não medida
inseguranca ~ sexo + raca_grupo + quartil_renda
```

Referências: Homem, Brancos, Q1 e Não para relato de tráfico. Renda é fator nominal e produz três contrastes com Q1; não se impõe tendência linear. Tráfico é fator Não/Sim, de C4A7 (2005) e DS1H (2008), sem usar frequência do problema.

A amostra exige desfecho e todos os preditores da fórmula válidos. Em 2005/2008, relato desconhecido de tráfico exclui a entrevista; em 2002, a ausência estrutural não exclui ninguém. Idade, estado civil, escolaridade e demais itens de desordem não integram a fórmula ou a seleção. São seis modelos principais e quatro de sensibilidade sem neutros (2005/2008). As OR de 2002 não possuem o mesmo conjunto de ajustes das de 2005/2008; não se interpreta a diferença entre elas como teste formal de mudança de efeito.

São exportados fluxo de perdas, testes globais, convergência, avisos, n, eventos, parâmetros, probabilidades extremas e alertas de instabilidade. Modelos sinalizados não recebem marcas de significância. Não há imputação, seleção automática por p, interação ou interpretação causal. OR é razão de chances, não razão de prevalências; insegurança à noite é frequente. Os p dos coeficientes são nominais, sem ajuste de multiplicidade.

Quatro figuras mostram Mulher/Homem, Negros/Brancos, Q2-Q4/Q1 e relato de tráfico Sim/Não; a última inclui somente 2005/2008. Barras = `exp(beta ± EP)` no eixo logarítmico: ±1 erro padrão de log(OR), não IC95% ou desvio padrão das respostas. Tabelas incluem EP da OR por delta (`OR × EP`), IC95%, p e n. Referências têm OR=1 por definição, sem EP artificial.

## Testes entre grupos, exportações e execução

Rao-Scott compara dois grupos para sexo/raça e é global para idade/renda, sem identificar pares específicos. Não se deduz significância pela sobreposição de IC. O teste é indisponível com menos de cinco respostas válidas em algum grupo, sem variação da resposta, poucas UPAs ou falha numérica.

Benjamini-Hochberg (`p.adjust(method = "BH")`) é aplicado por rodada × bloco × recorte. Para sexo, preserva-se a família da primeira versão somente quando p e contagens coincidem com a referência auditável; caso contrário, usa-se a família atual. * indica p ajustado < 0,05; †, somente p nominal < 0,05; ?, teste indisponível. Ausência de marca não demonstra igualdade.

Tabelas retráteis permitem busca, ordenação, CSV/TSV/JSON, cópia e impressão. O Excel é atualizado com as tabelas e recebe assinatura dos CSV, para impedir oferecer versão desatualizada. Os brutos e a base harmonizada são preservados. O executor da raiz é a entrada atual; a pasta aninhada `seguranca_rmbh/` contém uma cópia histórica anterior. `sessionInfo.txt` registra as versões usadas.

## Documentação técnica primária

- [svydesign](https://search.r-project.org/CRAN/refmans/survey/html/svydesign.html)
- [svymean, svyvar e SE](https://search.r-project.org/CRAN/refmans/survey/html/surveysummary.html)
- [svyciprop](https://search.r-project.org/CRAN/refmans/survey/html/svyciprop.html)
- [svychisq](https://search.r-project.org/CRAN/refmans/survey/html/svychisq.html)
- [svyquantile](https://search.r-project.org/CRAN/refmans/survey/html/svyquantile.html)
- [svyglm](https://search.r-project.org/CRAN/refmans/survey/html/svyglm.html)
- [regTermTest](https://search.r-project.org/CRAN/refmans/survey/html/regTermTest.html)
