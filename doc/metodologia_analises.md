# Especificação das análises - frequências, covariância e regressões

## Ampliação com renda, exportações e covariância

Esta revisão mantém as frequências por sexo, raça, idade e renda. Na matriz,
estado civil dá lugar aos indicadores de ocorrência de desordem social. As
regressões são reestimadas com sexo, raça e quartil de renda, mantendo a
classificação racial binária solicitada. Há três gráficos de OR. Raça mantém roxo/verde, sem vermelho
ou azul; os textos usam hífen em lugar de travessão.

### Renda individual registrada

Fontes: 2002 T9A/T9B + T16 + T21; 2005 T10 + T24; 2008 T10 + T16.
T9A/T9B são modos alternativos da mesma pergunta, não rendas a somar. Se há
dois valores válidos discordantes, a renda principal permanece ausente.
O segundo trabalho em casa aparece separadamente só em 2002; o formulário
instrui perguntar um segundo trabalho se o principal já é domiciliar.
T16/2002 admite valor recebido **ou a receber**, uma diferença de mensuração
em relação às demais rodadas. As outras fontes também variam nos questionários;
portanto a soma é a renda individual registrada, não uma mensuração perfeita
ou uniforme de todos os rendimentos. Não se substitui pela renda domiciliar.

Os códigos especiais de cinco/seis dígitos e códigos rotulados SI/NR/NA/NS
são retirados. Zero por salto exige informação explícita de ausência da fonte:
no trabalho principal, ausência de ocupação remunerada segundo o fluxo;
no adicional domiciliar, T14=Não; nas outras fontes, Não em todas as perguntas.
Estágio com valor desconhecido não recebe zero. Se algum componente aplicável
é desconhecido, a soma fica ausente. `renda_entrevistados.csv` permite auditar
cada componente e a fonte; a base de referência da regressão não é sobrescrita.

Os percentis 25/50/75 ponderados são estimados dentro de cada rodada, com
`survey::svyquantile(..., qrule="math", ci=FALSE)`. Q1: renda <= P25;
Q2: P25 < renda <= P50; Q3: P50 < renda <= P75; Q4: renda > P75.
Mesma renda sempre fica no mesmo grupo; não se usam `ntile` ou posição da linha
para quebrar empates. Grupos podem ser desiguais ou, em outros dados, vazios.
Quartis são definidos com toda renda válida da rodada, sem seleção prévia
por sexo, raça, idade ou desfecho. Valores são nominais e não permitem comparar
poder de compra entre anos; os quartis medem posição relativa no ano.
Os IC de insegurança/desordem são condicionais à classificação resultante e
não propagam a incerteza dos pontos de corte dos quartis.

### Matriz de covariância

Inclui os dois desfechos binários, indicadores nominais 0/1 (K-1 categorias)
Mulher, 35-59, 60+, Q2, Q3, Q4 e Negros, e oito indicadores de ocorrência:
barulho, pichações, pessoas em situação de rua, arruaça, vandalismo, gangues,
tráfico de drogas e prostituição. Estado civil foi retirado. Referências:
Homem, 18-34, Q1, Brancos; para desordem, 1 = Sim, 0 = Não. Há 17 linhas/colunas,
na mesma ordem nas três rodadas. Não se atribuem escores ordinais a sexo ou raça.

A disponibilidade segue os questionários: nenhuma ocorrência em 2002, sete itens
em 2005 (sem prostituição) e oito em 2008. Gravidade em 2002 não é ocorrência.
Linhas/colunas de itens não aplicados são NA, em cinza no mapa. Esses itens não
participam da seleção de casos. Usa-se uma única amostra completa em ambos os
desfechos, sexo, idade, quartil, raça e todos os itens aplicados de cada rodada.
Assim, os blocos estimáveis têm o mesmo denominador dentro de cada ano; as
amostras e conjuntos de itens diferem entre anos. Não se usa exclusão par a par.
Ausências nos itens aplicados retiram o caso somente da matriz daquela rodada.

`survey::svyvar` calcula covariâncias descritivas ponderadas e seus erros padrão
pelo desenho. O ponto é a média ponderada dos produtos centrados, com correção
n/(n-1). Não é `vcov(svymean)` (covariância de estimadores), correlação ou efeito
ajustado. A diagonal contém variâncias. Categorias mutuamente exclusivas podem
ter covariâncias negativas pela própria codificação. CSV/RDS incluem codificação,
aplicação por ano, n e erro padrão das covariâncias.

### Tabelas e exportação

Cada figura tem uma tabela `<details>` inicialmente fechada. Busca e ordenação
operam sobre as linhas; CSV/TSV/JSON/cópia e impressão/PDF preservam apenas as
linhas visíveis e sua ordem. Valores brutos são conservados na exportação, com
maior precisão que a formatação visual. Percentuais/IC das frequências são
exportados em pontos percentuais, com n e valores de p. A planilha Excel é um
retrato desta execução; uma assinatura dos CSV impede oferecer um Excel
anterior quando o relatório for recalculado com resultados diferentes.

Referências técnicas primárias:

- https://r-survey.r-forge.r-project.org/pkgdown/docs/reference/svyquantile.html
- https://search.r-project.org/CRAN/refmans/survey/html/surveysummary.html

## Escopo e execução

Mantêm-se os oito gráficos anteriores de frequência e seus resultados. A base
harmonizada e os arquivos brutos são preservados. `preparar_frequencias` aplica
a mesma recodificação racial tanto às frequências quanto às regressões.
O executor roda os dois scripts analíticos e atualiza o relatório integrado.
As tabelas dos novos gráficos também são retráteis e exportáveis.

## Desenho de trabalho e denominadores

As rodadas de 2002, 2005 e 2008 são analisadas separadamente. Não se presume
identificação longitudinal dos indivíduos.

Peso: `peso_pos_estratificacao` (`PESO` em 2002, `pesofinal` em 2005/2008).
Não se multiplica pelo fator de expansão nem se interpreta sua soma como
total populacional. O desenho usa `survey::svydesign` com UPA identificada
por rodada × município × setor censitário. Município compõe a identificação
do setor, sem ser presumido como estrato. Sem estratos oficiais/FPC identificados,
trata-se de um desenho de trabalho com as informações disponíveis, a revisar
com o plano amostral original antes de inferência definitiva.

O desenho é criado antes de selecionar os domínios. Cada percentual é a
soma dos pesos dos eventos dividida pela soma dos pesos das respostas válidas
**dentro de seu grupo e rodada**. Ausências não viram zero. São exportados n
válido, n de eventos, somas de pesos, percentuais, erros padrão e IC95% logit.
Proporções de exatamente 0%/100% conservam o ponto, sem fabricar um IC degenerado.

## Raça: definição operacional solicitada

| Grupo analítico | Respostas originais incluídas |
|---|---|
| Negros | Preto(a), Pardo(a), Indígena |
| Brancos | Branco(a), Amarelo(a) |
| Excluídas do recorte racial | Todas as demais respostas, incluindo Negro(a), Moreno(a), Claro(a), Pálido(a), Outra, e ausências |

É o agrupamento indicado para esta análise, sem presumir equivalência com
outras classificações raciais. A opção original “Negro(a)” não foi incluída,
pois a lista especificada inclui apenas preto, pardo e indígena no grupo Negros.

A variável nova é `raca_grupo`, fator com níveis Brancos/Negros. Nas frequências, a exclusão
ocorre apenas no recorte por raça: não retira os mesmos entrevistados das
frequências por sexo ou idade. Nas matrizes e regressões, raça válida é requisito
de inclusão porque integra essas análises. `recodificacao_raca.csv` mostra a destinação
completa dos rótulos por ano; `elegibilidade_por_recorte.csv` registra exclusões.

## Grupo etário

`idade_grupo` é um fator com níveis 18–34, 35–59 e 60+. Intervalos:
[18,35), [35,60) e [60,+infinito). Idade ausente não é uma categoria.
Raça/cor não define a população elegível desse recorte.

Nos recortes de raça e idade, a população de cada grupo é agregada sem
estratificação, exclusão por sexo ou ajuste por sexo. Os gráficos só exibem
as respectivas categorias de raça ou de idade.

## Insegurança e desordem social

Insegurança = Inseguro(a) + Muito inseguro(a). O complemento contém as demais
respostas válidas, inclusive neutras em 2005/2008. Em 2002 não havia opção neutra.
Essa definição foi mantida. As categorias originais de segurança também ficam
registradas nas tabelas, sem gráficos adicionais.

Desordem usa somente `_ocorrencia` (Sim/Não). Os sete itens comuns em
2005/2008 são barulho, pichações, pessoas em situação de rua, arruaça,
vandalismo, gangues e tráfico de drogas. Prostituição aparece apenas em 2008.
Em 2008, “Não” pode significar “não mencionou” na lista; os dados medem relato
ou percepção. Gravidade em 2002 não foi convertida em ocorrência.

## Testes e marcas dos gráficos

Usa-se Rao–Scott de segunda ordem (`svychisq`, estatística F), comparando:

- Homem versus Mulher no recorte por sexo.
- Brancos versus Negros no recorte por raça.
- Globalmente 18–34, 35–59 e 60+ no recorte etário.

O teste etário detecta associação entre grupo e resposta; a marca não identifica
qual par difere, nem indica uma tendência monotônica. Não há comparações par a par.
Não se deduz significância pela sobreposição dos intervalos.

Testes ficam indisponíveis com menos de cinco respostas válidas em qualquer
grupo, sem variação do desfecho, poucas UPAs ou falha numérica. O limite mínimo
é uma regra operacional, não garantia assintótica em células raras.

Nos novos recortes, o ajuste Benjamini–Hochberg é feito por rodada × bloco ×
recorte. O bloco de insegurança inclui dia/noite; desordem inclui os itens
aplicáveis no ano. Os grupos etários compõem um único teste global por item.

Para reproduzir os gráficos de sexo anexados, conservam-se o ajuste BH e as
marcações da primeira versão, cuja família de segurança incluía também outros
recortes sociodemográficos. `referencia_testes_sexo_v1.csv` conserva os testes
originais para auditoria. O script só usa essa referência se p nominal e contagens
atuais corresponderem; caso contrário, usa o ajuste da família atual. A família
utilizada em cada teste está na coluna `familia_BH`.

`*` indica p ajustado < 0,05; `†` indica somente p nominal < 0,05;
`?` indica teste indisponível. Ausência de marca não demonstra igualdade.

## Regressões logísticas - sexo, raça e quartil de renda

Modelo por período e rodada:

```r
inseguranca ~ sexo + raca_grupo + quartil_renda
```

Sexo, raça e quartil de renda entram simultaneamente, sem interação; são os
únicos preditores. Referências: Homem, Brancos e Q1. Negros inclui pretos, pardos e indígenas; Brancos,
brancos e amarelos. Demais respostas raciais e não respostas ficam ausentes.
Quartil de renda é fator nominal com níveis Q1-Q4. Os cortes são ponderados por
rodada e definidos em toda a renda válida, antes de selecionar casos completos,
usando exatamente a função e as regras de renda das frequências. Não são
recalculados dentro da subamostra do modelo. Empates são preservados e renda
desconhecida não recebe quartil. Q2, Q3 e Q4 produzem três contrastes com Q1;
não se usa um escore contínuo 1-4. O Wald global de renda testa os três contrastes
conjuntamente, enquanto os p dos coeficientes avaliam cada comparação com Q1.

Cada modelo seleciona casos completos no desfecho, sexo, raça e quartil de renda,
dentro do desenho original da rodada. Idade, estado civil, município, escolaridade
e desordem não entram na fórmula nem filtram a amostra. O intercepto é mantido.
A exigência de renda válida pode reduzir a amostra em relação à versão anterior;
o fluxo de perdas e n de cada modelo são atualizados. A classificação da renda é
tratada como fixa; a variância não propaga a incerteza dos pontos de corte.

São seis modelos principais (três anos x dois períodos). Em 2005/2008 também se
estimam quatro modelos de sensibilidade excluindo neutros; 2002 não tinha neutros.

`svyglm(..., family = quasibinomial(link = "logit"))` usa os pesos amostrais
e variância por setor. Família quasi evita tratar pesos como sucessos inteiros
e mantém a interpretação logística. OR = exp(beta); IC e p usam os mesmos
graus de liberdade residuais do modelo amostral. Testes globais de Wald por
preditor ficam em CSV.

Não há ajuste de multiplicidade nos coeficientes de regressão desta etapa;
as marcas da tabela são nominais. OR é razão de chances, não razão de
prevalências. Insegurança à noite é frequente: OR não deve ser interpretada
como aproximação automática da razão de prevalências.

Diagnósticos registram convergência, avisos, n, parâmetros, eventos,
probabilidades extremas, coeficientes e erros padrão muito grandes. São
sinais de possível instabilidade/separação, não teste formal que descarte
todo tipo de separação. Modelos sinalizados não recebem marcas de
significância. Casos completos podem produzir seleção: há fluxo de perdas.
Não há imputação, seleção automática por p, interações ou desordem nessa
etapa inicial. Associações observacionais não estabelecem causalidade.

### Gráficos de odds ratios e dispersão

As três figuras mostram Mulher/Homem (ajustada por raça e renda), Negros/Brancos
(ajustada por sexo e renda) e Q2/Q3/Q4 em relação a Q1 (ajustadas por sexo e raça).
O painel de renda separa dia/noite em linhas e anos em colunas. O eixo OR é
logarítmico. OR = exp(beta); barras = [exp(beta - EP), exp(beta + EP)], com EP
robusto do desenho (`sqrt(diag(vcov(modelo)))`). A transformação mantém limites
positivos. A referência OR=1 é indicada pela linha tracejada.

EP é o desvio padrão estimado da distribuição amostral do coeficiente, não o
desvio padrão de respostas individuais. Não se estima um desvio padrão de
odds individuais a partir desses preditores. O pedido de barras de desvio
padrão é implementado como ±1 EP no logaritmo da OR, explicitamente identificado
nas figuras. Esses intervalos não têm cobertura de 95% nem constituem teste a 5%.
As tabelas exportam EP de log(OR), os dois limites das barras, EP da OR pelo
método delta (OR × EP de log(OR)), IC95%, p e n. Referências têm OR=1 por definição,
sem EP ou IC estimado. IC95% usam a distribuição t e os graus de liberdade
residuais do modelo; os p usam a mesma convenção.

## Referências técnicas

Documentação primária do pacote survey consultada para a implementação:

- [svydesign](https://r-survey.r-forge.r-project.org/pkgdown/docs/reference/svydesign.html)
- [svyciprop](https://r-survey.r-forge.r-project.org/pkgdown/docs/reference/svyciprop.html)
- [svychisq](https://r-survey.r-forge.r-project.org/pkgdown/docs/reference/svychisq.html)
- [svyglm](https://r-survey.r-forge.r-project.org/pkgdown/docs/reference/svyglm.html)

`sessionInfo.txt` em cada análise registra as versões usadas.
