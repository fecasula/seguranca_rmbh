# Especificação das análises - frequências, covariância e regressões

As análises incluem frequências, matrizes de covariância e regressões de
insegurança ao andar na rua. As regressões incorporam desordem social nas
rodadas que mediram a ocorrência dos problemas no bairro.

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

O executor atualiza as frequências, as matrizes de covariância, as regressões
e os relatórios HTML. A base harmonizada e os arquivos brutos são preservados.
Os resultados são apresentados separadamente para 2002, 2005 e 2008.

## Desenho amostral e denominadores

O código declara um objeto amostral por rodada. A construção de `upa` e
`peso_analise` e a chamada de `survey` equivalem a:

```r
d$upa <- interaction(d$ano_rodada, d$municipio, d$setor_censitario,
                     drop = TRUE, lex.order = TRUE)
d$peso_analise <- as.numeric(d$peso_pos_estratificacao)
survey::svydesign(ids = ~upa, weights = ~peso_analise, data = d, nest = TRUE)
```

| Argumento | Variável e origem |
|---|---|
| `ids = ~upa` | `upa` é a interação de `ano_rodada`, `municipio` e `setor_censitario`. Os campos de município/setor nos arquivos originais são `F2`/`F3` em 2002, `CIDADE`/`NºSETOR` em 2005 e `F3`/`F6` em 2008. |
| `weights = ~peso_analise` | `peso_analise` é o valor numérico de `peso_pos_estratificacao`, originado de `PESO` em 2002 e `pesofinal` em 2005/2008. O código exige pesos finitos e positivos. |
| `data = d`, `nest = TRUE` | Dados de uma única rodada, com a opção de aninhamento ativada. |

A especificação atual omite `strata` e `fpc` pela falta de identificação dos
estratos oficiais e da correção para população finita. Município compõe o
identificador do setor. O fator de expansão fica fora da declaração e a soma
dos pesos representa o denominador ponderado de trabalho. As rodadas são
tratadas como cortes transversais. A inferência depende da adequação desse
desenho de trabalho ao plano amostral original.

O objeto de cada rodada é criado antes da seleção dos domínios e dos casos
completos. Cada percentual é a soma dos pesos dos eventos dividida pela soma
dos pesos das respostas válidas dentro do grupo e da rodada. São exportados
n válido, n de eventos, somas de pesos, proporções, erros padrão e IC95%.
Para proporções de 0% ou 100%, publica-se o ponto e o IC fica ausente.

| Função do pacote `survey` | Uso |
|---|---|
| `svydesign()` | Declara o desenho de cada rodada. |
| `svymean()` e `SE()` | Estimam a média ponderada do indicador binário (a proporção) e extraem seu erro padrão. |
| `svyciprop(method = "logit")` | Estima o IC95% de proporções internas a (0, 1). |
| `svychisq(statistic = "F")` | Testa a associação entre grupo e resposta com correção de Rao–Scott de segunda ordem. |
| `degf()` | Fornece graus de liberdade dos desenhos usados nos IC de proporções e nos diagnósticos e verifica se há graus de liberdade suficientes para os testes de associação. |
| `svyquantile(qrule = "math", ci = FALSE)` | Estima P25, P50 e P75 de renda para definir os quartis. |
| `svyvar()` | Estima as matrizes de covariância ponderadas e a incerteza pelo desenho. |
| `svyglm(family = quasibinomial(link = "logit"))` | Ajusta as regressões logísticas. Os métodos `coef()` e `vcov()` extraem coeficientes e variância pelo desenho para o cálculo de OR, EP, IC95% e p nominal. |
| `regTermTest(method = "Wald")` | Faz o teste global de cada preditor, com os graus de liberdade residuais do modelo. |

## Raça/cor

| Grupo analítico | Respostas originais incluídas |
|---|---|
| Brancos | Branco(a) |
| Negros | Preto(a) e Pardo(a) |

As demais respostas ficam fora das análises que usam raça/cor, incluindo
Amarelo(a), Indígena, a opção original Negro(a) da rodada de 2002 e as
não respostas. A regra usa os rótulos originais Preto(a) e Pardo(a) para
compor Negros.

`raca_grupo` é um fator com Brancos como categoria de referência. A regra
vale para as frequências por raça/cor, as matrizes de covariância e todos os
modelos de regressão. As frequências por sexo, idade e renda mantêm seus
próprios critérios de elegibilidade. `recodificacao_raca.csv` registra a
destinação de cada rótulo por rodada e `elegibilidade_por_recorte.csv`
registra as exclusões por recorte.

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

### Como o questionário mede desordem social

Em 2002, a pergunta C3 do módulo Polícia e Criminalidade leu uma lista de
problemas da vizinhança e pediu uma avaliação de gravidade para cada um.
As opções eram muito grave, grave, pouco grave ou não é um problema.
Entre os itens estavam meninos de rua, gangues e tráfico de drogas. A resposta
mede gravidade percebida e fica fora dos indicadores de ocorrência.

| Etapa | 2005 | 2008 |
|---|---|---|
| Lista de ocorrências no bairro | C4a. Cartão de resposta, p. 6. Ler as opções 1 a 7 e permitir múltiplas marcações. | DS1. Cartão de resposta, p. 12. Ler as opções a a h e permitir múltiplas marcações. |
| Frequência de cada problema mencionado | C4b. “Frequentemente” ou “Algumas vezes”. | DS2. “Frequentemente” ou “Às vezes”. |
| Primeiro recurso para resolver cada problema mencionado | C4c. Cartão de resposta, p. 7. Ler as opções 1 a 8 e registrar uma única resposta. | DS3. Cartão de resposta, p. 13. Ler as opções 1 a 8 e registrar uma única resposta. |

O enunciado de ocorrência e o fluxo das perguntas são equivalentes nas duas
rodadas. A lista de 2008 acrescentou “Pessoas se prostituindo na rua” na
posição f. Os itens compartilhados são vizinhos ou festas barulhentas,
pichações, pessoas em situação de rua, adolescentes fazendo arruaça, danos
a propriedades, gangues e tráfico de drogas. Em 2008, a redação de danos a
propriedades cita estragos em prédios, carros e casas, e o item de gangues
acrescenta exemplos de atividades. Tráfico de drogas é o item 7 de C4a em
2005 (`C4A7`) e o item h de DS1 em 2008 (`DS1H`).

Os indicadores analisados vêm de C4a e DS1. A categoria 1 é “Sim”/“Sim
(mencionou)” e a categoria 2 é “Não”/“Não mencionou”, conforme a rodada.
Os códigos de não resposta e não sabe ficam ausentes. Os indicadores medem
o que a pessoa relata sobre o bairro. As respostas de frequência e de
procura de ajuda ficam fora do cálculo de ocorrência e da contagem de itens.

Fontes no repositório:

- [Questionário de 2002, módulo Polícia e Criminalidade](Question%C3%A1rio%20Final%202002.zip)
- [Questionário de 2005, perguntas C4a–C4c](Question%C3%A1rio%20A%202005%20FINAL_FOR%20TRANSLATION.doc)
- [Questionário de 2008, módulo 4, páginas impressas 13–14](<Questionário PRMBH 2008 CORRIGIDO (1).pdf#page=13>)

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

## Regressões logísticas de insegurança

Os modelos são estimados separadamente para insegurança de dia e à noite,
em cada rodada. As especificações são:

| Especificação | Rodadas | Preditores |
|---|---|---|
| Principal | 2002 | Sexo, raça/cor e quartil de renda. |
| Principal | 2005 e 2008 | Sexo, raça/cor, quartil de renda e relato de tráfico de drogas. |
| Sensibilidade sem respostas neutras | 2005 e 2008 | Mesmos preditores do principal, com as respostas neutras excluídas do desfecho. |
| Total de itens de desordem social | 2005 e 2008 | Sexo, raça/cor, quartil de renda e número de itens de ocorrência relatados. |

Isso produz seis ajustes principais, quatro de sensibilidade e quatro com
a contagem. Em 2002 o questionário avaliou gravidade e não teve resposta
neutra de insegurança.

O indicador de tráfico (`desordem_trafico_drogas_ocorrencia`) entra como fator
binário, com “Não” ou “Não mencionou” como referência. A OR de “Sim” compara
quem relatou o problema no bairro com quem não o relatou, mantendo sexo,
raça/cor e quartil de renda fixos.

No modelo adicional, `total_itens_desordem` soma um ponto para cada ocorrência
relatada e zero para cada resposta “Não” ou “Não mencionou”. Todos os itens
aplicados precisam ter uma resposta válida. Em 2005 a escala completa vai de
0 a 7, e em 2008 vai de 0 a 8, incluindo prostituição. Tráfico é um dos
componentes da contagem e fica representado dentro dela. A diferença de
composição entre as duas listas limita a comparação direta entre rodadas.

A contagem entra como preditor numérico. Sua OR, `exp(beta)`, é o fator
multiplicativo na chance de declarar insegurança por item adicional
relatado, mantendo sexo, raça/cor e quartil de renda fixos. O modelo supõe
a mesma variação na log-chance por incremento unitário ao longo da escala.
Cada item contribui igualmente para a contagem.

Todos os modelos têm intercepto, ajuste mútuo e termos principais sem
interação. As referências sociodemográficas são Homem, Brancos e Q1. O
quartil de renda é um fator nominal. Os cortes são ponderados antes da seleção
de casos completos e são os mesmos das frequências. Q2, Q3 e Q4 produzem três
contrastes com Q1. O teste global de Wald de renda avalia os três contrastes
em conjunto. Os valores de p dos coeficientes avaliam cada contraste com Q1.

Cada ajuste usa casos completos no desfecho e nos seus preditores, como
subpopulação do desenho da rodada. O fluxo de perdas é exportado por
especificação. O modelo da contagem exige mais respostas de ocorrência que
o modelo com tráfico isolado, de modo que os tamanhos de amostra podem
diferir. As variâncias tratam a classificação de renda como fixa.

`svyglm(..., family = quasibinomial(link = "logit"))` usa os pesos e a
variância pelo desenho de trabalho. OR = exp(beta). Os IC95% e valores de p
usam a distribuição t e os graus de liberdade residuais do modelo. As marcas
de regressão são nominais, sem ajuste de multiplicidade. A OR representa uma
razão de chances. As associações são observacionais e não permitem concluir
causalidade.

Os diagnósticos registram convergência, avisos, n, eventos, probabilidades
extremas, coeficientes e erros padrão muito grandes. Esses sinais auxiliam a
identificar instabilidade e separação. Modelos sinalizados ficam sem marcas
de significância.

### Gráficos de odds ratios e dispersão

As três figuras do modelo principal mostram Mulher/Homem, Negros/Brancos e
Q2/Q3/Q4 em relação a Q1, ajustadas pelos demais preditores de cada rodada.
Em 2005 e 2008 esse ajuste inclui relato de tráfico de drogas. As OR de
tráfico são apresentadas nas tabelas. O gráfico do total de desordem mostra
as quatro OR por item adicional e seus IC95%, por rodada e período.
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
