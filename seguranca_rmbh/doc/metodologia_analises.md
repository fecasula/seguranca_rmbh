# Especificação das análises

## Rodadas e desenho de trabalho

As entrevistas adultas de 2002, 2005 e 2008 são analisadas separadamente.
O código não presume identificação longitudinal dos indivíduos: `nquest`
não é único entre rodadas e `id_individuo` está ausente em 2002/2005.
As análises são associações dentro de cada rodada, sem mudanças individuais.

O peso é `peso_pos_estratificacao`: `PESO` em 2002 e `pesofinal` em 2005/2008
no script de preparação. Não se multiplica esse peso pelo fator de expansão.
As somas dos pesos finais **não são interpretadas como totais populacionais**.

O desenho usa `survey::svydesign`, UPA identificada por rodada × município ×
setor censitário e variância pelo primeiro estágio. Município apenas compõe
a identificação da UPA para evitar colisões de códigos; não é estrato.
Não há FPC ou variável de estrato oficial identificada na base harmonizada.
Essa é uma aproximação ao desenho com as informações disponíveis.
Regional/distrito não são automaticamente estratos amostrais. Se o plano
original identificar estratos, acrescente-os à preparação e informe os nomes
em `config$estratos`.

Não respostas são excluídas do denominador específico de cada frequência;
nas regressões usa-se a subpopulação de casos completos. O desenho é criado
antes de selecionar subpopulações. Peso inválido ou município/setor ausente
interrompe a análise, sem substituição silenciosa.

## Variáveis sociodemográficas

| Variável | Frequências | Modelo inicial |
|---|---|---|
| Município | Belo Horizonte, Contagem, Betim, Outros | Mesmo agrupamento; referência Belo Horizonte |
| Sexo | Homem, Mulher | Referência Homem |
| Idade | 18–19, 20–29, 30–39 etc. | Contínua / 10; OR por dez anos |
| Raça/cor | Rótulos originais | Branca, preta, parda, amarela, indígena, outras respostas; referência branca |
| Escolaridade | Grau frequentado, fator nominal com levels explícitos | Quatro grupos abaixo; referência fundamental/alfabetização/EJA |
| Estado civil | Casado, solteiro, união consensual, separado/divorciado, viúvo | Mesmas categorias; referência casado |

18–19 é a parte observada do primeiro intervalo decenal da amostra adulta.
90–99 tem apenas duas entrevistas: idade contínua evita um coeficiente com
suporte extremamente pequeno. O modelo inicial supõe linearidade da idade
no logito; essa hipótese não é verificada por este ajuste.

Na raça/cor, “negro”, “moreno”, “claro” e “pálido” de 2002 são preservados
nas frequências. No modelo ficam em “Outras respostas”, junto de “Outra”,
sem inferir equivalência com preto/pardo. Esse grupo é operacional, não uma
categoria racial substantiva homogênea.

Agrupamento do grau frequentado no modelo:

1. Fundamental / alfabetização / EJA: ensino fundamental, alfabetização de
   adultos e educação de jovens e adultos.
2. Médio / técnico / pré-vestibular: ensino médio, curso técnico e pré-vestibular.
3. Superior / sequencial / normal: ensino superior, superior de tecnologia,
   curso sequencial e normal superior.
4. Pós-graduação: especialização, mestrado ou doutorado.

São opções operacionais para reduzir células raras. A preparação não distingue
conclusão nem preserva série/anos de estudo; não se interpreta como escolaridade
concluída. Escolaridade ausente não é classificada como “sem instrução”.

## Insegurança e respostas originais

Desfecho principal: 1 para “Inseguro(a)” ou “Muito inseguro(a)”; 0 para as demais
respostas válidas. O complemento é **demais respostas**, pois inclui neutros
em 2005/2008; não é correto chamar todo o complemento de “seguro”. Em 2002
não havia resposta neutra.

São exportadas também frequências das quatro/cinco respostas originais por
variável sociodemográfica e sexo. Dois gráficos adicionais mostram essas
distribuições por sexo e ano. Suas marcas correspondem ao teste global da
distribuição, não ao teste de uma única resposta.

Regressões de sensibilidade removem respostas neutras de 2005/2008 e repetem
os mesmos preditores. Isso avalia dependência da definição binária e altera
a população de respostas válidas; não elimina a diferença de questionário.

## Frequências, intervalos e testes por sexo

No domínio ano × categoria sociodemográfica × sexo, o percentual ponderado
é a soma dos pesos dos eventos / soma dos pesos das respostas válidas.
Exportam-se n de respostas, n de eventos, somas de pesos, proporção, erro
padrão e IC95% logit de `svyciprop`. Em 0%/100% o ponto é mantido e o IC
fica ausente, com motivo; não se fabrica um intervalo degenerado. Ausências
e domínios sem suporte não recebem percentual zero.

Comparações entre homens e mulheres dentro de cada categoria/ano usam
`svychisq(..., statistic = "F")`, correção de Rao–Scott de segunda ordem.
São testados o indicador binário de insegurança, a distribuição original
de segurança e a distribuição Sim/Não da desordem.

Testes ficam indisponíveis se há menos de cinco respostas válidas em pelo
menos um sexo, sem variação da resposta, poucas UPAs, ou falha numérica.
Não são substituídos por Fisher/qui-quadrado que ignore o desenho.
n=5 é uma regra de suporte mínimo, não garantia assintótica; verifique n e IC.

p nominal é conservado; ajuste Benjamini–Hochberg é aplicado por rodada/bloco:

- Todos os testes binários de segurança, reunindo dia/noite e os seis recortes.
- Todos os testes das distribuições originais de segurança.
- Todos os testes de ocorrência de desordem.

O ajuste usa os testes estimáveis. `*` indica p ajustado < 0,05; `†`, somente
p nominal < 0,05; `?`, teste indisponível. Testes por categoria não são testes
de interação sexo × variável sociodemográfica; diferença de significância
entre subgrupos não prova heterogeneidade do efeito.

## Ocorrência de desordem social

São usados os campos `_ocorrencia`, Sim/Não. As sete variáveis comuns de
2005/2008 são barulho, pichações, pessoas em situação de rua, arruaça,
vandalismo, gangues e tráfico de drogas. Prostituição aparece apenas em 2008.
No questionário de 2008, “Não” pode representar “não mencionou” na lista;
trata-se de relato/percepção, não registro objetivo ou incidência de eventos.

Em 2002 há gravidade para três itens, medida distinta que não é convertida
em ocorrência. Não aplicação consta em `disponibilidade_desordem_por_ano.csv`;
não respostas individuais entre rodadas aplicáveis ficam em outro arquivo.

## Regressões logísticas

Modelo inicial por período e rodada:

```r
inseguranca ~ sexo + municipio_grupo + idade_10 +
              raca_modelo + escolaridade_modelo + estado_civil
```

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

## Referências técnicas

Documentação primária do pacote survey consultada para a implementação:

- [svydesign](https://r-survey.r-forge.r-project.org/pkgdown/docs/reference/svydesign.html)
- [svyciprop](https://r-survey.r-forge.r-project.org/pkgdown/docs/reference/svyciprop.html)
- [svychisq](https://r-survey.r-forge.r-project.org/pkgdown/docs/reference/svychisq.html)
- [svyglm](https://r-survey.r-forge.r-project.org/pkgdown/docs/reference/svyglm.html)

`sessionInfo.txt` em cada análise registra as versões usadas.
