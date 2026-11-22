# Segurança e desordem social na RMBH

Análises ponderadas das rodadas 2002, 2005 e 2008 do BH Survey: frequências, matrizes de covariância e regressões logísticas de insegurança ao andar na rua de dia e à noite.

A [metodologia](doc/metodologia_analises.md), também incluída nos dois relatórios HTML, apresenta as variáveis originais do desenho amostral, as funções do pacote survey, os denominadores e a comparação dos questionários.

## Raça/cor

| Grupo | Respostas originais incluídas |
|---|---|
| Brancos | Somente Branco(a) |
| Negros | Preto(a) e Pardo(a) |

Amarelos, indígenas, demais opções e ausências são excluídos das análises com raça. A resposta separada Negro(a), presente em 2002, também fica fora: não é convertida em preto/pardo. A regra é compartilhada por frequências, covariâncias e regressões. Pessoas com raça excluída continuam elegíveis nas frequências por sexo, idade e renda. As exclusões são exportadas; gráficos de raça usam roxo/verde.

## Modelos e questionários

Em 2005/2008:

```r
inseguranca ~ sexo + raca_grupo + quartil_renda + desordem_trafico_drogas_ocorrencia
```

Tráfico vem de C4A7/2005 e DS1H/2008. Referências: Homem, Brancos, Q1 e Não. Casos completos no desfecho e nesses preditores, ajuste mútuo e sem interação. Em 2002, ocorrência não foi medida: mantém-se sexo, raça e renda; gravidade não é usada como substituta. Os conjuntos de ajustes diferem entre anos.

São seis modelos principais e quatro de sensibilidade sem respostas neutras. Quatro gráficos apresentam as OR de sexo, raça, renda e relato de tráfico; este último apenas em 2005/2008. Barras são `exp(beta ± EP)`, **não IC95%**. Tabelas incluem EP, IC95%, p nominal, n, testes globais e diagnósticos. OR é razão de chances, não razão de prevalências ou efeito causal.

C4a/C4b de 2005 e DS1/DS2 de 2008 mantêm o enunciado central e a frequência condicionada ao problema mencionado. Há inclusão de prostituição em 2008, mudanças de redação em vandalismo/gangues e a explicitação de “Não mencionou” em DS1. A metodologia documenta essas diferenças e as implicações para as matrizes da seção 4.

## Desenho, renda e resultados

O desenho usa `survey::svydesign(ids = ~upa, weights = ~peso_analise, data = d, nest = TRUE)`, por rodada. Peso final: PESO/2002 e pesofinal/2005-2008. UPA: interação de rodada, município e setor. Não se presumem estratos oficiais/FPC, nem se multiplica peso final por fator de expansão.

Quartis de renda individual registrada são ponderados por rodada, definidos antes dos recortes e com empates preservados. Q1 contém as menores rendas. Ausências não recebem zero ou quartil. Não se trata de renda domiciliar per capita.

- `resultados/frequencias/indice_graficos.html`: oito gráficos de frequência, matrizes, regressões e metodologia.
- `resultados/regressoes/tabela_modelos.html`: quatro gráficos de OR, tabelas e diagnósticos.
- `docs/`: cópia sincronizada dos relatórios para o GitHub Pages.

Tabelas oferecem busca, ordenação, CSV, TSV, JSON, copiar e imprimir/PDF. Com openxlsx, o Excel é atualizado e recebe assinatura dos CSV. Os brutos e a base harmonizada são preservados.

## Executar e validar

Use a **raiz do repositório** e abra `seguranca_rmbh.Rproj`. A pasta aninhada `seguranca_rmbh/` é uma cópia histórica anterior.

```r
install.packages(c("dplyr", "haven", "readr", "survey", "ggplot2", "openxlsx"))
source("executar_analises.R")
source("validacao/validar_resultados.R")
```

O executor atualiza frequências, covariâncias, modelos e as cópias em docs/. O renv.lock existente registra somente renv; use a instalação explícita acima. Para atualizar o fragmento metodológico e o README HTML:

```sh
pandoc doc/metodologia_analises.md --from=gfm --to=html5 --output=doc/metodologia_analises.html
pandoc README.md --from=gfm --to=html5 --standalone --metadata title="Segurança e desordem social na RMBH" --output=README.html
```

`sessionInfo.txt` registra as versões usadas; `resultados/validacao/verificacoes.txt`, as verificações estatísticas e estruturais.
