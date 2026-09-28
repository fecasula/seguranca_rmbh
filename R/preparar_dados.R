#=============================================================================== 
# PREPARAÇÃO DE DADOS BH SURVEY (2002, 2005, 2008)
#===============================================================================

rm(list = ls(all = TRUE))

# ----------- Pacotes ---------------

# install.packages("janitor")
library(tidyverse)
library(haven)
library(janitor)

# ----------- Carregamento de dados ---------------

svy_2002 <- read_sav("dados/brutos/PRMBH_2002.sav") |>
  clean_names()

svy_2005 <- read_sav("dados/brutos/PRMBH_2005.sav")|>
  clean_names()

svy_2008 <- read_sav("dados/brutos/PRMBH_2008.sav")|>
  clean_names()

# ----------- Tratamento e organização da base ---------------

# selecionar variáveis de amostragem; variaveis sociodemográficas 
# (sexo, idade, raça, escolaridade, estado civil); pergunta sobre risco
# de andar de noite e de dia; e módulo sobre desordem social (modulo c4 em 2005; modulo 4 em 2008) 

df_2005 <- svy_2005 |>
  mutate(ano_rodada = '2005')
  select(nquest = id, no_setor, fatorexp, pesofinal, 
         andar_dia = c1, andar_noite = c2,
         c4*)

# ...

bh_svy <- bind_rows(df_2003, df_2005, df_2008)

saveRDS(dados/tratados ...)