# limpando memória
rm(list = ls())

# carregando pacotes
library(tidyverse)
library(electionsBR)

# função que seleciona colunas de interesse
colunas_alvos <- function(df){
  df %>% 
    select(
      ANO_ELEICAO,
      CPF_CANDIDATO,
      DESCRICAO_UE,
      DESCRICAO_CARGO,
      DESC_SIT_TOT_TURNO
    )
}

# dados dos resultados das eleições 
# 2008
dfCAND <- candidate_local(
  year = c(2008),
  uf = "sp"
) %>% 
  colunas_alvos()
  
# 2012
dfTEMP <- candidate_local(
  year = c(2012),
  uf = "sp"
) %>% 
  colunas_alvos()

# unindo linhas
dfCAND <- dfCAND %>% 
  bind_rows(
    dfTEMP
  )

# 2016
dfTEMP <- candidate_local(
  year = c(2016),
  uf = "sp"
) %>% 
  colunas_alvos()

# unindo linhas 
dfCAND <- dfCAND %>% 
  bind_rows(
    dfTEMP
  )

# removendo variaveis 
rm("dfTEMP")

# limpando colunas caracteres
dfCAND <- dfCAND %>% 
  mutate_if(
    is.character,
    function(coluna){
      coluna %>% 
        str_to_lower() %>% 
        str_squish() %>% 
        abjutils::rm_accent()
    }
  )

# cidades do RMC
RMC <- c(
  'americana',
  'artur nogueira',
  'campinas',
  'cosmopolis',
  'engenheiro coelho',
  'holambra',
  'hortolandia',
  'indaiatuba',
  'itatiba',
  'jaguariuna',
  'monte mor',
  'morungaba',
  'nova odessa',
  'paulinia',
  'pedreira',
  "santa barbara d'oeste",
  'santo antonio de posse',
  'sumare',
  'valinhos',
  'vinhedo'
)

# filtrando observacoes de interesse
dfCAND <- dfCAND %>%
  filter(
    DESCRICAO_UE %in% RMC,
    DESCRICAO_CARGO == "prefeito"
  ) 

# gerando coluna incumbente
incumb_df <- dfCAND %>% 
  mutate(
    eleito = if_else(
      DESC_SIT_TOT_TURNO == "eleito", 1, 0
    )
  ) %>% 
  # removendo duplicados segundo turno
  group_by(
    ANO_ELEICAO, DESCRICAO_UE, CPF_CANDIDATO
  ) %>% 
  arrange(
    desc(eleito)
  ) %>% 
  slice(1) %>% 
  ungroup() %>%
  select(-DESC_SIT_TOT_TURNO) %>% 
  spread(
    ANO_ELEICAO, eleito
  ) %>%
  arrange(DESCRICAO_UE) %>% 
  rename(
    ano2008 = `2008`,
    ano2012 = `2012`,
    ano2016 = `2016`,
  ) %>% 
  mutate_at(
    vars(ano2008:ano2016),
    function(coluna) if_else(is.na(coluna), -1, coluna)
  ) %>% 
  mutate(
    incub12 = if_else(ano2008 == 1, 1, 0),
    incub16 = if_else(ano2012 == 1, 1, 0)
  ) 
  
# salvando dados incumbentes
incumb_df %>% 
  write_csv("~/arquivos_temporarios/projetos_pesquisa/mestrado_edy/dados/incunbemtes_12_16.csv")

# corrigindo incumbentes
read_csv("~/arquivos_temporarios/projetos_pesquisa/mestrado_edy/dados/incunbemtes_12_16.csv") %>% 
  mutate(
    incub12 = if_else(ano2008 == 1, 1, 0),
    incub16 = if_else(ano2012 == 1, 1, 0)
  ) %>% 
  write_csv("~/arquivos_temporarios/projetos_pesquisa/mestrado_edy/dados/incunbemtes_12_16.csv")


  
  
  
  
  
  






