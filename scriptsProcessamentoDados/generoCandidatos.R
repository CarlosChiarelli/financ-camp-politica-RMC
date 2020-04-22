rm(list = ls())

setwd('~/arquivos_temporarios/projetos_pesquisa/mestrado_edy/')

library(tidyverse)
library(electionsBR)
library(abjutils)
library(janitor)

cand12DF <- candidate_local(
  year = c(2012),
  uf = "sp"
) 

cand16DF <- candidate_local(
  year = c(2016),
  uf = "sp"
) 

selecionaColunas <- function(df){
  df %>% 
    select(
      CPF_CANDIDATO, ANO_ELEICAO, NUM_TURNO,
      DESCRICAO_ELEICAO, DESCRICAO_UE, DESCRICAO_CARGO,
      DESCRICAO_SEXO, DESC_SIT_TOT_TURNO
    )
}

candsDF <- cand12DF %>% 
  bind_rows(cand16DF) %>% 
  selecionaColunas()

rm(list = c('cand12DF', 'cand16DF'))

#dfTEMP <- dfTEMP %>% 
#  janitor::clean_names()

# resultado eleição
result <- c('ELEITO', "NÃO ELEITO", "2º TURNO", "#NULO#")

# filtrando prefeito e retirando candidatos duplicados 2 turnos
candsDF <- candsDF %>% 
  filter(DESCRICAO_CARGO == 'PREFEITO') %>% 
  mutate(DESC_SIT_TOT_TURNO = factor(DESC_SIT_TOT_TURNO, levels = result)) %>% 
  group_by(ANO_ELEICAO, DESCRICAO_UE, CPF_CANDIDATO) %>% 
  arrange(DESC_SIT_TOT_TURNO) %>% 
  slice(1) %>% 
  ungroup()

candsDF <- candsDF %>% 
  mutate_at(
    vars(DESCRICAO_UE),
    function(coluna){
      coluna %>% 
        str_to_lower() %>% 
        str_squish() %>% 
        rm_accent()
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
candsDF <- candsDF %>%
  filter(DESCRICAO_UE %in% RMC) 

candsDF %>% 
  write_csv('dados/generoCandidatos12e16.csv')
