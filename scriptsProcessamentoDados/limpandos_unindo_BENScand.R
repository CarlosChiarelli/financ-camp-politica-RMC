rm(list = ls())

setwd("~/arquivos_temporarios/projetos_pesquisa/mestrado_edy")

## dados 2012

# nomes colunas 2012
nomesCols12 <- c(
  'DATA_GERACAO',
  'HORA_GERACAO',
  'ANO_ELEICAO',
  'DESCRICAO_ELEICAO',
  'SIGLA_UF',
  'SQ_CANDIDATO',
  'CD_TIPO_BEM_CANDIDATO',
  'DS_TIPO_BEM_CANDIDATO',
  'DETALHE_BEM',
  'VALOR_BEM',
  'DATA_ULTIMA_ATUALIZACAO',
  'HORA_ULTIMA_ATUALIZACAO'
)

# lendo bens candidatos 2012
bensCand12_df <- read_csv2(
  "dados/2012/bens_cands/bem_candidato_2012_SP.txt",
  col_types = cols(.default = "c"),
  locale = locale(encoding = "Latin1"),
  col_names = F
  #col_names = nomesCols12
)

# nomes colunas
bensCand12_df <- bensCand12_df %>% 
  rename_all(
    function(nome) nome = nomesCols12
  )

# limpando nomes colunas 
bensCand12_df <- bensCand12_df %>% 
  janitor::clean_names()

# colunas de interesse
bensCand12_df <- bensCand12_df %>% 
  select(
    ano_eleicao, 
    sq_candidato,
    valor_bem
  )


## dados 2016

# lendo bens candidatos 2012
bensCand16_df <- read_csv2(
  "dados/2016/bens_cands/bem_candidato_2016_SP.csv",
  col_types = cols(.default = "c"),
  locale = locale(encoding = "Latin1")
)

# limpando nomes colunas 
bensCand16_df <- bensCand16_df %>% 
  janitor::clean_names()

# colunas de interesse
bensCand16_df <- bensCand16_df %>% 
  select(
    ano_eleicao, 
    sq_candidato,
    vr_bem_candidato
  )

# substituindo vírgula por ponto
bensCand16_df <- bensCand16_df %>% 
  mutate(
    vr_bem_candidato = str_replace(vr_bem_candidato, ",", ".")
  )

# alterando nome coluna
bensCand16_df <- bensCand16_df %>% 
  rename(
    valor_bem = vr_bem_candidato
  )

# unindo os dois bancos
bensCandCompleto_df <- bensCand12_df %>% 
  bind_rows(bensCand16_df)

# somando todos bens
bensCandCompleto_df <- bensCandCompleto_df %>% 
  mutate(valor_bem = as.numeric(valor_bem)) %>% 
  group_by(ano_eleicao, sq_candidato) %>% 
  summarise(valor_bemTOT = sum(valor_bem))

# salvando banco BENS
bensCandCompleto_df %>% 
  write_csv("dados/bens_candidatos_12_16.csv")

###### 
# No banco de dados de BENS não existe coluna cidade
# então vamos ver se o SEQUENCIAL CANDIDATO é único
# para o ANO, ESTADO

## R: são únicos para ANO e ESTADO
