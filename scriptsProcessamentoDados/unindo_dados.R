# nesse script vamos unir os dados das receitas dos prefeitos da RMC (2012-2016)
# e importar os dados destes candidatos, salvando ambas tabelas

# setando diretorio
setwd("~/arquivos_temporarios/projetos_pesquisa/mestrado_edy/")

# limpando memoria
rm(list = ls())

# bibliotecas
biblio <- c(
  "tidyverse",
  "descr",
  "abjutils",
  "janitor",
  "magrittr",
  "electionsBR"
)

# instalando e importando pacotes
for(pacote in biblio){
  # checa se o pacote está instalado
  if( !(pacote %in% rownames(installed.packages()))  )
    install.packages(pacote)
  library(pacote, character.only = TRUE)
}

######### UNINDO DADOS #########

# importando dados das receitas
df12 <- read_csv2(
  "dados/2012/receitCAND12SP_numCOLUNASok.txt",
  #col_types = "cccccccccccccccccccccccccccc"
  col_types = cols(.default = "c")
)

df16 <- read_csv2(
  "dados/2016/receitCAND16SP_numCOLUNASok.txt",
  #col_types = "ccccccccccccccccccccccccccccccccccc"
  col_types = cols(.default = "c")
)

# limpando nomes colunas e adicionando coluna ANO ELEICAO
df12 <- df12 %>% 
  clean_names() %>% 
  mutate(
    ano_eleicao = 2012
  )

df16 <- df16 %>% 
  clean_names() %>% 
  mutate(
    ano_eleicao = 2016
  )

# renomeando coluna municipio de 2016
df16 <- df16 %>% 
  rename(
    municipio = nome_da_ue
  )

# unindo linhas
dfRECEI <- df12 %>% 
  bind_rows(
    df16
  )

# removendo variaveis  
rm(list = c("df12", "df16"))

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

# padronizando caracteres de das colunas MUNICIPIO e CARGO
dfRECEI <- dfRECEI %>% 
  mutate_at(
    vars(
      cargo,
      municipio
    ),
    function(coluna) coluna %>% 
      str_to_lower() %>% 
      str_squish()
  )

# filtrando PREFEITO e RMC
dfRECEI <- dfRECEI %>% 
  filter(
    cargo == "prefeito",
    municipio %in% RMC
  ) 

# transformando VALOR RECEITA em numeros e padronizando TIPO RECEITA
dfRECEI <- dfRECEI %>% 
  mutate(
    valor_receita = valor_receita %>% 
      str_replace(",", ".") %>% 
      as.numeric(),
    tipo_receita = tipo_receita %>% 
      str_to_lower() %>% 
      str_squish() %>% 
      rm_accent()
  )

# salvando dados das receitas 2012 e 2016
dfRECEI %>% 
  write_csv("dados/receitas_PREFEITO_12_16_RMC.csv")

# dados dos resultados das eleições 2012 e 2016
dfCAND12 <- candidate_local(
  year = c(2012),
  uf = "sp"
)

dfCAND16 <- candidate_local(
  year = c(2016),
  uf = "sp"
)

# unindo linhas das eleicoes de 2012 e 2016
dfCAND <- dfCAND12 %>% 
  bind_rows(
    dfCAND16
  )

# transformando todas colunas em caracteres
dfCAND <- dfCAND %>% 
  mutate_all(funs(as.character(.)))

# removendo variaveis 
rm(
  list = c("dfCAND12", "dfCAND16")
)

# dados da quantidade de votos
votos_df12 <- vote_mun_zone_local(
  year = c(2012),
  uf = "sp"
)

votos_df16 <- vote_mun_zone_local(
  year = c(2016),
  uf = "sp"
)

dfVOTOS <- votos_df12 %>% 
  bind_rows(
    votos_df16
  )

# removendo variaveis 
rm(
  list = c("votos_df16", "votos_df12")
)

# selecionando colunas de interesse
dfVOTOS <- dfVOTOS %>% 
  select(
    ANO_ELEICAO,
    NUM_TURNO,
    DESCRICAO_CARGO,
    NOME_MUNICIPIO,
    SIGLA_UE,
    NUMERO_PARTIDO,
    SQ_CANDIDATO,
    TOTAL_VOTOS
  ) 
  
# renomeando para unir votos
dfVOTOS <- dfVOTOS %>% 
  rename(
    SEQUENCIAL_CANDIDATO = SQ_CANDIDATO,
    DESCRICAO_UE = NOME_MUNICIPIO
  )

# colunas em caracteres
dfVOTOS <- dfVOTOS %>% 
  mutate_all(funs(as.character(.)))

# filtrando candidatos sem sequencial
#dfTEMP <- dfCAND %>% 
#  filter(
#    SEQUENCIAL_CANDIDATO %in% dfVOTOS$SEQUENCIAL_CANDIDATO
#  )

# unindo info candidato com votos obtidos
# o merge está dando mais linhas do q deveria !!!
#dfTEMP %>% 
#  left_join(
#    dfVOTOS,
#    by = c("ANO_ELEICAO",
#           "NUM_TURNO",
#           "SIGLA_UE",
#           "SEQUENCIAL_CANDIDATO",
#           "DESCRICAO_UE"
#           )
#  )


# salvando tabela dos candidatos
dfCAND %>% 
  write_csv("dados/candidatos_2012_2016_SP.csv")

dfVOTOS %>% 
  write_csv("dados/votos_2012_2016_SP.csv")








