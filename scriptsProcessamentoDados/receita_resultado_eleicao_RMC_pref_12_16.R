# neste script vamos gerar a tabela final que liga cada candidato (seus dados)
# ao financiamento de campanhas destes

# limpando memoria
rm(list = ls())

# bibliotecas
biblio <- c(
  "tidyverse",
  "abjutils",
  "janitor",
  "magrittr"
)

# instalando e importando pacotes
for(pacote in biblio){
  # checa se o pacote está instalado
  if( !(pacote %in% rownames(installed.packages()))  )
    install.packages(pacote)
  library(pacote, character.only = TRUE)
}

# importando dados
dfRECEI <- read_csv(
  "~/arquivos_temporarios/projetos_pesquisa/mestrado_edy/dados/receitas_PREFEITO_12_16_RMC.csv",
  #col_types = "ccccccccccccccccccccccccccccccccccccc"
  col_types = cols(.default = "c")
)

dfCAND <- read_csv(
  "~/arquivos_temporarios/projetos_pesquisa/mestrado_edy/dados/candidatos_2012_2016_SP.csv",
  #col_types = "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
  col_types = cols(.default = "c")
)

# selecionando colunas de interesse
dfCAND <- dfCAND %>% 
  select(
    ANO_ELEICAO,
    NUM_TURNO,
    DESCRICAO_ELEICAO,
    SIGLA_UF,
    DESCRICAO_UE,
    DESCRICAO_CARGO,
    NOME_CANDIDATO:CPF_CANDIDATO,
    SIGLA_PARTIDO,
    NOME_PARTIDO,
    COMPOSICAO_LEGENDA,
    NOME_COLIGACAO,
    DESC_SIT_TOT_TURNO,
    NOME_TIPO_ELEICAO,
    SITUACAO_REELEICAO
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

# padronizando municipios e cargo da tabela CANDIDATOS
dfCAND <- dfCAND %>% 
  rename(
    MUNICIPIO = DESCRICAO_UE
  ) %>% 
  mutate_at(
    vars(
      MUNICIPIO,
      DESCRICAO_CARGO
    ),
    function(coluna) coluna %>% 
      str_to_lower() %>% 
      str_squish() %>% 
      rm_accent()
  )

# filtrando cidades RMC e prefeitos
dfCAND <- dfCAND %>% 
  filter(
    MUNICIPIO %in% RMC,
    DESCRICAO_CARGO == "prefeito"
  )

# deixando apenas uma observacao (linha) de candidato por ano 
# ou seja, vamos RETIRAR LINHAS DUPLICADAS devido ao SEGUNDO TURNO
resultadoLEVELS <- dfCAND$DESC_SIT_TOT_TURNO %>% 
  unique()
resultadoLEVELS <- resultadoLEVELS[c(4,3,1,2)]

# os levels que devem aparecer são ELEITO e NAO ELEITO
# selecionando o que possui maior prioridade
dfCAND <- dfCAND %>% 
  mutate(
    DESC_SIT_TOT_TURNO = factor(
      DESC_SIT_TOT_TURNO,
      levels = resultadoLEVELS
    )
  ) %>% 
  group_by(
    ANO_ELEICAO,
    CPF_CANDIDATO
  ) %>% 
  arrange(
    desc(DESC_SIT_TOT_TURNO)
  ) %>% 
  slice(1) %>% 
  ungroup()

# mostrando que todos CPFs estão completos
# dfRECEI %>% anti_join(dftemp, by=c("ano_eleicao" = "ANO_ELEICAO", "cpf_do_candidato"="CPF_CANDIDATO"))

# fazendo merge
dfCOMPLETO <- dfRECEI %>%
  left_join(
    dfCAND,
    by = c(
      "ano_eleicao" = "ANO_ELEICAO",
      "cpf_do_candidato" = "CPF_CANDIDATO"
    )
  )  
  
# salvando tabela completa
dfCOMPLETO %>% 
  write_csv("dados/RECEIT_RESULT_eleicaoRMC_pref12_16.csv")

  




