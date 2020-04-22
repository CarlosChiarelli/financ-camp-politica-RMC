rm(list = ls())

library(tidyverse)
library(lubridate)

setwd('~/arquivos_temporarios/projetos_pesquisa/mestrado_edy/')

# lendo dados das receitas
receitasDf <- read_csv('dados/receitas_PREFEITO_12_16_RMC.csv', col_types = cols(.default = 'c'))

# lendo dados filiados
filiadosDf <- read_csv('dados/filiados/filiadosRMC.csv', col_types = cols(.default = 'c'))

# selecionando colunas de interesse
# filiados
filiadosDf <- filiadosDf %>% 
  select(
    nome_do_filiado, sigla_do_partido, nome_do_municipio,
    data_da_filiacao, situacao_do_registro, data_da_desfiliacao,
    data_do_cancelamento
  ) %>% 
  mutate(
    data_da_filiacao = dmy(data_da_filiacao),
    data_da_desfiliacao = dmy(data_da_desfiliacao),
    data_do_cancelamento = dmy(data_do_cancelamento) 
  )

# receita
receitasDf <- receitasDf %>% 
  select(
    ano_eleicao, municipio, data_da_receita,  
    nome_do_doador, nome_do_doador_receita_federal
  ) %>% 
  mutate(
    data_da_receita = str_sub(data_da_receita, end = -9) %>% 
      dmy()
  )

# funcao que remove caracteres especiais e espaços a mais
simplificaNome <- function(nome){
  nome %>% 
    str_to_lower() %>% 
    str_remove_all(" ") %>% 
    str_replace_all("[[:punct:]]", " ") %>% 
    iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') 
}

# datas de inicio e fim das doações
doacoesDataIniFim <- receitasDf %>%
  group_by(ano_eleicao) %>%
  summarise(minData=min(data_da_receita),maxData=max(data_da_receita))

# data avançada (fictícia)
dataFuturo <- as_date('2021-01-01')

# corrigindo data de perda vinculo com partido
filiadosDf <- filiadosDf %>% 
  mutate(
    nenhumaData = if_else(
      situacao_do_registro != 'REGULAR' & is.na(data_da_desfiliacao) & is.na(data_do_cancelamento),
      1, 0
    )
  ) %>%
  # removendo quem não possui data de perca do vinculo
  filter(nenhumaData != 1) %>% 
  mutate_at(
    vars(data_da_desfiliacao, data_do_cancelamento),
    funs(
      # caso seja valor faltante preencho com uma data avançada 
      if_else(situacao_do_registro != 'REGULAR' & is.na(.), dataFuturo, .)
    )
  ) %>% 
  # criando coluna data perca vinculo com partido
  mutate(
    dataPercaVinculo = case_when(
      situacao_do_registro == 'REGULAR' ~ dataFuturo,
      situacao_do_registro != 'REGULAR' & data_da_desfiliacao <= data_do_cancelamento ~ data_da_desfiliacao,
      situacao_do_registro != 'REGULAR' & data_da_desfiliacao > data_do_cancelamento ~ data_do_cancelamento
    )
  ) %>% 
  select(-data_da_desfiliacao:-nenhumaData) 
  
# seprando filiados em 2012 e em 2016
# e deixando uma única observação para cada pessoa
filiad12Df <- filiadosDf %>% 
  filter(
    data_da_filiacao < as_date('2013-01-01')
  ) %>% 
  group_by(
    nome_do_filiado
  ) %>% 
  slice(which.max(data_da_filiacao)) %>% 
  ungroup() %>% 
  mutate(ano_eleicao = 2012)

filiad16Df <- filiadosDf %>% 
  filter(
    data_da_filiacao < as_date('2017-01-01')
  ) %>% 
  group_by(
    nome_do_filiado
  ) %>% 
  slice(which.max(data_da_filiacao)) %>% 
  ungroup() %>% 
  mutate(ano_eleicao = 2016)

# unindo filiados
filiadosDf <- filiad12Df %>% 
  bind_rows(filiad16Df)

# PRIMEIRO: ver se o doador é filiado e se data de doação está 
# dentro do período de filiação
# SEGUNDO: encontrar doadores filiados regulares

# simplificando nomes para fazer merge através deles 
receitasDf <- receitasDf %>% 
  mutate_at(
    vars(nome_do_doador, nome_do_doador_receita_federal),
    funs(
      . %>% 
        simplificaNome() %>% 
        simplificaNome()
    )
  )

filiadosDf <- filiadosDf %>% 
  mutate(
    nome_do_filiado = nome_do_filiado %>% 
      simplificaNome() %>% 
      simplificaNome()
  )

# vamos unir os doadores com filiados
# teremos que fazer duas unioes (nome doador no sistema e na receita)
aux <- receitasDf %>% 
  mutate(ano_eleicao = as.numeric(ano_eleicao)) %>% 
  left_join(
    filiadosDf,
    by = c(
      'ano_eleicao',
      'nome_do_doador'='nome_do_filiado'
    )
  ) %>% 
  left_join(
    filiadosDf,
    by = c(
      'ano_eleicao',
      'nome_do_doador_receita_federal'='nome_do_filiado'
    )
  )

# removendo doadores não filiados
aux <- aux %>% 
  filter(
    !(is.na(sigla_do_partido.x) & is.na(sigla_do_partido.y))
  )

# vamos deixar apenas nomes que aparacem (nome comum ou receita)
aux <- aux %>% 
  mutate(
    sigla_do_partido = if_else(is.na(sigla_do_partido.x), sigla_do_partido.y, sigla_do_partido.x),
    nome_do_municipio = if_else(is.na(nome_do_municipio.x), nome_do_municipio.y, nome_do_municipio.x),
    data_da_filiacao = if_else(is.na(data_da_filiacao.x), data_da_filiacao.y, data_da_filiacao.x),
    situacao_do_registro = if_else(is.na(situacao_do_registro.x), situacao_do_registro.y, situacao_do_registro.x),
    dataPercaVinculo = if_else(is.na(dataPercaVinculo.x), dataPercaVinculo.y, dataPercaVinculo.x)
  ) %>% 
  select(-sigla_do_partido.x:-dataPercaVinculo.y)

# removendo doadores que estavam sem vinculo 
# no momendo da doação
aux1 <- aux %>% 
  filter(data_da_receita <= dataPercaVinculo)

aux1 <- aux1 %>% 
  select(ano_eleicao, municipio, nome_do_doador) %>% 
  count(ano_eleicao, municipio, nome_do_doador) %>% 
  count(ano_eleicao, municipio) %>%
  arrange(desc(n, ano_eleicao)) 

maioresCidad <- aux1 %>% 
  .$municipio %>% 
  unique()

aux1 %>% 
  ggplot(aes(factor(municipio, rev(maioresCidad)), n, fill=factor(ano_eleicao)))+
  geom_col(position = position_dodge())+
  coord_flip()






# selecionando colunas de interesse
aux <- filiadosDf %>% 
  select(
    nome_do_filiado, sigla_do_partido, data_da_filiacao, 
    data_da_desfiliacao, data_do_cancelamento,
    situacao_do_registro, motivo_do_cancelamento
  )

# aqui vamos selecionar o nomes dos doadores em 2012 e 2016
doadoresDf <- receitasDf %>% 
  select(nome_do_doador) %>% 
  distinct() 

filiaDoadoDf <- aux %>% 
  inner_join(
    doadoresDf,
    #by = c('nome_do_doador_receita_federal'='nome_do_filiado')
    by = c('nome_do_filiado'='nome_do_doador')
  ) 

filiaDoadoDf2 <- aux %>% 
  inner_join(
    doadoresDf,
    #by = c('nome_do_doador_receita_federal'='nome_do_filiado')
    by = c('nome_do_filiado'='nome_do_doador')
  ) 

doadoresDf <- receitasDf %>% 
  select(nome_do_doador_receita_federal) %>% 
  distinct() 

filiaDoadoDf <- aux %>% 
  inner_join(
    doadoresDf,
    by = c('nome_do_filiado'='nome_do_doador_receita_federal')
    #by = c('nome_do_doador_receita_federal'='nome_do_doador')
  ) 

#### REMOVER ABAIXO
colunasNomesDoad <- receitasDf %>% 
  select(
    nome_do_doador, nome_do_doador_receita_federal,
    nome_do_doador_originario, nome_do_doador_originario_receita_federal
  ) %>% 
  map(
    function(col) unique(col)
  )

temp <- receitasDf %>% 
  mutate(
    nome = str_c(nome_do_doador, '|', nome_do_doador_receita_federal)
  ) %>% 
  select(nome) %>% 
  distinct()

aux %>% 
  filter(nome_do_filiado %in% temp$nome) %>% 
  count(nome_do_filiado) %>% 
  head()
#### REMOVER ACIMA

dataEleicao12ini <- as_date('2012-08-01')
dataEleicao12fim <- as_date('2012-08-28')

mesOutub12 <- seq(dataEleicao12ini, dataEleicao12fim, 1)

dataEleicao16ini <- as_date('2016-08-01')
dataEleicao16fim <- as_date('2016-08-30')

mesOutub16 <- seq(dataEleicao12ini, dataEleicao12fim, 1)

# coluna data em que perde vinculo com partido
filiaDoadoDf <- filiaDoadoDf %>% 
  mutate(
    dataDesfilAux = if_else(is.na(data_da_desfiliacao), data_do_cancelamento, data_da_desfiliacao),
    dataCancAux = if_else(is.na(data_do_cancelamento), data_da_desfiliacao, data_do_cancelamento),
    dataDesligamento = if_else(dataDesfilAux < dataCancAux, dataDesfilAux, dataCancAux)
  ) %>% 
  select(-dataDesfilAux, -dataCancAux)

# precisamos filtrar filiados doadores REGULARES em 2012 e 2016

# filiados 2012
filiad12Df <- filiaDoadoDf %>% 
  filter(data_da_filiacao <= dataEleicao12fim)

# pegando ultima filiacao
filiad12Df <- filiad12Df %>% 
  group_by(nome_do_filiado) %>% 
  arrange(desc(data_da_filiacao)) %>% 
  slice(1) %>% 
  ungroup()

# filtrando candidatos que perderam vinculo com
# partido no mês da eleição (CANCELADO ou DESFILIADO)
filiad12Df <- filiad12Df %>% 
  filter(
    !(situacao_do_registro != 'REGULAR' & dataDesligamento < dataEleicao12ini) 
  ) 

## MESMO PROCEDIMENTO para filiados em 2016
# filiados 2016
filiad16Df <- filiaDoadoDf %>% 
  filter(data_da_filiacao <= dataEleicao16fim)

# pegando ultima filiacao
filiad16Df <- filiad16Df %>% 
  group_by(nome_do_filiado) %>% 
  arrange(desc(data_da_filiacao)) %>% 
  slice(1) %>% 
  ungroup()

# filtrando candidatos que perderam vinculo com
# partido no mês da eleição (CANCELADO ou DESFILIADO)
filiad16Df <- filiad16Df %>% 
  filter(
    !(situacao_do_registro != 'REGULAR' & dataDesligamento < dataEleicao16ini) 
  ) 

# gerando dataframe com FILIADOS DOADORES 2012 e 2016
filiad12Df <- filiad12Df %>% 
  mutate(ano = 2012) %>% 
  select(nome_do_filiado, sigla_do_partido, ano)

filiad16Df <- filiad16Df %>% 
  mutate(ano = 2016) %>% 
  select(nome_do_filiado, sigla_do_partido, ano)

filiaDoadoDf <- filiad12Df %>% 
  bind_rows(filiad16Df)

# limpando a memoria 
variaveis <- ls()
ind <- variaveis %in% c('receitasDf', 'filiaDoadoDf')
rm(list = variaveis[!ind])

# filiados doadores por cidade
aux <- receitasDf %>% 
  select(ano_eleicao, municipio, nome_do_doador) %>% 
  mutate(ano_eleicao=as.numeric(ano_eleicao)) %>% 
  distinct() %>% 
  inner_join(
    filiaDoadoDf,
    by = c(
      'nome_do_doador'='nome_do_filiado',
      'ano_eleicao'='ano'
    )
  ) 

cidadesMaiores <- aux %>% 
  count(municipio, ano_eleicao) %>% 
  filter(ano_eleicao=='2016') %>% 
  arrange(desc(n)) %>% 
  .$municipio

aux %>% 
  count(ano_eleicao) %>% 
  ggplot(aes(factor(ano_eleicao), n, fill=factor(ano_eleicao)))+
  geom_col()

aux %>% 
  count(ano_eleicao, municipio) %>% 
  ggplot(aes(factor(municipio, rev(cidadesMaiores)), n, fill=factor(ano_eleicao)))+
  geom_col(position = position_dodge())+
  coord_flip()


### RM
temp <- receitasDf %>% 
  filter(ano_eleicao==2012) 

temp <- temp %>% 
  mutate(
    data_da_receita = str_sub(data_da_receita, end = -9) %>% 
      dmy()
  )

temp %>% 
  select(municipio, data_da_receita) %>% 
  count(data_da_receita) %>% 
  ggplot(aes(data_da_receita, n))+
  geom_point()+
  geom_vline(xintercept = c(dataEleicao12ini, dataEleicao12fim))

# fazendo para janela de doação 2016
temp <- receitasDf %>% 
  filter(ano_eleicao==2016) 

temp <- temp %>% 
  mutate(
    data_da_receita = str_sub(data_da_receita, end = -9) %>% 
      dmy()
  )

temp %>% 
  select(municipio, data_da_receita) %>% 
  count(data_da_receita) %>% 
  ggplot(aes(data_da_receita, n))+
  geom_point()+
  geom_vline(xintercept = c(dataEleicao16ini, dataEleicao16fim))

### RM  





