rm(list = ls())

library(tidyverse)
library(electionsBR)
library(filesstrings)

receitaDF <- read_csv(
  '~/arquivos_temporarios/projetos_pesquisa/mestrado_edy/dados/receitas_PREFEITO_12_16_RMC.csv',
  col_types = cols(.default = 'c')
)

partidos <- receitaDF %>% 
  count(sigla_partido) %>% 
  .$sigla_partido %>% 
  str_to_lower() %>% 
  str_replace_all(' ','_')

# url_baixar <-str_c(
#   'http://agencia.tse.jus.br/estatistica/sead/eleitorado/filiados/uf/filiados_',
#   partidos,
#   '_sp.zip'
# )

#ulr1='http://agencia.tse.jus.br/estatistica/sead/eleitorado/filiados/uf/filiados_psol_ac.zip'
#url2='http://agencia.tse.jus.br/estatistica/sead/eleitorado/filiados/uf/filiados_pc_do_b_ac.zip'

# localDownload='~/arquivos_temporarios/projetos_pesquisa/mestrado_edy/dados/filiados/'
# pastaCriada <- 'aplic/sead/lista_filiados/uf/'
# 
# extrairArq <- function(aux, url, partido){
#   download.file(
#     url = url,
#     destfile = aux
#   ))
#   
#   arqDezipar <- str_c(localDownload, partido, '.zip');
#   unzip(
#     zipfile = arqDezipar,
#     exdir = localDownload
#   );
#   
#   arqMov <- str_sub(url, start = 67) %>% str_replace('zip', 'csv');
#   temp <- str_c(localDownload, pastaCriada, arqMov);
#   
#   try(file.move(temp, localDownload));
# }

dir.create('~/arquivos_temporarios/projetos_pesquisa/mestrado_edy/dados/filiados')

# removendo PC do B que não aparece para download
# PMDB, PPS, PR não aparecem 

partidos <- receitaDF %>% 
  count(sigla_partido) %>% 
  .$sigla_partido 

setwd('~/arquivos_temporarios/projetos_pesquisa/mestrado_edy/dados/filiados')
# filiadosDF <- voter_affiliation(party = partidos, uf = 'SP')


### COMEÇANDO APÓS OBTENÇÃO DOS DADOS
partidCorrigir = c(
  'PMDB' = 'MDB',
  'PPS' = 'CIDADANIA',
  'PR' = 'PL',
  'PRB' = 'REPUBLICANOS',
  'PSDC' = 'DC',
  'SD' = 'SOLIDARIEDADE'
)

nomes <- partidCorrigir
partidCorrigir <- names(partidCorrigir) 
names(partidCorrigir) <- nomes

arqs <- dir() %>% str_subset('filiados') %>% str_subset('csv', negate = T)

#unzip(arqs[1])


# o download dos arquivos foram feitos manualmente pelo site
# http://www.tse.jus.br/partidos/filiacao-partidaria/relacao-de-filiados

# Baixou-se os filiados de SP com os partidos que aparecem nas receitas da RMC de 2012 a 2016

localDownload='~/arquivos_temporarios/projetos_pesquisa/mestrado_edy/dados/filiados/'
pastaCriada <- 'aplic/sead/lista_filiados/uf/'

extrairArq <- function(arqDezipar){
  unzip(
      zipfile = arqDezipar,
      exdir = localDownload
  )

  #arqMov <- str_sub(url, start = 67) %>% str_replace('zip', 'csv');
  localDentropasta <- str_c(localDownload, pastaCriada);
  
  arqsMover <- dir(localDentropasta) %>% str_subset('filiados')
  print(arqDezipar)
  
  for (aux in arqsMover) {
    caminhoArq <- str_c(localDentropasta, aux)
    file.move(caminhoArq, localDownload)
  }
}

# dezipandos os arquivos
for (nomeArq in arqs) {
  extrairArq(nomeArq)
}

arqs <- dir() %>% str_subset('filiados') %>% str_subset('csv', negate = F)

listaDfsFiliados <- map(
  arqs,
  function(nomeArq) read_csv2(
    nomeArq,
    locale = locale(encoding = 'Latin1'),
    col_types = cols(.default = 'c')
  )
)

listaDfsFiliados <- bind_rows(listaDfsFiliados, .id = "column_label") 

listaDfsFiliados %>% count(`NOME DO MUNICIPIO`) %>% arrange(desc(n))

filiadosDf <- listaDfsFiliados
rm(listaDfsFiliados)

# vamos filtrar apenas CIDADES de interesses
RMC <- c(
  'americana',
  'artur nogueira',
  'campinas',
  'cosmópolis',
  'engenheiro coelho',
  'holambra',
  'hortolândia',
  'indaiatuba',
  'itatiba',
  'jaguariúna',
  'monte mor',
  'morungaba',
  'nova odessa',
  'paulínia',
  'pedreira',
  "santa bárbara d'oeste",
  'santo antônio de posse',
  'sumaré',
  'valinhos',
  'vinhedo'
) %>% str_to_upper()

filiadosDf <- filiadosDf %>% 
  filter(`NOME DO MUNICIPIO`%in%RMC) %>%
  distinct()

filiadosDf <- filiadosDf %>% 
  janitor::clean_names()

filiadosDf %>% 
  ggplot(aes(situacao_do_registro))+
  geom_bar()+
  coord_flip()

# corrigindo partidos trocados
filiadosDf <- filiadosDf %>% 
  mutate(
    sigla_do_partido = if_else(
      sigla_do_partido %in% names(partidCorrigir),
      partidCorrigir[sigla_do_partido],
      sigla_do_partido
      )
  )

filiadosDf %>% 
  write_csv('filiadosRMC.csv')

# removendo .csv
file.remove(list = arqs)

rm(list = ls())