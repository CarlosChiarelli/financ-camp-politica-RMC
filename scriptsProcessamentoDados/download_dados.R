# neste script vamos baixar os dados excluindo arquivos desnecessarios
# que virão junto a estes

# limpando memoria
rm(list = ls())

# bibliotecas
biblio <- c(
  "tidyverse",
  "descr",
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

# criando pasta para guardar dados
if(!dir.exists("dados")) dir.create("dados")

# dados 2012
if(!dir.exists("dados/2012")) dir.create("dados/2012")

# dados 2016
if(!dir.exists("dados/2016")) dir.create("dados/2016")

##### DOWNLOAD DADOS #####

# link para download
# "Prestação de contas final (formato ZIP)"
linkDADOS2012 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/prestacao_contas/prestacao_final_2012.zip"

# "Prestações de contas finais (formato ZIP)
linkDADOS2016 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/prestacao_contas/prestacao_contas_final_2016.zip"

### 2012

# baixando .zip
download.file(
  linkDADOS2012,
  "dados/2012/prestacaoCONTAS2012.zip"
)

# descompactando arquivo
unzip(
  "dados/2012/prestacaoCONTAS2012.zip",
  exdir = "dados/2012/"
)

# listando arquivos necessarios e excluindo desnecessarios
arquivos <- list.files("dados/2012/")

arqEXCLUIR <- arquivos %>% 
  str_subset(
    "receitas_candidatos_2012_SP.txt|pdf",
    negate = T
  )

arqEXCLUIR <- str_c(
  "dados/2012/",
  arqEXCLUIR
)

# excluindo desnecessarios
file.remove(arqEXCLUIR)

# lendo linhas do .txt
linhasREC12 <- read_lines(
  "dados/2012/receitas_candidatos_2012_SP.txt",
  locale = locale()
)

# corrigindo encoding, removendo acentos e aspas
linhasREC12 <- linhasREC12 %>% 
  toUTF8() %>% 
  rm_accent() %>% 
  str_replace_all(
    '[\"]',
    '')

# os missings (NAs) aparecem como #NULO# ou -1 entao serao retirados
linhasREC12 <- linhasREC12 %>% 
  str_replace_all("#NULO#|-1","") 

# salvando linhas limpas
linhasREC12 %>% 
  write_lines(
    "dados/2012/receitCAND12SP_semASPAS.txt"
  )

# removendo variaveis e arquivos desnecessarios
file.remove("dados/2012/receitas_candidatos_2012_SP.txt")
rm(linhasREC12)


### 2016

# baixando .zip
download.file(
  linkDADOS2016,
  "dados/2016/prestacaoCONTAS2016.zip"
)

# descompactando arquivo
unzip(
  "dados/2016/prestacaoCONTAS2016.zip",
  exdir = "dados/2016/"
)

# listando arquivos necessarios e excluindo desnecessarios
arquivos <- list.files("dados/2016/")

arqEXCLUIR <- arquivos %>% 
  str_subset(
    "receitas_candidatos_prestacao_contas_final_2016_SP|pdf",
    negate = T
  )

arqEXCLUIR <- str_c(
  "dados/2016/",
  arqEXCLUIR
)

# excluindo desnecessarios
file.remove(arqEXCLUIR)

# lendo linhas do .txt
linhasREC16 <- read_lines(
  "dados/2016/receitas_candidatos_prestacao_contas_final_2016_SP.txt",
  locale = locale()
)

# corrigindo encoding, removendo acentos e aspas
linhasREC16 <- linhasREC16 %>% 
  toUTF8() %>% 
  rm_accent() %>% 
  str_replace_all(
    '[\"]',
    '')

# os missings (NAs) aparecem como #NULO# ou -1 entao serao retirados
linhasREC16 <- linhasREC16 %>% 
  str_replace_all("#NULO|-1","") 

# está faltando uma colunas nas linhas 133461 e 490696
# então iremos adicionar uma com NA
#linhasREC16[c(133461,490696)]<- linhasREC16[c(133461,490696)] %>%
#  str_sub(end = -2)

# salvando linhas limpas
linhasREC16 %>% 
  write_lines(
    "dados/2016/receitCAND16SP_semASPAS.txt"
  )

# removendo variaveis e arquivos desnecessarios
file.remove("dados/2016/receitas_candidatos_prestacao_contas_final_2016_SP.txt")
rm(linhasREC16)


# dftemp <- read_csv2("~/arquivos_temporarios/projetos_pesquisa/mestrado_edy/dados/2016/receitCAND16SP_semASPAS.txt")
# dftemp %>% slice(133459,133460,133461,490694,490695,490696) %>% view()
