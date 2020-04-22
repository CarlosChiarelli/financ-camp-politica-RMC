# neste script vamos estruturar os dados, pois existem separados de colunas ";" a mais
# que não deveriam existir

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

######### LIMPANDO DADOS #########

# importando dados 2012
dfREC12 <- read_csv2(
  "dados/2012/receitCAND12SP_semASPAS.txt",
  col_types = "cccccccccccccccccccccccccccc"
)

# existem algumas linhas em que a coluna "Setor economico do doador" está separado por ";" ao inves de ","
# nessas linhas iremos fazer essa substituicao para nao haver problema na leitura

# selecionando linhas com problema
linhasPROBLE <- dfREC12 %>%
  problems() %>%
  .$row

# adicionando para contabilizar nomes das colunas no .txt
linhasPROBLE_txt <- linhasPROBLE + 1

# gerando conjunto de caracteres para serem retirados
caracteresERRO <- dfREC12 %>% 
  slice(linhasPROBLE) %>% 
  .$`Data da receita` %>% 
  unique() 

caracteresRETIRAR <- str_c("; ", caracteresERRO)
caracteresCOLOCAR <- str_c(", ", caracteresERRO)

# importando linhas das REC 12 e removendo coluna extra
linhaREC12 <- read_lines("dados/2012/receitCAND12SP_semASPAS.txt")

# linha para modificar
linhaMODIF <- linhaREC12[linhasPROBLE_txt]

# substituindo as linhas com o defeito
for (i in 1:length(linhaMODIF)) {
  encontrou_padrao <- 0
  contador = 1
  
  while (encontrou_padrao == 0) {
    encontrou_padrao <- linhaMODIF[i] %>% 
      str_detect(caracteresRETIRAR[contador])
    if(encontrou_padrao){
      linhaMODIF[i] <- linhaMODIF[i] %>% 
        str_replace(
          caracteresRETIRAR[contador],
          caracteresCOLOCAR[contador]
        )
    }
    contador = contador + 1
  }
}

# sobreescrevendo linhas corrigidas
linhaREC12[linhasPROBLE_txt] <- linhaMODIF

# salvando REC12 com linhas corrigidas
linhaREC12 %>% 
  write_lines("dados/2012/receitCAND12SP_numCOLUNASok.txt")

# removendo variaveis usadas
rm(list = ls())

# importando dados de 2016
dfREC16 <- read_csv2(
  "dados/2016/receitCAND16SP_semASPAS.txt",
  col_types = "ccccccccccccccccccccccccccccccccccc"
)

# selecionando linhas com problema
linhasPROBLE <- dfREC16 %>%
  problems() %>%
  .$row

# adicionando para contabilizar nomes das colunas no .txt
linhasPROBLE_txt <- linhasPROBLE + 1

# lendo linhas REC 16
linhaREC16 <- read_lines("dados/2016/receitCAND16SP_semASPAS.txt")

# o problema em duas linhas é a existência de ";" entre NOMES DE DOADORES
caracINCORRETO <- c("OLGA MARTINS S; PASSARINI","MARIA SUELI GON;CALVES DE OLIVEIRA")
caracCORRETO <- c("OLGA MARTINS S PASSARINI","MARIA SUELI GONCALVES DE OLIVEIRA")

# linhas a serem corrigidas
linhaMODIF <- linhaREC16[linhasPROBLE_txt]

# trocando caracteres
for (i in 1:length(caracINCORRETO)) {
  linhaMODIF[i] <- linhaMODIF[i] %>% 
    str_replace(
      caracINCORRETO[i],
      caracCORRETO[i]
    )  
}

# sobreescrevendo linhas corrigidas
linhaREC16[linhasPROBLE_txt] <- linhaMODIF

# salvando REC12 com linhas corrigidas
linhaREC16 %>% 
  write_lines("dados/2016/receitCAND16SP_numCOLUNASok.txt")