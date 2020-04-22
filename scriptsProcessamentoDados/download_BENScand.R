rm(list = ls())

setwd("arquivos_temporarios/projetos_pesquisa/mestrado_edy/")

library(tidyverse) 
#library(tabulizer)

links_download <- str_c(
  "http://agencia.tse.jus.br/estatistica/sead/odsele/bem_candidato/bem_candidato_20", 
  c("12", "16"),
  ".zip"
)
  
## dados 2012

# fazendo download arquivo 01
download.file(
  links_download[1],
  "dados/2012/bem_cand12.zip"
)

# dezipando
unzip(
  "dados/2012/bans_cand.zip",
  exdir = "dados/2012/bens_cands"
)

# removendo .zip
file.remove("dados/2016/bem_cand12.zip")

# removendo desnecessarios
arqsRemov <- dir("dados/2012/bens_cands/") %>% 
  str_subset(
    "SP|LEIA",
    negate = T
  )

# deixando apenas cands SP e pdf LEIAME 
for (arq in arqsRemov) {
  file.remove(
    str_c(
      "dados/2012/bens_cands/",
      arq
    )
  )
}


## dados 2016

# fazendo download arquivo 01
download.file(
  links_download[2],
  "dados/2016/bem_cand16.zip"
)

# dezipando
unzip(
  "dados/2016/bem_cand16.zip",
  exdir = "dados/2016/bens_cands"
)

# removendo .zip
file.remove("dados/2016/bem_cand16.zip")

# removendo desnecessarios
arqsRemov <- dir("dados/2016/bens_cands/") %>% 
  str_subset(
    "SP|leia",
    negate = T
  )

# deixando apenas cands SP e pdf LEIAME 
for (arq in arqsRemov) {
  file.remove(
    str_c(
      "dados/2016/bens_cands/",
      arq
    )
  )
}



