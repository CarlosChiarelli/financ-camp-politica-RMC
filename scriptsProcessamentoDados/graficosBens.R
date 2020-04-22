# limpando memoria
rm(list = ls())

# bibliotecas
library(tidyverse)
source("~/arquivos_temporarios/R/temaGGPLOT2/TEMAelegante.R")

# lendo receita
receitaDF <- read_csv(
  "~/arquivos_temporarios/projetos_pesquisa/mestrado_edy/dados/RECEIT_RESULT_eleicaoRMC_pref12_16.csv",
  col_types = cols(.default = "c")
)

# lendo bens
bensDF <- read_csv("~/arquivos_temporarios/projetos_pesquisa/mestrado_edy/dados/bens_candidatos_12_16.csv")

# selecionando colunas de interesse
receitaDF <- receitaDF %>% 
  select(
    sequencial_candidato, municipio, sigla_partido,
    cargo, cpf_do_candidato, valor_receita, tipo_receita,
    ano_eleicao, DESC_SIT_TOT_TURNO
  )

RMC <- c(
  'americana' = 'americana',
  'artur nogueira' = 'artur nogueira',
  'campinas' = 'campinas',
  'cosmopolis' = 'cosmopólis',
  'engenheiro coelho' = 'engenheiro coelho',
  'holambra' = 'holambra',
  'hortolandia' = 'hortolândia',
  'indaiatuba' = 'indaiatuba',
  'itatiba' = 'itatiba',
  'jaguariuna' = 'jaguariúna',
  'monte mor' = 'monte mor',
  'morungaba' = 'morungaba',
  'nova odessa' = 'nova odessa',
  'paulinia' = 'paulínia',
  'pedreira' = 'pedreira',
  "santa barbara d'oeste" = "santa bárbara d'oeste",
  'santo antonio de posse' = 'santo antônio de posse',
  'sumare' = 'sumaré',
  'valinhos' = 'valinhos',
  'vinhedo' = 'vinhedo'
)

# corrigindo tipo da receita
nao_outros <- c(
  "Pessoas Físicas",
  "Pessoas Jurídicas",
  "Recursos Próprios",
  "Doação de Partido",
  "Outros Candidatos/Comitês"
)

names(nao_outros) <- c(
  "recursos de pessoas fisicas",
  "recursos de pessoas juridicas",
  "recursos proprios",
  "recursos de partido politico",
  "recursos de outros candidatos/comites"
)

ordemFAT_tipRec <- c(
  "Outras", 
  "Pessoas Jurídicas",
  "Recursos Próprios",
  "Pessoas Físicas",
  "Outros Candidatos/Comitês",
  "Doação de Partido"
)

# arrumando labels e fatores TIPO RECEITA
receitaDF <-  receitaDF %>% 
  mutate(
    tipo_receita = if_else(tipo_receita %in% names(nao_outros), tipo_receita, "outros"),
    tipo_receita = if_else(tipo_receita == "outros", "Outras", nao_outros[tipo_receita]),
    tipo_receita = factor(tipo_receita, levels = ordemFAT_tipRec),
    valor_receita = as.numeric(valor_receita)
  ) 

# somando o total de receita de cada candidato
totRecCand_df <- receitaDF %>% 
  group_by(
    ano_eleicao, municipio, sequencial_candidato, tipo_receita
  ) %>% 
  summarise(
    totReceTipo = sum(valor_receita)
  ) %>% 
  ungroup() %>% 
  group_by(
    ano_eleicao, municipio, sequencial_candidato
  ) %>% 
  mutate(
    totRece = sum(totReceTipo)
  ) %>% 
  ungroup() %>% 
  group_by(
    ano_eleicao, municipio
  ) %>% 
  mutate(
    totReceMun = sum(totReceTipo)
  ) %>% 
  ungroup()

# renomeando coluna sequencia
bensDF <- bensDF %>% 
  rename(sequencial_candidato = sq_candidato) %>% 
  mutate(
    ano_eleicao = as.character(ano_eleicao),
    sequencial_candidato = as.character(sequencial_candidato)
  )

# unindo bens com receita
totRecCand_df <- totRecCand_df %>% 
  left_join(bensDF)

# preenchendo missings bens com ZERO
totRecCand_df <- totRecCand_df %>% 
  mutate(valor_bemTOT = if_else(is.na(valor_bemTOT), 0, valor_bemTOT))

# aqui vamos unir ao coluna ELEITO ou NÂO
auxDF <- receitaDF %>% 
  select(ano_eleicao, municipio, sequencial_candidato, DESC_SIT_TOT_TURNO) %>% 
  filter(DESC_SIT_TOT_TURNO == "ELEITO") %>% 
  select(-DESC_SIT_TOT_TURNO) %>% 
  mutate(eleito = 1) %>% 
  distinct()

totRecCand_df <- totRecCand_df %>% 
  left_join(auxDF) %>% 
  mutate(eleito = if_else(is.na(eleito), 0, eleito))

# único tipo de receita que interessa é AUTOFINANCIAMENTO
totProprioDF <- totRecCand_df %>% 
  filter(tipo_receita == "Recursos Próprios")

# adicionando linha para eleitos que não autofinanciaram
eleitosSemAutofinanc_df <- totRecCand_df %>% 
  filter(eleito==1) %>% 
  mutate(autofinan = if_else(tipo_receita=="Recursos Próprios", 1, 0)) %>% 
  group_by(ano_eleicao, municipio, sequencial_candidato) %>% 
  mutate(autofinan = sum(autofinan)) %>% 
  ungroup() %>% 
  filter(autofinan==0) %>% 
  mutate(
    tipo_receita = "Recursos Próprios",
    totReceTipo = 0
  ) %>% 
  select(-autofinan) %>% 
  distinct()

# unindo linhas dos eleitos com autofinanciamento ZERO
totProprioDF <- totProprioDF %>% 
  bind_rows(eleitosSemAutofinanc_df)

###
### GRAFICO para generalizar ANO e MUNICIPIO

ano=2012
cidade='campinas'
  
# filtrando cidade, ano, eleito
auxDF <- totProprioDF %>% 
  filter(
    ano_eleicao == ano,
    municipio == cidade,
    eleito == 1
  ) 
  
# porcentagem bens, receita, recurso proprio
auxDF <- auxDF %>% 
  mutate(
    porcAutoFinBens = round((totReceTipo/valor_bemTOT)*100, 1),
    porcBensTotMun = round((valor_bemTOT/totRece)*100, 1)
  )

# unindo colunas tipo receita para plotar
auxDF <- auxDF %>%
  gather(tipoRec, valorRec, totReceTipo:valor_bemTOT)

# ADICIONAR 
# aqui vamos colocar a maior barra
maxRec <- auxDF$valorRec %>% max()

# criando ordem tipo receita com maior receita
tipoReceita <- auxDF %>% 
  arrange(valorRec) %>% 
  .$tipoRec

tipoReceita <- c("valor_bemTOT","totReceTipo","totRece","totReceMun" )

# ordem fatores tipos receita
auxDF <- auxDF %>% 
  mutate(tipoRec = factor(tipoRec, tipoReceita))

# titulo grafico
# 'Americana - Eleição 2012 - Bens do Eleito'
titulo = str_c(str_to_title(cidade), " - Eleição ", ano, ' - Bens do Eleito') 
# rotulos eixo X
rotulosX = rev(c("Receita do\nMunicípio","Receita do\n Candidato", "Receita do\nAutofinanciamento", "Bens do\n Candidato"))
# divisoes eixo Y
#maxRec <- auxDF %>% 
#  filter(tipoRec == "totReceMun") %>% 
#  .$valorRec
milhao <- if_else(maxRec > 1e6, T, F)
divEixoYreal <- seq(0, maxRec, length.out = 10)
maxRec <- if_else(maxRec > 1e6, maxRec/1e6, maxRec/1e3)
divEixoY <- seq(0, maxRec, length.out = 10) %>% round(1)
# nome eixo Y
titEixoY = str_c("\nValor (R$ ", if_else(milhao, "milhão", "mil"), ")\n")
# legenda com porcentagem
legenda = str_c(
  "Porcentagem dos BENS em Relação ao TOTAL da RECEITA:    ",
  auxDF$porcBensTotMun[1],
  "%\n\nPorcentagem dos RECURSOS PRÓPRIOS em Relação ao TOTAL de BENS:    ",
  auxDF$porcAutoFinBens[1],
  "%"
)

g <- auxDF %>% 
  ggplot(aes(tipoRec, valorRec, fill=factor(tipoRec)))+
  geom_col(col="black", alpha=.80)+
  #geom_text(aes(label = valorRec), size=20)+
  coord_flip()+
  scale_x_discrete(labels = rotulosX)+
  scale_y_continuous(
    labels = divEixoY,
    breaks = divEixoYreal
#    expand = expand_scale(
#      mult = c(.07, .25), 
#      add = c(2.5, 0)
#    )
  )+
  scale_fill_brewer(palette = "Set1")+
  labs(
    title = titulo,
    caption = legenda,
    x = "Receita Total\n",
    y = titEixoY
  )+
  temaPADRAO()+
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = 'gray', linetype=2, siz =2),
    plot.margin = unit(c(1,1,3,1),"lines"),
    text = element_text(size = 40),
    plot.title = element_text(size = 40),
    plot.caption = element_text(face = "bold"),
    axis.text.x = element_text(angle = 15, vjust = .1)
  )
  #annotation_custom(
  #  grob = grid::textGrob("Extra text.  Read all about it"),  
  #  xmin = 2, xmax = 2, ymin = -1, ymax = .005
  #)

g

ggsave(
  "~/imagens/mestrado_EDY_graficos/testeBensCand.jpeg",
  width = 10*2,
  height = 7*2
)

### reproduzindo para 2012 e 2016 todas cidades
cidades <- totProprioDF$municipio %>% unique()
anos <- totProprioDF$ano_eleicao %>% unique()

anos <- c(rep(2012, 20), rep(2016, 20))
cidades <- rep(cidades, 2)

map2(
  anos,
  cidades,
  function(ano, cidade){
    
    if(!(cidade %in% c('cosmopolis','valinhos') & ano == 2012)){
    # printando ano cidade para ver onde dá pau
    nomeGrafico <- str_c(cidade,"_",ano,".jpeg")
    print(nomeGrafico)
    
    # filtrando cidade, ano, eleito
    auxDF <- totProprioDF %>% 
      filter(
        ano_eleicao == ano,
        municipio == cidade,
        eleito == 1
      ) 
    
    # porcentagem bens, receita, recurso proprio
    auxDF <- auxDF %>% 
      mutate(
        # MUDANÇA legenda porcentagem
        #porcAutoFinBens = round((totReceTipo/valor_bemTOT)*100, 1),
        porcAutoFinBens = round((totReceTipo/valor_bemTOT)*100, 1),
        porcBensTotMun = round((valor_bemTOT/totReceMun)*100, 1)
      )
    
    # unindo colunas tipo receita para plotar
    auxDF <- auxDF %>%
      gather(tipoRec, valorRec, totReceTipo:valor_bemTOT)
    
    # criando ordem tipo receita com maior receita
    tipoReceita <- auxDF %>% 
      arrange(valorRec) %>% 
      .$tipoRec
    
    tipoReceita <- c("valor_bemTOT","totReceTipo","totRece","totReceMun" )
    
    # ordem fatores tipos receita
    auxDF <- auxDF %>% 
      mutate(tipoRec = factor(tipoRec, tipoReceita))
    
    # titulo grafico
    titulo = str_c(str_to_title(cidade), " - Eleição ", ano, ' - Bens do Eleito') 
    # rotulos eixo X
    rotulosX = rev(c("Receita de Todos\nos Candidatos","Receita do\n Eleito", "Receita do\nAutofinanciamento", "Bens do\n Eleito"))
    # divisoes eixo Y
    maxRec <- auxDF$valorRec %>% max()
    #maxRec <- auxDF %>% 
    #  filter(tipoRec == "totReceMun") %>% 
    #  .$valorRec
    milhao <- if_else(maxRec > 1e6, T, F)
    divEixoYreal <- seq(0, maxRec, length.out = 10)
    maxRec <- if_else(maxRec > 1e6, maxRec/1e6, maxRec/1e3)
    divEixoY <- seq(0, maxRec, length.out = 10) %>% round(1)
    # nome eixo Y
    titEixoY = str_c("\nValor (R$ ", if_else(milhao, "milhão", "mil"), ")\n")
    # legenda com porcentagem
    legenda = str_c(
      # Bens do Eleito X Receita de Todos os Candidatos 
      # = O Eleito possui Cinco vezes mais orçamento de toda a campanha eleitoral.
      # INALTERADO
      #"Porcentagem dos BENS DO ELEITO em relação a RECEITA DO MUNICÍPIO:    ",
      #auxDF$porcBensTotMun[1],
      'BENS DO ELEITO  x  RECEITA DE TODOS OS CANDIDATOS = \n',
      'O Eleito possui ',
      round(auxDF$porcBensTotMun[1]/100, 1),
      ' vezes do orçamento de toda a campanha eleitoral.',
      # Autofinanciamento X Bens do Candidato = O candidato usou X% dos seus Bens para se Eleger
      # INALTERADO
      # "%\n\nPorcentagem da RECEITA DE AUTOFINANCIAMENTO em relação ao BENS DO ELEITO:    ",
      # auxDF$porcAutoFinBens[1],
      # "%"
      "\n\nAUTOFINANCIAMENTO  x  BENS DO ELEITO =\n O candidato usou ",
      auxDF$porcAutoFinBens[1],
      "%",
      " dos seus Bens para se Eleger."
    )
    
    g <- auxDF %>% 
      ggplot(aes(tipoRec, valorRec, fill=factor(tipoRec)))+
      geom_col(col="black", alpha=.80)+
      #geom_text(aes(label = valorRec), size=20)+
      coord_flip()+
      scale_x_discrete(labels = rotulosX)+
      scale_y_continuous(
        labels = divEixoY,
        breaks = divEixoYreal
        #    expand = expand_scale(
        #      mult = c(.07, .25), 
        #      add = c(2.5, 0)
        #    )
      )+
      scale_fill_brewer(palette = "Set1")+
      labs(
        title = titulo,
        caption = legenda,
        x = "",
        y = titEixoY
      )+
      temaPADRAO()+
      theme(
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = 'gray', linetype=2, siz =2),
        plot.margin = unit(c(1,1,3,1),"lines"),
        text = element_text(size = 40),
        plot.title = element_text(size = 40),
        plot.caption = element_text(face = "bold"),
        axis.text.x = element_text(angle = 15, vjust = .1)
      )
    
    # salvando
    localSalvar <- str_c("~/imagens/mestrado_EDY_graficos/graficosBens_cidades12e16/", nomeGrafico)
    ggsave(
      localSalvar,
      width = 10*2,
      height = 7*2
    )
    }
    
    # retornando nada
    NULL
  }
)

## reproduzindo os gráficos FACETADO por ANO

map(
  cidades,
  function(cidade){
    
    # printando ano cidade para ver onde dá pau
    nomeGrafico <- str_c(cidade,".jpeg")
    print(nomeGrafico)
    
    # filtrando cidade, ano, eleito
    auxDF <- totProprioDF %>% 
      filter(
        municipio == cidade,
        eleito == 1
      ) 
    
    # porcentagem bens, receita, recurso proprio
    auxDF <- auxDF %>% 
      mutate(
        porcAutoFinBens = round((totReceTipo/valor_bemTOT)*100, 1),
        porcBensTotMun = round((valor_bemTOT/totRece)*100, 1)
      )
    
    # unindo colunas tipo receita para plotar
    auxDF <- auxDF %>%
      gather(tipoRec, valorRec, totReceTipo:valor_bemTOT)
    
    # ADICIONAR 
    # aqui vamos colocar a maior barra
    maxRec <- auxDF$valorRec %>% max()
    
    # criando ordem tipo receita com maior receita
    tipoReceita <- auxDF %>% 
      arrange(valorRec) %>% 
      .$tipoRec
    
    tipoReceita <- c("valor_bemTOT","totReceTipo","totRece","totReceMun" )
    
    # ordem fatores tipos receita
    auxDF <- auxDF %>% 
      mutate(tipoRec = factor(tipoRec, tipoReceita))
    
    # titulo grafico
    titulo = str_c("Receita Dos Eleitos ", str_to_title(cidade), " Nos Anos de 2012 e 2016") 
    # rotulos eixo X
    rotulosX = rev(c("Receita do\nMunicípio","Receita do\n Candidato", "Receita do\nAutofinanciamento", "Bens do\n Eleito"))
    # divisoes eixo Y
    #maxRec <- auxDF %>% 
    #  filter(tipoRec == "totReceMun") %>% 
    #  .$valorRec
    milhao <- if_else(maxRec > 1e6, T, F)
    divEixoYreal <- seq(0, maxRec, length.out = 10)
    maxRec <- if_else(maxRec > 1e6, maxRec/1e6, maxRec/1e3)
    divEixoY <- seq(0, maxRec, length.out = 10) %>% round(1)
    # nome eixo Y
    titEixoY = str_c("\nValor (R$ ", if_else(milhao, "milhão", "mil"), ")\n")
    # legenda com porcentagem
    legenda = str_c(
      "Porcentagem dos BENS em Relação ao TOTAL da RECEITA:    ",
      auxDF$porcBensTotMun[1],
      "%\n\nPorcentagem dos RECURSOS PRÓPRIOS em Relação ao TOTAL de BENS:    ",
      auxDF$porcAutoFinBens[1],
      "%"
    )
    
    g <- auxDF %>% 
      ggplot(aes(tipoRec, valorRec, fill=factor(tipoRec)))+
      geom_col(col="black", alpha=.80)+
      #geom_text(aes(label = valorRec), size=20)+
      coord_flip()+
      scale_x_discrete(labels = rotulosX)+
      scale_y_continuous(
        labels = divEixoY,
        breaks = divEixoYreal
        #    expand = expand_scale(
        #      mult = c(.07, .25), 
        #      add = c(2.5, 0)
        #    )
      )+
      scale_fill_brewer(palette = "Set1")+
      labs(
        title = titulo,
        #caption = legenda,
        x = "Receita Total\n",
        y = titEixoY
      )+
      facet_wrap(~ ano_eleicao, ncol = 1)+
      temaPADRAO()+
      theme(
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = 'gray', linetype=2, siz =2),
        plot.margin = unit(c(1,1,3,1),"lines"),
        text = element_text(size = 40),
        plot.title = element_text(size = 40),
        plot.caption = element_text(face = "bold"),
        axis.text.x = element_text(angle = 15, vjust = .1),
        axis.text.y = element_text(size=22)
      )
    
    # salvando
    localSalvar <- str_c("~/imagens/mestrado_EDY_graficos/graficosBens_cidades12e16/2012_2016_juntosFACETADO/", nomeGrafico)
    ggsave(
      localSalvar,
      width = 10*2,
      height = 7*2
    )
    
    # retornando nada
    NULL
  }
)
