rm(list = ls())

setwd('~/arquivos_temporarios/projetos_pesquisa/mestrado_edy/')

library(tidyverse)
source("~/arquivos_temporarios/R/temaGGPLOT2/TEMAelegante.R")

# RMC
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

nomes <- RMC
valores <- names(RMC)
RMC <- nomes
names(RMC) <- valores

# lendo dados
candsDF <- read_csv('dados/generoCandidatos12e16.csv')

# substituindo valores nulos
candsDF <- candsDF %>% 
  mutate(
    DESC_SIT_TOT_TURNO = if_else(DESC_SIT_TOT_TURNO=='#NULO#', 'NÃO ELEITO', DESC_SIT_TOT_TURNO),
    DESCRICAO_UE = RMC[DESCRICAO_UE]
  )

# contando numero genero candidatos
contCandsDF <- candsDF %>% 
  count(ANO_ELEICAO, DESCRICAO_UE, DESCRICAO_SEXO)

# divisoes eixo quantidade de candidatos
maxCand <- contCandsDF$n %>% max()
divisoesCands <- seq(0, 15, 2)

# ordem dos fatores
maioresCidad <- candsDF %>% 
  group_by(DESCRICAO_UE) %>% 
  summarise(cont=n()) %>% 
  ungroup() %>% 
  arrange(desc(cont)) %>% 
  .$DESCRICAO_UE

# 

contCandsDF %>% 
  ggplot(
    aes(
      factor(DESCRICAO_UE, levels = rev(maioresCidad)),
      n,
      fill=factor(DESCRICAO_SEXO)
    )
  )+
  geom_col(
    col='black',
    width = .7
  )+
  scale_y_continuous(
    labels = divisoesCands,
    breaks = divisoesCands
  )+
  facet_wrap(~ ANO_ELEICAO, scales = 'free_y')+
  coord_flip()+
  labs(
    title = 'Gênero dos Candidatos a Prefeito da RMC',
    x='Municípios',
    y='Quantidade de Candidatos',
    fill='sexo'
  )+
  temaPADRAO()+
  theme(
    legend.position = 'bottom',
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = 'gray'), #linetype=2, siz =1
    plot.margin = unit(c(1,1,3,1),"lines"),
    #text = element_text(size = 40),
    #plot.title = element_text(size = 40),
    plot.caption = element_text(face = "bold"),
    #axis.text.x = element_text(angle = 15, vjust = .1),
    axis.text.y = element_text(size=9)
  )

# salvando grafico
ggsave(
  "~/imagens/mestrado_EDY_graficos/generoCands.jpeg",
  width = 10,
  height = 7
)

# GENERO ELEITOS
eleitosDF <- candsDF %>% 
  filter(DESC_SIT_TOT_TURNO == 'ELEITO')

eleitosDF %>% 
  group_by(ANO_ELEICAO, DESCRICAO_SEXO) %>% 
  summarise(cont=n()) %>% 
  ggplot(aes(factor(DESCRICAO_SEXO), cont, fill=factor(DESCRICAO_SEXO)))+
  geom_col(col='black')+
  geom_text(aes(label=cont), vjust=-1, size=7)+
  labs(
    title = 'Gênero dos Candidatos a Prefeito Eleitos da RMC',
    x='Gênero',
    y='Quantidade de Candidatos',
    fill='sexo'
  )+
  scale_y_continuous(
    expand = expand_scale(
      mult = c(.07, .15)
      #add = c(2.5, 0)
    )
  )+
  facet_wrap(~ ANO_ELEICAO, scales = 'free_y')+
  temaPADRAO()+
  theme(
    legend.position = 'none',
    panel.grid.major.x = element_blank()
  )

# salvando grafico
ggsave(
  "~/imagens/mestrado_EDY_graficos/generoCandsEleitos.jpeg",
  width = 10,
  height = 7
)
