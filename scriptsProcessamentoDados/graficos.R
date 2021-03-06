# limpando memoria
rm(list = ls())

# bibliotecas
library(tidyverse)
source("~/arquivos_temporarios/R/temaGGPLOT2/TEMAelegante.R")

dfCOMPLETO <- read_csv("~/arquivos_temporarios/projetos_pesquisa/mestrado_edy/dados/RECEIT_RESULT_eleicaoRMC_pref12_16.csv",
                       col_types = cols(.default = "c"))

dfVOTO <- read_csv("~/arquivos_temporarios/projetos_pesquisa/mestrado_edy/dados/votos_2012_2016_SP.csv")
############## testes ##############

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

dftemp <- dfCOMPLETO %>% 
  mutate(
    valor_receita = as.numeric(valor_receita),
    municipio = RMC[municipio]
  ) %>% 
  select(
    municipio,
    ano_eleicao,
    valor_receita,
    tipo_receita
  )

maioreFATOR <- dftemp %>% 
  group_by(
    municipio
  ) %>% 
  summarise(totRECEI = sum(valor_receita)) %>% 
  ungroup() %>% 
  arrange((totRECEI)) %>% 
  .$municipio

dftemp %>% 
  group_by(
    ano_eleicao,
    municipio
  ) %>% 
  summarise(totRECEI = sum(valor_receita)) %>% 
  ungroup() %>% 
  ggplot(
    aes(
      factor(municipio, levels = maioreFATOR),
      totRECEI,
      fill=factor(ano_eleicao)
    )
  )+
  geom_bar(
    stat = "identity",
    position = position_dodge(),
    col="black",
    width = .75
  )+
  scale_y_continuous(
    breaks = seq(0, 1.6e7, 2e6),
    labels = seq(0, 16, 2),
    expand = c(.01,0)
  )+
  scale_fill_discrete(name="ano da eleição")+
  labs(
    title = "Total de Financiamento de Campanha\n de Cada Cidade da RMC",
    x= "municípios",
    y="total arrecadado em reais (MILHÕES)"
  )+
  coord_flip()+
  temaPADRAO()+
  theme(
    text = element_text(size = 15)
  )+
  guides(fill = guide_legend(reverse = TRUE))

ggsave("~/imagens/mestrado_EDY_graficos/RMC_prefeito_12_16_ARRECADACAO.jpeg", width = 10, height = 7)

# tipo de receita no RMC
nao_outros <- c(
  "recursos de pessoas fisicas",
  "recursos de pessoas juridicas",
  "recursos proprios"
)

dftemp %>% 
  mutate(
    tipo_receita = ifelse(
      tipo_receita %in% nao_outros, tipo_receita, "outros"
    )
  ) %>%
  group_by(
    ano_eleicao,
    tipo_receita
  ) %>% 
  summarise(total_rec = sum(valor_receita)) %>% 
  ungroup() %>% 
  mutate(
    tipo_receita = case_when(
      tipo_receita == "recursos de pessoas fisicas" ~ "recursos de pessoas físicas",
      tipo_receita == "recursos de pessoas juridicas" ~ "recursos de pessoas jurídicas",
      tipo_receita == "recursos proprios" ~ "recursos próprios",
      tipo_receita != "recursos de pessoas juridicas" & tipo_receita != "recursos de pessoas fisicas" & tipo_receita != "recursos proprios"~ tipo_receita
    )
  ) %>% 
  ggplot(
    aes(
      ano_eleicao,
      total_rec
    )
  )+
  geom_bar(
    aes(fill = factor(tipo_receita)),
    stat = "identity",
    position = position_dodge2(),
    col = "black",
    width = .8
  )+
  geom_text(
    aes(label = round(total_rec/1e6, 2)),
    position = position_dodge2(width = .8),
    vjust = -1,
    size = 5
  )+
  scale_y_continuous(
    labels = seq(0, 20, 5),
    breaks = seq(0, 20e6, 5e6),
    limits = c(0, 22.5e6)
  )+
  labs(
    title = "Perfil das Doações da RMC",
    fill = "tipo receita",
    x = "Ano da eleição",
    y = "total arrecadado (milhões)"
  )+
  guides(
    fill = guide_legend(ncol = 2)
  )+
  temaPADRAO()+
  theme(
    legend.position = "bottom",
    legend.key.size = unit(1.3, "lines")
  )

ggsave("~/imagens/mestrado_EDY_graficos/tipo_receitas_todasRMC.jpeg", width = 10, height = 7)

# tipo de receita facetado
dftemp %>% 
  mutate(
    tipo_receita = ifelse(
      tipo_receita %in% nao_outros, tipo_receita, "outros"
    )
  ) %>%
  group_by(
    ano_eleicao,
    tipo_receita
  ) %>% 
  summarise(total_rec = sum(valor_receita)) %>% 
  ungroup() %>% 
  mutate(
    tipo_receita = case_when(
      tipo_receita == "recursos de pessoas fisicas" ~ "pessoas físicas",
      tipo_receita == "recursos de pessoas juridicas" ~ "pessoas jurídicas",
      tipo_receita == "recursos proprios" ~ "recursos próprios",
      tipo_receita != "recursos de pessoas juridicas" & tipo_receita != "recursos de pessoas fisicas" & tipo_receita != "recursos proprios"~ tipo_receita
    )
  ) %>% 
  ggplot(
    aes(
      ano_eleicao,
      total_rec
    )
  )+
  geom_bar(
    aes(fill = factor(tipo_receita)),
    stat = "identity",
    position = position_dodge2(),
    col = "black",
    width = .8
  )+
  geom_text(
    aes(label = round(total_rec/1e6, 2)),
    position = position_dodge2(width = .8),
    vjust = -1,
    size = 5
  )+
  scale_y_continuous(
    labels = seq(0, 20, 5),
    breaks = seq(0, 20e6, 5e6),
    limits = c(0, 24e6)
  )+
  labs(
    title = "Perfil das Doações da RMC",
    fill = "tipo receita",
    x = "Ano da eleição",
    y = "total arrecadado (milhões)"
  )+
  facet_wrap(~ tipo_receita, nrow = 1)+
  guides(
    fill = guide_legend(ncol = 2)
  )+
  temaPADRAO()+
  theme(
    legend.position = "bottom",
    legend.key.size = unit(1.3, "lines")
  )

ggsave("~/imagens/mestrado_EDY_graficos/tipo_receitasSEPERADOS_todasRMC.jpeg", width = 10, height = 7)

## barras empilhadas
# tipo de receita no RMC
nao_outros <- c(
  "recursos de pessoas fisicas",
  "recursos de pessoas juridicas",
  "recursos proprios"
)

dftemp %>% 
  mutate(
    tipo_receita = ifelse(
      tipo_receita %in% nao_outros, tipo_receita, "outros"
    )
  ) %>%
  group_by(
    ano_eleicao,
    tipo_receita
  ) %>% 
  summarise(total_rec = sum(valor_receita)) %>% 
  group_by(ano_eleicao) %>% 
  mutate(tot_ano = sum(total_rec)) %>% 
  ungroup() %>% 
  mutate(
    tipo_receita = case_when(
      tipo_receita == "recursos de pessoas fisicas" ~ "recursos de pessoas físicas",
      tipo_receita == "recursos de pessoas juridicas" ~ "recursos de pessoas jurídicas",
      tipo_receita == "recursos proprios" ~ "recursos próprios",
      tipo_receita != "recursos de pessoas juridicas" & tipo_receita != "recursos de pessoas fisicas" & tipo_receita != "recursos proprios"~ tipo_receita
    )
  ) %>% 
  ggplot(
    aes(
      ano_eleicao,
      total_rec
    )
  )+
  geom_bar(
    aes(fill = factor(tipo_receita)),
    stat = "identity",
    col = "black",
    width = .8
  )+
#  geom_text(
#    aes(label = round(total_rec/1e6, 2)),
#    position = position_stack(),
#    vjust = -1,
#    size = 5
#  )+
#  annotate("text", c(2012,2016), c(33.11e6, 18.43e6), label = c(33.11, 18.43))+
  scale_y_continuous(
    labels = seq(0, 30, 10),
    breaks = seq(0, 3e7, 1e7),
    limits = c(0, 3.5e7)
  )+
  labs(
    title = "Perfil das Doações da RMC",
    fill = "tipo receita",
    x = "Ano da eleição",
    y = "total arrecadado (milhões)"
  )+
  guides(
    fill = guide_legend(ncol = 2)
  )+
  temaPADRAO()+
  theme(
    legend.position = "bottom",
    legend.key.size = unit(1.3, "lines")
  )

ggsave("~/imagens/mestrado_EDY_graficos/tipo_receitas_empilhada.jpeg", width = 10, height = 7)

# preço do voto
dfVOTOtemp <- dfVOTO %>% 
  mutate(
    DESCRICAO_UE = DESCRICAO_UE %>% 
      str_to_lower() %>% 
      str_squish() %>% 
      abjutils::rm_accent()
  ) %>% 
  filter(
    DESCRICAO_UE %in% names(RMC) | DESCRICAO_UE == "santa barbara d oeste",
    NUM_TURNO == 1,
    DESCRICAO_CARGO %in% c("Prefeito","PREFEITO")
  ) 

dfVOTOtemp <- dfVOTOtemp %>% 
  group_by(
    ANO_ELEICAO,
    DESCRICAO_UE
  ) %>% 
  summarise(
    tot_votos = sum(TOTAL_VOTOS)
  ) %>% 
  ungroup() %>% 
  mutate(
    DESCRICAO_UE = if_else(DESCRICAO_UE == "santa barbara d oeste", "santa barbara d'oeste", DESCRICAO_UE)
  ) %>% 
  group_by(
    ANO_ELEICAO
  ) %>% 
  summarise(total_votos = sum(tot_votos))

dftemp %>% 
  mutate(ano_eleicao = as.numeric(ano_eleicao)) %>% 
  group_by(
    ano_eleicao
  ) %>% 
  summarise(tot_rec = sum(valor_receita)) %>% 
  ungroup() %>% 
  inner_join(
    dfVOTOtemp,
    by = c("ano_eleicao" = "ANO_ELEICAO")
  ) %>% 
  mutate(
    valor_voto = tot_rec/total_votos
  ) %>% 
  ggplot(aes(factor(ano_eleicao), valor_voto))+
  geom_bar(
    stat = "identity",
    fill = "skyblue",
    col = "black"
  )+
  geom_text(
    aes(label = round(valor_voto,2)),
    vjust = -1,
    size = 6
  )+
  ylim(c(0,24))+
  labs(title = "Preço do Voto da RMC (R$ para cada voto)",
       x = "Ano da eleição",
       y = "Preço do voto")+
  temaPADRAO()
  
ggsave("~/imagens/mestrado_EDY_graficos/preco_do_voto.jpeg", width = 10, height = 7)

# patidos receita
partidos_ordem <- dfCOMPLETO %>% 
  group_by(
    sigla_partido
  ) %>% 
  summarise(tot_rec = sum(as.numeric(valor_receita))) %>% 
  ungroup() %>% 
  arrange(desc(tot_rec)) %>% 
  .$sigla_partido

dfCOMPLETO %>% 
  mutate(
    sigla_partido = factor(sigla_partido, levels = partidos_ordem)
  ) %>% 
  group_by(
    ano_eleicao,
    sigla_partido
  ) %>% 
  summarise(tot_rec = sum(as.numeric(valor_receita))) %>% 
  ungroup() %>% 
  ggplot(aes(sigla_partido, tot_rec))+
  geom_bar(
    aes(fill = factor(sigla_partido)),
    stat = "identity",
    col = "black"
  )+
  scale_y_continuous(
    breaks = seq(0, 1e7, 2.5e6),
    labels = seq(0, 10, 2.5)    
  )+
  labs(
    title = "Financiamento por Partido da RMC",
    x="Partidos",
    y="Valor Total da Receita (milhões)",
    fill = "Partidos"
  )+
  facet_wrap(~ ano_eleicao, ncol = 1, scales = "free_x")+
  temaPADRAO()+
  theme(
    axis.text.x = element_text(size = 13, angle = 30)
  )

ggsave("~/imagens/mestrado_EDY_graficos/partidos_receita.jpeg", width = 10*1.5, height = 7*1.5)

# mapa colorido com receita
library(brazilmaps)

mapa_sp <- get_brmap(
  geo = "City",
  geo.filter = list(State = 35)
)

# removendo acentos e letras maiusculas
mapa_sp <- mapa_sp %>% 
  mutate(
    nome = nome %>% 
      str_to_lower() %>% 
      abjutils::rm_accent()
  )

dfPlot_mapa <- dftemp %>% 
  group_by(
    ano_eleicao,
    municipio
  ) %>% 
  summarise(tot_rec = sum(valor_receita)) %>% 
  ungroup() %>% 
  mutate(tot_rec = log(tot_rec))
  
mapa_RMC <- mapa_sp %>% 
  inner_join(
    dfPlot_mapa,
    by = c("nome" = "municipio")
  ) %>% 
  ggplot()+
  geom_sf(
    aes(fill = tot_rec),
    col = "black"
  )+
  scale_fill_viridis_c(option = "C")

mapa_RMC2 <- mapa_RMC +
  facet_wrap(~ ano_eleicao)+
  labs(
    title = "Total arrecadado por cada cidade da RMC",
    fill = "milhões\n"
  )+
  theme_bw()+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )



library(rayshader)

#nc = st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
#gg_nc = ggplot(nc) +
#  geom_sf(aes(fill = AREA)) +
#  scale_fill_viridis("Area") +
#  ggtitle("Area of counties in North Carolina") +
#  theme_bw()

#{plot_gg(mapa_RMC, multicore = TRUE, width = 6 ,height=2.7, fov = 70)
#render_depth(focallength=100)
#render_snapshot(clear = T)
#}

#{plot_gg(mapa_RMC, multicore = TRUE, raytrace = TRUE, width = 7, height = 4, 
#         scale = 300, windowsize = c(1400, 866), zoom = 0.6, phi = 30, theta = 30)
#  render_snapshot(clear = T)
#}

{plot_gg(mapa_RMC2, width = 5, height = 5, raytrace = FALSE, multicore = T)
  render_camera(zoom = .6, phi=70)
  render_snapshot(filename = "~/imagens/mestrado_EDY_graficos/mapa3D_teste.jpeg",clear = T)
}

#save_3dprint("~/imagens/mapa3D_teste.jpeg")

# grafico incumbentes
incumb_df <- read_csv("~/arquivos_temporarios/projetos_pesquisa/mestrado_edy/dados/incunbemtes_12_16.csv")

# limpando nomes colunas
incumb_df <- incumb_df %>% 
  select(
    CPF_CANDIDATO, DESCRICAO_UE, ano2012:incub16
  ) %>% 
  janitor::clean_names() 

# DF para plotar receita de incumbentes
receitaINCUN_df <- dfCOMPLETO %>% 
  mutate(
    valor_receita = as.numeric(valor_receita)
    #municipio = RMC[municipio]
  ) %>% 
  select(
    cpf_do_candidato,
    municipio,
    ano_eleicao,
    valor_receita,
    tipo_receita,
    COMPOSICAO_LEGENDA
  ) %>% 
  janitor::clean_names() %>% 
  mutate(
    quantPART_colig = str_count(composicao_legenda, "/") + 1
  )
  
# unindo receita com dados de incumbente
receitaINCUN_df <- receitaINCUN_df %>% 
  inner_join(
    incumb_df,
    by = c(
      "cpf_do_candidato" = "cpf_candidato",
      "municipio" = "descricao_ue"
    )
  ) 

# colocando numero nos desafiantes
aux_temp <- receitaINCUN_df %>% 
  select(
    cpf_do_candidato:valor_receita
  ) %>% 
  group_by(
    municipio, ano_eleicao, cpf_do_candidato
  ) %>% 
  summarise(totREC = sum(valor_receita)) %>% 
  arrange(desc(totREC)) %>% 
  mutate(
    posicaoREC = 1:n()
  ) %>% 
  ungroup() %>% 
  select(-totREC)

# unindo posicao de receita total com RECEITA
receitaINCUN_df <- receitaINCUN_df %>% 
  left_join(aux_temp) 

# criando linhas de receita total da cidade
aux_temp <- receitaINCUN_df %>% 
  group_by(
    ano_eleicao, municipio, tipo_receita
  ) %>% 
  summarise(
    valor_receita = sum(valor_receita)
  ) %>% 
  ungroup() %>% 
  mutate(
    cpf_do_candidato = "total_receita"
  )

# unindo linhas
receitaINCUN_df <- receitaINCUN_df %>% 
  bind_rows(aux_temp)

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

# somando o total de receita de cada candidato
totREC_df <- receitaINCUN_df %>% 
  select(
    cpf_do_candidato:valor_receita, tipo_receita, quantPART_colig,
    ano2012:incub16, posicaoREC
  ) %>% 
  mutate(
    tipo_receita = if_else(tipo_receita %in% names(nao_outros), tipo_receita, "outros"),
    tipo_receita = if_else(
        tipo_receita == "outros",
        "Outras",
        nao_outros[tipo_receita]
      ),
    tipo_receita = factor(tipo_receita, levels = ordemFAT_tipRec)
    ) %>% 
  #filter(
  #  ano_eleicao == 2012,
  #  municipio == "campinas"
  #) %>%
  group_by(
    ano_eleicao, municipio, cpf_do_candidato, quantPART_colig, ano2012,
    ano2016,incub12, incub16, posicaoREC, tipo_receita
  ) %>% 
  summarise(
    totRECE = sum(valor_receita)
  ) %>% 
  ungroup() %>% 
  group_by(
    ano_eleicao, municipio
  ) %>% 
  mutate(
    recMUNIC = sum(totRECE)/2
  ) %>% 
  ungroup()

# barra receita cidade aparecer primeiro 
cpf_cands <- totREC_df %>% 
  arrange(desc(totRECE)) %>% 
  .$cpf_do_candidato %>% 
  unique()


######## REMOVER
# partido de cada candidato
partidos_df <- dfCOMPLETO %>% 
  select(
    ano_eleicao, municipio, cpf_do_candidato, sigla_partido
  ) %>% 
  distinct() 

# total receita cada candidato
# para inicializar a seta no local correto
totRecCand_df <- dfCOMPLETO %>% 
  select(
    cpf_do_candidato, ano_eleicao, municipio, valor_receita
  ) %>% 
  group_by(
    ano_eleicao, municipio, cpf_do_candidato
  ) %>% 
  summarise(
    RecCand = sum(as.numeric(valor_receita))
  ) %>% 
  distinct()

# unindo partido e receita total de cada candidato
partidos_df <- partidos_df %>% 
  left_join(totRecCand_df)

# unindo coluna partidos para plotar
totREC_df <- totREC_df %>% 
  left_join(partidos_df)
######## REMOVER

# colocando ordem barras e nome de cada barra 
totREC_df <- totREC_df %>% 
  mutate(
    cpf_do_candidato = factor(cpf_do_candidato, levels = cpf_cands),
    sitPOSIC12 = case_when(
      cpf_do_candidato == "total_receita" ~ "receita geral",
      cpf_do_candidato != "total_receita" & incub12 == 0 ~ str_c("desafi ", posicaoREC),
      cpf_do_candidato != "total_receita" & incub12 == 1 ~ str_c("incum ", posicaoREC)
    ),
    sitPOSIC16 = case_when(
      cpf_do_candidato == "total_receita" ~ "receita geral",
      cpf_do_candidato != "total_receita" & incub16 == 0 ~ str_c("desafi ", posicaoREC),
      cpf_do_candidato != "total_receita" & incub16 == 1 ~ str_c("incum ", posicaoREC)
    ),
    sitPOSIC12 = if_else(ano2012 == 1, str_c("eleito ", sitPOSIC12), sitPOSIC12),
    sitPOSIC16 = if_else(ano2016 == 1, str_c("eleito ", sitPOSIC16), sitPOSIC16),
    sitPOSIC12 = if_else(is.na(sitPOSIC12), "receita geral", sitPOSIC12),
    sitPOSIC16 = if_else(is.na(sitPOSIC16), "receita geral", sitPOSIC16)
  ) 

# grafico 01
plotar_df <- totREC_df %>% 
  filter(
    ano_eleicao == 2012,
    municipio == "campinas"
  ) %>% 
  mutate(
    tamColig = quantPART_colig/max(quantPART_colig, na.rm = T),
    tamColig = recMUNIC*tamColig
  ) %>% 
  group_by(ano_eleicao, municipio, cpf_do_candidato) %>% 
  mutate(numLin = 1:n()) %>% 
  ungroup() %>% 
  group_by(ano_eleicao, municipio) %>% 
  mutate(
    porcPartColig = sum(quantPART_colig, na.rm = T),
    porcPartColig = quantPART_colig/porcPartColig,
    rotuloX = str_c(
      sigla_partido, "\n(", quantPART_colig, " PARTIDO", if_else(quantPART_colig==1, ")", "S)")
    )
  ) %>% 
  ungroup()

# nomes barras
categ_cands <- plotar_df %>% 
  select(cpf_do_candidato, recMUNIC, sitPOSIC12) %>% 
  distinct()

# quantidade de partidos coligação
quantColig <- plotar_df %>% 
  select(cpf_do_candidato, quantPART_colig, rotuloX) %>% 
  distinct() %>% 
  mutate(
    #quantPART_colig = as.character(quantPART_colig) ,
    #quantPART_colig = if_else(is.na(quantPART_colig), "", quantPART_colig),
    rotuloX = if_else(is.na(rotuloX), "", rotuloX)
    #rotuloX = str_replace(rotuloX, "\n(1 PARTIDOS)", "\n(1 PARTIDO)")
    #rotuloX = if_else(
    #  str_detect(rotuloX, "1"),
    #  str_replace(rotuloX, "PARTIDOS", "PARTIDO"),
    #  rotuloX
    #)
  )

# grafico com incumbentes e tipo de receita  
plotar_df %>%   
  ggplot(
    aes(cpf_do_candidato, totRECE)
  )+
  geom_text(
    data = categ_cands,
    aes(label = sitPOSIC12, y = if_else(cpf_do_candidato == "total_receita",recMUNIC, .8*recMUNIC)),
    vjust = -1,
    angle = 10
  )+
  geom_bar(
    aes(fill = factor(tipo_receita)),
    stat = "identity",
    col = "black"
  )+
  #annotate(
  #  "text",
  #  
  #)
  #geom_text(
  #  aes(
  #    label = quantPART_colig,
  #    size = tamColig_txt,
  #    y=rel(-.05*recMUNIC)
  #  ),
  #  show.legend = F
  #)+
  scale_y_continuous(
    expand = expand_scale(
      mult = c(.05, .2), 
      add = c(2, 0)
    )
  )+
  scale_x_discrete(
    labels = quantColig$quantPART_colig,
    breaks = quantColig$cpf_do_candidato
  )+
  labs(
    title = "Receita dos competidores - Campinas eleição 2012",
    x = "número de partidos na coligação",
    fill = "tipo da receita",
    y = "Receita (R$)"
  )+
  guides(
    fill = guide_legend(ncol = 2)
  )+
  temaPADRAO()+
  theme(
    legend.position = "bottom",
    legend.key.size = unit(1.3, "lines")
  )

ggsave("~/imagens/mestrado_EDY_graficos/receita_incumbente_CIDADE.jpeg", width = 10, height = 7)

# arrumando texto dos gráficos finais
teste_rotulosNOMES <-c(
  totREC_df$sitPOSIC12,
  totREC_df$sitPOSIC16
) %>% 
  unique()

# substituindo para o padrão
teste_rotulos <- teste_rotulosNOMES %>% 
  str_replace_all("receita geral", "Receita Geral da Cidade") %>% 
  str_replace_all("eleito desafi ", "[ELEITO - DESAFIANTE - R$") %>% 
  str_replace_all("eleito incum ", "[ELEITO - INCUMBENTE - R$") %>% 
  str_replace_all("desafi ", "Desafiante - R$") %>% 
  str_replace_all("incum ", "Incumbente - R$")
teste_rotulos <- teste_rotulos %>% 
  str_c(
    if_else(
      str_detect(teste_rotulos, "ELEITO"), "]", ""
    )
  )
  
# colocando nomes iguais aos do DF
names(teste_rotulos) <- teste_rotulosNOMES
  
# atualizando rótulos
categ_cands <- categ_cands %>% 
  mutate(
    sitPOSIC12 = teste_rotulos[sitPOSIC12]
  )

# pegando receita municipio (rotulos eixo y) 
maxREC <- plotar_df %>% 
  .$recMUNIC %>% 
  unique()
intervREC <- seq(0, maxREC, length.out = 5)  
intervREC <- (intervREC/if_else(maxREC > 1e6, 1e6, 1e3)) %>% 
  round(1)
maxREC <- if_else(maxREC > 1e6, "milhão", "mil")

# garantindo que todos tipos de receita vão aparecer
remover_df <- tibble(
  cpf_do_candidato = "total_receita",
  tipo_receita = factor(ordemFAT_tipRec, levels = ordemFAT_tipRec),
  totRECE = 0
)

# grafico com incumbentes com BARRA COLIGACAO
plotar_df %>%   
  bind_rows(remover_df) %>% 
  #left_join(totRecCand_df)
  ggplot(
    aes(cpf_do_candidato, totRECE)
  )+
  geom_text(
    data = categ_cands,
    #aes(label = sitPOSIC12, y = if_else(cpf_do_candidato == "total_receita",recMUNIC, .8*recMUNIC)),
    aes(
      label = case_when(
        str_detect(sitPOSIC12, "ELEITO") ~ str_replace(sitPOSIC12, "]", "º]"),
        str_detect(sitPOSIC12, "Cidade") ~ sitPOSIC12,
        str_detect(sitPOSIC12, c("ELEITO","Cidade"), T) ~ str_c(sitPOSIC12, "º")
      ),
      y = recMUNIC,
      fontface = if_else(str_detect(sitPOSIC12, "ELEITO"), "bold", "plain")
    ),
    #vjust = -2.5,
    vjust = -3.5,
    #hjust = .4,
    hjust = .3,
    #angle = 9,
    angle = 20,
    #size = 4.5
    size = 4.75
  )+
  geom_bar(
    aes(fill = factor(tipo_receita)),
    stat = "identity",
    col = "black"
  )+
  geom_segment(
    aes(
      x = cpf_do_candidato,
      xend = cpf_do_candidato,
      y = RecCand,
      yend = recMUNIC*1.08
    ),
    arrow = arrow(
      #length = unit(0.30,"cm")
      length = unit(0.30,"cm")
    ),
    col = "gray50",
    size = 2
  )+
  geom_bar(
    aes(y = tamColig),
    stat = "identity",
    alpha = .2,
    width = .25,
    position =position_dodge(width=.5)
  )+
  geom_point(
    aes(y = tamColig),
    stat = "identity",
    #shape = "_",
    width = .25,
    size = 3,
    position =position_dodge(width=.5)
  )+
  #annotate(
  #  "text",
  #  
  #)
  #geom_text(
  #  aes(
  #    label = quantPART_colig,
  #    size = tamColig_txt,
  #    y=rel(-.05*recMUNIC)
  #  ),
  #  show.legend = F
#)+
  scale_y_continuous(
    expand = expand_scale(
      #mult = c(.05, .2), 
      mult = c(.07, .25), 
      #add = c(2, 0)
      add = c(2.5, 0)
    ),
    labels = intervREC,
    breaks = intervREC*if_else(maxREC=="milhão", 1e6, 1e3)
  )+
  scale_x_discrete(
    #labels = quantColig$quantPART_colig,
    #breaks = quantColig$cpf_do_candidato
    labels = quantColig$rotuloX,
    breaks = quantColig$cpf_do_candidato
  )+
  scale_fill_brewer(palette = "Dark2")+
  labs(
    title = "Receita dos Competidores - Campinas, eleição de 2012",
    x = "COLIGAÇÃO",
    fill = "Origem da Receita",
    y = str_c("Receita  (R$ ", maxREC, ")") %>% str_to_upper()
  )+
  guides(
    fill = guide_legend(ncol = 3)
  )+
  temaPADRAO()+
  theme(
    legend.position = "bottom",
    legend.key.size = unit(1.3, "lines"),
    axis.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(face = "bold")
  )

ggsave("~/imagens/mestrado_EDY_graficos/receita_incumbente_CIDADEv3.jpeg", width = 10*1.25, height = 7*1.25)






