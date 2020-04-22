cidadesRMC <- c(
  "Americana", 
  "Artur Nogueira", 
  "Campinas", 
  "Cosmópolis", 
  "Engenheiro Coelho", 
  "Holambra", 
  "Hortolândia", 
  "Indaiatuba", 
  "Itatiba", 
  "Jaguariúna", 
  "Monte Mor", 
  "Morungaba", 
  "Nova Odessa", 
  "Paulínia", 
  "Pedreira", 
  "Santa Bárbara d'Oeste", 
  "Santo Antônio de Posse", 
  "Sumaré", 
  "Valinhos",
  "Vinhedo"
)
  
cidadesRMC <- cidadesRMC %>% 
  #rm_accent() %>% 
  str_squish() %>% 
  str_to_lower()  

cidadesRMC <- str_c(
  "  '",
  cidadesRMC,
  "',"
)
  

cidadesRMC %>% 
  write_lines(
    "~/arquivos_temporarios/projetos_pesquisa/mestrado_edy/cidadesRMC.txt"
  )
  
