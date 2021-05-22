# Amostra de Script - Pré-processamento dos dados

library(tidyverse)
library(here)
library(lubridate)

#### Carregamento dos dados, seleção das variáveis, conversão de variáveis* ####

load(here("data/unsc_full.RData"))

unsc_full_filt <- unsc_full %>% 
  select(filename, country, spv, date, year, agenda_item2, agenda_item3, participanttype) %>% 
  mutate(date = dmy(date), # conversão para formato data
         spv = as.numeric(str_extract(spv, "[0-9]{4}")), # modificação para manter apenas o número dos encontros* (obs1)
         topics = ifelse(agenda_item3 == "Thematic", agenda_item2, agenda_item3)) %>% #* coluna com os tópicos em detalhes (obs2)
  tibble()

#*(obs1) Os encontros no CSNU podem durar muito tempo e por isso divididos em partes. 
# Por isso, um mesmo encontro pode ter diferentes nomenclaturas, como 7285 e 7285Resumption
# Para contar contabilizar corretamente o número de encontros, mantemos apenas a indicação dos números

#*(obs2) A coluna agenda_item3 contém os tópicos dos encontros, priorizando os tópicos que versaram sobre regiões geográficas específicas,
# mantendo o restante como "temático". A coluna agenda_item2 contém detalhes sobre o conteúdo de encontros temáticos.
# Reunimos as duas informações para identificar os tópicos mais debatidos com o maior nível de detalhe

#### Filtrar discursos para manter apenas discursos feitos em capacidade nacional*(obs3) ####

#*(obs3) Os membros nem sempre discursam em nome dos países que representam.
# Quando ocupam a posição de Presidente, eles podem proferir discursos meramente protocolares, como "passo a palavra ao representante X",
# além de discursar condição de membro de algum órgão subsidiário da ONU. 
# Por isso, para calcular o número de discursos por país, precisamos manter apenas os discursos presidenciais proferidos em capacidade nacional.

# Os arquivos "un_capacities_speeches" e "national_capacities_filenames" possuem os códigos de todos os discursos proferidos
# em capacidade nacional ou enquanto funcionário da ONU
# Por ser uma etapa simples, este código poderá ser rodado no RMD

#load(here("data/un_capacities_speeches.RData"))
#load(here("data/national_capacities_filenames.RData"))

#unsc_filt <- unsc_full %>% 
#  filter(!(participanttype == "The President" &
#             !filename %in% c(un_capacities_speeches,
#                              national_capacities_filenames)))

#### Traduzir nomes de países e tópicos para português ####

# O banco de dados ainda não possui colunas para nomes de países e tópicos em português
# Por ora, vamos traduzir apenas os nomes que aparecem nos resultados da análise
# Análises mais extensas demandariam a tradução de todos os nomes com ajuda do pacote "countrycode"

unsc_full_filt <- unsc_full_filt %>% 
  mutate(topics = recode(topics,
                         "Somalia" = "Somália",
                         "Sudan/South Sudan" = "Sudão/Sudão do Sul",
                         "Democratic Republic of the Congo" = "República Democrática do Congo",
                         "Israel/Palestine" = "Israel/Palestina",
                         "Afghanistan" = "Afeganistão",
                         "Syria" = "Síria",
                         "Sudan" = "Sudão",
                         "Terrorism" = "Terrorismo",
                         "Côte d'Ivoire" = "Costa do Marfim"),
         country = recode(country,
                          "United Kingdom Of Great Britain And Northern Ireland" = "Reino Unido",
                          "United States Of America" = "Estados Unidos",
                          "France" = "França",
                          "Russian Federation" = "Rússia",
                          "UN" = "ONU",
                          "Japan" = "Japão",
                          "EU" = "União Europeia",
                          "Germany" = "Alemanha",
                          "South Africa" = "África do Sul",
                          "Bolivia (Plurinational State Of)" = "Bolivia",
                          "Venezuela (Bolivarian Republic Of)" = "Venezuela"),
         agenda_item3 = recode(agenda_item3,
                               "Afghanistan" = "Afeganistão",
                               "Bosnia and Herzegovina" = "Bósnia e Herzegovina",
                               "Côte d'Ivoire" = "Costa do Marfim",
                               "Croatia" = "Croácia",
                               "Democratic Republic of the Congo" = "República Democrática do Congo",
                               "Iraq/Kuwait" = "Iraque/Kuwait",
                               "Israel/Palestine" = "Israel/Palestina",
                               "Libya" = "Líbia",
                               "Sierra Leone" = "Serra Leoa",
                               "Sudan" = "Sudão",
                               "Sudan/South Sudan" = "Sudão/Sudão do Sul",
                               "Syria" = "Síria",
                               "Ukraine" = "Ucrânia"))

save(unsc_full_filt, file = here("data/unsc_full_filt.RData"))

