# ETAPA 1 - PRÉ-PROCESSAMENTO DOS DADOS

# Pacotes para pré-processamento
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

#*(obs1) Os encontros no CSNU podem durar muito tempo e por isso são divididos em partes. 
# Um mesmo encontro pode ter diferentes nomenclaturas, como 7285 e 7285Resumption
# Para contabilizar corretamente o número de encontros, mantemos apenas a indicação dos números

#*(obs2) A coluna agenda_item3 contém os tópicos dos encontros, priorizando os tópicos que versaram sobre regiões geográficas específicas,
# mantendo o restante como "temático". A coluna agenda_item2 contém detalhes sobre o conteúdo de encontros temáticos.
# Reunimos as duas informações para identificar os tópicos mais debatidos com o maior nível de detalhe possível

#### Filtrar discursos para manter apenas discursos feitos em capacidade nacional*(obs3) ####

#*(obs3) Os membros nem sempre discursam em nome dos países que representam.
# Quando ocupam a posição de Presidente, eles podem proferir discursos meramente protocolares, como "passo a palavra ao representante X",
# discursos em nome de todo o Conselho, chamados de "Declarações Presidenciais", além de pronunciamentos em capacidade nacional ou
# representando algum órgão da ONU. 
# Para calcular o número de discursos por país, precisamos manter apenas os discursos presidenciais proferidos em capacidade nacional ou
# em nome de órgão da ONU, uma vez que representam entes específicos.

# Os arquivos "un_capacities_speeches" e "national_capacities_filenames" possuem os códigos de todos os discursos proferidos por Presidentes
# do Conselho em capacidade nacional ou enquanto funcionário da ONU
# Por ser uma etapa simples, este código poderá ser rodado direto no RMD

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
  mutate(agenda_item3 = recode(agenda_item3,
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
                               "Ukraine" = "Ucrânia"),
         country = recode(country,
                          "Bolivia (Plurinational State Of)" = "Bolivia",
                          "EU" = "União Europeia",
                          "France" = "França",
                          "Germany" = "Alemanha",
                          "Japan" = "Japão",
                          "Russian Federation" = "Rússia",
                          "South Africa" = "África do Sul",
                          "UN" = "ONU",
                          "United Kingdom Of Great Britain And Northern Ireland" = "Reino Unido",
                          "United States Of America" = "Estados Unidos",
                          "Venezuela (Bolivarian Republic Of)" = "Venezuela"),
         topics = recode(topics,
                         "Afghanistan" = "Afeganistão",
                         "Côte d'Ivoire" = "Costa do Marfim",
                         "Democratic Republic of the Congo" = "República Democrática do Congo",
                         "Israel/Palestine" = "Israel/Palestina",
                         "Somalia" = "Somália",
                         "Sudan" = "Sudão",
                         "Sudan/South Sudan" = "Sudão/Sudão do Sul",
                         "Syria" = "Síria",
                         "Terrorism" = "Terrorismo"))

#### Salvar base de dados filtrada ####

#save(unsc_full_filt, file = here("data/unsc_full_filt.RData"))

