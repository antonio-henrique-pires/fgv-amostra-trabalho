# ETAPA 2 - ANÁLISE E VISUALIZAÇÃO

# Pacotes para análise e visualização
library(tidyverse)
library(here)
library(lubridate)
library(scales)
library(sf)
library(maps)
library(kableExtra)

# Carregar banco de dados pré-processado
load(here("data/unsc_full_filt.RData")) 

# Carregar vetores com códigos dos discursos nacionais
load(here("data/un_capacities_speeches.RData"))
load(here("data/national_capacities_filenames.RData"))

#### Número de encontros ao longo do tempo - Linha ####

# Objeto em que cada linha é um encontro
meet_semesters <- unsc_full_filt %>% 
  select(date, spv) %>% # encontros e suas datas
  unique() %>% # remover duplicatas
  mutate(semesters = as.character(semester(date, with_year = TRUE))) %>% # conversão para semestres
  group_by(semesters) %>% 
  summarise(meetings = n()) %>%  # número de encontros por semestre
  arrange(desc(meetings))

# plot da evolução do número de encontros
meet_semesters %>% 
  ggplot(aes(semesters, meetings, group = 1)) + 
  geom_line() +
  labs(x = NULL,
       y = NULL) +
  geom_smooth(se = F) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 65,
                                   hjust = 1,
                                   vjust = 1))

#### Tópicos com mais encontros - Barras ####

# Objeto em que cada linha é um tópico
top_topics <- unsc_full_filt %>% 
  select(spv, topics) %>% # tópicos e respectivos encontros
  unique() %>% # remover duplicatas
  count(topics) %>% # frequência de tópicos
  arrange(desc(n)) %>% 
  slice(1:10) 

# plot ordenado dos 10 tópicos mais frequentes
top_topics %>% 
  ggplot(aes(reorder(topics, n), n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, 
       y = NULL) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))

#### Tópicos com mais encontros por ano - Tabela ####

# Objeto em que cada linha possui os dois tópicos  mais frequentes em um ano

top_topics_ano <- unsc_full_filt %>% 
  filter(agenda_item3 != "Thematic") %>% # remover temáticos
  select(agenda_item3, spv, date) %>% 
  unique() %>% # remover duplicatas
  mutate(year = as.character(year(date))) %>% # manter apenas ano
  group_by(year, agenda_item3) %>% 
  summarise(n = n()) %>% # frequência de tópicos por ano
  arrange(year, desc(n)) %>% 
  slice(1:2) %>% 
  mutate(ranking = c(1, 2)) %>% # indicar primeiros e segundos tópicos mais adotados
  pivot_wider(names_from = ranking, values_from = c(agenda_item3, n)) # criar colunas específicas para primeiros e segundos tópicos

# tabela em que cada linha é um ano com indicação dos primeiros e segundos tópicos mais adotados

top_topics_ano %>%
  relocate(n_1, .after = agenda_item3_1) %>% 
  knitr::kable(caption = "Tópicos com mais encontros por ano",
               col.names = c("", "", "", "", ""),
               longtable = F,
               booktabs = T) %>%
  column_spec(4, border_left = T, border_right = F) %>%
  add_header_above(c(" " = 1,
                     "Maior número de encontros" = 2,
                     "Segundo maior número de encontros" = 2)) %>% 
  kable_styling(latex_options = "HOLD_position")

#### Membros com mais discursos ####

# remover discursos não feitos em capacidade nacional
unsc_nat <- unsc_full_filt %>% 
  filter(!(participanttype == "The President" &
             !filename %in% c(un_capacities_speeches,
                              national_capacities_filenames)))

# objeto em que cada linha é um país
top_disc <- unsc_nat %>% 
  group_by(country) %>%
  summarise(discourses = n()) %>% # calcular número de discursos
  arrange(desc(discourses)) %>% 
  slice(1:10) 

# plot ordenado com os 10 países que mais discursaram
top_disc %>% 
  ggplot(aes(reorder(country, discourses), discourses)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() +
  labs(x = NULL,
       y = NULL)

#### Mapa América do Sul ####

# Carregar mapa filtrado para América do Sul

# importar shapefile apenas com países da América do Sul
world <- st_as_sf(map("world", plot = FALSE, fill = TRUE)) %>% 
  filter(ID %in% c("Brazil",
                   "Chile",
                   "Argentina",
                   "Bolivia",
                   "Venezuela",
                   "Ecuador",
                   "Paraguay",
                   "Uruguay",
                   "Peru",
                   "Guyana",
                   "French Guiana",
                   "Colombia",
                   "Suriname"))

# Objeto em que cada linha é um país
n_discourses <- unsc_nat %>% 
  group_by(country) %>%
  summarise(discourses = n())

# Unir shapefile com informações sobre número de discursos dos países
world <- world %>% 
  left_join(n_discourses, by = c("ID" = "country")) 

# Objeto em que cada linha é um país. Necessário para inserir códigos embutidos no texto
top_south_amer <- world %>% 
  tibble() %>% 
  select(ID, discourses) %>% 
  mutate(ID = recode(ID, 
                     "Brazil" = "Brasil",
                     "French Guiana" = "Guiana Francesa",
                     "Guyana" = "Guiana"),
         discourses = replace_na(discourses, 0)) %>% 
  arrange(desc(discourses)) 

# Mapa da América do Sul, com números de discursos totais dos países
world %>% ggplot() +
  geom_sf(aes(fill=discourses), color= "black", size=.05) +
  coord_sf(xlim = c(-85, -30), ylim = c(-60, 15), expand = FALSE) +
  scale_fill_distiller(palette = "PuBuGn",
                       name = NULL,
                       limits = c(min(world$discourses),
                                  max(world$discourses))) + # limites da legendo definidos a partir dos dados
  theme_void()

