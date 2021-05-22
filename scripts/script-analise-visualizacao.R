# Amostra de Script - Análise e Visualização

library(tidyverse)
library(here)
library(lubridate)
library(scales)
library(sf)
library(maps)

load(here("data/unsc_full.RData"))

## Número de encontros ao longo do tempo - Linha

unsc_full %>% 
  select(date, spv) %>% # seleção das variáveis necessárias
  unique() %>% # remoção das duplicatas
  mutate(semesters = as.character(semester(date, with_year = TRUE))) %>% # conversão para semestres
  group_by(semesters) %>% 
  summarise(meetings = n()) %>% # número de encontros por semestre
  ggplot(aes(semesters, meetings, group = 1)) + # plot da evolução do número de encontros
  geom_line() +
  labs(x = "Semestres",
       y = NULL,
       title = "Número de Encontros no CSNU (1995-2019)") +
  geom_smooth(se = F) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 65,
                                   hjust = 1,
                                   vjust = 1))

## Tópicos com mais encontros - Barras

unsc_full %>% # plotar tópicos mais debatidos
  select(spv, topics) %>%
  unique() %>%
  count(topics) %>%
  arrange(desc(n)) %>% 
  slice(1:10) %>%
  ggplot(aes(reorder(topics, n), n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, y = NULL, title = "Tópicos com mais encontros") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))

## Tópicos com mais encontros por ano - Tabela

top_topicos_ano <- unsc_full %>% 
  filter(agenda_item3 != "Thematic") %>%
  select(agenda_item3, spv, date) %>%
  unique() %>% 
  mutate(year = as.character(year(date))) %>%
  group_by(year, agenda_item3) %>% 
  summarise(n = n()) %>% 
  arrange(year, desc(n)) %>% 
  slice(1:2) %>% 
  mutate(ranking = c(1, 2)) %>% 
  pivot_wider(names_from = ranking, values_from = c(agenda_item3, n)) 

top_topicos_ano %>%
  relocate(n_1, .after = agenda_item3_1) %>% 
  knitr::kable(caption = "Tópicos com mais encontros por ano",
               col.names = c("", "", "", "", ""),
               longtable = F,
               booktabs = T) %>%
  column_spec(4, border_left = T, border_right = F) %>%
  add_header_above(c(" " = 1,
                     "Maior número de encontros" = 2,
                     "Segundo maior número de encontros" = 2))

## Membros com mais discursos

# Remover discursos não proferidos em capacidade nacional

load(here("data/un_capacities_speeches.RData"))
load(here("data/national_capacities_filenames.RData"))

unsc_filt <- unsc_full %>% 
  filter(!(participanttype == "The President" &
             !filename %in% c(un_capacities_speeches,
                              national_capacities_filenames)))

# Membros que mais discursaram - Barras

unsc_filt %>% 
  group_by(country) %>%
  summarise(discourses = n()) %>% 
  arrange(desc(discourses)) %>% 
  slice(1:10) %>% 
  ggplot(aes(reorder(country, discourses), discourses)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() +
  labs(x = NULL,
       y = "Número de discursos",
       title = "Membros que mais discursaram")

### Mapa América do Sul

# Carregar mapa filtrado para América do Sul

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

# Unir dados do mapa com número de discursos dos países, mantendo variável "ID" para a visualização

n_discourses <- unsc_filt %>% 
  group_by(country) %>%
  summarise(discourses = n())

world <- world %>% 
  left_join(n_discourses, by = c("ID" = "country"))  

# Gerar visualização

world %>% ggplot() +
  geom_sf(aes(fill=discourses), color= "black", size=.05) +
  coord_sf(xlim = c(-85, -30), ylim = c(-60, 15), expand = FALSE) +
  scale_fill_distiller(palette = "PuBuGn",
                       name = NULL,
                       limits = c(min(world$discourses),max(world$discourses))) +
  theme_void() +
  labs(title = "Número de discursos dos países da América do Sul")

