# Amostra de Script

library(tidyverse)
library(here)
library(quanteda)
library(lubridate)
library(scales)

load(here("data/unsc_filt_pt.RData"))

unsc_filt_pt <- unsc_filt_pt %>% 
  select(filename, country, spv, date, agenda_item3_pt, text) %>% 
  mutate(date = dmy(date))

unsc_filt_pt %>% 
  count(country) %>%
  arrange(desc(n)) %>% 
  slice(1:5) %>% 
  mutate(country = recode(country, "United Kingdom Of Great Britain And Northern Ireland" = "Reino Unido",
                          "United States Of America" = "Estados Unidos",
                          "France" = "França",
                          "Russian Federation" = "Rússia",
                          "UN" = "ONU")) %>% 
  ggplot(aes(reorder(country, n), n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, y = "Número de Discursos", title = "Membros que mais discursaram no Conselho de Segurança das Nações Unidas (1995-2019)")

unsc_filt_pt %>% 
  select(agenda_item3_pt, spv, date) %>% 
  unique() %>% 
  count(agenda_item3_pt) %>% 
  arrange(desc(n)) %>%
  slice(1:5) %>% 
  ggplot(aes(reorder(agenda_item3_pt, n), n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, y = "Número de Encontros", title = "Tópicos com mais encontros no Conselho de Segurança das Nações Unidas (1995-2019)")

unsc_corpus <- unsc_filt_pt %>% 
  corpus(docid_field = "filename", text_field = "text") %>%  
  tokens(remove_punct = TRUE) %>% 
  tokens_tolower() 

dfmat_unsc_sent <- unsc_corpus %>% 
  tokens_lookup(dictionary = data_dictionary_LSD2015) %>% 
  dfm()

unsc_sent <- dfmat_unsc_sent %>% 
  convert(to = "data.frame") %>% # conversão para dataframe
  cbind(docvars(dfmat_unsc_sent)) %>% # manutenção das variáveis originais
  mutate(positive = positive - neg_positive, # remoção dos falsos positivos e criação das variáveis de sentimento em números absolutos e relativos
         negative = negative - neg_negative,
         sentiment = positive - negative,
         positive_percent = positive / (positive + negative),
         negative_percent = negative / (positive + negative),
         sentiment_percent = positive_percent - negative_percent)

unsc_sent %>% 
  filter(country == "United States Of America") %>% 
  ggplot(aes(date, sentiment_percent)) + # "date" no eixo x e "sentiment_percent" no eixo y
  geom_point(stat = "identity", alpha = 1/10) +
  labs(x = NULL, y = "Score de sentimento (% Positivo - % Negativo)", title = "Sentimento dos Estados Unidos no Conselho de Segurança das Nações Unidas") + # rótulos no gráfico
  scale_x_date(date_breaks = "years" , labels=date_format("%Y")) + # indicação da data no eixo x, ano a ano
  geom_hline(yintercept=0) + # criação de linha de referência em y = 0
  lims(y = c(-1, 1)) + # limites do eixo y, com indicação de que vai de -1 a +1
  geom_smooth(se = F, color = "black", size = .5) + # curva de tendência
  theme_bw() + # escolha do tema do gráfico: black and white
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # ajuste dos anos no eixo x, para que sejam apresentados num ângulo de 45 graus

dfmat_unsc_terror <- unsc_corpus %>% 
  tokens_lookup(dictionary = dictionary(list(terror = c("terror*")))) %>% 
  dfm()

unsc_terror <- dfmat_unsc_terror %>% 
  convert(to = "data.frame") %>% # conversão para dataframe
  cbind(docvars(dfmat_unsc_terror)) %>%  # manutenção das variáveis originais 
  mutate(quarters = as.character(quarter(date, with_year = TRUE))) %>%  
  group_by(quarters) %>% 
  summarise(discourses = n(),
            terror = sum(terror),
            terror_disc = terror/discourses)

unsc_terror %>%
  ggplot(aes(x = quarters, y = terror_disc, group = 1)) +
  geom_line() +
  labs(x = NULL, y = "Menções a Terrorismo por Discurso", title = "Menções a Terrorismo no Conselho de Segurança das Nações Unidas") +
  geom_smooth(se = F) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
