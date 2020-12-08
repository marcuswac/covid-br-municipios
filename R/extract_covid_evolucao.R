library(dplyr)
library(lubridate)
library(readr)
library(stringr)
library(zoo)

covid_mun_ne <- read_csv("https://data.brasil.io/dataset/covid19/caso_full.csv.gz") %>%
  filter(place_type == "city", str_starts(city_ibge_code, "2"))

portes <- c("Menor que 10 mil", "10 a 20 mil", "20 a 100 mil", "Acima de 100 mil")

municipios_ne_porte <- covid_mun_ne %>%
  select(city_ibge_code, estimated_population_2019) %>%
  group_by(city_ibge_code) %>%
  summarise(populacao = max(estimated_population_2019, na.rm = TRUE)) %>%
  mutate(porte = factor(case_when(
    populacao < 10000 ~ portes[1],
    populacao < 20000 ~ portes[2],
    populacao < 100000 ~ portes[3],
    TRUE ~ portes[4],
  ), levels = portes)) %>%
  group_by(porte) %>%
  mutate(populacao_porte = sum(populacao, na.rm = TRUE))

covid_porte <- municipios_ne_porte %>%
  left_join(select(covid_mun_ne, city_ibge_code, date, new_confirmed, new_deaths),
            by = "city_ibge_code") %>%
  group_by(porte, data = date) %>%
  summarise(casos = sum(new_confirmed, na.rm = TRUE),
            obitos = sum(new_deaths, na.rm = TRUE),
            populacao_porte = max(populacao_porte, na.rm = TRUE)) %>%
  arrange(data) %>%
  group_by(porte) %>%
  mutate(
    incidencia = 10^5 * casos / populacao_porte,
    mortalidade = 10^5 * obitos / populacao_porte,
    casos_media7 = rollmean(casos, 7, align = "right", fill = NA),
    obitos_media7 = rollmean(obitos, 7, align = "right", fill = NA),
    incidencia_media7 = rollmean(incidencia, 7, align = "right", fill = NA),
    mortalidade_media7 = rollmean(mortalidade, 7, align = "right", fill = NA),
  )

write_csv2(covid_porte, "data/covid_evolucao_porte_municipios.csv")
