library(dplyr)
library(lubridate)
library(purrr)
library(readr)
library(tidyr)

source("R/utils.R")

filter_states = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
                  "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN",
                  "RO", "RR", "RS", "SC", "SE", "SP", "TO")

# Usar este quando os dados estivem na máquina local
esus_paths <- list.files("../covid-br-data/esus-notifica/",
                         "dados-.*csv.gz", full.names = TRUE)

#esus_paths <- paste0("https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/",
#                      "dados-", tolower(filter_states), ".csv")
# 
esus_tests <- aggregate_tests_by_result(esus_paths)
municipios <- load_municipios_info()

esus_tests <- municipios %>%
  right_join(esus_tests) %>%
  filter(!is.na(tipo_teste)) %>%
  mutate(tipo_teste = factor(case_when(
    tipo_teste == "RT-PCR" ~ "RT-PCR",
    str_starts(tipo_teste, "TESTE RÁPIDO") ~ "Teste rápido",
    TRUE ~ "Outros"
  )))

write.csv2(esus_tests, file.path("data", "esus_testes_municipios.csv"),
           row.names = FALSE)
