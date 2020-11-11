library(ggplot2)
library(jsonlite)
library(lubridate)
library(readr)
library(stringr)

load_municipios_info <- function() {
  fromJSON(
    "https://servicodados.ibge.gov.br/api/v1/localidades/municipios",
    flatten = TRUE
  ) %>%
  janitor::clean_names() %>%
  transmute(
    municipio_ibge = as.character(id),
    municipio = nome,
    uf = microrregiao_mesorregiao_uf_sigla
    #estado = microrregiao_mesorregiao_uf_nome,
    #microrregiao = microrregiao_nome,
    #mesorregiao = microrregiao_mesorregiao_nome,
    #regiao = microrregiao_mesorregiao_uf_regiao_nome
  )
}

load_testes_municipios <- function(input_file = "data/esus_testes_municipios.csv")


load_testes_semana_brasil <- function(testes_municipios) {
  read_csv2() %>%
  filter(data_inicio_semana >= ymd("2020-03-15"),
         data_inicio_semana <= today()) %>%
  left_join(municipios) %>%
  mutate(tipo_teste = case_when(
    tipo_teste == "RT-PCR" ~ "RT-PCR",
    str_starts(tipo_teste, "TESTE RÁPIDO") ~ "Teste rápido",
    TRUE ~ "Outros"
   )) %>%
  group_by(semana_epidemiologica = epiweek(as.Date(data)), tipo_teste) %>%
  summarise(
    testes_negativos = sum(testes_negativos),
    testes_positivos = sum(testes_positivos),
    testes_inconclusivos = sum(testes_negativos)
  )
}

aggregate_tests_by_result <- function(esus_paths) {
  map_df(esus_paths, function(esus_file) {
    read.csv2(esus_file, fileEncoding = "latin1", na.strings = c("", "null")) %>%
      #filter(tipoTeste == "RT-PCR") %>%
      mutate(dataNotificacao = as_date(dataNotificacao),
             semana_epi = epiweek(dataNotificacao)) %>%
      filter(dataNotificacao >= ymd("2020-03-01"), dataNotificacao <= today()) %>%
      group_by(semana_epi) %>%
      mutate(data_inicio_semana = min(dataNotificacao)) %>%
      group_by(semana_epi, municipio_ibge = municipioIBGE,
               tipo_teste = tipoTeste) %>%
      summarise(
        data_inicio_semana = first(data_inicio_semana),
        testes_negativos = sum(resultadoTeste == "Negativo", na.rm = TRUE),
        testes_positivos = sum(resultadoTeste == "Positivo", na.rm = TRUE),
        testes_inconclusivos = sum(resultadoTeste == "Inconclusivo ou Indeterminado", na.rm = TRUE)
      )
  })
}

aggregate_test_results <- function(testes_municipios, uf = "Todos") {
  if (uf != "Todos") {
    testes_municipios <- filter(testes_municipios, uf == uf)
  }
  testes_municipios %>%
    group_by(semana_epi, tipo_teste) %>%
    summarise_at(vars(starts_with("testes_"), sum, na.rm = TRUE))
}

aggregate_cities_cases <- function() {
  evolucao <- "https://github.com/wcota/covid19br/raw/master/cases-brazil-cities-time.csv.gz" %>%
    read_csv(col_types = cols(ibgeID = "c", date = "D", newCases = "i",
                              newDeaths = "i", .default = "_")) %>%
    filter(str_length(ibgeID) > 2) %>%
    group_by(ibge_id = ibgeID) %>%
    summarise(
      data_primeiro_caso = min(date[newCases > 0]),
      data_primeiro_obito = min(date[newDeaths > 0]),
      data_ultima_atualizacao = max(date),
      total_casos = sum(newCases),
      total_obitos = sum(newDeaths)
    )
  write_csv(evolucao, paste0("data/resumo_covid_municipios_",
                             max(evolucao$data_ultima_atualizacao), ".csv"))
  return(evolucao)
}

aggregate_tests_uf <- function(testes_mun) {
  testes_agg_uf <- testes_mun %>%
    filter(tipo_teste == "RT-PCR", !is.na(uf)) %>%
    group_by(uf) %>%
    summarise_at(vars(starts_with("testes_")), sum, na.rm = TRUE) %>%
    mutate(
      testes_total = testes_positivos + testes_negativos + testes_inconclusivos,
      taxa_positivo = round(testes_positivos / testes_total, 3)
    )
  
  write_csv(testes_agg_uf, "data/testes_pcr_acumulado_uf.csv")
}


