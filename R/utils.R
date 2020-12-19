library(data.table)
library(dplyr)
library(geobr)
library(ggplot2)
library(jsonlite)
library(lubridate)
library(purrr)
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

load_testes_municipios <- function(input_file, min_date = "2020-03-01") {
  read_csv(input_file) %>%
    filter(ultimo_dia_semana - days(7) >= ymd(min_date),
           ultimo_dia_semana <= today()) %>%
    mutate(tipo_teste = factor(tipo_teste,
                               levels = c("RT-PCR", "Teste rápido", "Outros")))
}

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
    #read.csv2(esus_file, fileEncoding = "latin1", na.strings = c("", "null")) %>%
    data.table::fread(esus_file, sep = ";", encoding = "Latin-1", na.strings = c("", "null")) %>%
      #filter(tipoTeste == "RT-PCR") %>%
      mutate(dataNotificacao = as_date(dataNotificacao),
             semana_epi = epiweek(dataNotificacao)) %>%
      filter(dataNotificacao >= ymd("2020-03-01"), dataNotificacao <= today()) %>%
      group_by(semana_epi) %>%
      mutate(ultimo_dia_semana = max(dataNotificacao)) %>%
      group_by(semana_epi, municipio_ibge = municipioIBGE,
               tipo_teste = tipoTeste) %>%
      summarise(
        ultimo_dia_semana = max(ultimo_dia_semana),
        testes_total = n(),
        testes_negativos = sum(resultadoTeste == "Negativo", na.rm = TRUE),
        testes_positivos = sum(resultadoTeste == "Positivo", na.rm = TRUE),
        testes_inconclusivos = sum(resultadoTeste == "Inconclusivo ou Indeterminado", na.rm = TRUE)
      )
  })
}

aggregate_tests_by_result_dt <- function(esus_paths) {
  #map_df(esus_paths, function(esus_file) {
    #read.csv2(esus_file, fileEncoding = "latin1", na.strings = c("", "null")) %>%
  rbindlist(lapply(esus_paths, function(esus_file) {
    print(esus_file)
    esus_dt <- fread(esus_file, na.strings = c("", "null"), showProgress = FALSE)
    esus_dt[, dataNotificacao := as.Date(dataNotificacao)]
    esus_dt <- esus_dt[dataNotificacao >= ymd("2020-03-01") & dataNotificacao <= today() &
                         ( evolucaoCaso != "Cancelado" | is.na(evolucaoCaso) )]
    esus_dt[, municipioIBGE := as.character(municipioIBGE)]
    esus_dt[, semana_epi := epiweek(dataNotificacao)]
    esus_dt[
      ,
      ultimo_dia_semana := max(dataNotificacao),
      by = semana_epi
    ]
    esus_dt <- esus_dt[,
      .(ultimo_dia_semana = max(ultimo_dia_semana),
        testes_total = .N,
        testes_negativos = sum(resultadoTeste == "Negativo", na.rm = TRUE),
        testes_positivos = sum(resultadoTeste == "Positivo", na.rm = TRUE),
        testes_inconclusivos = sum(resultadoTeste == "Inconclusivo ou Indeterminado", na.rm = TRUE),
        casos_confirmados = sum(resultadoTeste == "Positivo" | str_starts(classificacaoFinal, "Confirma"),
                                na.rm = TRUE)
      ),
      by = .(semana_epi, municipioIBGE, tipoTeste)
    ]
    setnames(esus_dt, c("municipioIBGE", "tipoTeste"), c("municipio_ibge", "tipo_teste"))
    esus_dt
  }))
}

aggregate_testes_estados <- function(testes_municipios) {
  testes_municipios %>%
    group_by(semana_epi) %>%
    mutate(ultimo_dia_semana = max(ultimo_dia_semana)) %>%
    filter(wday(ultimo_dia_semana) == 7) %>%
    group_by(semana_epi, tipo_teste, uf) %>%
    summarise(
      ultimo_dia_semana = max(ultimo_dia_semana),
      testes_positivos = sum(testes_positivos, na.rm = TRUE),
      testes_negativos = sum(testes_negativos, na.rm = TRUE),
      testes_inconclusivos = sum(testes_inconclusivos, na.rm = TRUE),
      testes_total = sum(testes_total, na.rm = TRUE),
      testes_com_resultado = testes_positivos + testes_negativos + testes_inconclusivos,
      testes_taxa_positivo = testes_positivos / testes_com_resultado,
      casos_confirmados = sum(casos_confirmados, na.rm = TRUE),
    )
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

update_testes_municipios <- function() {
  #esus_paths <- list.files("../covid-br-data/esus-notifica",
  #                         "dados-.*csv.gz", full.names = TRUE)
  esus_paths <- list.files("../covid-br-data",
                           "notificacoes2_.*csv.gz", full.names = TRUE)
  municipios <- read_csv2("data/municipios_info_ibge.csv", col_types = cols(municipio_ibge = "c"))
  esus_tests <- aggregate_tests_by_result_dt(esus_paths)
  
  esus_tests <- municipios %>%
    right_join(esus_tests) %>%
    filter(!is.na(tipo_teste)) %>%
    mutate(tipo_teste = factor(case_when(
      tipo_teste == "RT-PCR" ~ "RT-PCR",
      #str_starts(tipo_teste, "TESTE RÁPIDO") ~ "Teste rápido",
      TRUE ~ "Teste rápido"
    )))
  
  write.csv2(esus_tests, file.path("data", "esus_testes_municipios.csv"),
             row.names = FALSE)
}

update_testes_municipios_dt <- function() {
  # esus_paths <- list.files("../covid-br-data",
  #                          "notificacoes2_.*csv.gz", full.names = TRUE)
  esus_paths <- list.files("../covid-br-data/esus-notifica/api",
                          "notificacoes_.*csv.gz", full.names = TRUE)
  esus_tests <- aggregate_tests_by_result_dt(esus_paths)
  #esus_tests[!is.na(tipo_teste)]
  municipios <- fread("data/municipios_info_ibge.csv", colClasses = "character")
  esus_tests <- esus_tests[municipios, on = "municipio_ibge"]
  esus_tests[
    ,
    tipo_teste := factor(case_when(
      tipo_teste == "RT-PCR" ~ "RT-PCR",
      #str_starts(tipo_teste, "TESTE RÁPIDO") ~ "Teste rápido",
      is.na(tipo_teste) ~ NA_character_,
      TRUE ~ "Teste rápido"
    ))
  ]
  fwrite(esus_tests, file.path("data", "esus_testes_municipios.csv"))
}

update_testes_estados <- function() {
  mun_file <- file.path("data", "esus_testes_municipios.csv")
  testes_mun <- load_testes_municipios(mun_file)
  testes_estados <- aggregate_testes_estados(testes_mun)
  write_csv2(testes_estados, "data/esus_testes_estados.csv")
}

update_evolucao_uf <- function() {
  populacao_estados <- read_csv(file.path("data", "cities_info.csv")) %>%
    group_by(region, state) %>%
    summarise(populacao = sum(pop2019, na.rm = TRUE))
  
  evolucao <- "https://github.com/wcota/covid19br/raw/master/cases-brazil-states.csv" %>%
    read_csv() %>%
    filter(state != "TOTAL") %>%
    group_by(state, epi_week) %>%
    summarise(
      casos = sum(newCases, na.rm = TRUE),
      obitos = sum(newDeaths, na.rm = TRUE),
      casos_acumulado = max(totalCases, na.rm = TRUE),
      obitos_acumulado = max(deaths, na.rm = TRUE)) %>%
    left_join(populacao_estados, by = c("state")) %>%
    group_by(state) %>%
    mutate(
      incidencia = 10^5 * casos / populacao,
      incidencia_acumulado = 10^5 * casos_acumulado / populacao,
      incidencia_variacao = (incidencia - lag(incidencia)) / lag(incidencia),
      mortalidade = 10^5 * obitos / populacao,
      mortalidade_acumulado = 10^5 * obitos_acumulado / populacao,
      mortalidade_variacao = (mortalidade - lag(mortalidade)) / lag(mortalidade)
    ) %>%
    rename(regiao = region, uf = state, semana_epi = epi_week)
  write_csv2(evolucao, "data/covid_evolucao_estados.csv")
}

update_br_states_shapes <- function() {
  states_shapes <- read_state(year=2018, showProgress = FALSE)
  st_write(states_shapes, "data/states_shapes.gpkg")
}

update_covid_municipios <- function() {
  covid_mun <- "https://github.com/wcota/covid19br/raw/master/cases-brazil-cities-time.csv.gz" %>%
    read_csv() %>%
    filter(state != "TOTAL", city != "TOTAL", str_length(ibgeID) > 2) %>%
    group_by(ibgeID, state, epi_week) %>%
    summarise(
      casos = sum(newCases, na.rm = TRUE),
      obitos = sum(newDeaths, na.rm = TRUE),
      casos_acumulado = max(totalCases, na.rm = TRUE),
      obitos_acumulado = max(deaths, na.rm = TRUE)) %>%
    left_join(cities_info, by = "ibgeID") %>%
    group_by(ibgeID, state) %>%
    mutate(
      incidencia = 10^5 * casos / populacao,
      incidencia_acumulado = 10^5 * casos_acumulado / populacao,
      incidencia_variacao = (incidencia - lag(incidencia)) / lag(incidencia),
      mortalidade = 10^5 * obitos / populacao,
      mortalidade_acumulado = 10^5 * obitos_acumulado / populacao,
      mortalidade_variacao = (mortalidade - lag(mortalidade)) / lag(mortalidade)
    ) %>%
    rename(regiao = region, uf = state, semana_epi = epi_week)
  write_csv2(covid_mun, "data/covid_evolucao_municipios.csv")
}

update_municipios_info <- function() {
  mun_info <- load_municipios_info()
  write_csv2(mun_info, "data/municipios_info_ibge.csv")
}
