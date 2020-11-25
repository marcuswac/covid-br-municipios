library(dplyr)
library(forcats)
library(ggplot2)
library(lubridate)
library(readr)
library(scales)

#path <- "https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SRAG/2020/INFLUD-16-11-2020.csv"
path <- file.path("data", "INFLUD-16-11-2020.csv")

srag <- read_csv2(path, guess_max = 1000000)

cnes_estabelecimentos <- read.csv2("~/local/datasets/dados-sus/BASE_DE_DADOS_CNES/tbEstabelecimento202009.csv") %>%
  mutate(CO_CNES = as.character(CO_CNES))

cnes_natureza_juridica <- read.csv2("~/local/datasets/dados-sus/BASE_DE_DADOS_CNES/tbNaturezaJuridica202009.csv")

publicos <- c("AUTARQUIA FEDERAL", "AUTARQUIA MUNICIPAL", "EMPRESA PUBLICA",
              "MUNICIPIO", "ORGAO PUBLICO DO PODER EXECUTIVO ESTADUAL OU DO DISTRITO FEDERAL")

srag_filt <- srag %>%
  mutate_at(vars(starts_with("DT_")), dmy) %>%
  filter(DT_NOTIFIC >= ymd("2020-03-01")) %>%
  mutate(CO_UNI_NOT = as.character(CO_UNI_NOT)) %>%
  left_join(cnes_estabelecimentos, by = c("CO_UNI_NOT" = "CO_CNES")) %>%
  left_join(cnes_natureza_juridica, by = "CO_NATUREZA_JUR") %>%
  mutate(GESTAO = ifelse(DS_NATUREZA_JUR %in% publicos, "Público", "Privado"))

classificao_levels <- c("Não especificado", "Outros", "COVID-19")

srag_sem_uf_classificacao <- srag_filt %>%
  group_by(SEM_NOT) %>%
  mutate(
    CLASSI_FIN = factor(case_when(
      CLASSI_FIN == 5 ~ classificao_levels[3],
      !(is.na(CLASSI_FIN) & CLASSI_FIN == 4) ~ classificao_levels[2],
      TRUE ~ classificao_levels[1]
    ), levels = classificao_levels),
    DT_INICIO_SEM_NOT = min(DT_NOTIFIC, na.rm = TRUE),
    N_DIAS_SEMANA = length(unique(DT_NOTIFIC))
  ) %>%
  group_by(SG_UF_NOT, SEM_NOT, CLASSI_FIN, .drop = FALSE) %>%
  summarise(
    DT_INICIO_SEM_NOT = first(DT_INICIO_SEM_NOT),
    N_DIAS_SEMANA = first(N_DIAS_SEMANA),
    n = n()
  )

p <- srag_sem_uf_classificacao %>%
  ungroup() %>%
  filter(SEM_NOT <= max(SEM_NOT) - 1, DT_INICIO_SEM_NOT >= ymd("2020-04-01")) %>%
  ggplot(aes(DT_INICIO_SEM_NOT, n, fill = CLASSI_FIN)) +
  geom_col(position = "stack") +
  facet_wrap(~ SG_UF_NOT, scales = "free_y") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  labs(title = "Notificações de SRAG por classificação",
       x = "Data de notificação",
       y = "Notificações por semana",
       fill = "Classificação") +
  theme_bw()
p
ggsave("srag_uf_classificacao.png", p, width = 8*2, height = 4*2)

srag_sem_uf_gestao <- srag_filt %>%
  filter(SG_UF_NOT == "RJ", HOSPITAL == 1) %>%
  group_by(SEM_NOT) %>%
  mutate(
    DT_INICIO_SEM_NOT = min(DT_NOTIFIC, na.rm = TRUE),
    N_DIAS_SEMANA = length(unique(DT_NOTIFIC))
  ) %>%
  group_by(SG_UF_NOT, SEM_NOT, GESTAO,
           UTI = ifelse(is.na(UTI) | UTI != 1, "Não UTI", "UTI"), .drop = FALSE) %>%
  summarise(
    DT_INICIO_SEM_NOT = first(DT_INICIO_SEM_NOT),
    N_DIAS_SEMANA = first(N_DIAS_SEMANA),
    n = n()
  ) %>%
  group_by(SG_UF_NOT, SEM_NOT, UTI) %>%
  mutate(prop = n / sum(n))

p <- srag_sem_uf_gestao %>%
  ungroup() %>%
  filter(N_DIAS_SEMANA == 7) %>%
  ggplot(aes(DT_INICIO_SEM_NOT, n, fill = fct_rev(GESTAO))) +
  geom_col(position = "stack") +
  facet_wrap(~ UTI) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  labs(title = "Notificações de SRAG por tipo de gestão da unidade de saúde",
       subtitle = "Município: Rio de Janeiro - RJ",
       x = "Data de notificação",
       y = "Notificações por semana",
       fill = "Gestão") +
  theme_bw()
p
ggsave("srag_rj_gestao_uti.png", p, width = 8, height = 4)

p <- srag_sem_uf_gestao %>%
  ungroup() %>%
  filter(N_DIAS_SEMANA == 7) %>%
  ggplot(aes(DT_INICIO_SEM_NOT, prop, fill = fct_rev(GESTAO))) +
  geom_col(position = "stack") +
  facet_wrap(~ UTI) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  scale_y_continuous(labels = percent) +
  labs(title = "Proporção de notificações de SRAG por tipo de gestão da unidade de saúde",
       subtitle = "Município: Rio de Janeiro - RJ",
       x = "Data de notificação",
       y = "Notificações por semana",
       fill = "Gestão") +
  theme_bw()
p
ggsave("srag_rj_gestao_uti_prop.png", p, width = 8, height = 4)

