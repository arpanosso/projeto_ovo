library(tidyverse)
library(skimr)
library(GGally)

dados<- readxl::read_excel("data-raw/Dados Mestrado João.xlsx") |>
  janitor::clean_names()
dplyr::glimpse(dados)

dados <-dados |>
  dplyr::mutate(
    resistencia_kgf = ifelse(resistencia_kgf == "Perdi", NA, resistencia_kgf),
    resistencia_kgf = as.numeric(resistencia_kgf),
    dia=round(as.numeric(difftime(data,"2021-05-13",units = "days"))),
    dia = dplyr::case_when(
      dia < 3 ~ 1,
      dia < 20 ~15,
      dia < 30 ~ 30,
      dia < 45 ~ 45,
      dia < 60 ~ 60
    )
  )
dplyr::glimpse(dados)

dados |>
  dplyr::group_by(dia) |>
  dplyr::summarise(re=mean(resistencia_kgf))

skim(dados)

dados |>
  ggplot(aes(x = as.factor(tratamento), y = resistencia_kgf)) +
  geom_boxplot()

dados |>
  dplyr::filter(! (frequencia_ressonante < 4500 | frequencia_ressonante >6000)) |>
  # ggplot(aes(x = frequencia_ressonante, y = log10(resistencia_kgf),
  #            color=as.factor(dia))) +
  ggplot(aes(x = frequencia_ressonante, y = resistencia_kgf)) +
  geom_point() +
  geom_smooth(method = "lm")

visdat::vis_miss(dados)


dados  |>
  #filter(dia ==60) |>
  select(frequencia_ressonante:percent_casca)  |>
  cor(use = "p")  |>
  corrplot::corrplot()

glimpse(dados)

dados |>
  #filter(dia ==15) |>
  select(frequencia_ressonante:percent_casca) |>
  ggpairs()

dados |>
  ggplot(aes(x=frequencia_ressonante)) +
  geom_histogram(bins=30,color="black",fill="lightgray")


