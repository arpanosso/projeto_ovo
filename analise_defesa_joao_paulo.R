require(tidyverse)
dados<- readxl::read_excel("data-raw/Dados Mestrado João.xlsx") |>
  janitor::clean_names()
dplyr::glimpse(dados)

nd_filter <- c(0.105, 0.200, 0.391)
dados <- dados |>
  filter(nivel_de_fosforo_disp %in% nd_filter)

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

dados %>%
  ggplot(aes(x=resistencia_kgf, y=..density..))+
  geom_histogram(bins=50, color='black',fill='gray')

dados %>%
  ggplot(aes(x=frequencia_ressonante, y=..density..))+
  geom_histogram(bins=50, color='black',fill='gray')

dados %>%
  filter(frequencia_ressonante >= 5500,
    frequencia_ressonante <= 6500) %>%
  group_by(dia, tratamento) %>%
  summarise(frequencia_ressonante = mean(frequencia_ressonante, na.rm=TRUE),
            resistencia_kgf = mean(resistencia_kgf, na.rm=TRUE)
            ) %>%
  ggplot(aes(x=(frequencia_ressonante),
             y=resistencia_kgf))+
  geom_point() +
  geom_smooth(method = "lm")

dados$nivel_de_fosforo_disp %>% unique()




