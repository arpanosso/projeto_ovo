library(tidyverse)
library(lubridate)
library(skimr)
library(GGally)

dados<- readxl::read_excel("data-raw/Dados Mestrado João.xlsx") |>
  janitor::clean_names()
dplyr::glimpse(dados)
dados <- dados |>
  select(data,
         repeticao,
         linhagem,
         nivel_de_fosforo_disp,
         resistencia_kgf,
         frequencia_ressonante,
         shape_index,
         peso_g,
         altura_de_albumen,
         uh,
         espessura_mm
         )

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
  dplyr::group_by(dia, linhagem) |>
  dplyr::summarise(re=mean(resistencia_kgf, na.rm=TRUE)) |>
  ggplot(aes(x=dia, y=re, color=linhagem)) +
  geom_point() +
  geom_smooth(method = "lm")

tab <- dados |>
  dplyr::group_by(dia,linhagem) |>
  dplyr::summarise(re=mean(resistencia_kgf, na.rm=TRUE))

mod <- lm(re~dia + linhagem, data=tab)
summary.lm(mod)
plot(mod)

skim(dados)

dados |>
  ggplot(aes(x = as.factor(nivel_de_fosforo_disp), y = resistencia_kgf, fill=as.factor(dia))) +
  geom_boxplot() +
  facet_wrap(~dia)

dados |>
  group_by(nivel_de_fosforo_disp, linhagem, repeticao) |>
  summarise(re = mean(resistencia_kgf, na.rm=TRUE)) |>
  ggplot(aes(x = as.factor(nivel_de_fosforo_disp), y = re)) +
  geom_boxplot(fill="lightgray") +
  theme_bw() +
  labs(x="Nível de Fósforo disponível (%)",
       y = "Resistência (kgf)")

dados |>
  dplyr::filter(! (frequencia_ressonante < 4500 | frequencia_ressonante >6000)) |>
  # ggplot(aes(x = frequencia_ressonante, y = log10(resistencia_kgf),
  #            color=as.factor(dia))) +
  ggplot(aes(x = frequencia_ressonante, y = resistencia_kgf)) +
  geom_point() +
  geom_smooth(method = "lm")


names(dados)
dados  |>
  filter(linhagem == "Dekalb") |>
  select(resistencia_kgf:espessura_mm)  |>
  cor(use = "p")  |>
  corrplot::corrplot()

dados  |>
  filter(linhagem == "H&N") |>
  select(resistencia_kgf:espessura_mm)  |>
  cor(use = "p")  |>
  corrplot::corrplot()

dados |>
  filter(linhagem == "Dekalb") |>
  select(resistencia_kgf:espessura_mm) |>
  ggpairs()

dados |>
  filter(linhagem == "H&N") |>
  select(resistencia_kgf:espessura_mm) |>
  ggpairs()

dados |>
  ggplot(aes(x=frequencia_ressonante, fill=linhagem)) +
  geom_histogram(bins=30,color="black") +
  facet_wrap(~linhagem)


