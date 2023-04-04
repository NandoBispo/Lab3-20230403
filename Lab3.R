# PACOTES ----
if (!require(pacman)) install.packages("pacman")
# library(pacman)

# if (!require("DT")) install.packages('DT')
# xfun::session_info('DT')

pacman::p_load(tidyverse,  janitor, stargazer,  sjmisc, summarytools,
               kableExtra, moments, ggpubr, formattable, gridExtra, 
               glue, corrplot, sessioninfo, readxl, writexl, ggthemes,
               patchwork, qqplotr, plotly, lmtest, olsrr, gglm, DT,
               tidymodels, GGally, hrbrthemes)

# https://curso-r.githud.io/zen-do-r/git-githud.html
gitcreds::gitcreds_set()
usethis::use_git()
usethis::use_github()
# _________________________________________________

# DADOS ----
dados <- read.csv2("desemprego.csv")

dplyr::glimpse(dados)

dados <- dados|>
  dplyr::mutate(desemp = as.numeric(desemp),
                suic = as.numeric(suic))

dplyr::glimpse(dados)

# AED ----

## Tab1 ----
dados|>
  select(-ano)|>
  rename("Taxa de Desemprego" = desemp, "Índice de Suicídio" = suic)|>
  summarytools::descr(
    stats = c("min", "q1", "med", "mean","q3", "max",  "sd", "cv"),
    # round.digits = 3,
    justify = "c",
    style = "grid", #' rmarkdown',
    transpose = T
  ) |>
  # round(., 2) %>%
  kbl(
    caption = "Tabela 1: Medidas Resumo.",
    digits = 2,
    format.args=list(big.mark=".", decimal.mark=","),
    align = "c", 
    row.names = T,
    col.names =
      c("Min", "Q1", "Med", "Média", "Q3", "Max", "Desvio Padrão", "CV")
  )|>
  kable_material(c("striped", "hover", "condensed"))|>
  # kadle_styling(
  #   # dootstrap_options = c("striped", "hover", "condensed", "responsive"),
  #   dootstrap_options = c("striped", "hover"),
  #   full_width = F,
  #   fixed_thead = T # Fixa o cadeçalho ao rolar a tadela.
  # ) %>%
  # footnote(general = "Fonte: Instituto Nacional de Diabetes e de Doenças Digestivas e Renais - EUA") |>
  kable_material()
# add_header_adove(c("Características", "Medidas de Tendência Central e Variadilidade" = 8))

## Graf. Linha ----
dados|>
  ggplot(aes(x = ano, y = desemp))+
  geom_line()+
  labs(
    title = "Taxa de Demsemprego por ano.",
    x = "Ano",
    y = "Taxa de Desemprego"
  )+
  theme_bw()

dados|>
  ggplot(aes(x = ano, y = suic))+
  geom_line()+
  labs(
    title = "Índice de Suicídio por ano.",
    x = "Ano",
    y = "Índice de Suicídio"
  )+
  theme_bw()

dados|>
  select(suic, desemp)|>
  # cor()|>
  GGally::ggpairs()
  corrplot::corrplot()






