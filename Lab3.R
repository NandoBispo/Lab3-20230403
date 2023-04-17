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


pacman::p_load(dfSummary)

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
summarytools::st_options(lang = "pt")

dados|>
  select(-ano)|>
  rename("Índice de Desemprego" = desemp, "Índice de Suicídio" = suic)|>
  summarytools::descr(
    stats = c("min", "q1", "med", "mean","q3", "max",  "sd", "cv", "Skewness", "Kurtosis"),
    # round.digits = 3,
    justify = "c",
    style = "rmarkdown",
    # headings = T,
    # split.tables = 0.3,
    transpose = F
  )|>
  # round(., 2) %>%
  kbl(
    caption = "Tabela 1: Medidas Resumo.",
    digits = 2,
    format.args=list(big.mark=".", decimal.mark=","),
    align = "c", 
    row.names = T,
    # col.names = 
    #   # c("teste1", "2")
    #   c("Min", "Q1", "Med", "Média", "Q3", "Max", "D. Padrão", "CV", "Coef. Assimetria", "Curtose")
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

### Teste _____________________
dados|>
  select(-ano)|>
  summarytools::dfSummary(
    # plain.ascii  = FALSE,
    style        = "grid",
    graph.magnif = 0.75,
    valid.col    = FALSE,
    tmp.img.dir  = "/tmp"
    )|>
  view()
# ______________________________


## Histograma ----
h1 <- dados|>
  ggplot() +
  aes(x = desemp) +
  geom_histogram(
    # aes(y = after_stat(density)),
    binwidth = 1,
    fill = "lightblue",
    colour = "darkblue") +
  # geom_density(
  #   alpha = 0.2,
  #   fill = "blue",
  #   colour = "blue") +
  labs(
    # title = "Glicose",
    x = "Índice de Desemprego",
    y = "Frequência"
  )

h2 <- dados|>
  ggplot() +
  aes(x = suic) +
  geom_histogram(
    # aes(y = after_stat(density)),
    binwidth = 0.6,
    fill = "lightblue",
    colour = "darkblue") +
  # geom_density(
  #   alpha = 0.2,
  #   fill = "blue",
  #   colour = "blue") +
  labs(
    # title = "Glicose",
    x = "Índice de Suicídio",
    y = "Frequência"
  )

(h1+h2)+
  # plot_layout(nrow = 2, ncol = 3) + 
  plot_annotation(
    title = "Figura 1: Histograma das Índices de desemprego e suicídio, \n nos EUA de 1950 a 2019",
    # sudtitle = "",
    # caption = "Fonte: Instituto Nacional de diabetes e de Doenças Digestivas e Renais - EUA"
    tag_levels = c("A", "1"), tag_prefix = "Sub Fig. ", tag_sep = ".",
    # tag_levels = "A",
    tag_suffix = ":"
  ) & theme_bw() &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 8, hjust = 0, vjust = -0.4),
    legend.position = "none"
  )

## Densidade ----
d1 <- dados |>
  ggplot(aes(x = desemp)) +
  geom_density(
    # x = skullw, 
    fill = "lightblue",
    colour = "blue",
    alpha = 0.2) +
  geom_vline(
    # show.legend = T,
    xintercept = mean(dados$desemp),
    color = "red",
    linetype = "dashed" # "dotted"
  ) +
  geom_vline(
    xintercept = quantile(dados$desemp, 0.5),
    color = "blue",
    linetype = "dashed"
  ) +
  labs(
    title = "",
    # subtitle = ,
    x = "Índice de Desemprego",
    y = "Densidade"
  ) +
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    )) +
  theme_bw()+
  theme(legend.position = "none")

d2 <- dados |>
  ggplot(aes(x = suic)) +
  geom_density(
    # x = skullw, 
    fill = "lightblue",
    colour = "blue",
    alpha = 0.2) +
  geom_vline(
    # show.legend = T,
    xintercept = mean(dados$suic),
    color = "red",
    linetype = "dashed" # "dotted"
  ) +
  geom_vline(
    xintercept = quantile(dados$suic, 0.5),
    color = "blue",
    linetype = "dashed"
  ) +
  labs(
    title = "",
    x = "Índice de Suicídio",
    y = "Densidade"
  ) +
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))

(d1+d2)+
  plot_annotation(
    title = "Figura 2: Densidade das Índices de desemprego e suicídio, \n nos EUA de 1950 a 2019",
    caption = "Nota: Média representada pela linha vertical tracejada vermelha. \n Mediana representada pela linha vertical tracejada azul.",
    tag_levels = c("A", "1"), tag_prefix = "Sub Fig. ", tag_sep = ".",
    tag_suffix = ":"
  ) & theme_bw() &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 8, hjust = 0, vjust = 0),
    legend.position = "none"
  )

## Hist + Dens ----
hd1 <- dados|>
  ggplot() +
  aes(x = desemp) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 1,
    fill = "lightblue",
    colour = "darkblue") +
  geom_density(
    alpha = 0.2,
    fill = "blue",
    colour = "blue") +
  geom_vline(
    # show.legend = T,
    xintercept = mean(dados$desemp),
    color = "red",
    linetype = "dashed" # "dotted"
  ) +
  geom_vline(
    xintercept = quantile(dados$desemp, 0.5),
    color = "blue",
    linetype = "dashed"
  ) +
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    )) +
  labs(
    # title = "Glicose",
    x = "Índice de Desemprego",
    y = "Densidade"
  )

hd2 <- dados|>
  ggplot() +
  aes(x = suic) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 0.6,
    fill = "lightblue",
    colour = "darkblue") +
  geom_density(
    alpha = 0.2,
    fill = "blue",
    colour = "blue") +
  geom_vline(
    # show.legend = T,
    xintercept = mean(dados$suic),
    color = "red",
    linetype = "dashed" # "dotted"
  ) +
  geom_vline(
    xintercept = quantile(dados$suic, 0.5),
    color = "blue",
    linetype = "dashed"
  ) +
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    )) +
  labs(
    # title = "Glicose",
    x = "Índice de Suicídio",
    y = "Densidade"
  )

(hd1+hd2)+
  plot_annotation(
    title = "Figura 1: Histograma e Densidade dos Índices de desemprego e suicídio \nnos EUA de 1950 a 2019",
    caption = "Nota: Média representada pela linha vertical tracejada vermelha. \n Mediana representada pela linha vertical tracejada azul.",
    tag_levels = c("A", "1"), tag_prefix = "Sub Fig. ", tag_sep = ".",
    tag_suffix = ":"
  ) & theme_bw() &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 8, hjust = 0, vjust = -0.4),
    legend.position = "none"
  )

## BoxPlot ----
b1 <- dados|>
  ggplot(aes(y = desemp)) +
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    title = 'Índice de Desemprego',
    y = "Desempregados por 1000"
  ) +
  scale_x_continuous(
    labels = scales::number_format(
      dig.mark = ".",
      decimal.mark = ","))

b2 <- dados|>
  ggplot(aes(y = suic)) +
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    title = 'Índice de Suicídio',
    y = "Suicídio por 1000"
  ) +
  # stat_summary(
  #   fun="mean", geom="point", position = "identity", shape=18, size=3, color = "darkred")+
  scale_x_continuous(
    labels = scales::number_format(
      dig.mark = ".",
      decimal.mark = ","))

b1+b2 + plot_annotation(
  title = "Figura 3: BoxPlots dos Índices de desemprego e suicídio \n nos EUA de 1950 a 2019",
  tag_levels = c("A", "1"), tag_prefix = "Sub Fig. ", tag_sep = ".",
  tag_suffix = ":") &
  theme_bw(
    base_size = 10
    ) &
  theme(
    # title = element_text(size = 8),
    legend.position = "none",
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 8, hjust = 0, vjust = -0.6)
  )


















## Graf. Linha ----

library(hrbrthemes)

dados|>
  ggplot(aes(x = ano, y = desemp))+
  geom_line(color="#69b3a2", size=1, alpha=0.9)+
  labs(
    title = "Índice de Demsemprego por ano",
    x = "Ano",
    y = "Índice de Desemprego"
  )+
  theme_bw()
  theme_ipsum()

dados|>
  ggplot(aes(x = ano, y = suic))+
  geom_line(color="blue", size=1, alpha=0.9)+
  labs(
    title = "Índice de Suicídio por ano",
    x = "Ano",
    y = "Índice de Suicídio"
  )+
  theme_bw()

dados|>
  select(suic, desemp)|>
  # cor()|>
  GGally::ggpairs()
corrplot::corrplot()





# FIM ----


