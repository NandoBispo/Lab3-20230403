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

## fig1: Hist + Dens ----
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

## fig2: BoxPlot ----
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

## fig:3 Graf. Linha ----
l1 <- dados|>
  ggplot(aes(x = ano, y = desemp))+
  geom_line(color="blue", size=1, alpha=0.9)+
  labs(
    title = "Índice de Demsemprego por ano",
    x = "Ano",
    y = "Índice de Desemprego"
  )+
  theme_bw()

l2 <- dados|>
  ggplot(aes(x = ano, y = suic))+
  geom_line(color="blue", size=1, alpha=0.9)+
  labs(
    title = "Índice de Suicídio por ano",
    x = "Ano",
    y = "Índice de Suicídio"
  )+
  theme_bw()

l1/l2 + plot_annotation(
  title = "Figura 3: Evolução dos Índices de desemprego e suicídio \nnos EUA entre 1950 e 2019",
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

# Teste
dados|>
  select(suic, desemp)|>
  # cor()|>
  GGally::ggpairs()
corrplot::corrplot()

## Correlação ----
round(cor(dados$suic, dados$desemp), 4)

teste <- stats::cor.test(dados$suic, dados$desemp)

teste$statistic
teste$p.value|>round(5)
teste$conf.int[1:2]

aux <- cbind(
  c(
    teste$statistic, 
    teste$p.value|>round(5), 
    teste$conf.int[1:2])
  )

colnames(aux) <- c("Resultados")
rownames(aux) <- c("t", "p-valor", "LI", "LS")

aux|>
  kbl(
    caption = "Teste de Hipótese para Correlação",
    digits = 5,
    format.args=list(big.mark=".", decimal.mark=","),
    align = "c", row.names = T, booktabs = T
  )|>
  kable_styling(
    full_width = F, position = 'center', 
    latex_options = c("striped", "HOLD_position", "scale_down", "repeat_header")
  )|>
  column_spec(1, bold = T
  )|>
  footnote(
    general = "Teste realizado com 5% de significância",
    general_title = "Nota:",
    footnote_as_chunk = T
  )|>
kable_material()
  

## fig4: Dispersão ----
dados |>
  ggplot(aes(
    x = desemp, 
    y = suic, color = suic)) +
  geom_point()+
  labs(
    title = "Figura 4: Relação entre Índice de Desemprego e a Índice de Suicídio  \n entre 1950 e 2019, nos EUA",
    x = 'Índice de Desemprego',
    y = 'Índice de Suicídio'
  )+
  theme_bw()+
  theme(legend.position = "none")

## fig5: Disp+Reg ----
dados |>
  ggplot(aes(
    x = desemp, 
    y = suic, color = suic)) +
  geom_point()+
  geom_smooth(formula = "y ~ x", method="lm", se=F, color="red", fill="#69b3a2")+
  labs(
    title = "Figura 5: Relação entre Índice de Desemprego e a Índice de Suicídio  \n entre 1950 e 2019, nos EUA",
    x = 'Índice de Desemprego',
    y = 'Índice de Suicídio'
  )+
  ggpubr::stat_regline_equation(color="red", label.x = 8, label.y = 10.5, size = 3)+
  ggpubr::stat_cor(aes(label = ..r.label..),color="red", method = "pearson", label.x = 7, label.y = 10.5, p.accuracy = 0.001, size = 3)+
  # annotate("text", x = 7, y = 10.8,
  #          label = "Modelo Ajustado:",
  #          size=3, color="blue")+
  # annotate("text", x = 7, y = 12,
  #          label = "Coeficiente de Correlação:",
  #          size=3, color="blue")+
  theme_bw()+
  theme(legend.position = "none")
  # geom_smooth(method=lm, se = F)

# Regressão ----

## Modelo ----

mFit <- lm(suic ~ desemp, data = dados)

mFit$coefficients[[1]]
mFit$coefficients[2]

mFit$coefficients[[1]]|>
  scales::number(accuracy = 0.0001, big.mark = ".", decimal.mark = ",")

# Jeff
x = dados$suic
y = dados$desemp
m1 <- lm(y ~ x)
res <- residuals(m1)
d.ajustados <- predict(m1, as.data.frame(x), interval='confidence')
# ___________

(dados_mFit_resid <- broom::augment(mFit))

confint(mFit)

# Avaliação do Modelo (Significância) ----
fit_anova <- anova(mFit)
fit_sumario <- summary(mFit)
ic_parametros <- confint(mFit)


(round(summary(mFit)$coef[,4],5)) #p-val 
(summary(mFit)$coef[,3]) #t
(summary(mFit)$coef[,2]) #erro
(summary(mFit)$coef[,1]) #est

fit_sumario[["coefficients"]] %>% tibble::as_tibble() %>% 
  kbl(
    caption = "Tabela 1: Medidas Resumo.",
    digits = 4,
    format.args=list(big.mark=".", decimal.mark=","),
    align = "c", 
    row.names = T,
    col.names =
      c("Estimativa", "Erro Padrão", "Estatística t", "p-valor")
  ) %>% 
  kable_material(c("striped", "hover", "condensed"))|>
  kable_material()

fit_anova %>%
  kbl(
    caption = "Tabela 1: Medidas Resumo.",
    digits = 4,
    format.args=list(big.mark=".", decimal.mark=","),
    align = "c", 
    row.names = F,
    col.names =
      c("GL", "SQ", "QM", "Estatística", "p-valor")
  ) %>%
  kable_material(c("striped", "hover", "condensed"))|>
  kable_material()

ic_parametros %>% 
  kbl(
    caption = "Tabela 1: Medidas Resumo.",
    digits = 4,
    format.args=list(big.mark=".", decimal.mark=","),
    align = "c", 
    row.names = T
    # col.names =
    #   c("GL", "SQ", "QM", "Estatística", "p-valor")
  ) %>%
  kable_material(c("striped", "hover", "condensed"))|>
  kable_material()


tidy(mFit)

glance(mFit)


# Resíduos ----
## Gráfico de resíduos padronizads vs preditos ----
par(mfrow = c(2, 2))
plot(mFit)
par(mfrow = c(1, 1))

r1 <- dados_mFit_resid %>% 
  ggplot() + 
  geom_point(aes(x = .fitted, y = .resid, color = .resid)) +
  geom_hline(yintercept = 0, col = "tomato") +
  labs(
    x = "Valores Ajustados",
    y = "Resíduos",
    title = "Gráfico de Resíduos vs. Valores Ajustados"
  )+
  scale_x_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    )) +
  theme_minimal(base_size = 7.5)+
  theme(legend.position = "none" )

## Gráfico de normalidade dos resíduos ----

set.seed(20231)

r2 <- dados_mFit_resid %>% 
  ggplot(aes(sample = .std.resid)) + 
  qqplotr::stat_qq_band(fill = "lightgrey", alpha = 0.5) + # Plota a banda de confiança
  qqplotr::stat_qq_line(col = "tomato", alpha = 0.8) + # Plota a reta
  qqplotr::stat_qq_point(col = "blue", shape = 21) + # Plota os pontos
  labs(
    x = "Quantil Teórico",
    y = "Resíduos Padronizados",
    title = "Gráfico Quantil-Quantil Normal"
  )+
  theme_minimal(base_size = 7.5)

r3 <- dados_mFit_resid %>% 
  ggplot() + 
  geom_point(aes(x = .fitted, y = sqrt(.std.resid), color = .std.resid)) +
  geom_hline(yintercept = 0, col = "tomato") +
  labs(
    x = "Valores Ajustados",
    y = "Raiz dos Resíduos Padronizados",
    title = "Gráfico de Resíduos Padronizados vs. Valores Ajustados \n(Locação - Escala)"
  )+
  theme_minimal(base_size = 7.5)+
  theme(legend.position = "none" )

r1+r2+r3 + patchwork::plot_layout(ncol = 2) +
  patchwork::plot_annotation(
  title = "Figura 6: Gráficos para análise de pressupostos.",
  tag_levels = c("A", "1"), tag_prefix = "Sub Fig. ", tag_sep = ".",
  tag_suffix = ":") &
  theme_minimal(base_size = 8) &
  theme(
    # title = element_text(size = 8),
    legend.position = "none",
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 6, hjust = 0, vjust = -0.6)
  )

# dados_mFit_resid %>% 
#   ggplot() + 
#   geom_point(aes(x = .cooksd, y = .std.resid, color = .std.resid)) +
#   geom_hline(yintercept = 0, col = "tomato") +
#   labs(
#     x = "Valores Ajustados",
#     y = "Resíduos",
#     title = "Gráfico de Resíduos vs. Valores Ajustados"
#   )+
#   theme_minimal(base_size = 7.5)+
#   theme(legend.position = "none" )















# FIM ----


