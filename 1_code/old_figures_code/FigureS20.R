library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/old_figures/FigureS20",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/old_figures/FigureS20")



data <-
  data.frame(
    group = c(rep("Control", 6), rep("Control shRNA", 6), rep("ASS1 shRNA", 6)),
    value = c(
      0.053957904,
      0.053490032,
      0.159017749,
      0.051241302,
      0.041858059,
      0.056613077,
      0.133500188,
      0.297627808,
      0.186423423,
      0.296050149,
      0.217558505,
      0.186501624,
      0.037626219,
      0.13604456,
      0.127838162,
      0.065010091,
      0.125273539,
      0.129880021
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Control shRNA", "ASS1 shRNA")))

data

library(ggstatsplot)

plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      color = "black",
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "ug/mL (Serum)", title = "MPO") +
  theme(legend.position = "")

plot

ggsave(plot,
       filename = "figure_s20_1.pdf",
       width = 5,
       height = 5)





data <-
  data.frame(
    group = c(rep("Control", 4), rep("Control shRNA", 4), rep("ASS1 shRNA", 4)),
    value = c(
      18.925,
      19.195,
      1.13,
      8.655,
      63.6728,
      54.2468,
      43.612,
      45.374,
      42.6132,
      2.6328,
      22.8252,
      16.6556
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Control shRNA", "ASS1 shRNA")))

data

library(ggstatsplot)

plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      color = "black",
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "ug/mL (Serum)", title = "IL6") +
  theme(legend.position = "")

plot

ggsave(plot,
       filename = "figure_s20_2.pdf",
       width = 5,
       height = 5)




data <-
  data.frame(
    group = c(rep("Control", 6), rep("Control shRNA", 6), rep("ASS1 shRNA", 6)),
    value = c(
      2.997,
      2.51,
      4.558,
      2.51,
      2.207,
      2.283,
      3.95,
      6.046,
      7.862,
      5.189,
      6.617,
      8.056,
      4.058,
      1.674,
      2.283,
      4.416,
      2.51,
      2.131
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Control shRNA", "ASS1 shRNA")))


data

library(ggstatsplot)

plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      color = "black",
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "ug/mL (Serum)", title = "IL1-β") +
  theme(legend.position = "")

plot

ggsave(plot,
       filename = "figure_s20_3.pdf",
       width = 5,
       height = 5)
