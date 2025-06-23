library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/old_figures/FigureS16",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/old_figures/FigureS16")

data <-
  data.frame(
    group = c(rep("Control", 6), rep("DSS", 6), rep("MDLA", 6)),
    value = c(
      0.093382673,
      0.081367848,
      0.091599645,
      0.092383134,
      0.085329617,
      0.089801353,
      0.11992396,
      0.132932798,
      0.136898902,
      0.120414368,
      0.118046824,
      0.140200314,
      0.092930169,
      0.081009461,
      0.100518166,
      0.090687615,
      0.099777514,
      0.08978955
    )
  ) %>%
  dplyr:::mutate(group = factor(group, levels = c("Control", "DSS", "MDLA")))


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
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "ug/mL (serum)") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure_s16_1.pdf",
       width = 6,
       height = 5)


data <-
  data.frame(
    group = c(rep("Control", 6), rep("DSS", 6), rep("MDLA", 6)),
    value = c(
      0.1926,
      0.1468,
      0.0882,
      0.1926,
      0.2556,
      0.2238,
      2.3092,
      0.9922,
      2.2274,
      0.5931,
      1.5079,
      0.6461,
      0.2878,
      0.2716,
      0.0882,
      0.4036,
      0.3204,
      0.3534
    )
  ) %>%
  dplyr:::mutate(group = factor(group, levels = c("Control", "DSS", "MDLA")))


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
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "ug/mL (serum)") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure_s16_2.pdf",
       width = 6,
       height = 5)


data <-
  data.frame(
    group = c(rep("Control", 6), rep("DSS", 6), rep("MDLA", 6)),
    value = c(
      19.366,
      28.68945,
      13.4385,
      28.8,
      47.67555,
      5.463,
      6.2256,
      82.074,
      29.0409,
      18.874,
      52.10655,
      24.17655,
      19.2676,
      44.7306,
      14.3862,
      13.402,
      49.82925,
      18.89115
    )
  ) %>%
  dplyr:::mutate(group = factor(group, levels = c("Control", "DSS", "MDLA")))

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
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "ug/mL (serum)") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure_s16_3.pdf",
       width = 6,
       height = 5)
