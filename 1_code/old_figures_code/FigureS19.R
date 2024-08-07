library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/old_figures/FigureS19",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/old_figures/FigureS19")

data <-
  data.frame(
    group = c(rep("Control", 5), rep("Vector", 5), rep("ASS1 OE", 5)),
    value = c(
      0.087145,
      0.094587,
      0.100298,
      0.085989,
      0.119917,
      0.161738,
      0.198504,
      0.338729,
      0.201176,
      0.205177,
      0.341339,
      0.382622,
      0.316476,
      0.443231,
      0.427159
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "ASS1 OE")))

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
       filename = "figure_s19_1.pdf",
       width = 5,
       height = 5)

data <-
  data.frame(
    group = c(rep("Control", 5), rep("Vector", 5), rep("ASS1 OE", 5)),
    value = c(
      0.122,
      2.719,
      3.428,
      5.842,
      9.362,
      28.559,
      45.901,
      32.112,
      61.363,
      61.683,
      52.698,
      173.802,
      144.173,
      161.107,
      158.48
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "ASS1 OE")))


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
       filename = "figure_s19_2.pdf",
       width = 5,
       height = 5)
