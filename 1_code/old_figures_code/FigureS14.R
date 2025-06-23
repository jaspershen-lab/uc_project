library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/old_figures/FigureS14",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/old_figures/FigureS14")

###
data <-
  data.frame(
    group = c(rep("Control", 6), rep("DSS", 6), rep("Arg", 6)),
    value = c(
      0.098131838,
      0.105710891,
      0.097347999,
      0.111399376,
      0.100980355,
      0.134118575,
      0.166009727,
      0.187422317,
      0.181570793,
      0.180703437,
      0.189084859,
      0.198551298,
      0.235068644,
      0.209264094,
      0.210544788,
      0.206256047,
      0.238929785,
      0.239878489
    )
  ) %>%
  dplyr:::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))

library(ggstatsplot)

plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "ug/mL (Serum)") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "figure_s14_1.pdf",
       width = 5,
       height = 5)






###

data <-
  data.frame(
    group = c(rep("Control", 6), rep("DSS", 6), rep("Arg", 6)),
    value = c(
      0.5931,
      0.3534,
      0.5232,
      0.7355,
      0.4715,
      0.5931,
      1.1047,
      1.4495,
      1.5079,
      1.0858,
      1.2566,
      1.3913,
      1.6256,
      1.1424,
      1.6256,
      1.8039,
      2.9324,
      1.8638
    )
  ) %>%
  dplyr:::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))

library(ggstatsplot)

plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "ug/mL (Serum)") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "figure_s14_2.pdf",
       width = 5,
       height = 5)







###

data <-
  data.frame(
    group = c(rep("Control", 6), rep("DSS", 6), rep("Arg", 6)),
    value = c(
      23.281,
      26.3053,
      27.7039,
      20.5336,
      9.262,
      27.9945,
      44.3043,
      45.9209,
      49.9724,
      48.9988,
      41.6152,
      46.6491,
      81.3653,
      172.1694,
      144.0777,
      93.5147,
      71.5937,
      65.4553
    )
  ) %>%
  dplyr:::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))

library(ggstatsplot)

plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "ug/mL (Serum)") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "figure_s14_3.pdf",
       width = 5,
       height = 5)
