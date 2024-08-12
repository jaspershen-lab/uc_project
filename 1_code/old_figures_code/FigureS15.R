library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/old_figures/FigureS15",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/old_figures/FigureS15")

###
data <-
  data.frame(
    group = c(rep("DSS", 4), rep("Arg", 4)),
    value = c(
      1.101449,
      0.608696,
      1.565217,
      0.724638,
      2.26087,
      3.342995,
      2.251208,
      1.855072
    )
  ) %>%
  dplyr:::mutate(group = factor(group, levels = c("DSS", "Arg")))

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
  labs(x = "", y = "Relative abundance (fold change compared to control)") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "figure_s15_1.pdf",
       width = 5,
       height = 5)


###

data <-
  data.frame(
    group = c(rep("DSS", 4), rep("Arg", 4)),
    value = c(
      0.76087,
      0.751553,
      1.099379,
      1.388199,
      1.931677,
      1.723602,
      1.860248,
      1.63354
    )
  ) %>%
  dplyr:::mutate(group = factor(group, levels = c("DSS", "Arg")))


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
  labs(x = "", y = "Relative abundance (fold change compared to control)") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "figure_s15_2.pdf",
       width = 5,
       height = 5)







###

data <-
  data.frame(
    group = c(rep("DSS", 4), rep("Arg", 4)),
    value = c(
      1.712974,
      0.52713,
      1.368211,
      0.391685,
      4.24202,
      0.194109,
      0.333206,
      0.469586
    )
  ) %>%
  dplyr:::mutate(group = factor(group, levels = c("DSS", "Arg")))


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
  labs(x = "", y = "Relative abundance (fold change compared to control)") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "figure_s15_3.pdf",
       width = 5,
       height = 5)









###
data <-
  data.frame(
    group = c(rep("DSS", 4), rep("Arg", 4)),
    value = c(
      1.044068,
      0.664407,
      1.925424,
      0.949153,
      3.267797,
      6.562712,
      2.264407,
      1.342373
    )
  ) %>%
  dplyr:::mutate(group = factor(group, levels = c("DSS", "Arg")))


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
  labs(x = "", y = "Relative abundance (fold change compared to control)") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "figure_s15_4.pdf",
       width = 5,
       height = 5)









###
data <-
  data.frame(
    group = c(rep("DSS", 4), rep("Arg", 4)),
    value = c(
      0.910134,
      1.137667,
      0.655832,
      1.296367,
      2.193117,
      1.50478,
      2.376673,
      3.543021
    )
  ) %>%
  dplyr:::mutate(group = factor(group, levels = c("DSS", "Arg")))

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
  labs(x = "", y = "Relative abundance (fold change compared to control)") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "figure_s15_5.pdf",
       width = 5,
       height = 5)
