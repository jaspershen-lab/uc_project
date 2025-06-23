library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/old_figures/FigureS17",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/old_figures/FigureS17")



###A
data <-
  data.frame(
    group = c(rep("Vector", 3), rep("ASS1 OE", 3)),
    value = c(0.773, 0.819, 0.873, 1.23, 1.364, 1.639)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Vector", "ASS1 OE")))

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
  labs(x = "", y = "Protein/GAPDH") +
  theme(legend.position = "")
# scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure_s17a.pdf",
       width = 5,
       height = 5)





###B
data <-
  data.frame(
    group = c(rep("Vector", 3), rep("ASS1 OE", 3)),
    value = c(1.055, 1.124, 1.141, 1.215, 1.221, 0.879)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Vector", "ASS1 OE")))

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
  labs(x = "", y = "Protein/GAPDH") +
  theme(legend.position = "")
# scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure_s17b.pdf",
       width = 5,
       height = 5)




###C

data <-
  data.frame(
    group = c(rep("Vector", 3), rep("ASS1 OE", 3)),
    value = c(1.045, 0.723, 1.132, 1.212, 1.05, 1.257)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Vector", "ASS1 OE")))

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
  labs(x = "", y = "Protein/GAPDH") +
  theme(legend.position = "")
# scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure_s17c.pdf",
       width = 5,
       height = 5)










###D

data <-
  data.frame(
    group = c(rep("Vector", 3), rep("ASS1 OE", 3)),
    value = c(0.92, 0.838, 1.203, 0.896, 1.122, 1.235)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Vector", "ASS1 OE")))

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
  labs(x = "", y = "Protein/GAPDH") +
  theme(legend.position = "")
# scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure_s17d.pdf",
       width = 5,
       height = 5)







###E
data <-
  data.frame(
    group = c(rep("Vector", 3), rep("ASS1 OE", 3)),
    value = c(0.991, 1.062, 1.072, 1.003, 0.949, 0.932)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Vector", "ASS1 OE")))


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
  labs(x = "", y = "Protein/GAPDH") +
  theme(legend.position = "")
# scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure_s17e.pdf",
       width = 5,
       height = 5)








###F
data <-
  data.frame(
    group = c(rep("Vector", 3), rep("ASS1 OE", 3)),
    value = c(1.03, 1.114, 1.172, 1.238, 1.473, 1.022)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Vector", "ASS1 OE")))

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
  labs(x = "", y = "Protein/GAPDH") +
  theme(legend.position = "")
# scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure_s17f.pdf",
       width = 5,
       height = 5)
