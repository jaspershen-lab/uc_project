library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/old_figures/FigureS21",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/old_figures/FigureS21")



###A
data <-
  data.frame(
    group = c(rep("Control shRNA", 3), rep("ASS1 shRNA", 3)),
    value = c(1.529766, 1.080526, 1.074947, 0.869162, 0.357953, 0.64204)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control shRNA", "ASS1 shRNA")))

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
  labs(x = "", y = "Protein/GAPDH", title = "Colon") +
  theme(legend.position = "")
# scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure_s21a.pdf",
       width = 5,
       height = 5)








###B
data <-
  data.frame(
    group = c(rep("Control shRNA", 3), rep("ASS1 shRNA", 3)),
    value = c(0.70699, 1.206029, 1.336359, 0.788245, 1.694935, 1.05705)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control shRNA", "ASS1 shRNA")))

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
  labs(x = "", y = "Protein/GAPDH", title = "Heart") +
  theme(legend.position = "")
# scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure_s21b.pdf",
       width = 5,
       height = 5)















###C

data <-
  data.frame(
    group = c(rep("Control shRNA", 3), rep("ASS1 shRNA", 3)),
    value = c(1.165279, 1.167305, 1.152968, 1.15506, 1.077508, 0.923817)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control shRNA", "ASS1 shRNA")))

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
  labs(x = "", y = "Protein/GAPDH", title = "Liver") +
  theme(legend.position = "")
# scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure_s21c.pdf",
       width = 5,
       height = 5)




###D
data <-
  data.frame(
    group = c(rep("Control shRNA", 3), rep("ASS1 shRNA", 3)),
    value = c(1.066021, 0.952692, 1.019336, 0.97838, 0.912497, 0.830588)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control shRNA", "ASS1 shRNA")))


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
  labs(x = "", y = "Protein/GAPDH", title = "Spleen") +
  theme(legend.position = "")
# scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure_s21d.pdf",
       width = 5,
       height = 5)







###E
data <-
  data.frame(
    group = c(rep("Control shRNA", 3), rep("ASS1 shRNA", 3)),
    value = c(0.7132, 0.900109, 0.853242, 1.005441, 0.574214, 0.665253)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control shRNA", "ASS1 shRNA")))

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
  labs(x = "", y = "Protein/GAPDH", title = "Lung") +
  theme(legend.position = "")
# scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure_s21e.pdf",
       width = 5,
       height = 5)





###F
data <-
  data.frame(
    group = c(rep("Control shRNA", 3), rep("ASS1 shRNA", 3)),
    value = c(1.268208, 0.970683, 1.071775, 1.593889, 1.084613, 1.249411)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control shRNA", "ASS1 shRNA")))

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
  labs(x = "", y = "Protein/GAPDH", title = "Kidney") +
  theme(legend.position = "")
# scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure_s21f.pdf",
       width = 5,
       height = 5)
