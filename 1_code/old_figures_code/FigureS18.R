library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/old_figures/FigureS18",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/old_figures/FigureS18")

data <-
  data.frame(
    group = c(rep("Vector", 4), rep("ASS1 OE", 3), rep("ASS1 ShRNA", 3)),
    value = c(
      0.872392,
      0.756763,
      0.922766,
      2.867588,
      4.258503,
      2.674605486,
      0.664134,
      0.682423,
      0.263351,
      1.448079
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Vector", "ASS1 OE", "ASS1 ShRNA")))

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
  labs(x = "", y = "Relative abundance (fold change compared to control)", title = "Colon") +
  theme(legend.position = "")

plot

ggsave(plot,
       filename = "figure_s18_1.pdf",
       width = 5,
       height = 5)



#############
data <-
  data.frame(
    group = c(rep("Vector", 4), rep("ASS1 OE", 3), rep("ASS1 ShRNA", 3)),
    value = c(1.14, 0.74, 1.22, 0.89, 0.87, 1.39, 0.96, 1.35, 1.16, 1.17)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Vector", "ASS1 OE", "ASS1 ShRNA")))

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
  labs(x = "", y = "Relative abundance (fold change compared to control)", title = "Heart") +
  theme(legend.position = "")

plot

ggsave(plot,
       filename = "figure_s18_2.pdf",
       width = 5,
       height = 5)




#############
data <-
  data.frame(
    group = c(rep("Vector", 4), rep("ASS1 OE", 3), rep("ASS1 ShRNA", 3)),
    value = c(1.67, 0.6, 0.9, 0.84, 0.88, 0.44, 1.06, 1.06, 0.67, 1.51)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Vector", "ASS1 OE", "ASS1 ShRNA")))

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
  labs(x = "", y = "Relative abundance (fold change compared to control)", title = "Kidney") +
  theme(legend.position = "")

plot

ggsave(plot,
       filename = "figure_s18_3.pdf",
       width = 5,
       height = 5)









#############
data <-
  data.frame(
    group = c(rep("Vector", 4), rep("ASS1 OE", 3), rep("ASS1 ShRNA", 3)),
    value = c(
      0.511275,
      1.413308,
      0.735829,
      1.339588,
      0.871896,
      2.122759,
      0.812257592,
      1.189388,
      0.825179,
      0.534501
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Vector", "ASS1 OE", "ASS1 ShRNA")))

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
  labs(x = "", y = "Relative abundance (fold change compared to control)", title = "Lung") +
  theme(legend.position = "")

plot

ggsave(plot,
       filename = "figure_s18_4.pdf",
       width = 5,
       height = 5)















#############
data <-
  data.frame(
    group = c(rep("Vector", 4), rep("ASS1 OE", 3), rep("ASS1 ShRNA", 3)),
    value = c(1.44, 0.56, 1.2, 0.8, 0.8, 1.04, 0.72, 1.44, 0.4, 1.28)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Vector", "ASS1 OE", "ASS1 ShRNA")))

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
  labs(x = "", y = "Relative abundance (fold change compared to control)", title = "Spleen") +
  theme(legend.position = "")

plot

ggsave(plot,
       filename = "figure_s18_5.pdf",
       width = 5,
       height = 5)














#############

data <-
  data.frame(
    group = c(rep("Vector", 4), rep("ASS1 OE", 3), rep("ASS1 ShRNA", 3)),
    value = c(
      1.0497,
      1.8686,
      0.7413,
      0.3404,
      0.925,
      2.0994,
      2.0747,
      2.0234,
      1.3171,
      0.5367
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Vector", "ASS1 OE", "ASS1 ShRNA")))

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
  labs(x = "", y = "Relative abundance (fold change compared to control)", title = "Liver") +
  theme(legend.position = "")

plot

ggsave(plot,
       filename = "figure_s18_6.pdf",
       width = 5,
       height = 5)
