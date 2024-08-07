library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/old_figures/FigureS27",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/old_figures/FigureS27")

####A
data <-
  data.frame(
    group = c(
      rep("Vechicle", 3),
      rep("Model", 3),
      rep("C-03", 3),
      rep("C-04", 3),
      rep("C-05", 3),
      rep("C-06", 3),
      rep("C-07", 3),
      rep("C-08", 3),
      rep("C-09", 3),
      rep("C-10", 3),
      rep("C-01", 3),
      rep("C-02", 3),
      rep("C-11", 3)
    ),
    value = c(
      0.722955,
      0.331628,
      0.484738,
      1.025562,
      0.738188,
      1.049759,
      0.994376,
      0.779293,
      1.039213,
      1.08812,
      0.835138,
      1.090427,
      1.113069,
      1.474724,
      1.164203,
      0.965293,
      1.110952,
      1.080928,
      0.86161,
      1.203485,
      1.10981,
      0.923182,
      1.113403,
      0.943544,
      0.864573,
      0.930522,
      1.069831,
      0.833809,
      0.835292,
      0.729117,
      0.621659,
      0.49819,
      0.390403,
      0.542055,
      0.443535,
      0.204772,
      0.822231,
      1.160657,
      0.445295
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c(
      "Vechicle",
      "Model",
      "C-03",
      "C-04",
      "C-05",
      "C-06",
      "C-07",
      "C-08",
      "C-09",
      "C-10",
      "C-01",
      "C-02",
      "C-11"
    )
  ))


plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "significant",
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
  labs(x = "", y = "p-STAT3/STAT3") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))
# scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "figure_s27a.pdf",
       width = 10,
       height = 5)








####B
data <-
  data.frame(
    group = c(
      rep("Vechicle", 3),
      rep("Model", 3),
      rep("C-01", 3),
      rep("C-02", 3)
    ),
    value = c(
      0.855767,
      0.708494,
      0.498621,
      1.159398,
      1.048181,
      0.966027,
      0.708187,
      0.828864,
      0.779743,
      0.840562,
      0.931434,
      0.837597
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Vechicle", "Model", "C-01", "C-02")))

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
  labs(x = "", y = "p-STAT3/STAT3") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))
# scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "figure_s27b.pdf",
       width = 6,
       height = 5)
