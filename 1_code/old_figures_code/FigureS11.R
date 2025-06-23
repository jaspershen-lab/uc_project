library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/old_figures/FigureS11",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/old_figures/FigureS11")

###B
data <-
  data.frame(
    group = c("Control", "Control", "Control", "Control", "Control", "Control", "UC", "UC", "UC", "UC", "UC", "UC"),
    value = c(
      1.03787708,
      0.800852,
      1.161271,
      1.233662,
      0.837453,
      0.928885,
      1.940067,
      1.494008,
      1.927941,
      2.108827,
      1.913154,
      1.458594
    )
  )


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
  labs(x = "", y = "Protein/GADPH") +
  theme(legend.position = "") +
  scale_color_manual(values = group_color)

plot

ggsave(plot,
       filename = "figure_s11b_1.pdf",
       width = 5,
       height = 5)


data <-
  data.frame(
    group = c("Control", "Control", "Control", "Control", "Control", "Control", "UC", "UC", "UC", "UC", "UC", "UC"),
    value = c(
      1.04443959,
      1.146483,
      0.809077,
      0.92104896,
      1.13782,
      0.941131,
      1.197775,
      0.982706,
      0.979978,
      1.030388,
      0.914571,
      1.376009
    ))


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
  labs(x = "", y = "Protein/GADPH") +
  theme(legend.position = "") +
  scale_color_manual(values = group_color)

plot

ggsave(plot,
       filename = "figure_s11b_2.pdf",
       width = 5,
       height = 5)

