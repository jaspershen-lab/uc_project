library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/12_epigenetic/2_western_blot",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/12_epigenetic/2_western_blot")

data <-
  data.frame(
    group = c(rep("Con siRNA", 3), rep("c-Myc siRNA", 3)),
    value = c(
      1.071955177,
      0.908595857,
      1.019448966,
      0.255259096,
      0.20464967,
      0.327144854
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Con siRNA", "c-Myc siRNA")))


plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    centrality.type = "nonparametric",
    point.args = list(
      alpha = 0.9,
      size = 3,
      color = "black",
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4)
  ) +
  theme_base +
  labs(x = "Time (hours)", y = "Relative ASS1 mRNA level") +
  theme(legend.position = "")

plot

ggsave(plot,
       filename = "c-myc-protein.pdf",
       width = 5,
       height = 5)
