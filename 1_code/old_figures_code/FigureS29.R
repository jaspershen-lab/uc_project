library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/old_figures/FigureS29",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/old_figures/FigureS29")


###
##mpo
mpo <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.088388, 0.132394, 0.134153, 0.104275, 0.126303)
    ),
    data.frame(
      group = "Model",
      value = c(0.185608, 0.221242, 0.18568, 0.203116, 0.226136)
    ),
    data.frame(
      group = "Control+C-01",
      value = c(0.16712, 0.1579, 0.184497, 0.131268, 0.159108)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Model", "Control+C-01")))

il6 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(9.476, 13.861, 6.415, 2.579, 15.188)
    ),
    data.frame(
      group = "Model",
      value = c(82.256, 25.114, 25.135, 62.687, 35.876)
    ),
    data.frame(
      group = "Control+C-01",
      value = c(25.195, 10.314, 15.403, 24.812, 8.624)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Model", "Control+C-01")))


il1_beta <-
  rbind(
    data.frame(
      group = "Control",
      value = c(2.586, 1.597, 2.623, 2.169, 0.729, 0.445)
    ),
    data.frame(
      group = "Model",
      value = c(3.22, 3.477, 4.629, 8.28, 8.977, 5.015)
    ),
    data.frame(
      group = "Control+C-01",
      value = c(1.254, 1.941, 1.941, 0.345, 2.017, 3.109)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Model", "Control+C-01")))



###mpo
plot <-
  ggbetweenstats(
    data  = mpo,
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
  labs(x = "", y = "ug/mL Serum", title = "MPO") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))

plot

ggsave(plot,
       filename = "figure_s29_1.pdf",
       width = 5,
       height = 5)





###il6
plot <-
  ggbetweenstats(
    data  = il6,
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
  labs(x = "", y = "ug/mL Serum", title = "IL-6") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))

plot

ggsave(plot,
       filename = "figure_s29_2.pdf",
       width = 5,
       height = 5)



###il-1beta
plot <-
  ggbetweenstats(
    data  = il1_beta,
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
  labs(x = "", y = "ug/mL Serum", title = "IL-1beta") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))

plot

ggsave(plot,
       filename = "figure_s29_3.pdf",
       width = 5,
       height = 5)
