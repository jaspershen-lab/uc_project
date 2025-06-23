library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/old_figures/FigureS5",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/old_figures/FigureS5")

data <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "FigS5A")

colnames(data) <-
  c("Control", "UC")

data <-
  rbind(
    data.frame(group = "Control", value = as.numeric(as.character(data$Control))),
    data.frame(group = "UC", value = as.numeric(as.character(data$UC)))
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
  labs(x = "", y = "Relative abundance (fold change)") +
  theme(legend.position = "") +
  scale_color_manual(values = group_color)

plot

ggsave(plot,
       filename = "figure_s5a.pdf",
       width = 5,
       height = 5)


####figure S5b

data <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "FigS5B")


colnames(data) <-
  c("Control", "DSS")

data <-
  rbind(
    data.frame(group = "Control", value = as.numeric(as.character(data$Control))),
    data.frame(group = "DSS", value = as.numeric(as.character(data$DSS)))
  )


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
  labs(x = "", y = "Relative abundance (fold change)") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color4)

plot


ggsave(plot,
       filename = "figure_s5b.pdf",
       width = 5,
       height = 5)
