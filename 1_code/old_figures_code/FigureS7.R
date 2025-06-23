library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/old_figures/FigureS7",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/old_figures/FigureS7")

###A
data <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "FigS7A")

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
  labs(x = "", y = "Relative ARG1 mRNA level (FC compared to H)") +
  theme(legend.position = "") +
  scale_color_manual(values = group_color)

plot

ggsave(plot,
       filename = "figure_s7a.pdf",
       width = 5,
       height = 5)

###B
data <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "FigS7B")

data

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
  labs(x = "", y = "Relative NOS2 mRNA level (compared to control)") +
  theme(legend.position = "") +
  scale_color_manual(values = group_color)

plot

ggsave(plot,
       filename = "figure_s7b.pdf",
       width = 5,
       height = 5)


####C
data <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "FigS7C")

colnames(data) <-
  c("Control", "DSS")

data <-
  rbind(
    data.frame(group = "Control", value = as.numeric(as.character(data$Control))),
    data.frame(group = "DSS", value = as.numeric(as.character(data$DSS)))
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
  labs(x = "", y = "Relative ARG1 mRNA level") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "figure_s7c.pdf",
       width = 5,
       height = 5)


###D
data <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "FigS7D")

data

colnames(data) <-
  c("Control", "DSS")

data <-
  rbind(
    data.frame(group = "Control", value = as.numeric(as.character(data$Control))),
    data.frame(group = "DSS", value = as.numeric(as.character(data$DSS)))
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
  labs(x = "", y = "Relative nNOS2 mRNA level") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "figure_s7d.pdf",
       width = 5,
       height = 5)
