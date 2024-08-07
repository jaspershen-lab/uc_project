library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/old_figures/FigureS9",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/old_figures/FigureS9")

###A
data <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "FigS9A")


colnames(data)[2:3] <- c("group", "value")

data <-
  data %>%
  dplyr::mutate(group = case_when(group == "H" ~ "Control", group == "UC" ~ "UC")) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "UC")))

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
  labs(x = "", y = "ASS1 mRNA expression") +
  theme(legend.position = "") +
  scale_color_manual(values = group_color)

plot

ggsave(plot,
       filename = "figure_s9a.pdf",
       width = 5,
       height = 5)

###B
data <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "FigS9B")

colnames(data)[2:3] <- c("group", "value")

data <-
  data %>%
  dplyr::mutate(group = case_when(group == "H" ~ "Control", group == "UC" ~ "UC")) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "UC")))

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
  labs(x = "", y = "ASS1 mRNA expression") +
  theme(legend.position = "") +
  scale_color_manual(values = group_color)

plot

ggsave(plot,
       filename = "figure_s9b.pdf",
       width = 5,
       height = 5)


####C
data <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "FigS9C")

colnames(data)[2:3] <- c("group", "value")

data <-
  data %>%
  dplyr::mutate(
    group = case_when(
      group == "III" ~ "Active UC",
      group == "II" ~ "UC in remission",
      group == "I" ~ "Control"
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "UC in remission", "Active UC")))


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
      color = "black",
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "ASS1 mRNA expression") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))
# scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "figure_s9c.pdf",
       width = 5,
       height = 5)




####D
data <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "FigS9D")

colnames(data)[2:3] <- c("group", "value")

data <-
  data %>%
  dplyr::mutate(
    group = case_when(
      group == "III" ~ "Active UC",
      group == "II" ~ "UC in remission",
      group == "I" ~ "Control"
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "UC in remission", "Active UC")))


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
      color = "black",
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "ASS1 mRNA expression") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))
# scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "figure_s9d.pdf",
       width = 5,
       height = 5)
