library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/old_figures/FigureS26",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/old_figures/FigureS26")

####A
data1 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "FigS26A")

colnames(data1)[1] <- "Day"

data1 <-
  data1 %>%
  as.data.frame() %>%
  rename_with(~ gsub("\\.\\.\\.", "_", .)) %>%
  tidyr::pivot_longer(cols = -Day,
                      names_to = "sample_id",
                      values_to = "value") %>%
  dplyr::mutate(group = case_when(
    grepl("MDLA\\+NOS2 inhibitor", sample_id) ~ "MDLA+NOS2 inhibitor",
    grepl("MDLA\\+NOS3 inhibitor", sample_id) ~ "MDLA+NOS3 inhibitor",
    grepl("MDLA", sample_id) ~ "MDLA"
  )) %>%
  dplyr::mutate(Day = as.character(Day)) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("MDLA", "MDLA+NOS2 inhibitor", "MDLA+NOS3 inhibitor")
  ))


# Calculate medians
median_data <-
  data1 %>%
  dplyr::group_by(Day, group) %>%
  dplyr::summarize(median_value = median(value), .groups = 'drop')

library(ggpubr)

plot <-
  data1 %>%
  ggplot(aes(x = Day, y = value, color = group)) +
  geom_point(
    aes(x = Day, y = value, color = group),
    position = position_dodge(0.35),
    size = 2,
    alpha = 0.7
  ) +
  geom_boxplot(
    aes(color = group),
    outlier.shape = NA,
    fill = "transparent",
    position = position_dodge(0.35),
    width = 0.3
  ) +
  geom_line(
    data = median_data,
    aes(x = as.numeric(Day), y = median_value, group = group),
    position = position_dodge(0.35)
  ) +
  theme_base +
  ggsci::scale_color_d3(palette = "category10") +
  # scale_color_manual(values = disease_color4) +
  labs(x = "Day", y = "Body weight change")

plot

ggsave(plot,
       filename = "figure_s26a.pdf",
       width = 7,
       height = 5)










####B
data1 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "FigS26B")

colnames(data1)[1] <- "Day"

data1 <-
  data1 %>%
  as.data.frame() %>%
  rename_with(~ gsub("\\.\\.\\.", "_", .)) %>%
  tidyr::pivot_longer(cols = -Day,
                      names_to = "sample_id",
                      values_to = "value") %>%
  dplyr::mutate(group = case_when(
    grepl("MDLA\\+NOS2 inhibitor", sample_id) ~ "MDLA+NOS2 inhibitor",
    grepl("MDLA\\+NOS3 inhibitor", sample_id) ~ "MDLA+NOS3 inhibitor",
    grepl("MDLA", sample_id) ~ "MDLA"
  )) %>%
  dplyr::mutate(Day = as.character(Day)) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("MDLA", "MDLA+NOS2 inhibitor", "MDLA+NOS3 inhibitor")
  ))


# Calculate medians
median_data <-
  data1 %>%
  dplyr::group_by(Day, group) %>%
  dplyr::summarize(median_value = median(value), .groups = 'drop')

library(ggpubr)

plot <-
  data1 %>%
  ggplot(aes(x = Day, y = value, color = group)) +
  geom_point(
    aes(x = Day, y = value, color = group),
    position = position_dodge(0.35),
    size = 2,
    alpha = 0.7
  ) +
  geom_boxplot(
    aes(color = group),
    outlier.shape = NA,
    fill = "transparent",
    position = position_dodge(0.35),
    width = 0.3
  ) +
  geom_line(
    data = median_data,
    aes(x = as.numeric(Day), y = median_value, group = group),
    position = position_dodge(0.35)
  ) +
  theme_base +
  ggsci::scale_color_d3(palette = "category10") +
  # scale_color_manual(values = disease_color4) +
  labs(x = "Day", y = "Diseases activity index (DAI)")

plot

ggsave(plot,
       filename = "figure_s26b.pdf",
       width = 7,
       height = 5)














####C
data1 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "FigS26C")

data1 <-
  data.frame(
    group = c(
      rep("MDLA", 6),
      rep("MDLA+NOS2 inhibitor", 6),
      rep("MDLA+NOS3 inhibitor", 6)
    ),
    value = c(
      data1$MDLA,
      data1$`MDLA+NOS2 inhibitor`,
      data1$`MDLA+NOS3 inhibitor`
    )
  )






plot <-
  ggbetweenstats(
    data  = data1,
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
  labs(x = "", y = "Colon length (cm)") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))
# scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "figure_s26c.pdf",
       width = 5,
       height = 5)


####E
data1 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "FigS26E")

data1 <-
  data.frame(
    group = c(
      rep("MDLA", 4),
      rep("MDLA+NOS2 inhibitor", 4),
      rep("MDLA+NOS3 inhibitor", 4)
    ),
    value = c(
      data1$MDLA,
      data1$`MDLA+NOS2 inhibitor`,
      data1$`MDLA+NOS3 inhibitor`
    )
  )


plot <-
  ggbetweenstats(
    data  = data1,
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
  labs(x = "", y = "H-score") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))
# scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "figure_s26e.pdf",
       width = 5,
       height = 5)
