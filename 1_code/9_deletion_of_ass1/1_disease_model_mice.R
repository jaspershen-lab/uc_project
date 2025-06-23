library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

data1 <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 39)

data2 <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 40)

data3 <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 41)

dir.create(
  "3_data_analysis/9_deletion_of_ass1/1_disease_model_mice",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/9_deletion_of_ass1/1_disease_model_mice")

colnames(data1)[1] <- "Day"

data1 <-
  data1 %>%
  as.data.frame() %>%
  rename_with( ~ gsub("\\.\\.\\.", "_", .)) %>%
  tidyr::pivot_longer(cols = -Day,
                      names_to = "sample_id",
                      values_to = "value") %>%
  dplyr::mutate(group = case_when(
    grepl("Control shRNA", sample_id) ~ "Vector",
    grepl("Control", sample_id) ~ "Control",
    grepl("ASS1 shRNA", sample_id) ~ "shRNA",
  )) %>%
  dplyr::mutate(Day = as.character(Day)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "shRNA")))


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
  # stat_compare_means(
  #   method = "t.test",
  #   comparisons = list(c("Control", "Vector"), c("Control", "shRNA"), c("Vector", "shRNA")),
  #   label = "p.signif",
  #   # label.y = max(data1$value),
  #   position = position_dodge(0.35)
  # ) +
  geom_line(
    data = median_data,
    aes(x = as.numeric(Day), y = median_value, group = group),
    position = position_dodge(0.35)
  ) +
  theme_base +
  scale_color_manual(values = disease_color3) +
  labs(x = "Day", y = "Body weight (%, compared to baseline)")

plot

ggsave(plot,
       filename = "body_weight_change.pdf",
       width = 7,
       height = 5)


colnames(data2)[1] <- "Day"

data2 <-
  data2 %>%
  as.data.frame() %>%
  rename_with( ~ gsub("\\.\\.\\.", "_", .)) %>%
  tidyr::pivot_longer(cols = -Day,
                      names_to = "sample_id",
                      values_to = "value") %>%
  dplyr::mutate(group = case_when(
    grepl("Control shRNA", sample_id) ~ "Vector",
    grepl("Control", sample_id) ~ "Control",
    grepl("ASS1 shRNA", sample_id) ~ "shRNA",
  )) %>%
  dplyr::mutate(Day = as.character(Day)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "shRNA")))


# Calculate medians
median_data <-
  data2 %>%
  dplyr::group_by(Day, group) %>%
  dplyr::summarize(median_value = median(value), .groups = 'drop')

library(ggpubr)

plot <-
  data2 %>%
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
  scale_color_manual(values = disease_color3) +
  labs(x = "Day", y = "Disease activity index (DAI)")
plot
ggsave(plot,
       filename = "dai_change.pdf",
       width = 7,
       height = 5)

data3

data3 <-
  rbind(
    data.frame(group = "Control", value = data3$Control),
    data.frame(group = "Vector", value = data3$`Control shRNA`),
    data.frame(group = "shRNA", value = data3$`ASS1 shRNA`)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "shRNA")))


plot <-
  ggbetweenstats(
    data  = data3,
    x  = group,
    y  = value,
    type = "nonparametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    )
  ) +
  theme_base +
  labs(x = "", y = "Age (years)") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "colon_length_boxplot.pdf",
       width = 5,
       height = 5)
