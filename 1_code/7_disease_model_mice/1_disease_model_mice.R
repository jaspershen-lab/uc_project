library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

data1 <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 10)

data2 <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 11)

data3 <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 13)

dir.create("3_data_analysis/7_disease_model_mice")
setwd("3_data_analysis/7_disease_model_mice/")

data1 <-
  data1 %>%
  as.data.frame() %>%
  rename_with( ~ gsub("\\.\\.\\.", "_", .)) %>%
  tidyr::pivot_longer(cols = -Day,
                      names_to = "sample_id",
                      values_to = "value") %>%
  dplyr::mutate(group = case_when(
    grepl("Control", sample_id) ~ "Control",
    grepl("DSS", sample_id) ~ "DSS",
    grepl("Arg", sample_id) ~ "Arg",
  )) %>%
  dplyr::mutate(Day = as.character(Day)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))


# Calculate medians
median_data <-
  data1 %>%
  dplyr::group_by(Day, group) %>%
  dplyr::summarize(median_value = median(value), .groups = 'drop')

data1 %>%
  ggplot(aes(x = Day, y = value, color = group)) +
  geom_boxplot(
    aes(color = group),
    outlier.shape = NA,
    position = position_dodge(0.35),
    width = 0.3
  ) +
  theme_base +
  scale_color_manual(values = disease_color) +
  labs(x = "Day", y = "Body weight (%, compared to baseline)")


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
  #   comparisons = list(c("Control", "DSS"), c("Control", "Arg"), c("DSS", "Arg")),
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
  scale_color_manual(values = disease_color) +
  labs(x = "Day", y = "Body weight (%, compared to baseline)")

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
    grepl("Control", sample_id) ~ "Control",
    grepl("DSS", sample_id) ~ "DSS",
    grepl("Arg", sample_id) ~ "Arg",
  )) %>%
  dplyr::mutate(Day = as.character(Day)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))


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
  scale_color_manual(values = disease_color) +
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
    data.frame(group = "DSS", value = data3$Model),
    data.frame(group = "Arg", value = data3$Arg)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))


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
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "colon_length_boxplot.pdf",
       width = 5,
       height = 5)
