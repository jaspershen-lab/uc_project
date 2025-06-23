library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create("3_data_analysis/Figure9",
           showWarnings = TRUE,
           recursive = TRUE)
setwd("3_data_analysis/Figure9")

###Figure 9E

data <-
  readxl::read_xlsx("../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig9E")

colnames(data)[1] <- "Day"

data <-
  data %>%
  as.data.frame() %>%
  rename_with( ~ gsub("\\.\\.\\.", "_", .)) %>%
  tidyr::pivot_longer(cols = -Day,
                      names_to = "sample_id",
                      values_to = "value") %>%
  dplyr::mutate(group = case_when(
    grepl("Control", sample_id) ~ "Control",
    grepl("Model", sample_id) ~ "DSS",
    grepl("MDLA", sample_id) ~ "MDLA",
    grepl("C-01", sample_id) ~ "C_01"
  )) %>%
  dplyr::mutate(Day = as.character(Day)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "C_01")))


# Calculate medians
median_data <-
  data %>%
  dplyr::group_by(Day, group) %>%
  dplyr::summarize(median_value = median(value), .groups = 'drop')

plot <-
  data %>%
  ggplot(aes(x = as.character(Day), y = value, color = group)) +
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
    aes(
      x = as.numeric(Day) + 1,
      y = median_value,
      group = group
    ),
    position = position_dodge(0.35)
  ) +
  theme_base +
  scale_color_manual(values = disease_color5) +
  labs(x = "Day", y = "Disease activity index (DAI)")
plot


ggsave(plot,
       filename = "Figure9e.pdf",
       width = 7,
       height = 5)

###Figure 9F
data <-
  readxl::read_xlsx("../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig9F")
