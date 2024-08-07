library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

data <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 4)

dir.create("3_data_analysis/3_metabolomics_data_analysis/3_arginine_in_validation_cohorts")
setwd("3_data_analysis/3_metabolomics_data_analysis/3_arginine_in_validation_cohorts")

data1 <-
  data[, c(1, 2)] %>%
  as.data.frame()

data1 <-
  data1[-c(1), ]

data2 <-
  data[, c(4, 5)] %>%
  as.data.frame()

data2 <-
  data2[-c(1), ]

data3 <-
  data[, c(7, 8)] %>%
  as.data.frame()

data3 <-
  data3[-c(1), ]

colnames(data1) <-
  colnames(data2) <-
  colnames(data3) <-
  c("Control", "UC")

data1 <-
  rbind(
    data.frame(
      sample_id = paste0("Control_", 1:nrow(data1)),
      group = "Control",
      center = "Hospital 1",
      value = data1$Control
    ),
    data.frame(
      sample_id = paste0("UC_", 1:nrow(data1)),
      group = "UC",
      center = "Hospital 1",
      value = data1$UC
    )
  ) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(value = as.numeric(value))

data2 <-
  rbind(
    data.frame(
      sample_id = paste0("Control_", 1:nrow(data2)),
      group = "Control",
      center = "Hospital 2",
      value = data2$Control
    ),
    data.frame(
      sample_id = paste0("UC_", 1:nrow(data2)),
      group = "UC",
      center = "Hospital 2",
      value = data2$UC
    )
  ) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(value = as.numeric(value))

data3 <-
  rbind(
    data.frame(
      sample_id = paste0("Control_", 1:nrow(data3)),
      group = "Control",
      center = "Hospital 3",
      value = data3$Control
    ),
    data.frame(
      sample_id = paste0("UC_", 1:nrow(data3)),
      group = "UC",
      center = "Hospital 3",
      value = data3$UC
    )
  ) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(value = as.numeric(value))

data <-
  rbind(data1,
        data2,
        data3)

plot1 <-
  ggbetweenstats(
    data  = data1,
    x  = group,
    y  = value,
    type = "nonparametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    )
  ) +
  theme_base +
  labs(x = "", y = "Concentration (ug/mL)") +
  theme(legend.position = "") +
  scale_color_manual(values = group_color) +
  scale_y_continuous(limits = c(12, 51), expand = c(0.1, 0.1))

plot1

ggsave(plot1,
       filename = "center1.pdf",
       width = 5,
       height = 5)




plot2 <-
  ggbetweenstats(
    data  = data2,
    x  = group,
    y  = value,
    type = "nonparametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    )
  ) +
  theme_base +
  labs(x = "", y = "Concentration (ug/mL)") +
  theme(legend.position = "") +
  scale_color_manual(values = group_color) +
  scale_y_continuous(limits = c(12, 51), expand = c(0.1, 0.1))

plot2

ggsave(plot2,
       filename = "center2.pdf",
       width = 5,
       height = 5)



plot3 <-
  ggbetweenstats(
    data  = data3,
    x  = group,
    y  = value,
    type = "nonparametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    )
  ) +
  theme_base +
  labs(x = "", y = "Concentration (ug/mL)") +
  theme(legend.position = "") +
  scale_color_manual(values = group_color) +
  scale_y_continuous(limits = c(13, 51), expand = c(0.1, 0.1))

plot3

ggsave(plot3,
       filename = "center3.pdf",
       width = 5,
       height = 5)
