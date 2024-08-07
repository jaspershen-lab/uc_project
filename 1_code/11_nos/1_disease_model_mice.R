library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/11_nos/1_disease_model_mice",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/11_nos/1_disease_model_mice")

data1 <-
  data.frame(
    group = c(rep("Control", 10), rep("DSS", 10), rep("MDLA", 10)),
    value = c(
      4.90156,
      1.5827,
      5.65098,
      7.471,
      4.04508,
      3.08154,
      4.7945,
      2.22506,
      4.2592,
      5.75804,
      8.11336,
      7.36394,
      5.97216,
      5.65098,
      8.0063,
      6.9357,
      8.0063,
      9.0769,
      7.89924,
      4.2592,
      8.0063,
      6.82864,
      4.2592,
      2.118,
      3.61684,
      5.75804,
      4.04508,
      3.08154,
      3.1886,
      6.9357
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "MDLA")))

plot <-
  ggbetweenstats(
    data  = data1,
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
  labs(x = "", y = "NO level (uM)") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "no_level_1.pdf",
       width = 5,
       height = 5)


data2 <-
  data.frame(
    group = c(rep("Control", 10), rep("DSS", 10), rep("Arg", 10)),
    value = c(
      3.29566,
      2.118,
      5.75804,
      8.0063,
      4.2592,
      2.97448,
      2.203648,
      2.22506,
      7.36394,
      4.36626,
      6.9357,
      9.18396,
      6.9357,
      6.4004,
      8.0063,
      7.57806,
      5.43686,
      4.04508,
      7.25688,
      9.6122,
      14.9652,
      11.11104,
      14.32284,
      10.57574,
      8.0063,
      10.36162,
      10.6828,
      10.6828,
      5.75804,
      6.82864
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))


plot <-
  ggbetweenstats(
    data  = data2,
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
  labs(x = "", y = "NO level (uM)") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "no_level_2.pdf",
       width = 5,
       height = 5)
