library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

##MPO
data1 <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 33)

##IL6
data2 <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 34)

##IL1-b
data3 <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 35)

dir.create("3_data_analysis/8_overexpression_of_ass1/4_cytokine", showWarnings = TRUE, recursive = TRUE)
setwd("3_data_analysis/8_overexpression_of_ass1/4_cytokine")


data1

data1 <-
  rbind(
    data.frame(group = "Control", value = data1$Control),
    data.frame(group = "Vector", value = data1$Vector),
    data.frame(group = "ASS1_OE", value = data1$`ASS1 OE`)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "ASS1_OE")))


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
  labs(x = "", y = "ug/g") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color2)

plot

ggsave(plot,
       filename = "mpo.pdf",
       width = 6,
       height = 5)


data2

data2 <-
  rbind(
    data.frame(group = "Control", value = data2$Control),
    data.frame(group = "Vector", value = data2$Vector),
    data.frame(group = "ASS1_OE", value = data2$`ASS1 OE`)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "ASS1_OE")))


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
  labs(x = "", y = "ug/g") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color2)

plot

ggsave(plot,
       filename = "il6.pdf",
       width = 6,
       height = 5)

data3

data3 <-
  rbind(
    data.frame(group = "Control", value = data3$Control),
    data.frame(group = "Vector", value = data3$Vector),
    data.frame(group = "ASS1_OE", value = data3$`ASS1 OE`)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "ASS1_OE")))


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
  labs(x = "", y = "ug/g") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color2)

plot

ggsave(plot,
       filename = "il1b.pdf",
       width = 6,
       height = 5)
