library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

##MPO
data1 <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 14)

##IL6
data2 <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 15)

##IL1-b
data3 <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 16)

dir.create("3_data_analysis/7_disease_model_mice/4_cytokine", showWarnings = TRUE, recursive = TRUE)
setwd("3_data_analysis/7_disease_model_mice/4_cytokine")


data1

data1 <-
  rbind(
    data.frame(group = "Control", value = data1$Control),
    data.frame(group = "DSS", value = data1$Model),
    data.frame(group = "Arg", value = data1$Arg)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))


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
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "mpo.pdf",
       width = 6,
       height = 5)






data2

data2 <-
  rbind(
    data.frame(group = "Control", value = data2$Control),
    data.frame(group = "DSS", value = data2$Model),
    data.frame(group = "Arg", value = data2$Arg)
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
  labs(x = "", y = "ug/g") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "il6.pdf",
       width = 6,
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
  labs(x = "", y = "ug/g") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "il1b.pdf",
       width = 6,
       height = 5)
