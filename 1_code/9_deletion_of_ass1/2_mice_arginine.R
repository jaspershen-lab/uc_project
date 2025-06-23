library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

####shRNAinine in colon tissue
data1 <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 47)

###shRNAinine in blood samples
data2 <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 48)

dir.create(
  "3_data_analysis/9_deletion_of_ass1/2_mice_argine",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/9_deletion_of_ass1/2_mice_argine")

data1

data1 <-
  rbind(
    data.frame(group = "Control", value = data1$Control),
    data.frame(group = "Vector", value = data1$`Control shRNA`),
    data.frame(group = "shRNA", value = data1$`ASS1 shRNA`)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "shRNA")))


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
  labs(x = "", y = "Fold change (compared to controls)") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "colon_arginine.pdf",
       width = 5,
       height = 5)

data2

data2 <-
  rbind(
    data.frame(group = "Control", value = data2$Control),
    data.frame(group = "Vector", value = data2$`Control shRNA`),
    data.frame(group = "shRNA", value = data2$`ASS1 shRNA`)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "shRNA")))


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
  labs(x = "", y = "Fold change (compared to controls)") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "blood_arginine.pdf",
       width = 5,
       height = 5)
