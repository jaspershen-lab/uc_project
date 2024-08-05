library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

load("3_data_analysis/1_data_preparation/2_proteomics_data/proteomics_object.RData")

dir.create("3_data_analysis/2_data_overview/3_proteomics_data")
setwd("3_data_analysis/2_data_overview/3_proteomics_data")

library(tidymass)

###all the peaks
pca_object <-
  proteomics_object %>%
  `+`(1) %>%
  log(10) %>%
  scale_data() %>%
  run_pca()

plot <-
  massstat::pca_score_plot(
    object = proteomics_object,
    pca_object = pca_object,
    color_by = "group",
    point_alpha = 0.7
  ) +
  scale_fill_manual(values = group_color) +
  scale_color_manual(values = group_color) +
  theme_base

plot

ggsave(plot,
       filename = "pca_score_plot.pdf",
       width = 6.5,
       height = 5)

