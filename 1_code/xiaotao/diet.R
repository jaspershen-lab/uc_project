library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

data <-
  readxl::read_xlsx("2_data/diet K方检验.xlsx")

dir.create("3_data_analysis/new_figures/Figure_2", recursive = TRUE)
