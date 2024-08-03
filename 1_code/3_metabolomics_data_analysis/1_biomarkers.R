library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

###peaks
load("3_data_analysis/1_metabolomics_data/peaks/metabolomics_object.RData")

metabolomics_object@expression_data$Ctr_11

dir.create("3_data_analysis/3_metabolomics_data_analysis/1_biomarker")
setwd("3_data_analysis/3_metabolomics_data_analysis/1_biomarker")

library(tidymass)

###volcano plot

library(tidymass)

metabolomics_object %>%
  activate_mass_dataset(what = "variable_info") %>%
  dplyr::filter(p_value < 0.05 &
                  abs(log(fc, 2)) > log(1.5, 2) &
                  VIP > 1)

metabolomics_object %>%
  volcano_plot(
    fc_column_name = "fc",
    log2_fc = TRUE,
    point_size_scale = "VIP",
    p_value_column_name = "p_value",
    labs_x = "log2(Fold change)",
    labs_y = "-log(FDR, 10)",
    fc_up_cutoff = 2,
    fc_down_cutoff = 0.5,
    p_value_cutoff = 0.05,
    add_text = TRUE,
    text_from = "Compound.name"
  ) +
  scale_color_manual(values = up_down_color) +
  scale_size_continuous(range = c( 1, 8)) +
  theme_base
