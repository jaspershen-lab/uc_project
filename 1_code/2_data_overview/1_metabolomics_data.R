library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

###peaks
load("3_data_analysis/1_metabolomics_data/peaks/metabolomics_object.RData")

metabolomics_object@expression_data$Ctr_11

dir.create("3_data_analysis/2_data_overview/1_metabolomics_data")
setwd("3_data_analysis/2_data_overview/1_metabolomics_data")

library(tidymass)

###all the peaks
pca_object <-
  metabolomics_object %>%
  log(10) %>%
  scale_data() %>%
  run_pca()

plot <-
  massstat::pca_score_plot(
    object = metabolomics_object,
    pca_object = pca_object,
    color_by = "class",
    point_alpha = 0.7
  ) +
  scale_fill_manual(values = class_color) +
  scale_color_manual(values = class_color) +
  theme_base

plot

ggsave(plot,
       filename = "pca_score_plot_peaks.pdf",
       width = 6.5,
       height = 5)

####RSD for QC

rsd <-
  metabolomics_object %>%
  massdataset::activate_mass_dataset(what = "sample_info") %>%
  dplyr::filter(class == "QC") %>%
  log(10) %>%
  extract_expression_data() %>%
  apply(1, function(x) {
    sd(x, na.rm = TRUE) * 100 / mean(x, na.rm = TRUE)
  })

library(dplyr)

plot <-
  data.frame(metabolomics_object@variable_info, rsd) %>%
  dplyr::mutate(rsd = case_when(polarity == "Positive" ~ rsd, polarity == "Negative" ~ -rsd)) %>%
  ggplot(aes(rt, rsd)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_point(alpha = 0.5, aes(size = abs(rsd), color = polarity)) +
  scale_color_manual(values = polarity_color) +
  theme_base +
  labs(x = "Retention time (seconds)", y = "Relative standard deviation (RSD, %)")

ggsave(plot,
       filename = "rsd_peaks.pdf",
       width = 8,
       height = 5)
