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
###positive
pca_object <-
  metabolomics_object %>%
  activate_mass_dataset(what = "variable_info") %>%
  dplyr::filter(polarity == "Positive") %>%
  `+`(1) %>%
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
       filename = "pca_score_plot_peaks_positive.pdf",
       width = 6.5,
       height = 5)

##negative
pca_object <-
  metabolomics_object %>%
  activate_mass_dataset(what = "variable_info") %>%
  dplyr::filter(polarity == "Negative") %>%
  `+`(1) %>%
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
       filename = "pca_score_plot_peaks_negative.pdf",
       width = 6.5,
       height = 5)

####all features

pca_object <-
  metabolomics_object %>%
  `+`(1) %>%
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


###PCA using the biomarkers
object <-
  metabolomics_object %>%
  activate_mass_dataset(what = "variable_info") %>%
  dplyr::filter(p_value < 0.05 &
                  VIP > 1) %>%
  activate_mass_dataset(what = "sample_info") %>%
  dplyr::filter(class == "Subject")

pca_object <-
  object %>%
  `+`(1) %>%
  log(10) %>%
  scale_data() %>%
  run_pca()

plot <-
  massstat::pca_score_plot(
    object = object,
    pca_object = pca_object,
    color_by = "group",
    point_alpha = 0.7
  ) +
  scale_fill_manual(values = group_color) +
  scale_color_manual(values = group_color) +
  theme_base

plot

ggsave(plot,
       filename = "pca_score_plot_biomarkers.pdf",
       width = 6.5,
       height = 5)


###PCA using the identified biomarkers
object <-
  metabolomics_object %>%
  activate_mass_dataset(what = "variable_info") %>%
  dplyr::filter(p_value < 0.05 &
                  VIP > 1) %>%
  dplyr::filter(!is.na(Compound.name)) %>%
  activate_mass_dataset(what = "sample_info") %>%
  dplyr::filter(class == "Subject")

pca_object <-
  object %>%
  `+`(1) %>%
  log(10) %>%
  scale_data() %>%
  run_pca()

plot <-
  massstat::pca_score_plot(
    object = object,
    pca_object = pca_object,
    color_by = "group",
    point_alpha = 0.7
  ) +
  scale_fill_manual(values = group_color) +
  scale_color_manual(values = group_color) +
  theme_base

plot

ggsave(plot,
       filename = "pca_score_plot_identified_biomarkers.pdf",
       width = 6.5,
       height = 5)


####RSD for QC
rsd <-
  metabolomics_object %>%
  massdataset::activate_mass_dataset(what = "sample_info") %>%
  dplyr::filter(class == "QC") %>%
  `+`(1) %>%
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

plot

ggsave(plot,
       filename = "rsd_peaks.pdf",
       width = 8,
       height = 5)

plot <-
  metabolomics_object %>%
  massdataset::activate_mass_dataset(what = "sample_info") %>%
  dplyr::filter(class == "QC") %>%
  activate_mass_dataset(what = "variable_info") %>%
  dplyr::filter(polarity == "Positive") %>%
  `+`(1) %>%
  log(10) %>%
  massqc::massqc_cumulative_rsd_plot(color = polarity_color["Positive"])

plot

ggsave(plot,
       filename = "cumulative_rsd_positive.pdf",
       width = 3,
       height = 5)

plot <-
  metabolomics_object %>%
  massdataset::activate_mass_dataset(what = "sample_info") %>%
  dplyr::filter(class == "QC") %>%
  activate_mass_dataset(what = "variable_info") %>%
  dplyr::filter(polarity == "Negative") %>%
  `+`(1) %>%
  log(10) %>%
  massqc::massqc_cumulative_rsd_plot(color = polarity_color["Negative"])

plot

ggsave(plot,
       filename = "cumulative_rsd_negative.pdf",
       width = 3,
       height = 5)


dim(metabolomics_object)


temp_data <-
metabolomics_object %>%
  activate_mass_dataset(what = "variable_info") %>%
  dplyr::filter(p_value < 0.05 &
                  VIP > 1) %>%
  extract_variable_info()

library(ggpie)

plot <-
temp_data %>%
  dplyr::filter(polarity == "Positive") %>%
  dplyr::mutate(annotation = case_when(
    is.na(Compound.name) ~ "Unknown",
    TRUE ~ "Identified"
  )) %>%
  ggpie(group_key = "annotation", count_type = "full") +
  scale_fill_manual(values = c("Unknown" = "#868686FF",
                               "Identified" = unname(polarity_color["Positive"])))


ggsave(plot, filename = "annotation_positive.pdf", width = 5, height = 5)


plot <-
  temp_data %>%
  dplyr::filter(polarity == "Negative") %>%
  dplyr::mutate(annotation = case_when(
    is.na(Compound.name) ~ "Unknown",
    TRUE ~ "Identified"
  )) %>%
  ggpie(group_key = "annotation", count_type = "full") +
  scale_fill_manual(values = c("Unknown" = "#868686FF",
                               "Identified" = unname(polarity_color["Negative"])))


ggsave(plot, filename = "annotation_negative.pdf", width = 5, height = 5)


marker_pos <-
temp_data %>%
  dplyr::filter(polarity == "Positive") %>%
  dplyr::filter(!is.na(Compound.name)) %>%
  pull(Compound.name)

marker_neg <-
  temp_data %>%
  dplyr::filter(polarity == "Negative") %>%
  dplyr::filter(!is.na(Compound.name)) %>%
  pull(Compound.name)


library(VennDiagram)

venn.plot <- venn.diagram(
  x = list(Positive = marker_pos, Negative = marker_neg),
  category.names = c("Positive", "Negative"),
  filename = NULL,  # Use NULL to avoid saving to a file
  output = TRUE,
  col = "black",
  fill = polarity_color,
  alpha = 0.5,
  cex = 1.5,
  cat.cex = 1.5,
  cat.col = polarity_color,
  lwd = 2
)

grid::grid.draw(venn.plot)

