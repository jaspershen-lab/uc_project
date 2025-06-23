library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

###peaks
load("3_data_analysis/1_metabolomics_data/metabolites/metabolomics_object.RData")

metabolomics_object@expression_data$Ctr_11

dir.create("3_data_analysis/3_metabolomics_data_analysis/2_pathway_enrichment")
setwd("3_data_analysis/3_metabolomics_data_analysis/2_pathway_enrichment")

library(tidymass)

marker <-
  metabolomics_object %>%
  activate_mass_dataset(what = "variable_info") %>%
  dplyr::filter(p_value < 0.05 &
                  VIP > 1) %>%
  dplyr::filter(!is.na(Compound.name)) %>%
  extract_variable_info()

write.csv(marker, "marker.csv")

# data("kegg_hsa_pathway", package = "metpath")
# kegg_hsa_pathway
#
# get_pathway_class(kegg_hsa_pathway)
#
# pathway_class =
#   metpath::pathway_class(kegg_hsa_pathway)
# head(pathway_class)
#
# remain_idx =
#   pathway_class %>%
#   unlist() %>%
#   stringr::str_detect("Disease") %>%
#   `!`() %>%
#   which()
#
# remain_idx
#
# pathway_database =
#   kegg_hsa_pathway[remain_idx]
# pathway_database
#
# kegg_id <-
#   marker$KEGG
# kegg_id <-
#   unique(kegg_id[!is.na(kegg_id)])
# kegg_id
#
# result <-
#   enrich_kegg(
#     query_id = kegg_id,
#     query_type = "compound",
#     id_type = "KEGG",
#     pathway_database = pathway_database,
#     p_cutoff = 0.05,
#     p_adjust_method = "BH",
#     threads = 3
#   )
#
# dir.create("KEGG")
#
# save(result, file = "KEGG/result.RData")

load("KEGG/result.RData")
kegg_result <- result


###HMDB
data("hmdb_pathway", package = "metpath")
hmdb_pathway
#get the class of pathways
pathway_class =
  metpath::pathway_class(hmdb_pathway)

remain_idx = which(unlist(pathway_class) == "Metabolic;primary_pathway")

remain_idx

hmdb_pathway =
  hmdb_pathway[remain_idx]

hmdb_pathway

hmdb_id <-
  marker$HMDB
hmdb_id <-
  unique(hmdb_id[!is.na(hmdb_id)])
hmdb_id


# result =
#   enrich_hmdb(query_id = hmdb_id,
#               query_type = "compound",
#               id_type = "HMDB",
#               pathway_database = hmdb_pathway,
#               only_primary_pathway = TRUE,
#               p_cutoff = 0.05,
#               p_adjust_method = "BH",
#               threads = 3)
#
# dir.create("HMDB")
#
# save(result, file = "HMDB/result.RData")

load("HMDB/result.RData")
hmdb_result <- result

enrich_bar_plot(object = hmdb_result,
                x_axis = "p_value_adjust",
                cutoff = 0.05)


enrich_bar_plot(object = kegg_result,
                x_axis = "p_value_adjust",
                cutoff = 0.05)

plot <-
  enrich_scatter_plot(object = kegg_result) +
  theme_base +
  scale_size_continuous(range = c(1, 15))
plot
# ggsave(plot,
#        filename = "KEGG/kegg_enrich_scatter_plot.pdf",
#        width = 6.5,
#        height = 5)


metabolites <-
  kegg_result@result %>%
  dplyr::filter(p_value_adjust < 0.05 &
                  pathway_name == "mTOR signaling pathway") %>%
  pull(mapped_id) %>%
  stringr::str_split(";") %>%
  `[[`(1)


library(ggstatsplot)

####C00123 L-Leucine
temp_data <-
  metabolomics_object %>%
  activate_mass_dataset(what = "variable_info") %>%
  dplyr::filter(KEGG == "C00123") %>%
  dplyr::filter(variable_id == "M132T488_2_POS") %>%
  activate_mass_dataset(what = "sample_info") %>%
  dplyr::filter(class == "Subject")

temp_data <-
  temp_data %>%
  `+`(1) %>%
  log(10) %>%
  pivot_longer() %>%
  dplyr::left_join(temp_data@sample_info, by = "sample_id")

plot <-
  ggbetweenstats(
    data  = temp_data,
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
  labs(x = "", y = "Log(Intensity, 10)") +
  theme(legend.position = "") +
  scale_color_manual(values = group_color)

plot

ggsave(plot,
       filename = "L-Leucine_boxplot.pdf",
       width = 5,
       height = 5)



####C00123 L-Arginine
temp_data <-
  metabolomics_object %>%
  activate_mass_dataset(what = "variable_info") %>%
  dplyr::filter(KEGG == "C00062") %>%
  activate_mass_dataset(what = "sample_info") %>%
  dplyr::filter(class == "Subject")

temp_data <-
  temp_data %>%
  `+`(1) %>%
  log(10) %>%
  pivot_longer() %>%
  dplyr::left_join(temp_data@sample_info, by = "sample_id")

plot <-
  ggbetweenstats(
    data  = temp_data,
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
  labs(x = "", y = "Log(Intensity, 10)") +
  theme(legend.position = "") +
  scale_color_manual(values = group_color)

plot

ggsave(plot,
       filename = "L-Arginine_boxplot.pdf",
       width = 5,
       height = 5)
