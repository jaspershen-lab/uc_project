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

marker <-
  metabolomics_object %>%
  activate_mass_dataset(what = "variable_info") %>%
  dplyr::filter(p_value < 0.05 &
                  VIP > 1) %>%
  extract_variable_info() %>%
  pull(Compound.name)

length(marker)

marker[!is.na(marker)]

plot <-
  metabolomics_object %>%
  volcano_plot2(
    fc_column_name = "fc",
    log2_fc = TRUE,
    point_size_scale = "VIP",
    p_value_column_name = "p_value",
    labs_x = "log2(Fold change)",
    labs_y = "-log(FDR, 10)",
    fc_up_cutoff = 1,
    fc_down_cutoff = 1,
    p_value_cutoff = 0.05,
    point_alpha = 0.5,
    add_text = FALSE,
    text_from = "Compound.name"
  ) +
  scale_color_manual(values = up_down_color) +
  scale_size_continuous(range = c(1, 8)) +
  theme_base

plot

ggsave(plot,
       filename = "volcano_plot.pdf",
       width = 6.5,
       height = 5)

metabolomics_object %>%
  activate_mass_dataset(what = "variable_info") %>%
  dplyr::filter(p_value < 0.05 &
                  VIP > 1) %>%
  extract_variable_info() %>%
  dplyr::arrange(fc) %>%
  dplyr::filter(!is.na(Compound.name))

####tsne
library(Rtsne)
####using the biomarkers
object <-
  metabolomics_object %>%
  activate_mass_dataset(what = "variable_info") %>%
  dplyr::filter(p_value < 0.05 &
                  VIP > 1) %>%
  activate_mass_dataset(what = "sample_info") %>%
  dplyr::filter(class == "Subject")

expression_data <-
  object %>%
  `+`(1) %>%
  log(10) %>%
  scale_data() %>%
  extract_expression_data()

sample_info <-
  object %>%
  extract_sample_info()

tsne_result <- Rtsne(
  t(expression_data),
  dims = 2,
  perplexity = 30,
  verbose = TRUE,
  max_iter = 500
)

tsne_data <- cbind(data.frame(tsne_result$Y), sample_info)
colnames(tsne_data)[1:2] <- c("tSNE1", "tSNE2")

plot <-
  ggplot(tsne_data, aes(x = tSNE1, y = tSNE2)) +
  geom_hline(yintercept = 0,
             color = "black",
             linetype = 2) +
  geom_vline(xintercept = 0,
             color = "black",
             linetype = 2) +
  geom_point(aes(fill = group), shape = 21, size = 5) +
  labs(x = "t-SNE 1", y = "t-SNE 2") +
  scale_fill_manual(values = group_color) +
  theme_base

plot

ggsave(plot,
       filename = "tsne_plot_biomarkers.pdf",
       width = 6.5,
       height = 5)





####tsne
library(Rtsne)
####using the identified biomarkers
object <-
  metabolomics_object %>%
  activate_mass_dataset(what = "variable_info") %>%
  dplyr::filter(p_value < 0.05 &
                  VIP > 1) %>%
  dplyr::filter(!is.na(Compound.name)) %>%
  activate_mass_dataset(what = "sample_info") %>%
  dplyr::filter(class == "Subject")

expression_data <-
  object %>%
  `+`(1) %>%
  log(10) %>%
  scale_data() %>%
  extract_expression_data()

dim(expression_data)

sample_info <-
  object %>%
  extract_sample_info()

tsne_result <- Rtsne(
  t(expression_data),
  dims = 2,
  perplexity = 30,
  verbose = TRUE,
  max_iter = 500
)

tsne_data <- cbind(data.frame(tsne_result$Y), sample_info)
colnames(tsne_data)[1:2] <- c("tSNE1", "tSNE2")

plot <-
  ggplot(tsne_data, aes(x = tSNE1, y = tSNE2)) +
  geom_hline(yintercept = 0,
             color = "black",
             linetype = 2) +
  geom_vline(xintercept = 0,
             color = "black",
             linetype = 2) +
  geom_point(aes(fill = group), shape = 21, size = 5) +
  labs(x = "t-SNE 1", y = "t-SNE 2") +
  scale_fill_manual(values = group_color) +
  theme_base

plot

ggsave(plot,
       filename = "tsne_plot_identified_biomarkers.pdf",
       width = 6.5,
       height = 5)

###Heatmap
object <-
  metabolomics_object %>%
  activate_mass_dataset(what = "variable_info") %>%
  dplyr::filter(p_value < 0.05 &
                  VIP > 1) %>%
  dplyr::filter(!is.na(Compound.name)) %>%
  activate_mass_dataset(what = "sample_info") %>%
  dplyr::filter(class == "Subject")

expression_data <-
  object %>%
  `+`(1) %>%
  log(10) %>%
  scale_data() %>%
  extract_expression_data()

###remove some duplicated metabolites
duplicated_metabolites <-
  unique(object@variable_info$Compound.name[duplicated(object@variable_info$Compound.name)])

###Hypoxanthine
which(object@variable_info$Compound.name == "Hypoxanthine")

temp_data <-
  expression_data[c(11, 36), ] %>%
  t() %>%
  as.data.frame()

cor.test(temp_data$M137T307_POS, temp_data$M135T304_NEG, method = "spearman")

plot <-
  temp_data %>%
  ggplot(aes(M137T307_POS, M135T304_NEG)) +
  geom_abline(color = "red") +
  geom_point(size = 5) +
  theme_base +
  annotate(
    "text",
    x = 0.5,
    y = 0.5,
    label = paste(
      "Spearman's rho =",
      round(
        cor.test(temp_data$M137T307_POS, temp_data$M135T304_NEG, method = "spearman")$estimate,
        2
      ),
      "\np =",
      round(
        cor.test(temp_data$M137T307_POS, temp_data$M135T304_NEG, method = "spearman")$p.value,
        10
      )
    ),
    hjust = 0,
    vjust = 1
  )

plot

ggsave(plot,
       filename = "hypoxanthine_correlation.pdf",
       width = 5,
       height = 5)

###should remain positive

###L-Glutamine
which(object@variable_info$Compound.name == "L-Glutamine")

temp_data <-
  expression_data[c(9, 48), ] %>%
  t() %>%
  as.data.frame()

cor.test(temp_data$M147T706_POS, temp_data$M145T695_NEG, method = "spearman")

plot <-
  temp_data %>%
  ggplot(aes(M147T706_POS, M145T695_NEG)) +
  geom_abline(color = "red") +
  geom_point(size = 5) +
  theme_base +
  annotate(
    "text",
    x = 0.5,
    y = 0.5,
    label = paste(
      "Spearman's rho =",
      round(
        cor.test(temp_data$M147T706_POS, temp_data$M145T695_NEG, method = "spearman")$estimate,
        2
      ),
      "\np =",
      round(
        cor.test(temp_data$M147T706_POS, temp_data$M145T695_NEG, method = "spearman")$p.value,
        10
      )
    ),
    hjust = 0,
    vjust = 1
  )

plot

ggsave(plot,
       filename = "L-Glutamine_correlation.pdf",
       width = 5,
       height = 5)


duplicated_metabolites

which(object@variable_info$Compound.name == "Hypoxanthine")

object@expression_data[c(11, 36), ] %>% rowMeans()


###remove M135T304_NEG

which(object@variable_info$Compound.name == "L-Phenylalanine")

object@expression_data[c(12, 51), ] %>% rowMeans()


##remove M164T470_2_NEG

which(object@variable_info$Compound.name == "Chenodeoxycholate")

object@expression_data[c(24, 52), ] %>% rowMeans()

##remove M410T284_POS


which(object@variable_info$Compound.name == "L-Glutamine")

object@expression_data[c(9, 48), ] %>% rowMeans()


##remove M147T706_POS


##remove 4 metabolites

metabolomics_object <-
  metabolomics_object %>%
  activate_mass_dataset(what = "variable_info") %>%
  dplyr::filter(
    !variable_id %in% c(
      "M135T304_NEG",
      "M164T470_2_NEG",
      "M410T284_POS",
      "M147T706_POS"
    )
  )

object <-
  metabolomics_object %>%
  activate_mass_dataset(what = "variable_info") %>%
  dplyr::filter(p_value < 0.05 &
                  VIP > 1) %>%
  dplyr::filter(!is.na(Compound.name)) %>%
  activate_mass_dataset(what = "sample_info") %>%
  dplyr::filter(class == "Subject")

expression_data <-
  object %>%
  `+`(1) %>%
  log(10) %>%
  scale_data() %>%
  extract_expression_data()

library(ComplexHeatmap)

which(is.na(expression_data), arr.ind = TRUE) %>%
  as.data.frame() %>%
  pull(row) %>%
  table()

range(expression_data)

library(circlize)
library(viridis)
col_fun <- circlize::colorRamp2(seq(-2, 2, length.out = 11), rev(RColorBrewer::brewer.pal(name = "RdYlBu", n = 11)))
na_col <- "grey"

range(expression_data, na.rm = TRUE)

expression_data[expression_data < -2] <- -2
expression_data[expression_data > 2] <- 2

expression_data <-
  as.data.frame(expression_data)

rownames(expression_data) <-
  object@variable_info$Compound.name

# Create column annotations
column_annotation <- HeatmapAnnotation(
  Group = object@sample_info$group,
  col = list(Group = group_color),
  show_annotation_name = TRUE,
  annotation_name_side = "left",
  annotation_label = "Group"
)

row_annotation <-
  row_annotation(
    up_down = anno_block(gp = gpar(fill = up_down_color)),
    fc = anno_block(gp = gpar(fill = up_down_color))
    )


fc_col_fun <-
  colorRamp2(c(-1.3, 0, 1.3), c("#3B4992FF", "white", "#EE0000FF"))

ha_right <-
  rowAnnotation(
    fc = anno_simple(log(object@variable_info$fc, 2),
                     col = fc_col_fun),
    fdr = anno_lines(
      x = -log(object@variable_info$p_value, 10),
      add_points = FALSE,
      # axis_param = list(at = c(1.3, 0, 215)),
      width = unit(20, "mm")
    ),
    vip = anno_lines(
      x = object@variable_info$VIP,
      add_points = FALSE,
      # axis_param = list(at = c(1.3, 0, 215)),
      width = unit(20, "mm")
    )
  )

plot <-
  Heatmap(
    as.matrix(expression_data),
    name = "Z-score",
    show_row_names = TRUE,
    show_column_names = FALSE,
    col = col_fun,
    na_col = na_col,
    cluster_columns = TRUE,
    cluster_rows = TRUE,
    column_names_gp = gpar(fontsize = 6),
    row_names_gp = gpar(fontsize = 6),
    column_names_rot = 45,
    clustering_distance_columns = "euclidean",
    clustering_method_columns = "complete",
    clustering_distance_rows = "euclidean",
    clustering_method_rows = "complete",
    column_split = object@sample_info$group,
    row_split = 2,
    right_annotation = ha_right,
    border = TRUE,
    top_annotation = column_annotation,
    heatmap_legend_param = list(direction = "horizontal")
  )

plot <- ggplotify::as.ggplot(plot)
plot

ggsave(plot,
       filename = "heatmap_markders.pdf",
       width = 12,
       height = 5)

