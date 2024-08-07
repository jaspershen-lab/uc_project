library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/12_epigenetic/1_cell_data",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/12_epigenetic/1_cell_data")

data <-
  data.frame(
    group = c(rep("0", 3), rep("12", 3), rep("24", 3), rep("48", 3)),
    value = c(
      0.897462978,
      0.982103094,
      1.134557154,
      2.607522585,
      1.89902566,
      2.329144563,
      2.252871076,
      2.865153215,
      4.093237609,
      2.694504772,
      2.581733203,
      3.435013528
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("0", "12", "24", "48")))


# Calculate medians
median_data <-
  data %>%
  dplyr::group_by(group) %>%
  dplyr::summarize(median_value = median(value), .groups = 'drop')


plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    centrality.type = "nonparametric",
    point.args = list(
      alpha = 0.9,
      size = 3,
      color = "black",
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4)
  ) +
  geom_line(data = median_data,
            aes(x = as.numeric(group), y = median_value),
            position = position_dodge(0.35)) +
  theme_base +
  labs(x = "Time (hours)", y = "Relative ASS1 mRNA level") +
  theme(legend.position = "")

plot

ggsave(plot,
       filename = "figure8a.pdf",
       width = 6,
       height = 5)


####figure 8b
data1 <-
  data.frame(
    group = c(rep("Control", 3), rep("IL-6", 3)),
    value = c(
      0.175447504,
      0.259025795,
      0.167224769,
      0.83519312,
      0.60433561,
      0.581543272
    ),
    class = "Promoter-AcH3"
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "IL-6")))

data2 <-
  data.frame(
    group = c(rep("Control", 3), rep("IL-6", 3)),
    value = c(
      0.217560139,
      0.160394373,
      0.055438869,
      0.163425975,
      0.102518817,
      0.168638125
    ),
    class = "Exon1-AcH3"
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "IL-6")))


data <-
  rbind(data1, data2) %>%
  dplyr::mutate(class = factor(class, levels = c("Promoter-AcH3", "Exon1-AcH3")))


plot <-
  data %>%
  ggplot(aes(x = group, y = value)) +
  geom_boxplot(aes(group = group), outlier.shape = NA) +
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = 0.5) +
  facet_grid(cols = vars(class), scales = "free_y") +
  theme_base +
  labs(x = "", y = "Relative enrichment (% input)") +
  geom_signif(
    comparisons = list(c("Control", "IL-6")),
    map_signif_level = TRUE,
    textsize = 3,
    test = "t.test"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot

ggsave(plot,
       filename = "figure8b.pdf",
       width = 5,
       height = 5)


###Figure 8c
data1 <-
  data.frame(
    group = c(rep("Control", 3), rep("IL-6", 3)),
    value = c(
      0.167320238,
      0.109933136,
      0.21496421,
      0.096016614,
      0.133482433,
      0.175749141
    ),
    class = "Promoter-H3K27Me3"
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "IL-6")))

data2 <-
  data.frame(
    group = c(rep("Control", 3), rep("IL-6", 3)),
    value = c(
      0.071311546,
      0.041483663,
      0.050131888,
      0.073712097,
      0.014339611,
      0.079534149
    ),
    class = "Exon1-H3K27Me3"
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "IL-6")))

data <-
  rbind(data1, data2) %>%
  dplyr::mutate(class = factor(class, levels = c("Promoter-H3K27Me3", "Exon1-H3K27Me3")))

plot <-
  data %>%
  ggplot(aes(x = group, y = value)) +
  geom_boxplot(aes(group = group), outlier.shape = NA) +
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = 0.5) +
  facet_grid(cols = vars(class), scales = "free_y") +
  theme_base +
  labs(x = "", y = "Relative enrichment (% input)") +
  geom_signif(
    comparisons = list(c("Control", "IL-6")),
    map_signif_level = TRUE,
    textsize = 3,
    test = "t.test"
  )

plot

ggsave(plot,
       filename = "figure8c.pdf",
       width = 5,
       height = 5)

###H3K4Me3 Figure 8d
data1 <-
  data.frame(
    group = c(rep("Control", 3), rep("IL-6", 3)),
    value = c(
      0.233593023,
      0.329008845,
      0.234458474,
      0.9741304,
      0.756954468,
      0.669553272
    ),
    class = "Promoter-H3K4Me3"
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "IL-6")))

data2 <-
  data.frame(
    group = c(rep("Control", 3), rep("IL-6", 3)),
    value = c(
      0.178469436,
      0.127347462,
      0.087809013,
      0.147869514,
      0.180611064,
      0.10535037
    ),
    class = "Exon1-H3K4Me3"
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "IL-6")))


data <-
  rbind(data1, data2) %>%
  dplyr::mutate(class = factor(class, levels = c("Promoter-H3K4Me3", "Exon1-H3K4Me3")))

plot <-
  data %>%
  ggplot(aes(x = group, y = value)) +
  geom_boxplot(aes(group = group), outlier.shape = NA) +
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = 0.5) +
  facet_grid(cols = vars(class), scales = "free_y") +
  theme_base +
  labs(x = "", y = "Relative enrichment (% input)") +
  geom_signif(
    comparisons = list(c("Control", "IL-6")),
    map_signif_level = TRUE,
    textsize = 3,
    test = "t.test"
  )

plot

ggsave(plot,
       filename = "figure8d.pdf",
       width = 5,
       height = 5)

##H3K9Me2 Figure 8e
data1 <-
  data.frame(
    group = c(rep("Control", 3), rep("IL-6", 3)),
    value = c(
      0.134622751,
      0.164267967,
      0.094499494,
      0.066049115,
      0.076722804,
      0.042275803
    ),
    class = "Promoter-H3K9Me2"
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "IL-6")))

data2 <-
  data.frame(
    group = c(rep("Control", 3), rep("IL-6", 3)),
    value = c(
      0.100393676,
      0.209876351,
      0.123254144,
      0.135980258,
      0.128160587,
      0.057825048
    ),
    class = "Exon1-H3K9Me2"
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "IL-6")))

data <-
  rbind(data1, data2) %>%
  dplyr::mutate(class = factor(class, levels = c("Promoter-H3K9Me2", "Exon1-H3K9Me2")))

plot <-
  data %>%
  ggplot(aes(x = group, y = value)) +
  geom_boxplot(aes(group = group), outlier.shape = NA) +
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = 0.5) +
  facet_grid(cols = vars(class), scales = "free_y") +
  theme_base +
  labs(x = "", y = "Relative enrichment (% input)") +
  geom_signif(
    comparisons = list(c("Control", "IL-6")),
    map_signif_level = TRUE,
    textsize = 3,
    test = "t.test"
  )

plot

ggsave(plot,
       filename = "figure8e.pdf",
       width = 5,
       height = 5)

