library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/old_figures/Figure3",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/old_figures/Figure3")

###Figure 3D
data1 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig3D")

colnames(data1)[1] <- "Day"

data1 <-
  data1 %>%
  as.data.frame() %>%
  rename_with(~ gsub("\\.\\.\\.", "_", .)) %>%
  tidyr::pivot_longer(cols = -Day,
                      names_to = "sample_id",
                      values_to = "value") %>%
  dplyr::mutate(group = case_when(
    grepl("Control", sample_id) ~ "Control",
    grepl("Model", sample_id) ~ "DSS",
    grepl("MDLA", sample_id) ~ "MDLA",
    grepl("5-ASA", sample_id) ~ "5_ASA"
  )) %>%
  dplyr::mutate(Day = as.character(Day)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "MDLA", "5_ASA")))


# Calculate medians
median_data <-
  data1 %>%
  dplyr::group_by(Day, group) %>%
  dplyr::summarize(median_value = median(value), .groups = 'drop')

library(ggpubr)

plot <-
  data1 %>%
  ggplot(aes(x = Day, y = value, color = group)) +
  geom_point(
    aes(x = Day, y = value, color = group),
    position = position_dodge(0.35),
    size = 2,
    alpha = 0.7
  ) +
  geom_boxplot(
    aes(color = group),
    outlier.shape = NA,
    fill = "transparent",
    position = position_dodge(0.35),
    width = 0.3
  ) +
  # stat_compare_means(
  #   method = "t.test",
  #   comparisons = list(c("Control", "Vector"), c("Control", "shRNA"), c("Vector", "shRNA")),
  #   label = "p.signif",
  #   # label.y = max(data1$value),
  #   position = position_dodge(0.35)
  # ) +
  geom_line(
    data = median_data,
    aes(x = as.numeric(Day), y = median_value, group = group),
    position = position_dodge(0.35)
  ) +
  theme_base +
  scale_color_manual(values = disease_color4) +
  labs(x = "Day", y = "Body weight (%, compared to baseline)")

plot

ggsave(plot,
       filename = "figure3d.pdf",
       width = 7,
       height = 5)


library(ez)

ezANOVA(
  data = data1 %>% dplyr::filter(group %in% c("Control", "DSS")),
  dv = value,
  wid = sample_id,
  within = Day,
  between = group,
  detailed = TRUE
)

ezANOVA(
  data = data1 %>% dplyr::filter(group %in% c("Control", "MDLA")),
  dv = value,
  wid = sample_id,
  within = Day,
  between = group,
  detailed = TRUE
)

ezANOVA(
  data = data1 %>% dplyr::filter(group %in% c("DSS", "MDLA")),
  dv = value,
  wid = sample_id,
  within = Day,
  between = group,
  detailed = TRUE
)

ezANOVA(
  data = data1 %>% dplyr::filter(group %in% c("DSS", "5_ASA")),
  dv = value,
  wid = sample_id,
  within = Day,
  between = group,
  detailed = TRUE
)


###Figure 3E
data2 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig3E")

colnames(data2)[1] <- "Day"

data2 <-
  data2 %>%
  as.data.frame() %>%
  rename_with(~ gsub("\\.\\.\\.", "_", .)) %>%
  tidyr::pivot_longer(cols = -Day,
                      names_to = "sample_id",
                      values_to = "value") %>%
  dplyr::mutate(group = case_when(
    grepl("Control", sample_id) ~ "Control",
    grepl("Model", sample_id) ~ "DSS",
    grepl("MDLA", sample_id) ~ "MDLA",
    grepl("5-ASA", sample_id) ~ "5_ASA"
  )) %>%
  dplyr::mutate(Day = as.character(Day)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "MDLA", "5_ASA")))


# Calculate medians
median_data <-
  data2 %>%
  dplyr::group_by(Day, group) %>%
  dplyr::summarize(median_value = median(value), .groups = 'drop')

library(ggpubr)

plot <-
  data2 %>%
  ggplot(aes(x = Day, y = value, color = group)) +
  geom_point(
    aes(x = Day, y = value, color = group),
    position = position_dodge(0.35),
    size = 2,
    alpha = 0.7
  ) +
  geom_boxplot(
    aes(color = group),
    outlier.shape = NA,
    fill = "transparent",
    position = position_dodge(0.35),
    width = 0.3
  ) +
  geom_line(
    data = median_data,
    aes(x = as.numeric(Day), y = median_value, group = group),
    position = position_dodge(0.35)
  ) +
  theme_base +
  scale_color_manual(values = disease_color4) +
  labs(x = "Day", y = "Disease activity index (DAI)")
plot
ggsave(plot,
       filename = "figure3e.pdf",
       width = 7,
       height = 5)


library(ez)

ezANOVA(
  data = data2 %>% dplyr::filter(group %in% c("Control", "DSS")),
  dv = value,
  wid = sample_id,
  within = Day,
  between = group,
  detailed = TRUE
)

ezANOVA(
  data = data2 %>% dplyr::filter(group %in% c("DSS", "MDLA")),
  dv = value,
  wid = sample_id,
  within = Day,
  between = group,
  detailed = TRUE
)

ezANOVA(
  data = data2 %>% dplyr::filter(group %in% c("DSS", "5_ASA")),
  dv = value,
  wid = sample_id,
  within = Day,
  between = group,
  detailed = TRUE
)


##figure 3F
data3 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig3F")
data3

data3 <-
  rbind(
    data.frame(group = "Control", value = data3$Control),
    data.frame(group = "Vector", value = data3$`Control shRNA`),
    data.frame(group = "shRNA", value = data3$`ASS1 shRNA`)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "shRNA")))


plot <-
  ggbetweenstats(
    data  = data3,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "Age (years)") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure3f.pdf",
       width = 5,
       height = 5)

###figure3 H
data1 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig3H")

data1 <-
  rbind(
    data.frame(group = "Control", value = data1$Control),
    data.frame(group = "DSS", value = data1$Model),
    data.frame(group = "MDLA", value = data1$MDLA),
    data.frame(group = "5_ASA", value = data1$`5-ASA`)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "MDLA", "5_ASA")))

plot <-
  ggbetweenstats(
    data  = data1,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "H-score") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure3h.pdf",
       width = 5,
       height = 5)

####figure 3I
##MPO
data1 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig3I")
data1

data1 <-
  rbind(
    data.frame(group = "Control", value = data1$Control),
    data.frame(group = "DSS", value = data1$Model),
    data.frame(group = "MDLA", value = data1$MDLA)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "MDLA")))


plot <-
  ggbetweenstats(
    data  = data1,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "ug/g") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure3i.pdf",
       width = 5,
       height = 5)

####figure 3J
##IL6
data2 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig3J")

data2

data2 <-
  rbind(
    data.frame(group = "Control", value = data2$Control),
    data.frame(group = "DSS", value = data2$Model),
    data.frame(group = "MDLA", value = data2$MDLA)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "MDLA")))

plot <-
  ggbetweenstats(
    data  = data2,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "ug/g") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure3j.pdf",
       width = 5,
       height = 5)

###figure 3K
data3 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig3K")
data3

data3 <-
  rbind(
    data.frame(group = "Control", value = data3$Control),
    data.frame(group = "DSS", value = data3$Model),
    data.frame(group = "MDLA", value = data3$MDLA)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "MDLA")))

plot <-
  ggbetweenstats(
    data  = data3,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "ug/g") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure3k.pdf",
       width = 5,
       height = 5)


##Figure 3L
##ass1
ass1 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.900087996, 1.211398515, 1.260430791)
    ),
    data.frame(
      group = "DSS",
      value = c(1.101689475, 1.43116659, 1.459360862)
    ),
    data.frame(
      group = "MDLA",
      value = c(0.998222529, 1.367850446, 1.133646961)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "MDLA")))

##cox2
cox2 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(1.527164504, 0.454045375, 1.018790122)
    ),
    data.frame(
      group = "DSS",
      value = c(10.12292312, 9.474752317, 8.6264342)
    ),
    data.frame(
      group = "MDLA",
      value = c(0.750734679, 1.871352871, 2.322172585)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "MDLA")))

###p_stat3/stat3
p_stat3_stat3 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.856897892, 0.457195455, 1.685906653)
    ),
    data.frame(
      group = "DSS",
      value = c(2.449707728, 2.716964685, 2.650601635)
    ),
    data.frame(
      group = "MDLA",
      value = c(1.65900696, 1.835296071, 1.502519025)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "MDLA")))

##p_mtor/mtro
p_mtor_mtor <-
  rbind(
    data.frame(
      group = "Control",
      value = c(1.014039768, 0.720476131, 1.265484101)
    ),
    data.frame(
      group = "DSS",
      value = c(2.088533137, 2.313158423, 2.923047638)
    ),
    data.frame(
      group = "MDLA",
      value = c(0.903140697, 0.637429175, 1.319202251)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "MDLA")))

###p_s6/s6
p_s6_s6 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(1.097327162, 0.789284781, 1.113388057)
    ),
    data.frame(
      group = "DSS",
      value = c(4.864618847, 8.09624538, 11.36424816)
    ),
    data.frame(
      group = "MDLA",
      value = c(1.29125288, 2.621969703, 2.769383016)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "MDLA")))

###ass1
plot <-
  ggbetweenstats(
    data  = ass1,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "ASS1") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "figure3l_1.pdf",
       width = 5,
       height = 5)

##cox2
plot <-
  ggbetweenstats(
    data  = cox2,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "COX2") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "figure3l_2.pdf",
       width = 5,
       height = 5)

##p_stat3/stat3
plot <-
  ggbetweenstats(
    data  = p_stat3_stat3,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "p_STAT3/STAT3") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "figure3l_3.pdf",
       width = 5,
       height = 5)


##p_mtor/mtor
plot <-
  ggbetweenstats(
    data  = p_mtor_mtro,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "p_mTOR/mTOR") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "figure3l_4.pdf",
       width = 5,
       height = 5)

###p_s6/s6
plot <-
  ggbetweenstats(
    data  = p_s6_s6,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "p_S6/S6") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "figure3l_5.pdf",
       width = 5,
       height = 5)


##Figure 3M
data1 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig3M")

data1

data1 <-
  rbind(
    data.frame(group = "Control", value = data1$Control),
    data.frame(group = "DSS", value = data1$Model),
    data.frame(group = "MDLA", value = data1$MDLA)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "MDLA")))


plot <-
  ggbetweenstats(
    data  = data1,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "Fold change (compared to controls)") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure3m.pdf",
       width = 5,
       height = 5)


###figure 3N
data2 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig3N")
data2

data2 <-
  rbind(
    data.frame(group = "Control", value = data2$Control),
    data.frame(group = "DSS", value = data2$Model),
    data.frame(group = "MDLA", value = data2$MDLA)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "MDLA")))

plot <-
  ggbetweenstats(
    data  = data2,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "Fold change (compared to controls)") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure3n.pdf",
       width = 5,
       height = 5)
