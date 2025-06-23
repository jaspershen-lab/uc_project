library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/old_figures/Figure5",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/old_figures/Figure5")

###figure 5A
data1 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig5A")

colnames(data1)[1] <- "Day"

data1 <-
  data1 %>%
  as.data.frame() %>%
  rename_with(~ gsub("\\.\\.\\.", "_", .)) %>%
  tidyr::pivot_longer(cols = -Day,
                      names_to = "sample_id",
                      values_to = "value") %>%
  dplyr::mutate(group = case_when(
    grepl("Control shRNA", sample_id) ~ "Vector",
    grepl("Control", sample_id) ~ "Control",
    grepl("ASS1 shRNA", sample_id) ~ "shRNA",
  )) %>%
  dplyr::mutate(Day = as.character(Day)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "shRNA")))


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
  #   comparisons = list(c("Control", "Vector"), c("Control", "ASS1_OE"), c("Vector", "ASS1_OE")),
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
  scale_color_manual(values = disease_color3) +
  labs(x = "Day", y = "Body weight (%, compared to baseline)")

plot

ggsave(plot,
       filename = "Figure5a.pdf",
       width = 7,
       height = 5)


ezANOVA(
  data = data1 %>% dplyr::filter(group %in% c("Control", "Vector")),
  dv = value,
  wid = sample_id,
  within = Day,
  between = group,
  type = 3
) %>%
  print()

ezANOVA(
  data = data1 %>% dplyr::filter(group %in% c("Control", "shRNA")),
  dv = value,
  wid = sample_id,
  within = Day,
  between = group,
  type = 3
) %>%
  print()

ezANOVA(
  data = data1 %>% dplyr::filter(group %in% c("Vector", "shRNA")),
  dv = value,
  wid = sample_id,
  within = Day,
  between = group,
  type = 3
) %>%
  print()


###figure 5B

data2 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig5B")
colnames(data2)[1] <- "Day"

data2 <-
  data2 %>%
  as.data.frame() %>%
  rename_with(~ gsub("\\.\\.\\.", "_", .)) %>%
  tidyr::pivot_longer(cols = -Day,
                      names_to = "sample_id",
                      values_to = "value") %>%
  dplyr::mutate(group = case_when(
    grepl("Control shRNA", sample_id) ~ "Vector",
    grepl("Control", sample_id) ~ "Control",
    grepl("ASS1 shRNA", sample_id) ~ "shRNA",
  )) %>%
  dplyr::mutate(Day = as.character(Day)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "shRNA")))


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
  scale_color_manual(values = disease_color3) +
  labs(x = "Day", y = "Disease activity index (DAI)")
plot
ggsave(plot,
       filename = "Figure5b.pdf",
       width = 7,
       height = 5)


library(ez)

ezANOVA(
  data = data2 %>% dplyr::filter(group %in% c("Control", "Vector")),
  dv = value,
  wid = sample_id,
  within = Day,
  between = group,
  type = 3
) %>%
  print()


ezANOVA(
  data = data2 %>% dplyr::filter(group %in% c("Control", "shRNA")),
  dv = value,
  wid = sample_id,
  within = Day,
  between = group,
  type = 3
) %>%
  print()

ezANOVA(
  data = data2 %>% dplyr::filter(group %in% c("Vector", "shRNA")),
  dv = value,
  wid = sample_id,
  within = Day,
  between = group,
  type = 3
) %>%
  print()

###figure 5c
data3 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig5C")
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
  labs(x = "", y = "Colon length (cm)") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "Figure5c.pdf",
       width = 5,
       height = 5)


t.test(data3$value[data3$group == "Control"],
       data3$value[data3$group == "Vector"],
       paired = FALSE,
       var.equal = FALSE) %>%
  print()

t.test(data3$value[data3$group == "Control"],
       data3$value[data3$group == "shRNA"],
       paired = FALSE,
       var.equal = FALSE) %>%
  print()

t.test(data3$value[data3$group == "Vector"],
       data3$value[data3$group == "shRNA"],
       paired = FALSE,
       var.equal = FALSE) %>%
  print()


###figure 5e
data1 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig5E")

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
  scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "Figure5e.pdf",
       width = 5,
       height = 5)

###figure 5f
##MPO
data1 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig5F")

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
  scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "Figure5f.pdf",
       width = 5,
       height = 5)



###figure 5g
data2 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig5G")
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
  scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "Figure5g.pdf",
       width = 5,
       height = 5)


###figure 5h
data3 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig5H")

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
  labs(x = "", y = "ug/g") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "Figure5h.pdf",
       width = 5,
       height = 5)

###figure 5I
##ass1
ass1 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.921223127, 0.702061438, 0.852736496)
    ),
    data.frame(
      group = "Vector",
      value = c(1.316292227, 1.059194091, 1.256017455)
    ),
    data.frame(
      group = "shRNA",
      value = c(0.854155902, 0.817451134, 0.660971603)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "shRNA")))

##cox2
cox2 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.384267, 0.173545, 0.159117)
    ),
    data.frame(
      group = "Vector",
      value = c(0.857023, 1.088228, 1.160407)
    ),
    data.frame(
      group = "shRNA",
      value = c(0.255624, 0.194426, 0.204373)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "shRNA")))



###p_stat3/stat3
p_stat3_stat3 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.062965053, 0.155410901, 0.157160994)
    ),
    data.frame(
      group = "Vector",
      value = c(0.94761884, 1.216038401, 0.96554786)
    ),
    data.frame(
      group = "shRNA",
      value = c(0.520599573, 0.597126805, 0.501258006)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "shRNA")))


##p_mtor/mtro
p_mtor_mtor <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.444373321, 0.293225989, 0.601721266)
    ),
    data.frame(
      group = "Vector",
      value = c(0.938967096, 0.790558757, 1.059329091)
    ),
    data.frame(
      group = "shRNA",
      value = c(0.640625455, 0.533132979, 0.688374373)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "shRNA")))

###p_s6/s6
p_s6_s6 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.30471287, 0.347673014, 0.151352041)
    ),
    data.frame(
      group = "Vector",
      value = c(0.999214379, 1.087956702, 1.167296043)
    ),
    data.frame(
      group = "shRNA",
      value = c(0.45535544, 0.590913849, 0.276623692)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "shRNA")))


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
  scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "Figure5i_1.pdf",
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
  scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "Figure5i_2.pdf",
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
  scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "Figure5i_3.pdf",
       width = 5,
       height = 5)


##p_mtor/mtor
plot <-
  ggbetweenstats(
    data  = p_mtor_mtor,
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
  scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "Figure5i_4.pdf",
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
  scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "Figure5i_5.pdf",
       width = 5,
       height = 5)


###figure 5J
data1 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig5J")

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
  scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "Figure5j.pdf",
       width = 5,
       height = 5)


##figure 5K
data2 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig5K")
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
  scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "Figure5k.pdf",
       width = 5,
       height = 5)
