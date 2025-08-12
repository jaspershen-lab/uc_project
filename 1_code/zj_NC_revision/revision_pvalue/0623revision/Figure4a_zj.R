library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/old_figures/Figure2",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/old_figures/Figure2")

###Figure 2A
data1 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig2A")

data1 <-
  data1 %>%
  as.data.frame() %>%
  rename_with( ~ gsub("\\.\\.\\.", "_", .)) %>%
  tidyr::pivot_longer(cols = -Day,
                      names_to = "sample_id",
                      values_to = "value") %>%
  dplyr::mutate(group = case_when(
    grepl("Control", sample_id) ~ "Control",
    grepl("DSS", sample_id) ~ "DSS",
    grepl("Arg", sample_id) ~ "Arg",
  )) %>%
  dplyr::mutate(Day = as.character(Day)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))


# Calculate medians
median_data <-
  data1 %>%
  dplyr::group_by(Day, group) %>%
  dplyr::summarize(median_value = median(value), .groups = 'drop')

data1 %>%
  ggplot(aes(x = Day, y = value, color = group)) +
  geom_boxplot(
    aes(color = group),
    outlier.shape = NA,
    position = position_dodge(0.35),
    width = 0.3
  ) +
  theme_base +
  scale_color_manual(values = disease_color) +
  labs(x = "Day", y = "Body weight (%, compared to baseline)")


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
  #   comparisons = list(c("Control", "DSS"), c("Control", "Arg"), c("DSS", "Arg")),
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
  scale_color_manual(values = disease_color) +
  labs(x = "Day", y = "Body weight (%, compared to baseline)")
plot
ggsave(plot,
       filename = "figure2a.pdf",
       width = 7,
       height = 5)


# Perform repeated measures ANOVA
library(ez)
ezANOVA(
  data = data1 %>% dplyr::filter(group %in% c("Control", "DSS")),
  dv = value,
  wid = sample_id,
  within = Day,
  between = group,
  type = 3
) %>%
  print()

ezANOVA(
  data = data1 %>% dplyr::filter(group %in% c("Control", "Arg")),
  dv = value,
  wid = sample_id,
  within = Day,
  between = group,
  type = 3
) %>%
  print()

ezANOVA(
  data = data1 %>% dplyr::filter(group %in% c("DSS", "Arg")),
  dv = value,
  wid = sample_id,
  within = Day,
  between = group,
  type = 3
) %>%
  print()

# Display the results
print(anova_results)



###Figure 2B
data2 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig2B")

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
    grepl("DSS", sample_id) ~ "DSS",
    grepl("Arg", sample_id) ~ "Arg",
  )) %>%
  dplyr::mutate(Day = as.character(Day)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))

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
  scale_color_manual(values = disease_color) +
  labs(x = "Day", y = "Disease activity index (DAI)")
plot
ggsave(plot,
       filename = "figure2b.pdf",
       width = 7,
       height = 5)


library(ez)

ezANOVA(
  data = data2 %>% dplyr::filter(group %in% c("Control", "DSS")),
  dv = value,
  wid = sample_id,
  within = Day,
  between = group,
  type = 3
) %>%
  print()


ezANOVA(
  data = data2 %>% dplyr::filter(group %in% c("Control", "Arg")),
  dv = value,
  wid = sample_id,
  within = Day,
  between = group,
  type = 3
) %>%
  print()

ezANOVA(
  data = data2 %>% dplyr::filter(group %in% c("DSS", "Arg")),
  dv = value,
  wid = sample_id,
  within = Day,
  between = group,
  type = 3
) %>%
  print()

##Figure 2C
data3 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig2C")
data3 <-
  rbind(
    data.frame(group = "Control", value = data3$Control),
    data.frame(group = "DSS", value = data3$Model),
    data.frame(group = "Arg", value = data3$Arg)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))

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
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "figure2c.pdf",
       width = 5,
       height = 5)


t.test(data3$value[data3$group == "Control"], data3$value[data3$group == "DSS"])
t.test(data3$value[data3$group == "Control"], data3$value[data3$group == "Arg"])
t.test(data3$value[data3$group == "DSS"], data3$value[data3$group == "Arg"])

###Figure 2E
data1 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig2E")

data1

data1 <-
  rbind(
    data.frame(group = "Control", value = data1$Control),
    data.frame(group = "DSS", value = data1$Model),
    data.frame(group = "Arg", value = data1$Arg)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))

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
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "figure2e.pdf",
       width = 5,
       height = 5)


t.test(data1$value[data1$group == "Control"], data1$value[data1$group == "DSS"])
t.test(data1$value[data1$group == "Control"], data1$value[data1$group == "Arg"])
t.test(data1$value[data1$group == "DSS"], data1$value[data1$group == "Arg"])

###Figure 2F
##MPO
data1 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig2F")

data1

data1 <-
  rbind(
    data.frame(group = "Control", value = data1$Control),
    data.frame(group = "DSS", value = data1$Model),
    data.frame(group = "Arg", value = data1$Arg)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))

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
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "figure2f.pdf",
       width = 6,
       height = 5)


t.test(data1$value[data1$group == "Control"], data1$value[data1$group == "DSS"])
t.test(data1$value[data1$group == "Control"], data1$value[data1$group == "Arg"])
t.test(data1$value[data1$group == "DSS"], data1$value[data1$group == "Arg"])

###Figure 2G
##IL6
data2 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig2G")

data2

data2 <-
  rbind(
    data.frame(group = "Control", value = data2$Control),
    data.frame(group = "DSS", value = data2$Model),
    data.frame(group = "Arg", value = data2$Arg)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))


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
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "figure2g.pdf",
       width = 6,
       height = 5)


t.test(data2$value[data2$group == "Control"], data2$value[data2$group == "DSS"])
t.test(data2$value[data2$group == "Control"], data2$value[data2$group == "Arg"])
t.test(data2$value[data2$group == "DSS"], data2$value[data2$group == "Arg"])

###Figure 2H
data3 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig2H")

data3 <-
  rbind(
    data.frame(group = "Control", value = data3$Control),
    data.frame(group = "DSS", value = data3$Model),
    data.frame(group = "Arg", value = data3$Arg)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))


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
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "figure2h.pdf",
       width = 6,
       height = 5)

###Figure 2I
##ass1
ass1 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.472687, 0.462967, 0.65197)
    ),
    data.frame(
      group = "DSS",
      value = c(0.970502, 1.038478, 0.940908)
    ),
    data.frame(
      group = "Arg",
      value = c(1.422338, 1.7171, 1.122251)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))

##cox2
cox2 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.501775, 0.510625, 0.388891)
    ),
    data.frame(group = "DSS", value = c(0.696029, 0.68, 0.735193)),
    data.frame(
      group = "Arg",
      value = c(1.078246, 0.957625, 1.074904)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))

###p_stat3/stat3
p_stat3_stat3 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.50265, 0.300418, 0.165834)
    ),
    data.frame(
      group = "DSS",
      value = c(0.780873, 0.770544, 0.551948)
    ),
    data.frame(
      group = "Arg",
      value = c(0.94389, 1.029203, 1.035195)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))

##p_mtor/mtro
p_mtor_mtro <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.627902, 0.664195, 0.629197)
    ),
    data.frame(
      group = "DSS",
      value = c(0.94838, 0.948402, 0.894815)
    ),
    data.frame(
      group = "Arg",
      value = c(1.199815, 1.460173, 1.044177)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))

###p_s6/s6
p_s6_s6 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.106791, 0.254717, 0.264669)
    ),
    data.frame(
      group = "DSS",
      value = c(0.824032, 0.532145, 0.578058)
    ),
    data.frame(
      group = "Arg",
      value = c(1.533274, 1.265295, 1.794139)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))

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
       filename = "figure2i_1.pdf",
       width = 6,
       height = 5)

t.test(ass1$value[ass1$group == "Control"], ass1$value[ass1$group == "DSS"])
t.test(ass1$value[ass1$group == "Control"], ass1$value[ass1$group == "Arg"])
t.test(ass1$value[ass1$group == "DSS"], ass1$value[ass1$group == "Arg"])


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
       filename = "figure2i_2.pdf",
       width = 6,
       height = 5)

t.test(cox2$value[cox2$group == "Control"], cox2$value[cox2$group == "DSS"])
t.test(cox2$value[cox2$group == "Control"], cox2$value[cox2$group == "Arg"])
t.test(cox2$value[cox2$group == "DSS"], cox2$value[cox2$group == "Arg"])

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
       filename = "figure2i_3.pdf",
       width = 6,
       height = 5)

t.test(p_stat3_stat3$value[p_stat3_stat3$group == "Control"], p_stat3_stat3$value[p_stat3_stat3$group == "DSS"])
t.test(p_stat3_stat3$value[p_stat3_stat3$group == "Control"], p_stat3_stat3$value[p_stat3_stat3$group == "Arg"])
t.test(p_stat3_stat3$value[p_stat3_stat3$group == "DSS"], p_stat3_stat3$value[p_stat3_stat3$group == "Arg"])


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
       filename = "figure2i_4.pdf",
       width = 6,
       height = 5)

t.test(p_mtor_mtro$value[p_mtor_mtro$group == "Control"], p_mtor_mtro$value[p_mtor_mtro$group == "DSS"])
t.test(p_mtor_mtro$value[p_mtor_mtro$group == "Control"], p_mtor_mtro$value[p_mtor_mtro$group == "Arg"])
t.test(p_mtor_mtro$value[p_mtor_mtro$group == "DSS"], p_mtor_mtro$value[p_mtor_mtro$group == "Arg"])

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
       filename = "figure2i_5.pdf",
       width = 6,
       height = 5)


t.test(p_s6_s6$value[p_s6_s6$group == "Control"], p_s6_s6$value[p_s6_s6$group == "DSS"])
t.test(p_s6_s6$value[p_s6_s6$group == "Control"], p_s6_s6$value[p_s6_s6$group == "Arg"])
t.test(p_s6_s6$value[p_s6_s6$group == "DSS"], p_s6_s6$value[p_s6_s6$group == "Arg"])

###figure 2j
data1 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig2J")

data1

data1 <-
  rbind(
    data.frame(group = "Control", value = data1$Control),
    data.frame(group = "DSS", value = data1$Model),
    data.frame(group = "Arg", value = data1$Arg)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))


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
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "figure2j.pdf",
       width = 5,
       height = 5)

t.test(data1$value[data1$group == "Control"], data1$value[data1$group == "DSS"])
t.test(data1$value[data1$group == "Control"], data1$value[data1$group == "Arg"])
t.test(data1$value[data1$group == "DSS"], data1$value[data1$group == "Arg"])


###Figure 2K
data2 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig2K")

data2

data2 <-
  rbind(
    data.frame(group = "Control", value = data2$Control),
    data.frame(group = "DSS", value = data2$Model),
    data.frame(group = "Arg", value = data2$Arg)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))

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
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "figure2k.pdf",
       width = 5,
       height = 5)


t.test(data2$value[data2$group == "Control"], data2$value[data2$group == "DSS"])
t.test(data2$value[data2$group == "Control"], data2$value[data2$group == "Arg"])
t.test(data2$value[data2$group == "DSS"], data2$value[data2$group == "Arg"])
