library(r4projects)
library(ggpubr)
library(ggstatsplot)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/old_figures/Figure4",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/old_figures/Figure4")

###Figure 4A
data1 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig4A")

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
    grepl("Vector", sample_id) ~ "Vector",
    grepl("ASS1 OE", sample_id) ~ "ASS1_OE",
  )) %>%
  dplyr::mutate(Day = as.character(Day)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "ASS1_OE")))


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
  scale_color_manual(values = disease_color2) +
  labs(x = "Day", y = "Body weight (%, compared to baseline)")

plot

# Perform repeated measures ANOVA
library(ez)
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
  data = data1 %>% dplyr::filter(group %in% c("Control", "ASS1_OE")),
  dv = value,
  wid = sample_id,
  within = Day,
  between = group,
  type = 3
) %>%
  print()

ezANOVA(
  data = data1 %>% dplyr::filter(group %in% c("Vector", "ASS1_OE")),
  dv = value,
  wid = sample_id,
  within = Day,
  between = group,
  type = 3
) %>%
  print()

ggsave(plot,
       filename = "figure4a.pdf",
       width = 7,
       height = 5)


###figure 4B

data2 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig4B")
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
    grepl("Vector", sample_id) ~ "Vector",
    grepl("ASS1 OE", sample_id) ~ "ASS1_OE",
  )) %>%
  dplyr::mutate(Day = as.character(Day)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "ASS1_OE")))


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
  scale_color_manual(values = disease_color2) +
  labs(x = "Day", y = "Disease activity index (DAI)")
plot
ggsave(plot,
       filename = "figure4b.pdf",
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
    data = data2 %>% dplyr::filter(group %in% c("Control", "ASS1_OE")),
    dv = value,
    wid = sample_id,
    within = Day,
    between = group,
    type = 3
  ) %>%
  print()

  ezANOVA(
    data = data2 %>% dplyr::filter(group %in% c("Vector", "ASS1_OE")),
    dv = value,
    wid = sample_id,
    within = Day,
    between = group,
    type = 3
  ) %>%
  print()

###figure 4c
data3 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig4C")
data3

data3 <-
  rbind(
    data.frame(group = "Control", value = data3$Control),
    data.frame(group = "Vector", value = data3$Vector),
    data.frame(group = "ASS1_OE", value = data3$`ASS1 OE`)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "ASS1_OE")))

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
  scale_color_manual(values = disease_color2)

plot

ggsave(plot,
       filename = "figure4c.pdf",
       width = 5,
       height = 5)


t.test(data3$value[data3$group == "Control"],
       data3$value[data3$group == "Vector"],
       paired = FALSE)

t.test(data3$value[data3$group == "Control"],
       data3$value[data3$group == "ASS1_OE"],
       paired = FALSE)

t.test(data3$value[data3$group == "Vector"],
       data3$value[data3$group == "ASS1_OE"],
       paired = FALSE)


###figure 4e
data1 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig4E")

data1

data1 <-
  rbind(
    data.frame(group = "Control", value = data1$Control),
    data.frame(group = "Vector", value = data1$Vector),
    data.frame(group = "ASS1_OE", value = data1$`ASS1 OE`)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "ASS1_OE")))


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
  scale_color_manual(values = disease_color2)

plot

ggsave(plot,
       filename = "figure4e.pdf",
       width = 5,
       height = 5)

###figure 4f
##MPO
data1 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig4F")

data1

data1 <-
  rbind(
    data.frame(group = "Control", value = data1$Control),
    data.frame(group = "Vector", value = data1$Vector),
    data.frame(group = "ASS1_OE", value = data1$`ASS1 OE`)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "ASS1_OE")))


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
  scale_color_manual(values = disease_color2)

plot

ggsave(plot,
       filename = "figure4f.pdf",
       width = 5,
       height = 5)



###figure 4g
data2 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig4G")
data2

data2 <-
  rbind(
    data.frame(group = "Control", value = data2$Control),
    data.frame(group = "Vector", value = data2$Vector),
    data.frame(group = "ASS1_OE", value = data2$`ASS1 OE`)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "ASS1_OE")))


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
  scale_color_manual(values = disease_color2)

plot

ggsave(plot,
       filename = "figure4g.pdf",
       width = 5,
       height = 5)


###figure 4h
data3 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig4H")

data3

data3 <-
  rbind(
    data.frame(group = "Control", value = data3$Control),
    data.frame(group = "Vector", value = data3$Vector),
    data.frame(group = "ASS1_OE", value = data3$`ASS1 OE`)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "ASS1_OE")))


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
  scale_color_manual(values = disease_color2)

plot

ggsave(plot,
       filename = "figure4h.pdf",
       width = 5,
       height = 5)

###Figure 4I
##ass1
ass1 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.53453176, 0.397696811, 0.483292516)
    ),
    data.frame(
      group = "Vector",
      value = c(0.813297801, 0.828871742, 0.908489192)
    ),
    data.frame(
      group = "ASS1_OE",
      value = c(1.14420917, 0.970864244, 1.154266284)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "ASS1_OE")))

##cox2
cox2 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.19193968, 0.15960863, 0.208248959)
    ),
    data.frame(
      group = "Vector",
      value = c(0.324458685, 0.404361544, 0.400871761)
    ),
    data.frame(
      group = "ASS1_OE",
      value = c(1.186206518, 0.934861035, 1.011665032)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "ASS1_OE")))


###p_stat3/stat3
p_stat3_stat3 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.023475695, 0.244773394, 0.277638313)
    ),
    data.frame(
      group = "Vector",
      value = c(0.34083203, 0.69272339, 0.538751658)
    ),
    data.frame(
      group = "ASS1_OE",
      value = c(0.956917577, 0.887418202, 0.941327513)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "ASS1_OE")))


##p_mtor/mtro
p_mtor_mtor <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.695294588, 0.636828916, 0.556887131)
    ),
    data.frame(
      group = "Vector",
      value = c(0.936676416, 0.947879098, 0.850644879)
    ),
    data.frame(
      group = "ASS1_OE",
      value = c(1.424628311, 1.067333844, 1.203624668)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "ASS1_OE")))

###p_s6/s6
p_s6_s6 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.21747534, 0.235741638, 0.284831828)
    ),
    data.frame(
      group = "Vector",
      value = c(0.356215461, 0.387966277, 0.486224979)
    ),
    data.frame(
      group = "ASS1_OE",
      value = c(1.155752219, 1.041099128, 1.167995339)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "ASS1_OE")))


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
  scale_color_manual(values = disease_color2)

plot

ggsave(plot,
       filename = "figure4i_1.pdf",
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
  scale_color_manual(values = disease_color2)

plot

ggsave(plot,
       filename = "figure4i_2.pdf",
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
  scale_color_manual(values = disease_color2)

plot

ggsave(plot,
       filename = "figure4i_3.pdf",
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
  scale_color_manual(values = disease_color2)

plot

ggsave(plot,
       filename = "figure4i_4.pdf",
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
  scale_color_manual(values = disease_color2)

plot

ggsave(plot,
       filename = "figure4i_5.pdf",
       width = 5,
       height = 5)


###figure 4J
data1 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig4J")

data1

data1 <-
  rbind(
    data.frame(group = "Control", value = data1$Control),
    data.frame(group = "Vector", value = data1$Vector),
    data.frame(group = "ASS1_OE", value = data1$`ASS1 OE`)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "ASS1_OE")))


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
  scale_color_manual(values = disease_color2)

plot

ggsave(plot,
       filename = "figure4j.pdf",
       width = 5,
       height = 5)


##figure 4K
data2 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig4K")
data2

data2 <-
  rbind(
    data.frame(group = "Control", value = data2$Control),
    data.frame(group = "Vector", value = data2$Vector),
    data.frame(group = "ASS1_OE", value = data2$`ASS1 OE`)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "ASS1_OE")))


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
  scale_color_manual(values = disease_color2)

plot

ggsave(plot,
       filename = "figure4k.pdf",
       width = 5,
       height = 5)
