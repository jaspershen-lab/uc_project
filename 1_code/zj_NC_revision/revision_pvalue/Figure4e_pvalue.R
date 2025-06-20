# Figure 4e - p-mTOR/mTOR 完整分析（显示p值）

library(r4projects)
library(ggstatsplot)
library(dplyr)
library(ggplot2)

setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')


dir.create(
  "3_data_analysis/zj_NC_revision/revision_pvalue/Figure4_pvalue/Figure4e",
  showWarnings = FALSE,
  recursive = TRUE
)
setwd("3_data_analysis/zj_NC_revision/revision_pvalue/Figure4_pvalue/Figure4e")


##ASS1
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

##COX2
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

###p_STAT3/STAT3
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

##p_mTOR/mTOR - 修正后的数据
p_mtor_mtor <- data.frame(
  group = rep(c("Control", "DSS", "Arg"), each = 3),
  value = c(0.627902, 0.664195, 0.629197,
            0.94838, 0.948402, 0.894815,
            1.199815, 1.460173, 1.044177)
) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))

###p_S6/S6
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


# ASS1 - ggbetweenstats参数检验版本
ass1_plot_parametric <-
  ggbetweenstats(
    data = ass1,
    x = group,
    y = value,
    type = "parametric",
    pairwise.comparisons = TRUE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    results.subtitle = TRUE,
    centrality.plotting = FALSE
  ) +
  theme_base +
  labs(x = "", y = "Protein/GAPDH", title = "ASS1") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

ass1_plot_parametric
ggsave(ass1_plot_parametric,
       filename = "ass1_parametric.pdf",
       width = 6,
       height = 5)

# ASS1 - ggpubr箱线图版本
ass1_plot_ttest <- ggboxplot(ass1,
                             x = "group",
                             y = "value",
                             color = "group",
                             add = "jitter",
                             add.params = list(size = 3, alpha = 0.9)) +
  stat_compare_means(method = "anova",
                     label.y = max(ass1$value) * 1.15,
                     size = 4) +
  stat_compare_means(comparisons = list(c("Control", "DSS"),
                                        c("Control", "Arg"),
                                        c("DSS", "Arg")),
                     method = "t.test",
                     label = "p.format",
                     size = 4) +
  theme_base +
  labs(x = "", y = "Protein/GAPDH", title = "ASS1") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

ass1_plot_ttest
ggsave(ass1_plot_ttest,
       filename = "ass1_ttest.pdf",
       width = 6,
       height = 6)

# COX2 - ggbetweenstats参数检验版本
cox2_plot_parametric <-
  ggbetweenstats(
    data = cox2,
    x = group,
    y = value,
    type = "parametric",
    pairwise.comparisons = TRUE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    results.subtitle = TRUE,
    centrality.plotting = FALSE
  ) +
  theme_base +
  labs(x = "", y = "Protein/GAPDH", title = "COX2") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

cox2_plot_parametric
ggsave(cox2_plot_parametric,
       filename = "cox2_parametric.pdf",
       width = 6,
       height = 5)

# COX2 - ggpubr箱线图版本
cox2_plot_ttest <- ggboxplot(cox2,
                             x = "group",
                             y = "value",
                             color = "group",
                             add = "jitter",
                             add.params = list(size = 3, alpha = 0.9)) +
  stat_compare_means(method = "anova",
                     label.y = max(cox2$value) * 1.15,
                     size = 4) +
  stat_compare_means(comparisons = list(c("Control", "DSS"),
                                        c("Control", "Arg"),
                                        c("DSS", "Arg")),
                     method = "t.test",
                     label = "p.format",
                     size = 4) +
  theme_base +
  labs(x = "", y = "Protein/GAPDH", title = "COX2") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

cox2_plot_ttest
ggsave(cox2_plot_ttest,
       filename = "cox2_ttest.pdf",
       width = 6,
       height = 6)

# p_STAT3/STAT3 - ggbetweenstats参数检验版本
p_stat3_plot_parametric <-
  ggbetweenstats(
    data = p_stat3_stat3,
    x = group,
    y = value,
    type = "parametric",
    pairwise.comparisons = TRUE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    results.subtitle = TRUE,
    centrality.plotting = FALSE
  ) +
  theme_base +
  labs(x = "", y = "p-STAT3/STAT3", title = "p-STAT3/STAT3") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

p_stat3_plot_parametric
ggsave(p_stat3_plot_parametric,
       filename = "p_stat3_stat3_parametric.pdf",
       width = 6,
       height = 5)

# p_STAT3/STAT3 - ggpubr箱线图版本
p_stat3_plot_ttest <- ggboxplot(p_stat3_stat3,
                                x = "group",
                                y = "value",
                                color = "group",
                                add = "jitter",
                                add.params = list(size = 3, alpha = 0.9)) +
  stat_compare_means(method = "anova",
                     label.y = max(p_stat3_stat3$value) * 1.15,
                     size = 4) +
  stat_compare_means(comparisons = list(c("Control", "DSS"),
                                        c("Control", "Arg"),
                                        c("DSS", "Arg")),
                     method = "t.test",
                     label = "p.format",
                     size = 4) +
  theme_base +
  labs(x = "", y = "p-STAT3/STAT3", title = "p-STAT3/STAT3") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

p_stat3_plot_ttest
ggsave(p_stat3_plot_ttest,
       filename = "p_stat3_stat3_ttest.pdf",
       width = 6,
       height = 6)

# p_mTOR/mTOR - ggbetweenstats参数检验版本
p_mtor_plot_parametric <-
  ggbetweenstats(
    data = p_mtor_mtor,
    x = group,
    y = value,
    type = "parametric",
    pairwise.comparisons = TRUE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    results.subtitle = TRUE,
    centrality.plotting = FALSE
  ) +
  theme_base +
  labs(x = "", y = "p-mTOR/mTOR", title = "p-mTOR/mTOR") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

p_mtor_plot_parametric
ggsave(p_mtor_plot_parametric,
       filename = "p_mtor_mtor_parametric.pdf",
       width = 6,
       height = 5)

# p_mTOR/mTOR - ggpubr箱线图版本（修正后的数据）
p_mtor_plot_ttest <- ggboxplot(p_mtor_mtor,
                               x = "group",
                               y = "value",
                               color = "group",
                               add = "jitter",
                               add.params = list(size = 3, alpha = 0.9)) +
  stat_compare_means(method = "anova",
                     label.y = max(p_mtor_mtor$value) * 1.15,
                     size = 4) +
  stat_compare_means(comparisons = list(c("Control", "DSS"),
                                        c("Control", "Arg"),
                                        c("DSS", "Arg")),
                     method = "t.test",
                     label = "p.format",
                     size = 4) +
  theme_base +
  labs(x = "", y = "p-mTOR/mTOR", title = "p-mTOR/mTOR") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

p_mtor_plot_ttest
ggsave(p_mtor_plot_ttest,
       filename = "p_mtor_mtor_ttest.pdf",
       width = 6,
       height = 6)

# p_S6/S6 - ggbetweenstats参数检验版本
p_s6_plot_parametric <-
  ggbetweenstats(
    data = p_s6_s6,
    x = group,
    y = value,
    type = "parametric",
    pairwise.comparisons = TRUE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    results.subtitle = TRUE,
    centrality.plotting = FALSE
  ) +
  theme_base +
  labs(x = "", y = "p-S6/S6", title = "p-S6/S6") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

p_s6_plot_parametric
ggsave(p_s6_plot_parametric,
       filename = "p_s6_s6_parametric.pdf",
       width = 6,
       height = 5)

# p_S6/S6 - ggpubr箱线图版本
p_s6_plot_ttest <- ggboxplot(p_s6_s6,
                             x = "group",
                             y = "value",
                             color = "group",
                             add = "jitter",
                             add.params = list(size = 3, alpha = 0.9)) +
  stat_compare_means(method = "anova",
                     label.y = max(p_s6_s6$value) * 1.15,
                     size = 4) +
  stat_compare_means(comparisons = list(c("Control", "DSS"),
                                        c("Control", "Arg"),
                                        c("DSS", "Arg")),
                     method = "t.test",
                     label = "p.format",
                     size = 4) +
  theme_base +
  labs(x = "", y = "p-S6/S6", title = "p-S6/S6") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

p_s6_plot_ttest
ggsave(p_s6_plot_ttest,
       filename = "p_s6_s6_ttest.pdf",
       width = 6,
       height = 6)

# 输出所有蛋白的t检验p值
print("=== Western Blot t-test p值汇总 ===")

# ASS1
print("\n--- ASS1 ---")
ass1_control_vs_dss <- t.test(ass1$value[ass1$group == "Control"],
                              ass1$value[ass1$group == "DSS"])$p.value
ass1_control_vs_arg <- t.test(ass1$value[ass1$group == "Control"],
                              ass1$value[ass1$group == "Arg"])$p.value
ass1_dss_vs_arg <- t.test(ass1$value[ass1$group == "DSS"],
                          ass1$value[ass1$group == "Arg"])$p.value
print(paste("Control vs DSS: p =", round(ass1_control_vs_dss, 6)))
print(paste("Control vs Arg: p =", round(ass1_control_vs_arg, 6)))
print(paste("DSS vs Arg: p =", round(ass1_dss_vs_arg, 6)))

# COX2
print("\n--- COX2 ---")
cox2_control_vs_dss <- t.test(cox2$value[cox2$group == "Control"],
                              cox2$value[cox2$group == "DSS"])$p.value
cox2_control_vs_arg <- t.test(cox2$value[cox2$group == "Control"],
                              cox2$value[cox2$group == "Arg"])$p.value
cox2_dss_vs_arg <- t.test(cox2$value[cox2$group == "DSS"],
                          cox2$value[cox2$group == "Arg"])$p.value
print(paste("Control vs DSS: p =", round(cox2_control_vs_dss, 6)))
print(paste("Control vs Arg: p =", round(cox2_control_vs_arg, 6)))
print(paste("DSS vs Arg: p =", round(cox2_dss_vs_arg, 6)))

# p_STAT3/STAT3
print("\n--- p_STAT3/STAT3 ---")
stat3_control_vs_dss <- t.test(p_stat3_stat3$value[p_stat3_stat3$group == "Control"],
                               p_stat3_stat3$value[p_stat3_stat3$group == "DSS"])$p.value
stat3_control_vs_arg <- t.test(p_stat3_stat3$value[p_stat3_stat3$group == "Control"],
                               p_stat3_stat3$value[p_stat3_stat3$group == "Arg"])$p.value
stat3_dss_vs_arg <- t.test(p_stat3_stat3$value[p_stat3_stat3$group == "DSS"],
                           p_stat3_stat3$value[p_stat3_stat3$group == "Arg"])$p.value
print(paste("Control vs DSS: p =", round(stat3_control_vs_dss, 6)))
print(paste("Control vs Arg: p =", round(stat3_control_vs_arg, 6)))
print(paste("DSS vs Arg: p =", round(stat3_dss_vs_arg, 6)))

# p_mTOR/mTOR
print("\n--- p_mTOR/mTOR ---")
mtor_control_vs_dss <- t.test(p_mtor_mtor$value[p_mtor_mtor$group == "Control"],
                              p_mtor_mtor$value[p_mtor_mtor$group == "DSS"])$p.value
mtor_control_vs_arg <- t.test(p_mtor_mtor$value[p_mtor_mtor$group == "Control"],
                              p_mtor_mtor$value[p_mtor_mtor$group == "Arg"])$p.value
mtor_dss_vs_arg <- t.test(p_mtor_mtor$value[p_mtor_mtor$group == "DSS"],
                          p_mtor_mtor$value[p_mtor_mtor$group == "Arg"])$p.value
print(paste("Control vs DSS: p =", round(mtor_control_vs_dss, 6)))
print(paste("Control vs Arg: p =", round(mtor_control_vs_arg, 6)))
print(paste("DSS vs Arg: p =", round(mtor_dss_vs_arg, 6)))

# p_S6/S6
print("\n--- p_S6/S6 ---")
s6_control_vs_dss <- t.test(p_s6_s6$value[p_s6_s6$group == "Control"],
                            p_s6_s6$value[p_s6_s6$group == "DSS"])$p.value
s6_control_vs_arg <- t.test(p_s6_s6$value[p_s6_s6$group == "Control"],
                            p_s6_s6$value[p_s6_s6$group == "Arg"])$p.value
s6_dss_vs_arg <- t.test(p_s6_s6$value[p_s6_s6$group == "DSS"],
                        p_s6_s6$value[p_s6_s6$group == "Arg"])$p.value
print(paste("Control vs DSS: p =", round(s6_control_vs_dss, 6)))
print(paste("Control vs Arg: p =", round(s6_control_vs_arg, 6)))
print(paste("DSS vs Arg: p =", round(s6_dss_vs_arg, 6)))

