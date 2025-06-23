library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')
library(ggpubr)

# 读取数据
mpo_data <- readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx", sheet = 14)
il6_data <- readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx", sheet = 15)
il1b_data <- readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx", sheet = 16)

dir.create(
  "3_data_analysis/zj_NC_revision/revision_pvalue/Figure4_pvalue/Figure4d",
  showWarnings = FALSE,
  recursive = TRUE
)
setwd("3_data_analysis/zj_NC_revision/revision_pvalue/Figure4_pvalue/Figure4d")

# 方法1: 使用参数检验 (ANOVA + t-test)
# MPO数据处理和绘图
mpo_processed <- rbind(
  data.frame(group = "Control", value = mpo_data$Control),
  data.frame(group = "DSS", value = mpo_data$Model),
  data.frame(group = "Arg", value = mpo_data$Arg)
) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))

mpo_plot_parametric <-
  ggbetweenstats(
    data = mpo_processed,
    x = group,
    y = value,
    type = "parametric",  # 使用参数检验
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
  labs(x = "", y = "ug/g", title = "MPO (ANOVA)") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

mpo_plot_parametric
ggsave(mpo_plot_parametric,
       filename = "figure4d_mpo_parametric.pdf",
       width = 6,
       height = 5)

# 方法2: 使用ggpubr包的比较方法
mpo_plot_ggpubr <- ggboxplot(mpo_processed,
                             x = "group",
                             y = "value",
                             color = "group",
                             add = "jitter",
                             add.params = list(size = 3, alpha = 0.9)) +
  stat_compare_means(method = "anova", label.y = max(mpo_processed$value) * 1.2) +
  stat_compare_means(comparisons = list(c("Control", "DSS"),
                                        c("Control", "Arg"),
                                        c("DSS", "Arg")),
                     method = "t.test",
                     label = "p.format") +
  theme_base +
  labs(x = "", y = "ug/g", title = "MPO (t-test)") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)
mpo_plot_ggpubr
ggsave(mpo_plot_ggpubr,
       filename = "figure4d_mpo_ttest.pdf",
       width = 6,
       height = 6)

# IL6数据 - 参数检验
il6_processed <- rbind(
  data.frame(group = "Control", value = il6_data$Control),
  data.frame(group = "DSS", value = il6_data$Model),
  data.frame(group = "Arg", value = il6_data$Arg)
) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))

il6_plot_parametric <-
  ggbetweenstats(
    data = il6_processed,
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
  labs(x = "", y = "ug/g", title = "IL-6 (ANOVA)") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

ggsave(il6_plot_parametric,
       filename = "il6_parametric.pdf",
       width = 6,
       height = 5)

# IL6 - t检验
il6_plot_ggpubr <- ggboxplot(il6_processed,
                             x = "group",
                             y = "value",
                             color = "group",
                             add = "jitter",
                             add.params = list(size = 3, alpha = 0.9)) +
  stat_compare_means(method = "anova", label.y = max(il6_processed$value) * 1.2) +
  stat_compare_means(comparisons = list(c("Control", "DSS"),
                                        c("Control", "Arg"),
                                        c("DSS", "Arg")),
                     method = "t.test",
                     label = "p.format") +
  theme_base +
  labs(x = "", y = "ug/g", title = "IL-6 (t-test)") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)
il6_plot_ggpubr
ggsave(il6_plot_ggpubr,
       filename = "il6_ttest.pdf",
       width = 6,
       height = 6)

# IL1B数据 - 参数检验
il1b_processed <- rbind(
  data.frame(group = "Control", value = il1b_data$Control),
  data.frame(group = "DSS", value = il1b_data$Model),
  data.frame(group = "Arg", value = il1b_data$Arg)
) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))

il1b_plot_parametric <-
  ggbetweenstats(
    data = il1b_processed,
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
  labs(x = "", y = "ug/g", title = "IL-1β (ANOVA)") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

ggsave(il1b_plot_parametric,
       filename = "il1b_parametric.pdf",
       width = 6,
       height = 5)

# IL1B - t检验
il1b_plot_ggpubr <- ggboxplot(il1b_processed,
                              x = "group",
                              y = "value",
                              color = "group",
                              add = "jitter",
                              add.params = list(size = 3, alpha = 0.9)) +
  stat_compare_means(method = "anova", label.y = max(il1b_processed$value) * 1.2) +
  stat_compare_means(comparisons = list(c("Control", "DSS"),
                                        c("Control", "Arg"),
                                        c("DSS", "Arg")),
                     method = "t.test",
                     label = "p.format") +
  theme_base +
  labs(x = "", y = "ug/g", title = "IL-1β (t-test)") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)
il1b_plot_ggpubr
ggsave(il1b_plot_ggpubr,
       filename = "il1b_ttest.pdf",
       width = 6,
       height = 6)

# 方法3: 手动计算并添加具体p值
print("=== 各数据集的具体p值 ===")

# MPO配对t检验
mpo_control_vs_dss <- t.test(mpo_data$Control, mpo_data$Model)$p.value
mpo_control_vs_arg <- t.test(mpo_data$Control, mpo_data$Arg)$p.value
mpo_dss_vs_arg <- t.test(mpo_data$Model, mpo_data$Arg)$p.value

print(paste("MPO - Control vs DSS: p =", round(mpo_control_vs_dss, 6)))
print(paste("MPO - Control vs Arg: p =", round(mpo_control_vs_arg, 6)))
print(paste("MPO - DSS vs Arg: p =", round(mpo_dss_vs_arg, 6)))

# IL6配对t检验
il6_control_vs_dss <- t.test(il6_data$Control, il6_data$Model)$p.value
il6_control_vs_arg <- t.test(il6_data$Control, il6_data$Arg)$p.value
il6_dss_vs_arg <- t.test(il6_data$Model, il6_data$Arg)$p.value

print(paste("IL6 - Control vs DSS: p =", round(il6_control_vs_dss, 6)))
print(paste("IL6 - Control vs Arg: p =", round(il6_control_vs_arg, 6)))
print(paste("IL6 - DSS vs Arg: p =", round(il6_dss_vs_arg, 6)))

# IL1B配对t检验
il1b_control_vs_dss <- t.test(il1b_data$Control, il1b_data$Model)$p.value
il1b_control_vs_arg <- t.test(il1b_data$Control, il1b_data$Arg)$p.value
il1b_dss_vs_arg <- t.test(il1b_data$Model, il1b_data$Arg)$p.value

print(paste("IL1B - Control vs DSS: p =", round(il1b_control_vs_dss, 6)))
print(paste("IL1B - Control vs Arg: p =", round(il1b_control_vs_arg, 6)))
print(paste("IL1B - DSS vs Arg: p =", round(il1b_dss_vs_arg, 6)))
