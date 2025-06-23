library(r4projects)
library(ggstatsplot)
library(ggpubr)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')


###使用原代码图像!!!





# 读取数据
data1 <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 18)
data2 <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 19)

dir.create(
  "3_data_analysis/zj_NC_revision/revision_pvalue/Figure4_pvalue/Figure4b",
  showWarnings = FALSE,
  recursive = TRUE
)
setwd("3_data_analysis/zj_NC_revision/revision_pvalue/Figure4_pvalue/Figure4b")

# 处理colon arginine数据
data1 <-
  rbind(
    data.frame(group = "Control", value = data1$Control),
    data.frame(group = "DSS", value = data1$Model),
    data.frame(group = "Arg", value = data1$Arg)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))

# 方法1: 使用ggbetweenstats的参数检验
plot1_parametric <-
  ggbetweenstats(
    data  = data1,
    x  = group,
    y  = value,
    type = "parametric",          # 改为参数检验 (ANOVA + t-test)
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
  labs(x = "", y = "Fold change (compared to controls)",
       title = "Colon Arginine") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

plot1_parametric

ggsave(plot1_parametric,
       filename = "figure4b_colon_arginine_parametric.pdf",
       width = 6,
       height = 5)

# 方法2: 使用ggpubr的t检验（推荐）
plot1_ttest <- ggboxplot(data1,
                         x = "group",
                         y = "value",
                         color = "group",
                         add = "jitter",
                         add.params = list(size = 3, alpha = 0.9)) +
  stat_compare_means(method = "anova",
                     label.y = max(data1$value) * 1.15,
                     size = 4) +
  stat_compare_means(comparisons = list(c("Control", "DSS"),
                                        c("Control", "Arg"),
                                        c("DSS", "Arg")),
                     method = "t.test",
                     label = "p.format",
                     size = 4) +
  theme_base +
  labs(x = "", y = "Fold change (compared to controls)",
       title = "Colon Arginine") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

plot1_ttest

ggsave(plot1_ttest,
       filename = "figure4b_colon_arginine_ttest.pdf",
       width = 6,
       height = 6)

# 处理blood arginine数据
data2 <-
  rbind(
    data.frame(group = "Control", value = data2$Control),
    data.frame(group = "DSS", value = data2$Model),
    data.frame(group = "Arg", value = data2$Arg)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))

# 方法1: 使用ggbetweenstats的参数检验
plot2_parametric <-
  ggbetweenstats(
    data  = data2,
    x  = group,
    y  = value,
    type = "parametric",          # 改为参数检验 (ANOVA + t-test)
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
  labs(x = "", y = "Fold change (compared to controls)",
       title = "Blood Arginine") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

plot2_parametric

ggsave(plot2_parametric,
       filename = "figure4b_blood_arginine_parametric.pdf",
       width = 6,
       height = 5)

# 方法2: 使用ggpubr的t检验（推荐）
plot2_ttest <- ggboxplot(data2,
                         x = "group",
                         y = "value",
                         color = "group",
                         add = "jitter",
                         add.params = list(size = 3, alpha = 0.9)) +
  stat_compare_means(method = "anova",
                     label.y = max(data2$value) * 1.15,
                     size = 4) +
  stat_compare_means(comparisons = list(c("Control", "DSS"),
                                        c("Control", "Arg"),
                                        c("DSS", "Arg")),
                     method = "t.test",
                     label = "p.format",
                     size = 4) +
  theme_base +
  labs(x = "", y = "Fold change (compared to controls)",
       title = "Blood Arginine") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

plot2_ttest

ggsave(plot2_ttest,
       filename = "figure4b_blood_arginine_ttest.pdf",
       width = 6,
       height = 6)

# 输出具体的t检验p值
print("=== Colon Arginine t-test p值 ===")
colon_control_vs_dss <- t.test(data1$value[data1$group == "Control"],
                               data1$value[data1$group == "DSS"])$p.value
colon_control_vs_arg <- t.test(data1$value[data1$group == "Control"],
                               data1$value[data1$group == "Arg"])$p.value
colon_dss_vs_arg <- t.test(data1$value[data1$group == "DSS"],
                           data1$value[data1$group == "Arg"])$p.value

print(paste("Control vs DSS: p =", round(colon_control_vs_dss, 6)))
print(paste("Control vs Arg: p =", round(colon_control_vs_arg, 6)))
print(paste("DSS vs Arg: p =", round(colon_dss_vs_arg, 6)))

print("=== Blood Arginine t-test p值 ===")
blood_control_vs_dss <- t.test(data2$value[data2$group == "Control"],
                               data2$value[data2$group == "DSS"])$p.value
blood_control_vs_arg <- t.test(data2$value[data2$group == "Control"],
                               data2$value[data2$group == "Arg"])$p.value
blood_dss_vs_arg <- t.test(data2$value[data2$group == "DSS"],
                           data2$value[data2$group == "Arg"])$p.value

print(paste("Control vs DSS: p =", round(blood_control_vs_dss, 6)))
print(paste("Control vs Arg: p =", round(blood_control_vs_arg, 6)))
print(paste("DSS vs Arg: p =", round(blood_dss_vs_arg, 6)))
