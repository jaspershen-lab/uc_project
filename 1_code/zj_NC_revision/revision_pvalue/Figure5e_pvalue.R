library(r4projects)
library(ggstatsplot)
library(ggpubr)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

data1 <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 20)

data2 <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 21)

data3 <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 22)

dir.create(
  "3_data_analysis/zj_NC_revision/revision_pvalue/Figure5_pvalue/Figure5e",
  showWarnings = FALSE,
  recursive = TRUE
)
setwd("3_data_analysis/zj_NC_revision/revision_pvalue/Figure5_pvalue/Figure5e")

# =============================================================================
# Body Weight数据处理和基于平均值的统计分析
# =============================================================================

colnames(data1)[1] <- "Day"

# 体重数据预处理
data1_processed <-
  data1 %>%
  as.data.frame() %>%
  rename_with( ~ gsub("\\.\\.\\.", "_", .)) %>%
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

# 计算每个动物的平均体重
animal_averages_weight <- data1_processed %>%
  group_by(sample_id, group) %>%
  summarise(avg_weight = mean(value, na.rm = TRUE), .groups = 'drop')

# 提取各组平均体重数据
control_weight <- animal_averages_weight$avg_weight[animal_averages_weight$group == "Control"]
dss_weight <- animal_averages_weight$avg_weight[animal_averages_weight$group == "DSS"]
mdla_weight <- animal_averages_weight$avg_weight[animal_averages_weight$group == "MDLA"]
asa_weight <- animal_averages_weight$avg_weight[animal_averages_weight$group == "5_ASA"]

# Body Weight统计分析
cat("=== BODY WEIGHT ANALYSIS (Figure 5e) ===\n")
cat("Sample sizes: Control =", length(control_weight), ", DSS =", length(dss_weight),
    ", MDLA =", length(mdla_weight), ", 5-ASA =", length(asa_weight), "\n")

# 进行t检验
test_control_vs_dss_weight <- t.test(control_weight, dss_weight)
test_dss_vs_mdla_weight <- t.test(dss_weight, mdla_weight)
test_dss_vs_asa_weight <- t.test(dss_weight, asa_weight)
test_mdla_vs_asa_weight <- t.test(mdla_weight, asa_weight)

control_vs_dss_p_weight <- test_control_vs_dss_weight$p.value
dss_vs_mdla_p_weight <- test_dss_vs_mdla_weight$p.value
dss_vs_asa_p_weight <- test_dss_vs_asa_weight$p.value
mdla_vs_asa_p_weight <- test_mdla_vs_asa_weight$p.value

cat("Control vs DSS: p =", format.pval(control_vs_dss_p_weight, digits = 4), "\n")
cat("DSS vs MDLA: p =", format.pval(dss_vs_mdla_p_weight, digits = 4), "\n")
cat("DSS vs 5-ASA: p =", format.pval(dss_vs_asa_p_weight, digits = 4), "\n")
cat("MDLA vs 5-ASA: p =", format.pval(mdla_vs_asa_p_weight, digits = 4), "\n")

# =============================================================================
# DAI数据处理和基于平均值的统计分析
# =============================================================================

colnames(data2)[1] <- "Day"

# DAI数据预处理
data2_processed <-
  data2 %>%
  as.data.frame() %>%
  rename_with( ~ gsub("\\.\\.\\.", "_", .)) %>%
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

# 计算每个动物的平均DAI
animal_averages_dai <- data2_processed %>%
  group_by(sample_id, group) %>%
  summarise(avg_dai = mean(value, na.rm = TRUE), .groups = 'drop')

# 提取各组平均DAI数据
control_dai <- animal_averages_dai$avg_dai[animal_averages_dai$group == "Control"]
dss_dai <- animal_averages_dai$avg_dai[animal_averages_dai$group == "DSS"]
mdla_dai <- animal_averages_dai$avg_dai[animal_averages_dai$group == "MDLA"]
asa_dai <- animal_averages_dai$avg_dai[animal_averages_dai$group == "5_ASA"]

# DAI统计分析
cat("\n=== DAI ANALYSIS (Figure 5e) ===\n")
cat("Sample sizes: Control =", length(control_dai), ", DSS =", length(dss_dai),
    ", MDLA =", length(mdla_dai), ", 5-ASA =", length(asa_dai), "\n")

test_control_vs_dss_dai <- t.test(control_dai, dss_dai)
test_dss_vs_mdla_dai <- t.test(dss_dai, mdla_dai)
test_dss_vs_asa_dai <- t.test(dss_dai, asa_dai)
test_mdla_vs_asa_dai <- t.test(mdla_dai, asa_dai)

control_vs_dss_p_dai <- test_control_vs_dss_dai$p.value
dss_vs_mdla_p_dai <- test_dss_vs_mdla_dai$p.value
dss_vs_asa_p_dai <- test_dss_vs_asa_dai$p.value
mdla_vs_asa_p_dai <- test_mdla_vs_asa_dai$p.value

cat("Control vs DSS: p =", format.pval(control_vs_dss_p_dai, digits = 4), "\n")
cat("DSS vs MDLA: p =", format.pval(dss_vs_mdla_p_dai, digits = 4), "\n")
cat("DSS vs 5-ASA: p =", format.pval(dss_vs_asa_p_dai, digits = 4), "\n")
cat("MDLA vs 5-ASA: p =", format.pval(mdla_vs_asa_p_dai, digits = 4), "\n")

# =============================================================================
# Colon Length数据处理和统计分析
# =============================================================================

# 修正结肠长度数据处理
data3_processed <-
  rbind(
    data.frame(group = "Control", value = data3$Control),
    data.frame(group = "DSS", value = data3$Model),  # 修正为正确的列名
    data.frame(group = "MDLA", value = data3$MDLA),
    data.frame(group = "5_ASA", value = data3$`5-ASA`)  # 修正为正确的列名
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "MDLA", "5_ASA"))) %>%
  filter(!is.na(value))

# Colon Length统计分析（直接使用单次测量值）
control_colon <- data3_processed$value[data3_processed$group == "Control"]
dss_colon <- data3_processed$value[data3_processed$group == "DSS"]
mdla_colon <- data3_processed$value[data3_processed$group == "MDLA"]
asa_colon <- data3_processed$value[data3_processed$group == "5_ASA"]

cat("\n=== COLON LENGTH ANALYSIS (Figure 5e) ===\n")
cat("Sample sizes: Control =", length(control_colon), ", DSS =", length(dss_colon),
    ", MDLA =", length(mdla_colon), ", 5-ASA =", length(asa_colon), "\n")

test_control_vs_dss_colon <- t.test(control_colon, dss_colon)
test_dss_vs_mdla_colon <- t.test(dss_colon, mdla_colon)
test_dss_vs_asa_colon <- t.test(dss_colon, asa_colon)
test_mdla_vs_asa_colon <- t.test(mdla_colon, asa_colon)

control_vs_dss_p_colon <- test_control_vs_dss_colon$p.value
dss_vs_mdla_p_colon <- test_dss_vs_mdla_colon$p.value
dss_vs_asa_p_colon <- test_dss_vs_asa_colon$p.value
mdla_vs_asa_p_colon <- test_mdla_vs_asa_colon$p.value

cat("Control vs DSS: p =", format.pval(control_vs_dss_p_colon, digits = 4), "\n")
cat("DSS vs MDLA: p =", format.pval(dss_vs_mdla_p_colon, digits = 4), "\n")
cat("DSS vs 5-ASA: p =", format.pval(dss_vs_asa_p_colon, digits = 4), "\n")
cat("MDLA vs 5-ASA: p =", format.pval(mdla_vs_asa_p_colon, digits = 4), "\n")

# =============================================================================
# 原版图形（保持一致的样式）
# =============================================================================

# 体重变化图 - 原版样式
median_data1 <-
  data1_processed %>%
  dplyr::group_by(Day, group) %>%
  dplyr::summarize(median_value = median(value), .groups = 'drop')

plot_bodyweight_original <-
  data1_processed %>%
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
    data = median_data1,
    aes(x = as.numeric(Day), y = median_value, group = group),
    position = position_dodge(0.35)
  ) +
  theme_base +
  scale_color_manual(values = disease_color4) +
  labs(x = "Day", y = "Body weight (%, compared to baseline)")

plot_bodyweight_original
ggsave(plot_bodyweight_original,
       filename = "body_weight_change_original.pdf",
       width = 7,
       height = 5)

# DAI变化图 - 原版样式
median_data2 <-
  data2_processed %>%
  dplyr::group_by(Day, group) %>%
  dplyr::summarize(median_value = median(value), .groups = 'drop')

plot_dai_original <-
  data2_processed %>%
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
    data = median_data2,
    aes(x = as.numeric(Day), y = median_value, group = group),
    position = position_dodge(0.35)
  ) +
  theme_base +
  scale_color_manual(values = disease_color4) +
  labs(x = "Day", y = "Disease activity index (DAI)")

plot_dai_original
ggsave(plot_dai_original,
       filename = "dai_change_original.pdf",
       width = 7,
       height = 5)

# 结肠长度 - 原版样式（与原代码一致的非参数检验，但显示p值）
plot_colon_nonparametric <-
  ggbetweenstats(
    data = data3_processed,
    x = group,
    y = value,
    type = "nonparametric",        # 与原代码一致：非参数检验
    pairwise.comparisons = TRUE,   # 改为TRUE显示配对比较
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    results.subtitle = TRUE,       # 显示统计结果
    centrality.plotting = FALSE
  ) +
  theme_base +
  labs(x = "", y = "Colon length (cm)", title = "Colon Length (Nonparametric)") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color4)

plot_colon_nonparametric
ggsave(plot_colon_nonparametric,
       filename = "colon_length_nonparametric.pdf",
       width = 7,
       height = 5)

# =============================================================================
# 基于平均值的图形绘制（显示基于均值计算的p值）
# =============================================================================

# 体重平均值数据框
weight_avg_df <- data.frame(
  group = c(rep("Control", length(control_weight)),
            rep("DSS", length(dss_weight)),
            rep("MDLA", length(mdla_weight)),
            rep("5_ASA", length(asa_weight))),
  value = c(control_weight, dss_weight, mdla_weight, asa_weight)
) %>%
  mutate(group = factor(group, levels = c("Control", "DSS", "MDLA", "5_ASA")))

# DAI平均值数据框
dai_avg_df <- data.frame(
  group = c(rep("Control", length(control_dai)),
            rep("DSS", length(dss_dai)),
            rep("MDLA", length(mdla_dai)),
            rep("5_ASA", length(asa_dai))),
  value = c(control_dai, dss_dai, mdla_dai, asa_dai)
) %>%
  mutate(group = factor(group, levels = c("Control", "DSS", "MDLA", "5_ASA")))

# 体重平均值图形（使用ggpubr进行主要比较）
plot_weight_avg <- ggboxplot(weight_avg_df,
                             x = "group",
                             y = "value",
                             color = "group",
                             add = "jitter",
                             add.params = list(size = 3, alpha = 0.9)) +
  stat_compare_means(method = "anova",
                     label.y = max(weight_avg_df$value) * 1.15,
                     size = 4) +
  stat_compare_means(comparisons = list(c("Control", "DSS"),
                                        c("DSS", "MDLA"),
                                        c("DSS", "5_ASA")),
                     method = "t.test",
                     label = "p.format",
                     size = 3,
                     step.increase = 0.1) +
  theme_base +
  labs(x = "", y = "Average Body Weight (%)", title = "Average Body Weight with P-values") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color4)

plot_weight_avg
ggsave(plot_weight_avg,
       filename = "body_weight_average_pvalues.pdf",
       width = 7,
       height = 6)

# DAI平均值图形（使用ggpubr进行主要比较）
plot_dai_avg <- ggboxplot(dai_avg_df,
                          x = "group",
                          y = "value",
                          color = "group",
                          add = "jitter",
                          add.params = list(size = 3, alpha = 0.9)) +
  stat_compare_means(method = "anova",
                     label.y = max(dai_avg_df$value) * 1.15,
                     size = 4) +
  stat_compare_means(comparisons = list(c("Control", "DSS"),
                                        c("DSS", "MDLA"),
                                        c("DSS", "5_ASA")),
                     method = "t.test",
                     label = "p.format",
                     size = 3,
                     step.increase = 0.1) +
  theme_base +
  labs(x = "", y = "Average DAI", title = "Average DAI with P-values") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color4)

plot_dai_avg
ggsave(plot_dai_avg,
       filename = "dai_average_pvalues.pdf",
       width = 7,
       height = 6)

# 结肠长度 - 基于t检验的图形
plot_colon_custom <- ggboxplot(data3_processed,
                               x = "group",
                               y = "value",
                               color = "group",
                               add = "jitter",
                               add.params = list(size = 3, alpha = 0.9)) +
  stat_compare_means(method = "anova",
                     label.y = max(data3_processed$value) * 1.15,
                     size = 4) +
  stat_compare_means(comparisons = list(c("Control", "DSS"),
                                        c("DSS", "MDLA"),
                                        c("DSS", "5_ASA")),
                     method = "t.test",
                     label = "p.format",
                     size = 3,
                     step.increase = 0.1) +
  theme_base +
  labs(x = "", y = "Colon length (cm)", title = "Colon Length with T-test P-values") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color4)

plot_colon_custom
ggsave(plot_colon_custom,
       filename = "colon_length_custom_pvalues.pdf",
       width = 7,
       height = 6)

# =============================================================================
# 统计结果汇总
# =============================================================================

# 创建基于平均值的完整结果表格
complete_results <- data.frame(
  Experiment = rep(c("Body_Weight_Avg", "DAI_Avg", "Colon_Length"), each = 4),
  Comparison = rep(c("Control_vs_DSS", "DSS_vs_MDLA", "DSS_vs_5ASA", "MDLA_vs_5ASA"), 3),
  P_value = c(
    control_vs_dss_p_weight, dss_vs_mdla_p_weight, dss_vs_asa_p_weight, mdla_vs_asa_p_weight,
    control_vs_dss_p_dai, dss_vs_mdla_p_dai, dss_vs_asa_p_dai, mdla_vs_asa_p_dai,
    control_vs_dss_p_colon, dss_vs_mdla_p_colon, dss_vs_asa_p_colon, mdla_vs_asa_p_colon
  ),
  Mean_Group1 = c(
    mean(control_weight), mean(dss_weight), mean(dss_weight), mean(mdla_weight),
    mean(control_dai), mean(dss_dai), mean(dss_dai), mean(mdla_dai),
    mean(control_colon), mean(dss_colon), mean(dss_colon), mean(mdla_colon)
  ),
  Mean_Group2 = c(
    mean(dss_weight), mean(mdla_weight), mean(asa_weight), mean(asa_weight),
    mean(dss_dai), mean(mdla_dai), mean(asa_dai), mean(asa_dai),
    mean(dss_colon), mean(mdla_colon), mean(asa_colon), mean(asa_colon)
  )
)

complete_results$Mean_Difference <- complete_results$Mean_Group2 - complete_results$Mean_Group1
complete_results$Significance <- ifelse(complete_results$P_value < 0.001, "***",
                                        ifelse(complete_results$P_value < 0.01, "**",
                                               ifelse(complete_results$P_value < 0.05, "*", "ns")))

cat("\n=== COMPLETE RESULTS TABLE (Figure 5e - Based on Averages) ===\n")
print(complete_results)

# 保存基于平均值的完整结果
write.csv(complete_results, "figure5e_average_based_statistical_analysis.csv", row.names = FALSE)
