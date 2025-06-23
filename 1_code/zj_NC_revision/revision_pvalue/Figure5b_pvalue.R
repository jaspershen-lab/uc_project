library(r4projects)
library(ggstatsplot)
library(ggpubr)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

data1 <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 39)

data2 <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 40)

data3 <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 41)

dir.create(
  "3_data_analysis/zj_NC_revision/revision_pvalue/Figure5_pvalue/Figure5b",
  showWarnings = FALSE,
  recursive = TRUE
)
setwd("3_data_analysis/zj_NC_revision/revision_pvalue/Figure5_pvalue/Figure5b")

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
    grepl("Control shRNA", sample_id) ~ "Vector",
    grepl("Control", sample_id) ~ "Control",
    grepl("ASS1 shRNA", sample_id) ~ "shRNA",
  )) %>%
  dplyr::mutate(Day = as.character(Day)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "shRNA")))

# 计算每个动物的平均体重
animal_averages_weight <- data1_processed %>%
  group_by(sample_id, group) %>%
  summarise(avg_weight = mean(value, na.rm = TRUE), .groups = 'drop')

# 提取各组平均体重数据
control_weight <- animal_averages_weight$avg_weight[animal_averages_weight$group == "Control"]
vector_weight <- animal_averages_weight$avg_weight[animal_averages_weight$group == "Vector"]
shrna_weight <- animal_averages_weight$avg_weight[animal_averages_weight$group == "shRNA"]

# Body Weight统计分析
cat("=== BODY WEIGHT ANALYSIS (Figure 5b) ===\n")
cat("Sample sizes: Control =", length(control_weight), ", Vector =", length(vector_weight), ", shRNA =", length(shrna_weight), "\n")

# 进行t检验
test_control_vs_vector_weight <- t.test(control_weight, vector_weight)
test_vector_vs_shrna_weight <- t.test(vector_weight, shrna_weight)
test_control_vs_shrna_weight <- t.test(control_weight, shrna_weight)

control_vs_vector_p_weight <- test_control_vs_vector_weight$p.value
vector_vs_shrna_p_weight <- test_vector_vs_shrna_weight$p.value
control_vs_shrna_p_weight <- test_control_vs_shrna_weight$p.value

cat("Control vs Vector: p =", format.pval(control_vs_vector_p_weight, digits = 4), "\n")
cat("Vector vs shRNA: p =", format.pval(vector_vs_shrna_p_weight, digits = 4), "\n")
cat("Control vs shRNA: p =", format.pval(control_vs_shrna_p_weight, digits = 4), "\n")

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
    grepl("Control shRNA", sample_id) ~ "Vector",
    grepl("Control", sample_id) ~ "Control",
    grepl("ASS1 shRNA", sample_id) ~ "shRNA",
  )) %>%
  dplyr::mutate(Day = as.character(Day)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "shRNA")))

# 计算每个动物的平均DAI
animal_averages_dai <- data2_processed %>%
  group_by(sample_id, group) %>%
  summarise(avg_dai = mean(value, na.rm = TRUE), .groups = 'drop')

# 提取各组平均DAI数据
control_dai <- animal_averages_dai$avg_dai[animal_averages_dai$group == "Control"]
vector_dai <- animal_averages_dai$avg_dai[animal_averages_dai$group == "Vector"]
shrna_dai <- animal_averages_dai$avg_dai[animal_averages_dai$group == "shRNA"]

# DAI统计分析
cat("\n=== DAI ANALYSIS (Figure 5b) ===\n")
cat("Sample sizes: Control =", length(control_dai), ", Vector =", length(vector_dai), ", shRNA =", length(shrna_dai), "\n")

test_control_vs_vector_dai <- t.test(control_dai, vector_dai)
test_vector_vs_shrna_dai <- t.test(vector_dai, shrna_dai)
test_control_vs_shrna_dai <- t.test(control_dai, shrna_dai)

control_vs_vector_p_dai <- test_control_vs_vector_dai$p.value
vector_vs_shrna_p_dai <- test_vector_vs_shrna_dai$p.value
control_vs_shrna_p_dai <- test_control_vs_shrna_dai$p.value

cat("Control vs Vector: p =", format.pval(control_vs_vector_p_dai, digits = 4), "\n")
cat("Vector vs shRNA: p =", format.pval(vector_vs_shrna_p_dai, digits = 4), "\n")
cat("Control vs shRNA: p =", format.pval(control_vs_shrna_p_dai, digits = 4), "\n")

# =============================================================================
# Colon Length数据处理和统计分析
# =============================================================================

data3_processed <-
  rbind(
    data.frame(group = "Control", value = data3$Control),
    data.frame(group = "Vector", value = data3$`Control shRNA`),
    data.frame(group = "shRNA", value = data3$`ASS1 shRNA`)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "shRNA"))) %>%
  filter(!is.na(value))

# Colon Length统计分析（直接使用单次测量值）
control_colon <- data3_processed$value[data3_processed$group == "Control"]
vector_colon <- data3_processed$value[data3_processed$group == "Vector"]
shrna_colon <- data3_processed$value[data3_processed$group == "shRNA"]

cat("\n=== COLON LENGTH ANALYSIS (Figure 5b) ===\n")
cat("Sample sizes: Control =", length(control_colon), ", Vector =", length(vector_colon), ", shRNA =", length(shrna_colon), "\n")

test_control_vs_vector_colon <- t.test(control_colon, vector_colon)
test_vector_vs_shrna_colon <- t.test(vector_colon, shrna_colon)
test_control_vs_shrna_colon <- t.test(control_colon, shrna_colon)

control_vs_vector_p_colon <- test_control_vs_vector_colon$p.value
vector_vs_shrna_p_colon <- test_vector_vs_shrna_colon$p.value
control_vs_shrna_p_colon <- test_control_vs_shrna_colon$p.value

cat("Control vs Vector: p =", format.pval(control_vs_vector_p_colon, digits = 4), "\n")
cat("Vector vs shRNA: p =", format.pval(vector_vs_shrna_p_colon, digits = 4), "\n")
cat("Control vs shRNA: p =", format.pval(control_vs_shrna_p_colon, digits = 4), "\n")

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
  scale_color_manual(values = disease_color3) +
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
  scale_color_manual(values = disease_color3) +
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
  scale_color_manual(values = disease_color3)

plot_colon_nonparametric
ggsave(plot_colon_nonparametric,
       filename = "colon_length_nonparametric.pdf",
       width = 6,
       height = 5)

# =============================================================================
# 基于平均值的图形绘制（显示基于均值计算的p值）
# =============================================================================

# 体重平均值数据框
weight_avg_df <- data.frame(
  group = c(rep("Control", length(control_weight)),
            rep("Vector", length(vector_weight)),
            rep("shRNA", length(shrna_weight))),
  value = c(control_weight, vector_weight, shrna_weight)
) %>%
  mutate(group = factor(group, levels = c("Control", "Vector", "shRNA")))

# DAI平均值数据框
dai_avg_df <- data.frame(
  group = c(rep("Control", length(control_dai)),
            rep("Vector", length(vector_dai)),
            rep("shRNA", length(shrna_dai))),
  value = c(control_dai, vector_dai, shrna_dai)
) %>%
  mutate(group = factor(group, levels = c("Control", "Vector", "shRNA")))

# 体重平均值图形（显示基于平均值的p值）
plot_weight_avg <- ggboxplot(weight_avg_df,
                             x = "group",
                             y = "value",
                             color = "group",
                             add = "jitter",
                             add.params = list(size = 3, alpha = 0.9)) +
  theme_base +
  labs(x = "", y = "Average Body Weight (%)", title = "Average Body Weight with P-values") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color3)

# 手动添加p值注释
max_y_weight <- max(weight_avg_df$value)
y_pos_weight <- c(max_y_weight * 1.02, max_y_weight * 1.06, max_y_weight * 1.10)

plot_weight_avg <- plot_weight_avg +
  annotate("text", x = 1.5, y = y_pos_weight[1],
           label = paste("p =", format.pval(control_vs_vector_p_weight, digits = 3)),
           size = 4) +
  annotate("segment", x = 1, xend = 2, y = y_pos_weight[1] * 0.999, yend = y_pos_weight[1] * 0.999) +
  annotate("text", x = 2.5, y = y_pos_weight[2],
           label = paste("p =", format.pval(vector_vs_shrna_p_weight, digits = 3)),
           size = 4) +
  annotate("segment", x = 2, xend = 3, y = y_pos_weight[2] * 0.999, yend = y_pos_weight[2] * 0.999) +
  annotate("text", x = 2, y = y_pos_weight[3],
           label = paste("p =", format.pval(control_vs_shrna_p_weight, digits = 3)),
           size = 4) +
  annotate("segment", x = 1, xend = 3, y = y_pos_weight[3] * 0.999, yend = y_pos_weight[3] * 0.999)

plot_weight_avg
ggsave(plot_weight_avg,
       filename = "body_weight_average_pvalues.pdf",
       width = 6,
       height = 6)

# DAI平均值图形（显示基于平均值的p值）
plot_dai_avg <- ggboxplot(dai_avg_df,
                          x = "group",
                          y = "value",
                          color = "group",
                          add = "jitter",
                          add.params = list(size = 3, alpha = 0.9)) +
  theme_base +
  labs(x = "", y = "Average DAI", title = "Average DAI with P-values") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color3)

# 手动添加p值注释
max_y_dai <- max(dai_avg_df$value)
y_pos_dai <- c(max_y_dai * 1.05, max_y_dai * 1.15, max_y_dai * 1.25)

plot_dai_avg <- plot_dai_avg +
  annotate("text", x = 1.5, y = y_pos_dai[1],
           label = paste("p =", format.pval(control_vs_vector_p_dai, digits = 3)),
           size = 4) +
  annotate("segment", x = 1, xend = 2, y = y_pos_dai[1] * 0.98, yend = y_pos_dai[1] * 0.98) +
  annotate("text", x = 2.5, y = y_pos_dai[2],
           label = paste("p =", format.pval(vector_vs_shrna_p_dai, digits = 3)),
           size = 4) +
  annotate("segment", x = 2, xend = 3, y = y_pos_dai[2] * 0.98, yend = y_pos_dai[2] * 0.98) +
  annotate("text", x = 2, y = y_pos_dai[3],
           label = paste("p =", format.pval(control_vs_shrna_p_dai, digits = 3)),
           size = 4) +
  annotate("segment", x = 1, xend = 3, y = y_pos_dai[3] * 0.98, yend = y_pos_dai[3] * 0.98)

plot_dai_avg
ggsave(plot_dai_avg,
       filename = "dai_average_pvalues.pdf",
       width = 6,
       height = 6)

# 结肠长度 - 基于均值计算p值的自定义图形
plot_colon_custom <- ggboxplot(data3_processed,
                               x = "group",
                               y = "value",
                               color = "group",
                               add = "jitter",
                               add.params = list(size = 3, alpha = 0.9)) +
  theme_base +
  labs(x = "", y = "Colon length (cm)", title = "Colon Length with Custom P-values") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color3)

# 手动添加基于t检验的p值注释
max_y_colon <- max(data3_processed$value)
y_positions <- c(max_y_colon * 1.05, max_y_colon * 1.15, max_y_colon * 1.25)

plot_colon_custom <- plot_colon_custom +
  annotate("text", x = 1.5, y = y_positions[1],
           label = paste("p =", format.pval(control_vs_vector_p_colon, digits = 3)),
           size = 4) +
  annotate("segment", x = 1, xend = 2, y = y_positions[1] * 0.98, yend = y_positions[1] * 0.98) +
  annotate("text", x = 2.5, y = y_positions[2],
           label = paste("p =", format.pval(vector_vs_shrna_p_colon, digits = 3)),
           size = 4) +
  annotate("segment", x = 2, xend = 3, y = y_positions[2] * 0.98, yend = y_positions[2] * 0.98) +
  annotate("text", x = 2, y = y_positions[3],
           label = paste("p =", format.pval(control_vs_shrna_p_colon, digits = 3)),
           size = 4) +
  annotate("segment", x = 1, xend = 3, y = y_positions[3] * 0.98, yend = y_positions[3] * 0.98)

plot_colon_custom
ggsave(plot_colon_custom,
       filename = "colon_length_custom_pvalues.pdf",
       width = 6,
       height = 6)

# =============================================================================
# 统计结果汇总
# =============================================================================

# 创建基于平均值的完整结果表格
complete_results <- data.frame(
  Experiment = rep(c("Body_Weight_Avg", "DAI_Avg", "Colon_Length"), each = 3),
  Comparison = rep(c("Control_vs_Vector", "Vector_vs_shRNA", "Control_vs_shRNA"), 3),
  P_value = c(
    control_vs_vector_p_weight, vector_vs_shrna_p_weight, control_vs_shrna_p_weight,
    control_vs_vector_p_dai, vector_vs_shrna_p_dai, control_vs_shrna_p_dai,
    control_vs_vector_p_colon, vector_vs_shrna_p_colon, control_vs_shrna_p_colon
  ),
  Mean_Group1 = c(
    mean(control_weight), mean(vector_weight), mean(control_weight),
    mean(control_dai), mean(vector_dai), mean(control_dai),
    mean(control_colon), mean(vector_colon), mean(control_colon)
  ),
  Mean_Group2 = c(
    mean(vector_weight), mean(shrna_weight), mean(shrna_weight),
    mean(vector_dai), mean(shrna_dai), mean(shrna_dai),
    mean(vector_colon), mean(shrna_colon), mean(shrna_colon)
  )
)

complete_results$Mean_Difference <- complete_results$Mean_Group2 - complete_results$Mean_Group1
complete_results$Significance <- ifelse(complete_results$P_value < 0.001, "***",
                                        ifelse(complete_results$P_value < 0.01, "**",
                                               ifelse(complete_results$P_value < 0.05, "*", "ns")))

cat("\n=== COMPLETE RESULTS TABLE (Figure 5b - Based on Averages) ===\n")
print(complete_results)

# 保存基于平均值的完整结果
write.csv(complete_results, "figure5b_average_based_statistical_analysis.csv", row.names = FALSE)
