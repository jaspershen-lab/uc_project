library(r4projects)
library(ggstatsplot)
library(ggpubr)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

data1 <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 29)

data2 <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 30)

data3 <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 31)

dir.create(
  "3_data_analysis/zj_NC_revision/revision_pvalue/Figure4_pvalue/Figure5a",
  showWarnings = FALSE,
  recursive = TRUE
)
setwd("3_data_analysis/zj_NC_revision/revision_pvalue/Figure4_pvalue/Figure5a")


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
    grepl("Vector", sample_id) ~ "Vector",
    grepl("ASS1 OE", sample_id) ~ "ASS1_OE",
  )) %>%
  dplyr::mutate(Day = as.character(Day)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "ASS1_OE")))

# 计算每个动物的平均体重
animal_averages_weight <- data1_processed %>%
  group_by(sample_id, group) %>%
  summarise(avg_weight = mean(value, na.rm = TRUE), .groups = 'drop')

# 提取各组平均体重数据
control_weight <- animal_averages_weight$avg_weight[animal_averages_weight$group == "Control"]
vector_weight <- animal_averages_weight$avg_weight[animal_averages_weight$group == "Vector"]
ass1_oe_weight <- animal_averages_weight$avg_weight[animal_averages_weight$group == "ASS1_OE"]

# Body Weight统计分析
cat("=== BODY WEIGHT ANALYSIS ===\n")
cat("Sample sizes: Control =", length(control_weight), ", Vector =", length(vector_weight), ", ASS1_OE =", length(ass1_oe_weight), "\n")

# 进行t检验
test_control_vs_vector_weight <- t.test(control_weight, vector_weight)
test_vector_vs_ass1_weight <- t.test(vector_weight, ass1_oe_weight)
test_control_vs_ass1_weight <- t.test(control_weight, ass1_oe_weight)

control_vs_vector_p_weight <- test_control_vs_vector_weight$p.value
vector_vs_ass1_p_weight <- test_vector_vs_ass1_weight$p.value
control_vs_ass1_p_weight <- test_control_vs_ass1_weight$p.value

cat("Control vs Vector: p =", format.pval(control_vs_vector_p_weight, digits = 4), "\n")
cat("Vector vs ASS1_OE: p =", format.pval(vector_vs_ass1_p_weight, digits = 4), "\n")
cat("Control vs ASS1_OE: p =", format.pval(control_vs_ass1_p_weight, digits = 4), "\n")

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
    grepl("Vector", sample_id) ~ "Vector",
    grepl("ASS1 OE", sample_id) ~ "ASS1_OE",
  )) %>%
  dplyr::mutate(Day = as.character(Day)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "ASS1_OE")))

# 计算每个动物的平均DAI
animal_averages_dai <- data2_processed %>%
  group_by(sample_id, group) %>%
  summarise(avg_dai = mean(value, na.rm = TRUE), .groups = 'drop')

# 提取各组平均DAI数据
control_dai <- animal_averages_dai$avg_dai[animal_averages_dai$group == "Control"]
vector_dai <- animal_averages_dai$avg_dai[animal_averages_dai$group == "Vector"]
ass1_oe_dai <- animal_averages_dai$avg_dai[animal_averages_dai$group == "ASS1_OE"]

# DAI统计分析
cat("\n=== DAI ANALYSIS ===\n")
cat("Sample sizes: Control =", length(control_dai), ", Vector =", length(vector_dai), ", ASS1_OE =", length(ass1_oe_dai), "\n")

test_control_vs_vector_dai <- t.test(control_dai, vector_dai)
test_vector_vs_ass1_dai <- t.test(vector_dai, ass1_oe_dai)
test_control_vs_ass1_dai <- t.test(control_dai, ass1_oe_dai)

control_vs_vector_p_dai <- test_control_vs_vector_dai$p.value
vector_vs_ass1_p_dai <- test_vector_vs_ass1_dai$p.value
control_vs_ass1_p_dai <- test_control_vs_ass1_dai$p.value

cat("Control vs Vector: p =", format.pval(control_vs_vector_p_dai, digits = 4), "\n")
cat("Vector vs ASS1_OE: p =", format.pval(vector_vs_ass1_p_dai, digits = 4), "\n")
cat("Control vs ASS1_OE: p =", format.pval(control_vs_ass1_p_dai, digits = 4), "\n")

# =============================================================================
# Colon Length数据处理和统计分析
# =============================================================================

data3_processed <-
  rbind(
    data.frame(group = "Control", value = data3$Control),
    data.frame(group = "Vector", value = data3$Vector),
    data.frame(group = "ASS1_OE", value = data3$`ASS1 OE`)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "ASS1_OE"))) %>%
  filter(!is.na(value))

# Colon Length统计分析（直接使用单次测量值）
control_colon <- data3_processed$value[data3_processed$group == "Control"]
vector_colon <- data3_processed$value[data3_processed$group == "Vector"]
ass1_oe_colon <- data3_processed$value[data3_processed$group == "ASS1_OE"]

cat("\n=== COLON LENGTH ANALYSIS ===\n")
cat("Sample sizes: Control =", length(control_colon), ", Vector =", length(vector_colon), ", ASS1_OE =", length(ass1_oe_colon), "\n")

test_control_vs_vector_colon <- t.test(control_colon, vector_colon)
test_vector_vs_ass1_colon <- t.test(vector_colon, ass1_oe_colon)
test_control_vs_ass1_colon <- t.test(control_colon, ass1_oe_colon)

control_vs_vector_p_colon <- test_control_vs_vector_colon$p.value
vector_vs_ass1_p_colon <- test_vector_vs_ass1_colon$p.value
control_vs_ass1_p_colon <- test_control_vs_ass1_colon$p.value

cat("Control vs Vector: p =", format.pval(control_vs_vector_p_colon, digits = 4), "\n")
cat("Vector vs ASS1_OE: p =", format.pval(vector_vs_ass1_p_colon, digits = 4), "\n")
cat("Control vs ASS1_OE: p =", format.pval(control_vs_ass1_p_colon, digits = 4), "\n")

# =============================================================================
# 基于平均值的结肠长度图形绘制（显示p值）
# =============================================================================

# 结肠长度 - 使用ggpubr显示基于平均值计算的p值
plot_colon_custom <- ggboxplot(data3_processed,
                               x = "group",
                               y = "value",
                               color = "group",
                               add = "jitter",
                               add.params = list(size = 3, alpha = 0.9)) +
  theme_base +
  labs(x = "", y = "Colon length (cm)", title = "Colon Length with Custom P-values") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color2)

# 手动添加p值注释
max_y <- max(data3_processed$value)
y_positions <- c(max_y * 1.05, max_y * 1.15, max_y * 1.25)

# 添加p值注释
plot_colon_custom <- plot_colon_custom +
  annotate("text", x = 1.5, y = y_positions[1],
           label = paste("p =", format.pval(control_vs_vector_p_colon, digits = 3)),
           size = 4) +
  annotate("segment", x = 1, xend = 2, y = y_positions[1] * 0.98, yend = y_positions[1] * 0.98) +
  annotate("text", x = 2.5, y = y_positions[2],
           label = paste("p =", format.pval(vector_vs_ass1_p_colon, digits = 3)),
           size = 4) +
  annotate("segment", x = 2, xend = 3, y = y_positions[2] * 0.98, yend = y_positions[2] * 0.98) +
  annotate("text", x = 2, y = y_positions[3],
           label = paste("p =", format.pval(control_vs_ass1_p_colon, digits = 3)),
           size = 4) +
  annotate("segment", x = 1, xend = 3, y = y_positions[3] * 0.98, yend = y_positions[3] * 0.98)

plot_colon_custom
ggsave(plot_colon_custom,
       filename = "colon_length_custom_pvalues.pdf",
       width = 6,
       height = 6)

# =============================================================================
# 创建基于平均值的体重和DAI数据框用于绘图
# =============================================================================

# 体重平均值数据框
weight_avg_df <- data.frame(
  group = c(rep("Control", length(control_weight)),
            rep("Vector", length(vector_weight)),
            rep("ASS1_OE", length(ass1_oe_weight))),
  value = c(control_weight, vector_weight, ass1_oe_weight)
) %>%
  mutate(group = factor(group, levels = c("Control", "Vector", "ASS1_OE")))

# DAI平均值数据框
dai_avg_df <- data.frame(
  group = c(rep("Control", length(control_dai)),
            rep("Vector", length(vector_dai)),
            rep("ASS1_OE", length(ass1_oe_dai))),
  value = c(control_dai, vector_dai, ass1_oe_dai)
) %>%
  mutate(group = factor(group, levels = c("Control", "Vector", "ASS1_OE")))

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
  scale_color_manual(values = disease_color2)

# 手动添加p值注释
max_y_weight <- max(weight_avg_df$value)
y_pos_weight <- c(max_y_weight * 1.02, max_y_weight * 1.06, max_y_weight * 1.10)

plot_weight_avg <- plot_weight_avg +
  annotate("text", x = 1.5, y = y_pos_weight[1],
           label = paste("p =", format.pval(control_vs_vector_p_weight, digits = 3)),
           size = 4) +
  annotate("segment", x = 1, xend = 2, y = y_pos_weight[1] * 0.999, yend = y_pos_weight[1] * 0.999) +
  annotate("text", x = 2.5, y = y_pos_weight[2],
           label = paste("p =", format.pval(vector_vs_ass1_p_weight, digits = 3)),
           size = 4) +
  annotate("segment", x = 2, xend = 3, y = y_pos_weight[2] * 0.999, yend = y_pos_weight[2] * 0.999) +
  annotate("text", x = 2, y = y_pos_weight[3],
           label = paste("p =", format.pval(control_vs_ass1_p_weight, digits = 3)),
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
  scale_color_manual(values = disease_color2)

# 手动添加p值注释
max_y_dai <- max(dai_avg_df$value)
y_pos_dai <- c(max_y_dai * 1.05, max_y_dai * 1.15, max_y_dai * 1.25)

plot_dai_avg <- plot_dai_avg +
  annotate("text", x = 1.5, y = y_pos_dai[1],
           label = paste("p =", format.pval(control_vs_vector_p_dai, digits = 3)),
           size = 4) +
  annotate("segment", x = 1, xend = 2, y = y_pos_dai[1] * 0.98, yend = y_pos_dai[1] * 0.98) +
  annotate("text", x = 2.5, y = y_pos_dai[2],
           label = paste("p =", format.pval(vector_vs_ass1_p_dai, digits = 3)),
           size = 4) +
  annotate("segment", x = 2, xend = 3, y = y_pos_dai[2] * 0.98, yend = y_pos_dai[2] * 0.98) +
  annotate("text", x = 2, y = y_pos_dai[3],
           label = paste("p =", format.pval(control_vs_ass1_p_dai, digits = 3)),
           size = 4) +
  annotate("segment", x = 1, xend = 3, y = y_pos_dai[3] * 0.98, yend = y_pos_dai[3] * 0.98)

plot_dai_avg
ggsave(plot_dai_avg,
       filename = "dai_average_pvalues.pdf",
       width = 6,
       height = 6)

# =============================================================================
# 统计结果汇总
# =============================================================================

# 创建基于平均值的完整结果表格
complete_results <- data.frame(
  Experiment = rep(c("Body_Weight_Avg", "DAI_Avg", "Colon_Length"), each = 3),
  Comparison = rep(c("Control_vs_Vector", "Vector_vs_ASS1_OE", "Control_vs_ASS1_OE"), 3),
  P_value = c(
    control_vs_vector_p_weight, vector_vs_ass1_p_weight, control_vs_ass1_p_weight,
    control_vs_vector_p_dai, vector_vs_ass1_p_dai, control_vs_ass1_p_dai,
    control_vs_vector_p_colon, vector_vs_ass1_p_colon, control_vs_ass1_p_colon
  ),
  Mean_Group1 = c(
    mean(control_weight), mean(vector_weight), mean(control_weight),
    mean(control_dai), mean(vector_dai), mean(control_dai),
    mean(control_colon), mean(vector_colon), mean(control_colon)
  ),
  Mean_Group2 = c(
    mean(vector_weight), mean(ass1_oe_weight), mean(ass1_oe_weight),
    mean(vector_dai), mean(ass1_oe_dai), mean(ass1_oe_dai),
    mean(vector_colon), mean(ass1_oe_colon), mean(ass1_oe_colon)
  )
)

complete_results$Mean_Difference <- complete_results$Mean_Group2 - complete_results$Mean_Group1
complete_results$Significance <- ifelse(complete_results$P_value < 0.001, "***",
                                        ifelse(complete_results$P_value < 0.01, "**",
                                               ifelse(complete_results$P_value < 0.05, "*", "ns")))

cat("\n=== COMPLETE RESULTS TABLE (Based on Averages) ===\n")
print(complete_results)

# 保存基于平均值的完整结果
write.csv(complete_results, "figure5a_average_based_statistical_analysis.csv", row.names = FALSE)
