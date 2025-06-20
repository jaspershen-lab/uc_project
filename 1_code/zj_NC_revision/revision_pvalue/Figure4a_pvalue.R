library(r4projects)
library(ggstatsplot)
library(ggpubr)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')



#############fig4a#######
# 读取数据
data1 <- readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx", sheet = 10)
data2 <- readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx", sheet = 11)
data3 <- readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx", sheet = 13)


dir.create(
  "3_data_analysis/zj_NC_revision/revision_pvalue/Figure4_pvalue",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/zj_NC_revision/revision_pvalue/Figure4_pvalue")

# =============================================================================
# Body Weight数据处理和统计分析
# =============================================================================

# 数据预处理
data1_processed <- data1 %>%
  as.data.frame() %>%
  rename_with(~ gsub("\\.\\.\\.", "_", .)) %>%
  tidyr::pivot_longer(cols = -Day,
                      names_to = "sample_id",
                      values_to = "value") %>%
  dplyr::mutate(group = case_when(
    grepl("Control", sample_id) ~ "Control",
    grepl("DSS", sample_id) ~ "DSS",
    grepl("Arg", sample_id) ~ "Arg",
    TRUE ~ "Unknown"
  )) %>%
  dplyr::mutate(Day = as.character(Day)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg"))) %>%
  filter(!is.na(value) & group != "Unknown")

# 计算每个动物的平均体重
animal_averages_weight <- data1_processed %>%
  group_by(sample_id, group) %>%
  summarise(avg_weight = mean(value, na.rm = TRUE), .groups = 'drop')

# 正确提取各组数据
control_weight <- animal_averages_weight$avg_weight[animal_averages_weight$group == "Control"]
dss_weight <- animal_averages_weight$avg_weight[animal_averages_weight$group == "DSS"]
arg_weight <- animal_averages_weight$avg_weight[animal_averages_weight$group == "Arg"]

# Body Weight统计分析
cat("=== BODY WEIGHT ANALYSIS ===\n")
cat("Sample sizes: Control =", length(control_weight), ", DSS =", length(dss_weight), ", Arg =", length(arg_weight), "\n")

# 进行t检验
test_control_vs_dss_weight <- t.test(control_weight, dss_weight)
test_dss_vs_arg_weight <- t.test(dss_weight, arg_weight)

control_vs_dss_p_weight <- test_control_vs_dss_weight$p.value
dss_vs_arg_p_weight <- test_dss_vs_arg_weight$p.value

cat("Control vs DSS: p =", format.pval(control_vs_dss_p_weight, digits = 4), "\n")
cat("DSS vs Arg: p =", format.pval(dss_vs_arg_p_weight, digits = 4), "\n")

# =============================================================================
# DAI数据处理和统计分析
# =============================================================================

# DAI数据预处理
colnames(data2)[1] <- "Day"
data2_processed <- data2 %>%
  as.data.frame() %>%
  rename_with(~ gsub("\\.\\.\\.", "_", .)) %>%
  tidyr::pivot_longer(cols = -Day,
                      names_to = "sample_id",
                      values_to = "value") %>%
  dplyr::mutate(group = case_when(
    grepl("Control", sample_id) ~ "Control",
    grepl("DSS", sample_id) ~ "DSS",
    grepl("Arg", sample_id) ~ "Arg",
    TRUE ~ "Unknown"
  )) %>%
  dplyr::mutate(Day = as.character(Day)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg"))) %>%
  filter(!is.na(value) & group != "Unknown")

# 计算每个动物的平均DAI
animal_averages_dai <- data2_processed %>%
  group_by(sample_id, group) %>%
  summarise(avg_dai = mean(value, na.rm = TRUE), .groups = 'drop')

# 正确提取各组DAI数据
control_dai <- animal_averages_dai$avg_dai[animal_averages_dai$group == "Control"]
dss_dai <- animal_averages_dai$avg_dai[animal_averages_dai$group == "DSS"]
arg_dai <- animal_averages_dai$avg_dai[animal_averages_dai$group == "Arg"]

# DAI统计分析
cat("\n=== DAI ANALYSIS ===\n")
cat("Sample sizes: Control =", length(control_dai), ", DSS =", length(dss_dai), ", Arg =", length(arg_dai), "\n")

test_control_vs_dss_dai <- t.test(control_dai, dss_dai)
test_dss_vs_arg_dai <- t.test(dss_dai, arg_dai)

control_vs_dss_p_dai <- test_control_vs_dss_dai$p.value
dss_vs_arg_p_dai <- test_dss_vs_arg_dai$p.value

cat("Control vs DSS: p =", format.pval(control_vs_dss_p_dai, digits = 4), "\n")
cat("DSS vs Arg: p =", format.pval(dss_vs_arg_p_dai, digits = 4), "\n")

# =============================================================================
# Colon Length数据处理和统计分析
# =============================================================================

data3_processed <- rbind(
  data.frame(group = "Control", value = data3$Control),
  data.frame(group = "DSS", value = data3$Model),
  data.frame(group = "Arg", value = data3$Arg)
) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg"))) %>%
  filter(!is.na(value))

# Colon Length统计分析
control_colon <- data3_processed$value[data3_processed$group == "Control"]
dss_colon <- data3_processed$value[data3_processed$group == "DSS"]
arg_colon <- data3_processed$value[data3_processed$group == "Arg"]

cat("\n=== COLON LENGTH ANALYSIS ===\n")
cat("Sample sizes: Control =", length(control_colon), ", DSS =", length(dss_colon), ", Arg =", length(arg_colon), "\n")

test_control_vs_dss_colon <- t.test(control_colon, dss_colon)
test_dss_vs_arg_colon <- t.test(dss_colon, arg_colon)
test_control_vs_arg_colon <- t.test(control_colon, arg_colon)

control_vs_dss_p_colon <- test_control_vs_dss_colon$p.value
dss_vs_arg_p_colon <- test_dss_vs_arg_colon$p.value
control_vs_arg_p_colon <- test_control_vs_arg_colon$p.value

cat("Control vs DSS: p =", format.pval(control_vs_dss_p_colon, digits = 4), "\n")
cat("DSS vs Arg: p =", format.pval(dss_vs_arg_p_colon, digits = 4), "\n")
cat("Control vs Arg: p =", format.pval(control_vs_arg_p_colon, digits = 4), "\n")


# =============================================================================
# 统计结果汇总
# =============================================================================
# 创建完整的结果表格
complete_results <- data.frame(
  Experiment = c("Body_Weight", "Body_Weight", "DAI", "DAI", "Colon_Length", "Colon_Length"),
  Comparison = rep(c("Control_vs_DSS", "DSS_vs_Arg"), 3),
  P_value = c(
    control_vs_dss_p_weight, dss_vs_arg_p_weight,
    control_vs_dss_p_dai, dss_vs_arg_p_dai,
    control_vs_dss_p_colon, dss_vs_arg_p_colon
  ),
  Significance = c(
    weight_control_vs_dss_sig, weight_dss_vs_arg_sig,
    dai_control_vs_dss_sig, dai_dss_vs_arg_sig,
    colon_control_vs_dss_sig, colon_dss_vs_arg_sig
  ),
  Mean_Group1 = c(
    mean(control_weight), mean(dss_weight),
    mean(control_dai), mean(dss_dai),
    mean(control_colon), mean(dss_colon)
  ),
  Mean_Group2 = c(
    mean(dss_weight), mean(arg_weight),
    mean(dss_dai), mean(arg_dai),
    mean(dss_colon), mean(arg_colon)
  )
)

complete_results$Mean_Difference <- complete_results$Mean_Group2 - complete_results$Mean_Group1

cat("\n=== COMPLETE RESULTS TABLE ===\n")
print(complete_results)

# 保存完整结果
write.csv(complete_results, "figure4a_complete_statistical_analysis.csv", row.names = FALSE)
