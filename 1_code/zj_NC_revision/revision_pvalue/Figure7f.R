library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')



# =============================================================================
# Figure 7f - 精氨酸水平 p值计算（基于原代码Fig9M, 9N）
# =============================================================================

cat("=== FIGURE 7F ARGININE LEVELS ANALYSIS ===\n")
cat("Analyzing arginine levels in colon tissue and blood\n\n")

###Figure 7f - 结肠组织精氨酸 (原代码中的Fig9M)
colon_arginine_data <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig9M")

###Figure 7f - 血液精氨酸 (原代码中的Fig9N)
blood_arginine_data <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig9N")


dir.create(
  "3_data_analysis/zj_NC_revision/revision_pvalue/Figure7_pvalue/Figure7f",
  showWarnings = FALSE,
  recursive = TRUE
)
setwd("3_data_analysis/zj_NC_revision/revision_pvalue/Figure7_pvalue/Figure7f")

# =============================================================================
# 1. 结肠组织精氨酸数据处理和统计分析
# =============================================================================

colon_arginine_processed <-
  rbind(
    data.frame(group = "Control", value = as.numeric(as.character(colon_arginine_data$Control))),
    data.frame(group = "DSS", value = as.numeric(as.character(colon_arginine_data$Model))),
    data.frame(group = "C_01", value = as.numeric(as.character(colon_arginine_data$`C-01`)))
  ) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "C_01")))

# 结肠组织精氨酸统计分析
control_colon_arg <- colon_arginine_processed$value[colon_arginine_processed$group == "Control"]
dss_colon_arg <- colon_arginine_processed$value[colon_arginine_processed$group == "DSS"]
c01_colon_arg <- colon_arginine_processed$value[colon_arginine_processed$group == "C_01"]

cat("=== COLON TISSUE ARGININE ANALYSIS ===\n")
cat("Sample sizes: Control =", length(control_colon_arg), ", DSS =", length(dss_colon_arg), ", C-01 =", length(c01_colon_arg), "\n")

test_control_vs_dss_colon_arg <- t.test(control_colon_arg, dss_colon_arg)
test_dss_vs_c01_colon_arg <- t.test(dss_colon_arg, c01_colon_arg)
test_control_vs_c01_colon_arg <- t.test(control_colon_arg, c01_colon_arg)

control_vs_dss_p_colon_arg <- test_control_vs_dss_colon_arg$p.value
dss_vs_c01_p_colon_arg <- test_dss_vs_c01_colon_arg$p.value
control_vs_c01_p_colon_arg <- test_control_vs_c01_colon_arg$p.value

cat("Control vs DSS: p =", format.pval(control_vs_dss_p_colon_arg, digits = 4), "\n")
cat("DSS vs C-01: p =", format.pval(dss_vs_c01_p_colon_arg, digits = 4), "\n")
cat("Control vs C-01: p =", format.pval(control_vs_c01_p_colon_arg, digits = 4), "\n")

# 计算均值和标准差
cat("\n结肠组织精氨酸水平 (均值 ± 标准差):\n")
cat("Control:", round(mean(control_colon_arg), 3), "±", round(sd(control_colon_arg), 3), "\n")
cat("DSS:", round(mean(dss_colon_arg), 3), "±", round(sd(dss_colon_arg), 3), "\n")
cat("C-01:", round(mean(c01_colon_arg), 3), "±", round(sd(c01_colon_arg), 3), "\n")

# 计算变化倍数
colon_fold_change_dss <- mean(dss_colon_arg) / mean(control_colon_arg)
colon_fold_change_c01 <- mean(c01_colon_arg) / mean(control_colon_arg)
colon_fold_change_c01_vs_dss <- mean(c01_colon_arg) / mean(dss_colon_arg)

cat("\n结肠组织精氨酸变化倍数:\n")
cat("DSS vs Control:", round(colon_fold_change_dss, 3), "倍\n")
cat("C-01 vs Control:", round(colon_fold_change_c01, 3), "倍\n")
cat("C-01 vs DSS:", round(colon_fold_change_c01_vs_dss, 3), "倍\n")

# =============================================================================
# 2. 血液精氨酸数据处理和统计分析
# =============================================================================

blood_arginine_processed <-
  rbind(
    data.frame(group = "Control", value = as.numeric(as.character(blood_arginine_data$Control))),
    data.frame(group = "DSS", value = as.numeric(as.character(blood_arginine_data$Model))),
    data.frame(group = "C_01", value = as.numeric(as.character(blood_arginine_data$`C-01`)))
  ) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "C_01")))

# 血液精氨酸统计分析
control_blood_arg <- blood_arginine_processed$value[blood_arginine_processed$group == "Control"]
dss_blood_arg <- blood_arginine_processed$value[blood_arginine_processed$group == "DSS"]
c01_blood_arg <- blood_arginine_processed$value[blood_arginine_processed$group == "C_01"]

cat("\n=== BLOOD ARGININE ANALYSIS ===\n")
cat("Sample sizes: Control =", length(control_blood_arg), ", DSS =", length(dss_blood_arg), ", C-01 =", length(c01_blood_arg), "\n")

test_control_vs_dss_blood_arg <- t.test(control_blood_arg, dss_blood_arg)
test_dss_vs_c01_blood_arg <- t.test(dss_blood_arg, c01_blood_arg)
test_control_vs_c01_blood_arg <- t.test(control_blood_arg, c01_blood_arg)

control_vs_dss_p_blood_arg <- test_control_vs_dss_blood_arg$p.value
dss_vs_c01_p_blood_arg <- test_dss_vs_c01_blood_arg$p.value
control_vs_c01_p_blood_arg <- test_control_vs_c01_blood_arg$p.value

cat("Control vs DSS: p =", format.pval(control_vs_dss_p_blood_arg, digits = 4), "\n")
cat("DSS vs C-01: p =", format.pval(dss_vs_c01_p_blood_arg, digits = 4), "\n")
cat("Control vs C-01: p =", format.pval(control_vs_c01_p_blood_arg, digits = 4), "\n")

# 计算均值和标准差
cat("\n血液精氨酸水平 (均值 ± 标准差):\n")
cat("Control:", round(mean(control_blood_arg), 3), "±", round(sd(control_blood_arg), 3), "\n")
cat("DSS:", round(mean(dss_blood_arg), 3), "±", round(sd(dss_blood_arg), 3), "\n")
cat("C-01:", round(mean(c01_blood_arg), 3), "±", round(sd(c01_blood_arg), 3), "\n")

# 计算变化倍数
blood_fold_change_dss <- mean(dss_blood_arg) / mean(control_blood_arg)
blood_fold_change_c01 <- mean(c01_blood_arg) / mean(control_blood_arg)
blood_fold_change_c01_vs_dss <- mean(c01_blood_arg) / mean(dss_blood_arg)

cat("\n血液精氨酸变化倍数:\n")
cat("DSS vs Control:", round(blood_fold_change_dss, 3), "倍\n")
cat("C-01 vs Control:", round(blood_fold_change_c01, 3), "倍\n")
cat("C-01 vs DSS:", round(blood_fold_change_c01_vs_dss, 3), "倍\n")

# =============================================================================
# 统计结果汇总
# =============================================================================

# 创建完整的结果表格
complete_results <- data.frame(
  Sample_Type = c(rep("Colon_Tissue", 3), rep("Blood", 3)),
  Comparison = rep(c("Control_vs_DSS", "DSS_vs_C01", "Control_vs_C01"), 2),
  P_value = c(
    control_vs_dss_p_colon_arg, dss_vs_c01_p_colon_arg, control_vs_c01_p_colon_arg,
    control_vs_dss_p_blood_arg, dss_vs_c01_p_blood_arg, control_vs_c01_p_blood_arg
  ),
  Mean_Group1 = c(
    mean(control_colon_arg), mean(dss_colon_arg), mean(control_colon_arg),
    mean(control_blood_arg), mean(dss_blood_arg), mean(control_blood_arg)
  ),
  Mean_Group2 = c(
    mean(dss_colon_arg), mean(c01_colon_arg), mean(c01_colon_arg),
    mean(dss_blood_arg), mean(c01_blood_arg), mean(c01_blood_arg)
  ),
  SD_Group1 = c(
    sd(control_colon_arg), sd(dss_colon_arg), sd(control_colon_arg),
    sd(control_blood_arg), sd(dss_blood_arg), sd(control_blood_arg)
  ),
  SD_Group2 = c(
    sd(dss_colon_arg), sd(c01_colon_arg), sd(c01_colon_arg),
    sd(dss_blood_arg), sd(c01_blood_arg), sd(c01_blood_arg)
  ),
  N_Group1 = c(
    length(control_colon_arg), length(dss_colon_arg), length(control_colon_arg),
    length(control_blood_arg), length(dss_blood_arg), length(control_blood_arg)
  ),
  N_Group2 = c(
    length(dss_colon_arg), length(c01_colon_arg), length(c01_colon_arg),
    length(dss_blood_arg), length(c01_blood_arg), length(c01_blood_arg)
  ),
  Fold_Change = c(
    colon_fold_change_dss, colon_fold_change_c01_vs_dss, colon_fold_change_c01,
    blood_fold_change_dss, blood_fold_change_c01_vs_dss, blood_fold_change_c01
  )
)

complete_results$Mean_Difference <- complete_results$Mean_Group2 - complete_results$Mean_Group1
complete_results$Percent_Change <- ((complete_results$Mean_Group2 - complete_results$Mean_Group1) / complete_results$Mean_Group1) * 100
complete_results$Significance <- ifelse(complete_results$P_value < 0.001, "***",
                                        ifelse(complete_results$P_value < 0.01, "**",
                                               ifelse(complete_results$P_value < 0.05, "*", "ns")))

cat("\n=== COMPLETE RESULTS TABLE (Figure 7f - Arginine Levels) ===\n")
print(complete_results)

# 保存结果
write.csv(complete_results, "figure7f_arginine_levels_statistical_analysis.csv", row.names = FALSE)

# =============================================================================
# 效应大小分析
# =============================================================================

# Cohen's d计算函数
cohens_d <- function(x, y) {
  pooled_sd <- sqrt(((length(x) - 1) * var(x) + (length(y) - 1) * var(y)) /
                      (length(x) + length(y) - 2))
  d <- (mean(x) - mean(y)) / pooled_sd
  return(d)
}

cat("\n=== 效应大小分析 (Cohen's d) ===\n")

cat("结肠组织精氨酸:\n")
cat("DSS vs Control: d =", round(cohens_d(dss_colon_arg, control_colon_arg), 3), "\n")
cat("C-01 vs DSS: d =", round(cohens_d(c01_colon_arg, dss_colon_arg), 3), "\n")
cat("C-01 vs Control: d =", round(cohens_d(c01_colon_arg, control_colon_arg), 3), "\n")

cat("\n血液精氨酸:\n")
cat("DSS vs Control: d =", round(cohens_d(dss_blood_arg, control_blood_arg), 3), "\n")
cat("C-01 vs DSS: d =", round(cohens_d(c01_blood_arg, dss_blood_arg), 3), "\n")
cat("C-01 vs Control: d =", round(cohens_d(c01_blood_arg, control_blood_arg), 3), "\n")

# =============================================================================
# 数据摘要
# =============================================================================

cat("\n=== 数据摘要 ===\n")

cat("结肠组织精氨酸摘要:\n")
colon_summary <- colon_arginine_processed %>%
  group_by(group) %>%
  summarise(
    n = n(),
    mean = round(mean(value), 3),
    sd = round(sd(value), 3),
    median = round(median(value), 3),
    min = round(min(value), 3),
    max = round(max(value), 3),
    .groups = 'drop'
  )
print(colon_summary)

cat("\n血液精氨酸摘要:\n")
blood_summary <- blood_arginine_processed %>%
  group_by(group) %>%
  summarise(
    n = n(),
    mean = round(mean(value), 3),
    sd = round(sd(value), 3),
    median = round(median(value), 3),
    min = round(min(value), 3),
    max = round(max(value), 3),
    .groups = 'drop'
  )
print(blood_summary)

# =============================================================================
# 治疗效果解读
# =============================================================================

cat("\n=== 治疗效果解读 ===\n")

cat("结肠组织精氨酸:\n")
if(control_vs_dss_p_colon_arg < 0.05) {
  cat("- DSS显著", ifelse(mean(dss_colon_arg) > mean(control_colon_arg), "增加", "减少"), "精氨酸水平\n")
}
if(dss_vs_c01_p_colon_arg < 0.05) {
  cat("- C-01治疗显著", ifelse(mean(c01_colon_arg) > mean(dss_colon_arg), "增加", "减少"), "精氨酸水平\n")
}
if(control_vs_c01_p_colon_arg < 0.05) {
  cat("- C-01治疗后精氨酸水平与对照组", ifelse(abs(mean(c01_colon_arg) - mean(control_colon_arg)) < 0.1, "接近", "不同"), "\n")
}

cat("\n血液精氨酸:\n")
if(control_vs_dss_p_blood_arg < 0.05) {
  cat("- DSS显著", ifelse(mean(dss_blood_arg) > mean(control_blood_arg), "增加", "减少"), "精氨酸水平\n")
}
if(dss_vs_c01_p_blood_arg < 0.05) {
  cat("- C-01治疗显著", ifelse(mean(c01_blood_arg) > mean(dss_blood_arg), "增加", "减少"), "精氨酸水平\n")
}
if(control_vs_c01_p_blood_arg < 0.05) {
  cat("- C-01治疗后精氨酸水平与对照组", ifelse(abs(mean(c01_blood_arg) - mean(control_blood_arg)) < 0.1, "接近", "不同"), "\n")
}
