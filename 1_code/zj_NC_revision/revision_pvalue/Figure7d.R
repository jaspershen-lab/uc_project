library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')


# =============================================================================
# Figure 7d - MPO, IL-6, IL-1β p值计算（基于原代码Fig9I, 9J, 9K）
# =============================================================================

###Figure 7d - MPO数据 (原代码中的Fig9I)
mpo_data <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig9I")

###Figure 7d - IL-6数据 (原代码中的Fig9J)
il6_data <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig9J")

###Figure 7d - IL-1β数据 (原代码中的Fig9K)
il1b_data <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig9K")


dir.create(
  "3_data_analysis/zj_NC_revision/revision_pvalue/Figure7_pvalue/Figure7d",
  showWarnings = FALSE,
  recursive = TRUE
)
setwd("3_data_analysis/zj_NC_revision/revision_pvalue/Figure7_pvalue/Figure7d")

# =============================================================================
# 1. MPO数据处理和统计分析
# =============================================================================

mpo_processed <-
  rbind(
    data.frame(group = "Control", value = as.numeric(as.character(mpo_data$Control))),
    data.frame(group = "DSS", value = as.numeric(as.character(mpo_data$Model))),
    data.frame(group = "C_01", value = as.numeric(as.character(mpo_data$`C-01`)))
  ) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "C_01")))

# MPO统计分析
control_mpo <- mpo_processed$value[mpo_processed$group == "Control"]
dss_mpo <- mpo_processed$value[mpo_processed$group == "DSS"]
c01_mpo <- mpo_processed$value[mpo_processed$group == "C_01"]

cat("=== MPO ANALYSIS (Figure 7d) ===\n")
cat("Sample sizes: Control =", length(control_mpo), ", DSS =", length(dss_mpo), ", C-01 =", length(c01_mpo), "\n")

test_control_vs_dss_mpo <- t.test(control_mpo, dss_mpo)
test_dss_vs_c01_mpo <- t.test(dss_mpo, c01_mpo)
test_control_vs_c01_mpo <- t.test(control_mpo, c01_mpo)

control_vs_dss_p_mpo <- test_control_vs_dss_mpo$p.value
dss_vs_c01_p_mpo <- test_dss_vs_c01_mpo$p.value
control_vs_c01_p_mpo <- test_control_vs_c01_mpo$p.value

cat("Control vs DSS: p =", format.pval(control_vs_dss_p_mpo, digits = 4), "\n")
cat("DSS vs C-01: p =", format.pval(dss_vs_c01_p_mpo, digits = 4), "\n")
cat("Control vs C-01: p =", format.pval(control_vs_c01_p_mpo, digits = 4), "\n")

# =============================================================================
# 2. IL-6数据处理和统计分析
# =============================================================================

il6_processed <-
  rbind(
    data.frame(group = "Control", value = as.numeric(as.character(il6_data$Control))),
    data.frame(group = "DSS", value = as.numeric(as.character(il6_data$Model))),
    data.frame(group = "C_01", value = as.numeric(as.character(il6_data$`C-01`)))
  ) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "C_01")))

# IL-6统计分析
control_il6 <- il6_processed$value[il6_processed$group == "Control"]
dss_il6 <- il6_processed$value[il6_processed$group == "DSS"]
c01_il6 <- il6_processed$value[il6_processed$group == "C_01"]

cat("\n=== IL-6 ANALYSIS (Figure 7d) ===\n")
cat("Sample sizes: Control =", length(control_il6), ", DSS =", length(dss_il6), ", C-01 =", length(c01_il6), "\n")

test_control_vs_dss_il6 <- t.test(control_il6, dss_il6)
test_dss_vs_c01_il6 <- t.test(dss_il6, c01_il6)
test_control_vs_c01_il6 <- t.test(control_il6, c01_il6)

control_vs_dss_p_il6 <- test_control_vs_dss_il6$p.value
dss_vs_c01_p_il6 <- test_dss_vs_c01_il6$p.value
control_vs_c01_p_il6 <- test_control_vs_c01_il6$p.value

cat("Control vs DSS: p =", format.pval(control_vs_dss_p_il6, digits = 4), "\n")
cat("DSS vs C-01: p =", format.pval(dss_vs_c01_p_il6, digits = 4), "\n")
cat("Control vs C-01: p =", format.pval(control_vs_c01_p_il6, digits = 4), "\n")

# =============================================================================
# 3. IL-1β数据处理和统计分析
# =============================================================================

il1b_processed <-
  rbind(
    data.frame(group = "Control", value = as.numeric(as.character(il1b_data$Control))),
    data.frame(group = "DSS", value = as.numeric(as.character(il1b_data$Model))),
    data.frame(group = "C_01", value = as.numeric(as.character(il1b_data$`C-01`)))
  ) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "C_01")))

# IL-1β统计分析
control_il1b <- il1b_processed$value[il1b_processed$group == "Control"]
dss_il1b <- il1b_processed$value[il1b_processed$group == "DSS"]
c01_il1b <- il1b_processed$value[il1b_processed$group == "C_01"]

cat("\n=== IL-1β ANALYSIS (Figure 7d) ===\n")
cat("Sample sizes: Control =", length(control_il1b), ", DSS =", length(dss_il1b), ", C-01 =", length(c01_il1b), "\n")

test_control_vs_dss_il1b <- t.test(control_il1b, dss_il1b)
test_dss_vs_c01_il1b <- t.test(dss_il1b, c01_il1b)
test_control_vs_c01_il1b <- t.test(control_il1b, c01_il1b)

control_vs_dss_p_il1b <- test_control_vs_dss_il1b$p.value
dss_vs_c01_p_il1b <- test_dss_vs_c01_il1b$p.value
control_vs_c01_p_il1b <- test_control_vs_c01_il1b$p.value

cat("Control vs DSS: p =", format.pval(control_vs_dss_p_il1b, digits = 4), "\n")
cat("DSS vs C-01: p =", format.pval(dss_vs_c01_p_il1b, digits = 4), "\n")
cat("Control vs C-01: p =", format.pval(control_vs_c01_p_il1b, digits = 4), "\n")

# =============================================================================
# 统计结果汇总
# =============================================================================

# 创建完整的结果表格
complete_results <- data.frame(
  Experiment = c(rep("MPO", 3), rep("IL6", 3), rep("IL1B", 3)),
  Comparison = rep(c("Control_vs_DSS", "DSS_vs_C01", "Control_vs_C01"), 3),
  P_value = c(
    control_vs_dss_p_mpo, dss_vs_c01_p_mpo, control_vs_c01_p_mpo,
    control_vs_dss_p_il6, dss_vs_c01_p_il6, control_vs_c01_p_il6,
    control_vs_dss_p_il1b, dss_vs_c01_p_il1b, control_vs_c01_p_il1b
  ),
  Mean_Group1 = c(
    mean(control_mpo), mean(dss_mpo), mean(control_mpo),
    mean(control_il6), mean(dss_il6), mean(control_il6),
    mean(control_il1b), mean(dss_il1b), mean(control_il1b)
  ),
  Mean_Group2 = c(
    mean(dss_mpo), mean(c01_mpo), mean(c01_mpo),
    mean(dss_il6), mean(c01_il6), mean(c01_il6),
    mean(dss_il1b), mean(c01_il1b), mean(c01_il1b)
  ),
  SD_Group1 = c(
    sd(control_mpo), sd(dss_mpo), sd(control_mpo),
    sd(control_il6), sd(dss_il6), sd(control_il6),
    sd(control_il1b), sd(dss_il1b), sd(control_il1b)
  ),
  SD_Group2 = c(
    sd(dss_mpo), sd(c01_mpo), sd(c01_mpo),
    sd(dss_il6), sd(c01_il6), sd(c01_il6),
    sd(dss_il1b), sd(c01_il1b), sd(c01_il1b)
  ),
  N_Group1 = c(
    length(control_mpo), length(dss_mpo), length(control_mpo),
    length(control_il6), length(dss_il6), length(control_il6),
    length(control_il1b), length(dss_il1b), length(control_il1b)
  ),
  N_Group2 = c(
    length(dss_mpo), length(c01_mpo), length(c01_mpo),
    length(dss_il6), length(c01_il6), length(c01_il6),
    length(dss_il1b), length(c01_il1b), length(c01_il1b)
  )
)

complete_results$Mean_Difference <- complete_results$Mean_Group2 - complete_results$Mean_Group1
complete_results$Significance <- ifelse(complete_results$P_value < 0.001, "***",
                                        ifelse(complete_results$P_value < 0.01, "**",
                                               ifelse(complete_results$P_value < 0.05, "*", "ns")))

cat("\n=== COMPLETE RESULTS TABLE (Figure 7d - MPO, IL-6, IL-1β) ===\n")
print(complete_results)

# 保存结果
write.csv(complete_results, "figure7d_cytokines_statistical_analysis.csv", row.names = FALSE)

# 输出数据摘要
cat("\n=== 数据摘要 ===\n")

cat("MPO摘要:\n")
mpo_summary <- mpo_processed %>%
  group_by(group) %>%
  summarise(
    n = n(),
    mean = round(mean(value), 3),
    sd = round(sd(value), 3),
    median = round(median(value), 3),
    .groups = 'drop'
  )
print(mpo_summary)

cat("\nIL-6摘要:\n")
il6_summary <- il6_processed %>%
  group_by(group) %>%
  summarise(
    n = n(),
    mean = round(mean(value), 3),
    sd = round(sd(value), 3),
    median = round(median(value), 3),
    .groups = 'drop'
  )
print(il6_summary)

cat("\nIL-1β摘要:\n")
il1b_summary <- il1b_processed %>%
  group_by(group) %>%
  summarise(
    n = n(),
    mean = round(mean(value), 3),
    sd = round(sd(value), 3),
    median = round(median(value), 3),
    .groups = 'drop'
  )
print(il1b_summary)

# 效应大小计算（Cohen's d）
cat("\n=== 效应大小 (Cohen's d) ===\n")

# 计算Cohen's d的函数
cohens_d <- function(x, y) {
  pooled_sd <- sqrt(((length(x) - 1) * var(x) + (length(y) - 1) * var(y)) /
                      (length(x) + length(y) - 2))
  d <- (mean(x) - mean(y)) / pooled_sd
  return(d)
}

cat("MPO效应大小:\n")
cat("DSS vs Control: d =", round(cohens_d(dss_mpo, control_mpo), 3), "\n")
cat("C-01 vs DSS: d =", round(cohens_d(c01_mpo, dss_mpo), 3), "\n")

cat("\nIL-6效应大小:\n")
cat("DSS vs Control: d =", round(cohens_d(dss_il6, control_il6), 3), "\n")
cat("C-01 vs DSS: d =", round(cohens_d(c01_il6, dss_il6), 3), "\n")

cat("\nIL-1β效应大小:\n")
cat("DSS vs Control: d =", round(cohens_d(dss_il1b, control_il1b), 3), "\n")
cat("C-01 vs DSS: d =", round(cohens_d(c01_il1b, dss_il1b), 3), "\n")
