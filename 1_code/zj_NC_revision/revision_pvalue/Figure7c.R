library(r4projects)
library(ggstatsplot)
library(ggpubr)
library(ez)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')


# =============================================================================
# Figure 7c - 完整分析：DAI + Colon Length + H-score
# =============================================================================

###Figure 7c - DAI数据 (原代码中的Fig9E)
dai_data <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig9E")

###Figure 7c - Colon Length数据 (原代码中的Fig9F)
colon_data <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig9F")

###Figure 7c - H-score数据 (原代码中的Fig9H)
hscore_data <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig9H")

colnames(dai_data)[1] <- "Day"


dir.create(
  "3_data_analysis/zj_NC_revision/revision_pvalue/Figure7_pvalue/Figure7c",
  showWarnings = FALSE,
  recursive = TRUE
)
setwd("3_data_analysis/zj_NC_revision/revision_pvalue/Figure7_pvalue/Figure7c")

# =============================================================================
# 1. DAI数据处理和基于平均值的统计分析
# =============================================================================

# DAI数据预处理
dai_processed <-
  dai_data %>%
  as.data.frame() %>%
  rename_with( ~ gsub("\\.\\.\\.", "_", .)) %>%
  tidyr::pivot_longer(cols = -Day,
                      names_to = "sample_id",
                      values_to = "value") %>%
  dplyr::mutate(group = case_when(
    grepl("Control", sample_id) ~ "Control",
    grepl("Model", sample_id) ~ "DSS",
    grepl("MDLA", sample_id) ~ "MDLA",  # 如果有MDLA数据
    grepl("C-01", sample_id) ~ "C_01"
  )) %>%
  dplyr::mutate(Day = as.character(Day)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "C_01"))) %>%
  filter(!is.na(value))

# 计算每个动物的平均DAI
animal_averages_dai <- dai_processed %>%
  group_by(sample_id, group) %>%
  summarise(avg_dai = mean(value, na.rm = TRUE), .groups = 'drop')

# 提取各组平均DAI数据
control_dai <- animal_averages_dai$avg_dai[animal_averages_dai$group == "Control"]
dss_dai <- animal_averages_dai$avg_dai[animal_averages_dai$group == "DSS"]
c01_dai <- animal_averages_dai$avg_dai[animal_averages_dai$group == "C_01"]

# DAI统计分析
cat("=== DAI ANALYSIS (Figure 7c) ===\n")
cat("Sample sizes: Control =", length(control_dai), ", DSS =", length(dss_dai), ", C-01 =", length(c01_dai), "\n")

# 进行t检验
test_control_vs_dss_dai <- t.test(control_dai, dss_dai)
test_dss_vs_c01_dai <- t.test(dss_dai, c01_dai)
test_control_vs_c01_dai <- t.test(control_dai, c01_dai)

control_vs_dss_p_dai <- test_control_vs_dss_dai$p.value
dss_vs_c01_p_dai <- test_dss_vs_c01_dai$p.value
control_vs_c01_p_dai <- test_control_vs_c01_dai$p.value

cat("Control vs DSS: p =", format.pval(control_vs_dss_p_dai, digits = 4), "\n")
cat("DSS vs C-01: p =", format.pval(dss_vs_c01_p_dai, digits = 4), "\n")
cat("Control vs C-01: p =", format.pval(control_vs_c01_p_dai, digits = 4), "\n")

# =============================================================================
# 2. Colon Length数据处理和统计分析（单次测量，直接t检验）
# =============================================================================

colon_processed <-
  rbind(
    data.frame(group = "Control", value = as.numeric(as.character(colon_data$Control))),
    data.frame(group = "DSS", value = as.numeric(as.character(colon_data$Model))),
    data.frame(group = "C_01", value = as.numeric(as.character(colon_data$`C-01`)))
  ) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "C_01")))

# Colon Length统计分析
control_colon <- colon_processed$value[colon_processed$group == "Control"]
dss_colon <- colon_processed$value[colon_processed$group == "DSS"]
c01_colon <- colon_processed$value[colon_processed$group == "C_01"]

cat("\n=== COLON LENGTH ANALYSIS (Figure 7c) ===\n")
cat("Sample sizes: Control =", length(control_colon), ", DSS =", length(dss_colon), ", C-01 =", length(c01_colon), "\n")

test_control_vs_dss_colon <- t.test(control_colon, dss_colon)
test_dss_vs_c01_colon <- t.test(dss_colon, c01_colon)
test_control_vs_c01_colon <- t.test(control_colon, c01_colon)

control_vs_dss_p_colon <- test_control_vs_dss_colon$p.value
dss_vs_c01_p_colon <- test_dss_vs_c01_colon$p.value
control_vs_c01_p_colon <- test_control_vs_c01_colon$p.value

cat("Control vs DSS: p =", format.pval(control_vs_dss_p_colon, digits = 4), "\n")
cat("DSS vs C-01: p =", format.pval(dss_vs_c01_p_colon, digits = 4), "\n")
cat("Control vs C-01: p =", format.pval(control_vs_c01_p_colon, digits = 4), "\n")

# =============================================================================
# 3. H-score数据处理和统计分析（单次测量，直接t检验）
# =============================================================================

hscore_processed <-
  rbind(
    data.frame(group = "Control", value = as.numeric(as.character(hscore_data$Control))),
    data.frame(group = "DSS", value = as.numeric(as.character(hscore_data$Model))),
    data.frame(group = "C_01", value = as.numeric(as.character(hscore_data$`C-01`)))
  ) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "C_01")))

# H-score统计分析
control_hscore <- hscore_processed$value[hscore_processed$group == "Control"]
dss_hscore <- hscore_processed$value[hscore_processed$group == "DSS"]
c01_hscore <- hscore_processed$value[hscore_processed$group == "C_01"]

cat("\n=== H-SCORE ANALYSIS (Figure 7c) ===\n")
cat("Sample sizes: Control =", length(control_hscore), ", DSS =", length(dss_hscore), ", C-01 =", length(c01_hscore), "\n")

test_control_vs_dss_hscore <- t.test(control_hscore, dss_hscore)
test_dss_vs_c01_hscore <- t.test(dss_hscore, c01_hscore)
test_control_vs_c01_hscore <- t.test(control_hscore, c01_hscore)

control_vs_dss_p_hscore <- test_control_vs_dss_hscore$p.value
dss_vs_c01_p_hscore <- test_dss_vs_c01_hscore$p.value
control_vs_c01_p_hscore <- test_control_vs_c01_hscore$p.value

cat("Control vs DSS: p =", format.pval(control_vs_dss_p_hscore, digits = 4), "\n")
cat("DSS vs C-01: p =", format.pval(dss_vs_c01_p_hscore, digits = 4), "\n")
cat("Control vs C-01: p =", format.pval(control_vs_c01_p_hscore, digits = 4), "\n")

# =============================================================================
# 统计结果汇总
# =============================================================================

# 创建完整的结果表格
complete_results <- data.frame(
  Experiment = c(rep("DAI_Average", 3), rep("Colon_Length", 3), rep("H_Score", 3)),
  Comparison = rep(c("Control_vs_DSS", "DSS_vs_C01", "Control_vs_C01"), 3),
  P_value = c(
    control_vs_dss_p_dai, dss_vs_c01_p_dai, control_vs_c01_p_dai,
    control_vs_dss_p_colon, dss_vs_c01_p_colon, control_vs_c01_p_colon,
    control_vs_dss_p_hscore, dss_vs_c01_p_hscore, control_vs_c01_p_hscore
  ),
  Mean_Group1 = c(
    mean(control_dai), mean(dss_dai), mean(control_dai),
    mean(control_colon), mean(dss_colon), mean(control_colon),
    mean(control_hscore), mean(dss_hscore), mean(control_hscore)
  ),
  Mean_Group2 = c(
    mean(dss_dai), mean(c01_dai), mean(c01_dai),
    mean(dss_colon), mean(c01_colon), mean(c01_colon),
    mean(dss_hscore), mean(c01_hscore), mean(c01_hscore)
  ),
  SD_Group1 = c(
    sd(control_dai), sd(dss_dai), sd(control_dai),
    sd(control_colon), sd(dss_colon), sd(control_colon),
    sd(control_hscore), sd(dss_hscore), sd(control_hscore)
  ),
  SD_Group2 = c(
    sd(dss_dai), sd(c01_dai), sd(c01_dai),
    sd(dss_colon), sd(c01_colon), sd(c01_colon),
    sd(dss_hscore), sd(c01_hscore), sd(c01_hscore)
  ),
  N_Group1 = c(
    length(control_dai), length(dss_dai), length(control_dai),
    length(control_colon), length(dss_colon), length(control_colon),
    length(control_hscore), length(dss_hscore), length(control_hscore)
  ),
  N_Group2 = c(
    length(dss_dai), length(c01_dai), length(c01_dai),
    length(dss_colon), length(c01_colon), length(c01_colon),
    length(dss_hscore), length(c01_hscore), length(c01_hscore)
  )
)

complete_results$Mean_Difference <- complete_results$Mean_Group2 - complete_results$Mean_Group1
complete_results$Significance <- ifelse(complete_results$P_value < 0.001, "***",
                                        ifelse(complete_results$P_value < 0.01, "**",
                                               ifelse(complete_results$P_value < 0.05, "*", "ns")))

cat("\n=== COMPLETE RESULTS TABLE (Figure 7c - DAI, Colon Length, H-score) ===\n")
print(complete_results)

# 保存完整结果
write.csv(complete_results, "figure7c_complete_statistical_analysis.csv", row.names = FALSE)
