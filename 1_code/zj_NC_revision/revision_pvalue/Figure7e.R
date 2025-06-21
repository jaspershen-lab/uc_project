library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/zj_NC_revision/revision_pvalue/Figure7_pvalue/Figure7e",
  showWarnings = FALSE,
  recursive = TRUE
)
setwd("3_data_analysis/zj_NC_revision/revision_pvalue/Figure7_pvalue/Figure7e")

# =============================================================================
# Figure 7e - Western Blot 5个蛋白 p值计算（基于原代码Fig9L数据）
# =============================================================================

cat("=== FIGURE 7E WESTERN BLOT ANALYSIS ===\n")
cat("Analyzing 5 proteins: ASS1, COX2, p-STAT3/STAT3, p-mTOR/mTOR, p-S6/S6\n\n")

# =============================================================================
# 1. ASS1蛋白数据和统计分析
# =============================================================================

ass1 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.932496003, 0.945467085, 0.85711974)
    ),
    data.frame(
      group = "Control+C-01",
      value = c(0.843728177, 0.767791352, 0.71317981)
    ),
    data.frame(
      group = "Model",
      value = c(1.137810984, 1.119667318, 1.049997849)
    ),
    data.frame(
      group = "Model+C-01",
      value = c(1.033551823, 1.03744504, 0.790372832)
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("Control", "Control+C-01", "Model", "Model+C-01")
  ))

# ASS1统计分析
control_ass1 <- ass1$value[ass1$group == "Control"]
control_c01_ass1 <- ass1$value[ass1$group == "Control+C-01"]
model_ass1 <- ass1$value[ass1$group == "Model"]
model_c01_ass1 <- ass1$value[ass1$group == "Model+C-01"]

cat("=== ASS1 ANALYSIS ===\n")
cat("Sample sizes: Control =", length(control_ass1), ", Control+C-01 =", length(control_c01_ass1),
    ", Model =", length(model_ass1), ", Model+C-01 =", length(model_c01_ass1), "\n")

# 主要比较
test_control_vs_control_c01_ass1 <- t.test(control_ass1, control_c01_ass1)
test_model_vs_model_c01_ass1 <- t.test(model_ass1, model_c01_ass1)
test_control_vs_model_ass1 <- t.test(control_ass1, model_ass1)
test_control_c01_vs_model_c01_ass1 <- t.test(control_c01_ass1, model_c01_ass1)

control_vs_control_c01_p_ass1 <- test_control_vs_control_c01_ass1$p.value
model_vs_model_c01_p_ass1 <- test_model_vs_model_c01_ass1$p.value
control_vs_model_p_ass1 <- test_control_vs_model_ass1$p.value
control_c01_vs_model_c01_p_ass1 <- test_control_c01_vs_model_c01_ass1$p.value

cat("Control vs Control+C-01: p =", format.pval(control_vs_control_c01_p_ass1, digits = 4), "\n")
cat("Model vs Model+C-01: p =", format.pval(model_vs_model_c01_p_ass1, digits = 4), "\n")
cat("Control vs Model: p =", format.pval(control_vs_model_p_ass1, digits = 4), "\n")
cat("Control+C-01 vs Model+C-01: p =", format.pval(control_c01_vs_model_c01_p_ass1, digits = 4), "\n")

# =============================================================================
# 2. COX2蛋白数据和统计分析
# =============================================================================

cox2 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.24216383, 0.359695875, 0.446936391)
    ),
    data.frame(
      group = "Control+C-01",
      value = c(0.209963008, 0.264762331, 0.433598685)
    ),
    data.frame(
      group = "Model",
      value = c(0.99362222, 1.068268923, 0.94746889)
    ),
    data.frame(
      group = "Model+C-01",
      value = c(0.508863567, 0.450994016, 0.635114066)
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("Control", "Control+C-01", "Model", "Model+C-01")
  ))

# COX2统计分析
control_cox2 <- cox2$value[cox2$group == "Control"]
control_c01_cox2 <- cox2$value[cox2$group == "Control+C-01"]
model_cox2 <- cox2$value[cox2$group == "Model"]
model_c01_cox2 <- cox2$value[cox2$group == "Model+C-01"]

cat("\n=== COX2 ANALYSIS ===\n")
cat("Sample sizes: Control =", length(control_cox2), ", Control+C-01 =", length(control_c01_cox2),
    ", Model =", length(model_cox2), ", Model+C-01 =", length(model_c01_cox2), "\n")

test_control_vs_control_c01_cox2 <- t.test(control_cox2, control_c01_cox2)
test_model_vs_model_c01_cox2 <- t.test(model_cox2, model_c01_cox2)
test_control_vs_model_cox2 <- t.test(control_cox2, model_cox2)
test_control_c01_vs_model_c01_cox2 <- t.test(control_c01_cox2, model_c01_cox2)

control_vs_control_c01_p_cox2 <- test_control_vs_control_c01_cox2$p.value
model_vs_model_c01_p_cox2 <- test_model_vs_model_c01_cox2$p.value
control_vs_model_p_cox2 <- test_control_vs_model_cox2$p.value
control_c01_vs_model_c01_p_cox2 <- test_control_c01_vs_model_c01_cox2$p.value

cat("Control vs Control+C-01: p =", format.pval(control_vs_control_c01_p_cox2, digits = 4), "\n")
cat("Model vs Model+C-01: p =", format.pval(model_vs_model_c01_p_cox2, digits = 4), "\n")
cat("Control vs Model: p =", format.pval(control_vs_model_p_cox2, digits = 4), "\n")
cat("Control+C-01 vs Model+C-01: p =", format.pval(control_c01_vs_model_c01_p_cox2, digits = 4), "\n")

# =============================================================================
# 3. p-STAT3/STAT3蛋白数据和统计分析
# =============================================================================

p_stat3_stat3 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.454174061, 0.373788725, 0.16910512)
    ),
    data.frame(
      group = "Control+C-01",
      value = c(0.491016867, 0.566680744, 0.349992236)
    ),
    data.frame(
      group = "Model",
      value = c(1.346611532, 1.041260504, 0.990059299)
    ),
    data.frame(
      group = "Model+C-01",
      value = c(0.391008949, 0.481233638, 0.272249052)
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("Control", "Control+C-01", "Model", "Model+C-01")
  ))

# p-STAT3/STAT3统计分析
control_stat3 <- p_stat3_stat3$value[p_stat3_stat3$group == "Control"]
control_c01_stat3 <- p_stat3_stat3$value[p_stat3_stat3$group == "Control+C-01"]
model_stat3 <- p_stat3_stat3$value[p_stat3_stat3$group == "Model"]
model_c01_stat3 <- p_stat3_stat3$value[p_stat3_stat3$group == "Model+C-01"]

cat("\n=== p-STAT3/STAT3 ANALYSIS ===\n")
cat("Sample sizes: Control =", length(control_stat3), ", Control+C-01 =", length(control_c01_stat3),
    ", Model =", length(model_stat3), ", Model+C-01 =", length(model_c01_stat3), "\n")

test_control_vs_control_c01_stat3 <- t.test(control_stat3, control_c01_stat3)
test_model_vs_model_c01_stat3 <- t.test(model_stat3, model_c01_stat3)
test_control_vs_model_stat3 <- t.test(control_stat3, model_stat3)
test_control_c01_vs_model_c01_stat3 <- t.test(control_c01_stat3, model_c01_stat3)

control_vs_control_c01_p_stat3 <- test_control_vs_control_c01_stat3$p.value
model_vs_model_c01_p_stat3 <- test_model_vs_model_c01_stat3$p.value
control_vs_model_p_stat3 <- test_control_vs_model_stat3$p.value
control_c01_vs_model_c01_p_stat3 <- test_control_c01_vs_model_c01_stat3$p.value

cat("Control vs Control+C-01: p =", format.pval(control_vs_control_c01_p_stat3, digits = 4), "\n")
cat("Model vs Model+C-01: p =", format.pval(model_vs_model_c01_p_stat3, digits = 4), "\n")
cat("Control vs Model: p =", format.pval(control_vs_model_p_stat3, digits = 4), "\n")
cat("Control+C-01 vs Model+C-01: p =", format.pval(control_c01_vs_model_c01_p_stat3, digits = 4), "\n")

# =============================================================================
# 4. p-mTOR/mTOR蛋白数据和统计分析
# =============================================================================

p_mtor_mtor <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.424938745, 0.346677812, 0.789497748)
    ),
    data.frame(
      group = "Control+C-01",
      value = c(0.580617642, 0.602262815, 0.67246608)
    ),
    data.frame(
      group = "Model",
      value = c(1.265325664, 0.903722882, 1.487364037)
    ),
    data.frame(
      group = "Model+C-01",
      value = c(0.563522038, 0.722959039, 0.869416008)
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("Control", "Control+C-01", "Model", "Model+C-01")
  ))

# p-mTOR/mTOR统计分析
control_mtor <- p_mtor_mtor$value[p_mtor_mtor$group == "Control"]
control_c01_mtor <- p_mtor_mtor$value[p_mtor_mtor$group == "Control+C-01"]
model_mtor <- p_mtor_mtor$value[p_mtor_mtor$group == "Model"]
model_c01_mtor <- p_mtor_mtor$value[p_mtor_mtor$group == "Model+C-01"]

cat("\n=== p-mTOR/mTOR ANALYSIS ===\n")
cat("Sample sizes: Control =", length(control_mtor), ", Control+C-01 =", length(control_c01_mtor),
    ", Model =", length(model_mtor), ", Model+C-01 =", length(model_c01_mtor), "\n")

test_control_vs_control_c01_mtor <- t.test(control_mtor, control_c01_mtor)
test_model_vs_model_c01_mtor <- t.test(model_mtor, model_c01_mtor)
test_control_vs_model_mtor <- t.test(control_mtor, model_mtor)
test_control_c01_vs_model_c01_mtor <- t.test(control_c01_mtor, model_c01_mtor)

control_vs_control_c01_p_mtor <- test_control_vs_control_c01_mtor$p.value
model_vs_model_c01_p_mtor <- test_model_vs_model_c01_mtor$p.value
control_vs_model_p_mtor <- test_control_vs_model_mtor$p.value
control_c01_vs_model_c01_p_mtor <- test_control_c01_vs_model_c01_mtor$p.value

cat("Control vs Control+C-01: p =", format.pval(control_vs_control_c01_p_mtor, digits = 4), "\n")
cat("Model vs Model+C-01: p =", format.pval(model_vs_model_c01_p_mtor, digits = 4), "\n")
cat("Control vs Model: p =", format.pval(control_vs_model_p_mtor, digits = 4), "\n")
cat("Control+C-01 vs Model+C-01: p =", format.pval(control_c01_vs_model_c01_p_mtor, digits = 4), "\n")

# =============================================================================
# 5. p-S6/S6蛋白数据和统计分析
# =============================================================================

p_s6_s6 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.223036828, 0.168153833, 0.26598216)
    ),
    data.frame(
      group = "Control+C-01",
      value = c(0.271514135, 0.291666223, 0.366328349)
    ),
    data.frame(
      group = "Model",
      value = c(1.24010449, 1.206885064, 1.147673926)
    ),
    data.frame(
      group = "Model+C-01",
      value = c(0.472225336, 0.627325837, 0.569366437)
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("Control", "Control+C-01", "Model", "Model+C-01")
  ))

# p-S6/S6统计分析
control_s6 <- p_s6_s6$value[p_s6_s6$group == "Control"]
control_c01_s6 <- p_s6_s6$value[p_s6_s6$group == "Control+C-01"]
model_s6 <- p_s6_s6$value[p_s6_s6$group == "Model"]
model_c01_s6 <- p_s6_s6$value[p_s6_s6$group == "Model+C-01"]

cat("\n=== p-S6/S6 ANALYSIS ===\n")
cat("Sample sizes: Control =", length(control_s6), ", Control+C-01 =", length(control_c01_s6),
    ", Model =", length(model_s6), ", Model+C-01 =", length(model_c01_s6), "\n")

test_control_vs_control_c01_s6 <- t.test(control_s6, control_c01_s6)
test_model_vs_model_c01_s6 <- t.test(model_s6, model_c01_s6)
test_control_vs_model_s6 <- t.test(control_s6, model_s6)
test_control_c01_vs_model_c01_s6 <- t.test(control_c01_s6, model_c01_s6)

control_vs_control_c01_p_s6 <- test_control_vs_control_c01_s6$p.value
model_vs_model_c01_p_s6 <- test_model_vs_model_c01_s6$p.value
control_vs_model_p_s6 <- test_control_vs_model_s6$p.value
control_c01_vs_model_c01_p_s6 <- test_control_c01_vs_model_c01_s6$p.value

cat("Control vs Control+C-01: p =", format.pval(control_vs_control_c01_p_s6, digits = 4), "\n")
cat("Model vs Model+C-01: p =", format.pval(model_vs_model_c01_p_s6, digits = 4), "\n")
cat("Control vs Model: p =", format.pval(control_vs_model_p_s6, digits = 4), "\n")
cat("Control+C-01 vs Model+C-01: p =", format.pval(control_c01_vs_model_c01_p_s6, digits = 4), "\n")

# =============================================================================
# 统计结果汇总
# =============================================================================

# 创建完整的结果表格
complete_results <- data.frame(
  Protein = rep(c("ASS1", "COX2", "p_STAT3_STAT3", "p_mTOR_mTOR", "p_S6_S6"), each = 4),
  Comparison = rep(c("Control_vs_Control+C01", "Model_vs_Model+C01", "Control_vs_Model", "Control+C01_vs_Model+C01"), 5),
  P_value = c(
    control_vs_control_c01_p_ass1, model_vs_model_c01_p_ass1, control_vs_model_p_ass1, control_c01_vs_model_c01_p_ass1,
    control_vs_control_c01_p_cox2, model_vs_model_c01_p_cox2, control_vs_model_p_cox2, control_c01_vs_model_c01_p_cox2,
    control_vs_control_c01_p_stat3, model_vs_model_c01_p_stat3, control_vs_model_p_stat3, control_c01_vs_model_c01_p_stat3,
    control_vs_control_c01_p_mtor, model_vs_model_c01_p_mtor, control_vs_model_p_mtor, control_c01_vs_model_c01_p_mtor,
    control_vs_control_c01_p_s6, model_vs_model_c01_p_s6, control_vs_model_p_s6, control_c01_vs_model_c01_p_s6
  ),
  Mean_Group1 = c(
    mean(control_ass1), mean(model_ass1), mean(control_ass1), mean(control_c01_ass1),
    mean(control_cox2), mean(model_cox2), mean(control_cox2), mean(control_c01_cox2),
    mean(control_stat3), mean(model_stat3), mean(control_stat3), mean(control_c01_stat3),
    mean(control_mtor), mean(model_mtor), mean(control_mtor), mean(control_c01_mtor),
    mean(control_s6), mean(model_s6), mean(control_s6), mean(control_c01_s6)
  ),
  Mean_Group2 = c(
    mean(control_c01_ass1), mean(model_c01_ass1), mean(model_ass1), mean(model_c01_ass1),
    mean(control_c01_cox2), mean(model_c01_cox2), mean(model_cox2), mean(model_c01_cox2),
    mean(control_c01_stat3), mean(model_c01_stat3), mean(model_stat3), mean(model_c01_stat3),
    mean(control_c01_mtor), mean(model_c01_mtor), mean(model_mtor), mean(model_c01_mtor),
    mean(control_c01_s6), mean(model_c01_s6), mean(model_s6), mean(model_c01_s6)
  )
)

complete_results$Mean_Difference <- complete_results$Mean_Group2 - complete_results$Mean_Group1
complete_results$Significance <- ifelse(complete_results$P_value < 0.001, "***",
                                        ifelse(complete_results$P_value < 0.01, "**",
                                               ifelse(complete_results$P_value < 0.05, "*", "ns")))

cat("\n=== COMPLETE RESULTS TABLE (Figure 7e - Western Blot 5 Proteins) ===\n")
print(complete_results)

# 保存结果
write.csv(complete_results, "figure7e_western_blot_statistical_analysis.csv", row.names = FALSE)

# =============================================================================
# 治疗效果分析总结
# =============================================================================

cat("\n=== 治疗效果分析总结 ===\n")

treatment_effects <- data.frame(
  Protein = c("ASS1", "COX2", "p_STAT3_STAT3", "p_mTOR_mTOR", "p_S6_S6"),
  C01_Effect_in_Control = c(
    format.pval(control_vs_control_c01_p_ass1, digits = 3),
    format.pval(control_vs_control_c01_p_cox2, digits = 3),
    format.pval(control_vs_control_c01_p_stat3, digits = 3),
    format.pval(control_vs_control_c01_p_mtor, digits = 3),
    format.pval(control_vs_control_c01_p_s6, digits = 3)
  ),
  C01_Effect_in_Model = c(
    format.pval(model_vs_model_c01_p_ass1, digits = 3),
    format.pval(model_vs_model_c01_p_cox2, digits = 3),
    format.pval(model_vs_model_c01_p_stat3, digits = 3),
    format.pval(model_vs_model_c01_p_mtor, digits = 3),
    format.pval(model_vs_model_c01_p_s6, digits = 3)
  ),
  Disease_Effect = c(
    format.pval(control_vs_model_p_ass1, digits = 3),
    format.pval(control_vs_model_p_cox2, digits = 3),
    format.pval(control_vs_model_p_stat3, digits = 3),
    format.pval(control_vs_model_p_mtor, digits = 3),
    format.pval(control_vs_model_p_s6, digits = 3)
  )
)

print(treatment_effects)
