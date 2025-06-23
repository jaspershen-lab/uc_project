library(r4projects)
library(ggstatsplot)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/zj_NC_revision/revision_pvalue/Figure6_pvalue",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/zj_NC_revision/revision_pvalue/Figure6_pvalue")

# Figure 6 P值分析（原fig8 code）

library(dplyr)

cat("FIGURE 6 COMPLETE P-VALUE ANALYSIS")
cat("\n", rep("=", 60), "\n")

# =============================================================================
# Figure 6a: ASS1 mRNA时间序列分析
# =============================================================================
cat("\n【FIGURE 6a - ASS1 mRNA Time Course】\n")
cat(rep("-", 50), "\n")

# 原始数据
figure6a_data <- data.frame(
  group = c(rep("0", 3), rep("12", 3), rep("24", 3), rep("48", 3)),
  value = c(
    0.897462978, 0.982103094, 1.134557154,  # 0h
    2.607522585, 1.89902566, 2.329144563,   # 12h
    2.252871076, 2.865153215, 4.093237609,  # 24h
    2.694504772, 2.581733203, 3.435013528   # 48h
  ),
  time_numeric = rep(c(0, 12, 24, 48), each = 3)
) %>%
  mutate(group = factor(group, levels = c("0", "12", "24", "48")))

# 1. 总体ANOVA分析
anova_result <- aov(value ~ group, data = figure6a_data)
anova_p <- summary(anova_result)[[1]][["Pr(>F)"]][1]
cat("Overall ANOVA p-value:", format.pval(anova_p, digits = 4), "\n")

# 2. 线性趋势分析
linear_model <- lm(value ~ time_numeric, data = figure6a_data)
linear_p <- summary(linear_model)$coefficients[2, 4]
cat("Linear trend p-value:", format.pval(linear_p, digits = 4), "\n")
cat("R-squared:", round(summary(linear_model)$r.squared, 4), "\n")

# 3. 两两比较 (图中显示的比较)
cat("\nPairwise comparisons:\n")
comparisons <- list(c("0", "12"), c("12", "24"), c("24", "48"))

for(comp in comparisons) {
  group1_data <- figure6a_data[figure6a_data$group == comp[1], "value"]
  group2_data <- figure6a_data[figure6a_data$group == comp[2], "value"]

  t_result <- t.test(group1_data, group2_data)
  significance <- ifelse(t_result$p.value < 0.001, "***",
                         ifelse(t_result$p.value < 0.01, "**",
                                ifelse(t_result$p.value < 0.05, "*", "n.s.")))

  cat(sprintf("  %sh vs %sh: p = %s (%s)\n",
              comp[1], comp[2],
              format.pval(t_result$p.value, digits = 4),
              significance))
}

# 4. 所有两两比较 (Bonferroni校正)
pairwise_result <- pairwise.t.test(figure6a_data$value, figure6a_data$group,
                                   p.adjust.method = "bonferroni")
cat("\nAll pairwise comparisons (Bonferroni corrected):\n")
print(pairwise_result$p.value)

# =============================================================================
# Figure 6b: AcH3 ChIP分析
# =============================================================================
cat("\n\n【FIGURE 6b - AcH3 ChIP】\n")
cat(rep("-", 50), "\n")

# Promoter区域
ach3_promoter <- data.frame(
  group = c(rep("Control", 3), rep("IL-6", 3)),
  value = c(0.175447504, 0.259025795, 0.167224769,
            0.83519312, 0.60433561, 0.581543272)
)

ach3_promoter_test <- t.test(value ~ group, data = ach3_promoter)
cat("Promoter region:\n")
cat("  t-test p-value:", format.pval(ach3_promoter_test$p.value, digits = 4), "\n")
cat("  Mean Control:", round(mean(ach3_promoter[1:3, "value"]), 4), "\n")
cat("  Mean IL-6:", round(mean(ach3_promoter[4:6, "value"]), 4), "\n")
cat("  Fold change:", round(mean(ach3_promoter[4:6, "value"]) / mean(ach3_promoter[1:3, "value"]), 2), "\n")

# Exon1区域
ach3_exon <- data.frame(
  group = c(rep("Control", 3), rep("IL-6", 3)),
  value = c(0.217560139, 0.160394373, 0.055438869,
            0.163425975, 0.102518817, 0.168638125)
)

ach3_exon_test <- t.test(value ~ group, data = ach3_exon)
cat("Exon1 region:\n")
cat("  t-test p-value:", format.pval(ach3_exon_test$p.value, digits = 4), "\n")
cat("  Mean Control:", round(mean(ach3_exon[1:3, "value"]), 4), "\n")
cat("  Mean IL-6:", round(mean(ach3_exon[4:6, "value"]), 4), "\n")

# =============================================================================
# Figure 6c: H3K27Me3 ChIP分析
# =============================================================================
cat("\n\n【FIGURE 6c - H3K27Me3 ChIP】\n")
cat(rep("-", 50), "\n")

# Promoter区域
h3k27me3_promoter <- data.frame(
  group = c(rep("Control", 3), rep("IL-6", 3)),
  value = c(0.167320238, 0.109933136, 0.21496421,
            0.096016614, 0.133482433, 0.175749141)
)

h3k27me3_promoter_test <- t.test(value ~ group, data = h3k27me3_promoter)
cat("Promoter region:\n")
cat("  t-test p-value:", format.pval(h3k27me3_promoter_test$p.value, digits = 4), "\n")
cat("  Mean Control:", round(mean(h3k27me3_promoter[1:3, "value"]), 4), "\n")
cat("  Mean IL-6:", round(mean(h3k27me3_promoter[4:6, "value"]), 4), "\n")

# Exon1区域
h3k27me3_exon <- data.frame(
  group = c(rep("Control", 3), rep("IL-6", 3)),
  value = c(0.071311546, 0.041483663, 0.050131888,
            0.073712097, 0.014339611, 0.079534149)
)

h3k27me3_exon_test <- t.test(value ~ group, data = h3k27me3_exon)
cat("Exon1 region:\n")
cat("  t-test p-value:", format.pval(h3k27me3_exon_test$p.value, digits = 4), "\n")
cat("  Mean Control:", round(mean(h3k27me3_exon[1:3, "value"]), 4), "\n")
cat("  Mean IL-6:", round(mean(h3k27me3_exon[4:6, "value"]), 4), "\n")

# =============================================================================
# Figure 6d: H3K4Me3 ChIP分析
# =============================================================================
cat("\n\n【FIGURE 6d - H3K4Me3 ChIP】\n")
cat(rep("-", 50), "\n")

# Promoter区域
h3k4me3_promoter <- data.frame(
  group = c(rep("Control", 3), rep("IL-6", 3)),
  value = c(0.233593023, 0.329008845, 0.234458474,
            0.9741304, 0.756954468, 0.669553272)
)

h3k4me3_promoter_test <- t.test(value ~ group, data = h3k4me3_promoter)
cat("Promoter region:\n")
cat("  t-test p-value:", format.pval(h3k4me3_promoter_test$p.value, digits = 4), "\n")
cat("  Mean Control:", round(mean(h3k4me3_promoter[1:3, "value"]), 4), "\n")
cat("  Mean IL-6:", round(mean(h3k4me3_promoter[4:6, "value"]), 4), "\n")
cat("  Fold change:", round(mean(h3k4me3_promoter[4:6, "value"]) / mean(h3k4me3_promoter[1:3, "value"]), 2), "\n")

# Exon1区域
h3k4me3_exon <- data.frame(
  group = c(rep("Control", 3), rep("IL-6", 3)),
  value = c(0.178469436, 0.127347462, 0.087809013,
            0.147869514, 0.180611064, 0.10535037)
)

h3k4me3_exon_test <- t.test(value ~ group, data = h3k4me3_exon)
cat("Exon1 region:\n")
cat("  t-test p-value:", format.pval(h3k4me3_exon_test$p.value, digits = 4), "\n")
cat("  Mean Control:", round(mean(h3k4me3_exon[1:3, "value"]), 4), "\n")
cat("  Mean IL-6:", round(mean(h3k4me3_exon[4:6, "value"]), 4), "\n")

# =============================================================================
# Figure 6e: H3K9Me2 ChIP分析
# =============================================================================
cat("\n\n【FIGURE 6e - H3K9Me2 ChIP】\n")
cat(rep("-", 50), "\n")

# Promoter区域
h3k9me2_promoter <- data.frame(
  group = c(rep("Control", 3), rep("IL-6", 3)),
  value = c(0.134622751, 0.164267967, 0.094499494,
            0.066049115, 0.076722804, 0.042275803)
)

h3k9me2_promoter_test <- t.test(value ~ group, data = h3k9me2_promoter)
cat("Promoter region:\n")
cat("  t-test p-value:", format.pval(h3k9me2_promoter_test$p.value, digits = 4), "\n")
cat("  Mean Control:", round(mean(h3k9me2_promoter[1:3, "value"]), 4), "\n")
cat("  Mean IL-6:", round(mean(h3k9me2_promoter[4:6, "value"]), 4), "\n")
cat("  Fold change:", round(mean(h3k9me2_promoter[4:6, "value"]) / mean(h3k9me2_promoter[1:3, "value"]), 2), "\n")

# Exon1区域
h3k9me2_exon <- data.frame(
  group = c(rep("Control", 3), rep("IL-6", 3)),
  value = c(0.100393676, 0.209876351, 0.123254144,
            0.135980258, 0.128160587, 0.057825048)
)

h3k9me2_exon_test <- t.test(value ~ group, data = h3k9me2_exon)
cat("Exon1 region:\n")
cat("  t-test p-value:", format.pval(h3k9me2_exon_test$p.value, digits = 4), "\n")
cat("  Mean Control:", round(mean(h3k9me2_exon[1:3, "value"]), 4), "\n")
cat("  Mean IL-6:", round(mean(h3k9me2_exon[4:6, "value"]), 4), "\n")

# =============================================================================
# 汇总结果表
# =============================================================================
cat("\n\n【SUMMARY TABLE - ALL P-VALUES】\n")
cat(rep("=", 80), "\n")

summary_table <- data.frame(
  Figure_Panel = c(
    "6a_ASS1_timecourse_ANOVA",
    "6a_Linear_trend",
    "6a_0h_vs_12h",
    "6a_12h_vs_24h",
    "6a_24h_vs_48h",
    "6b_AcH3_promoter",
    "6b_AcH3_exon1",
    "6c_H3K27Me3_promoter",
    "6c_H3K27Me3_exon1",
    "6d_H3K4Me3_promoter",
    "6d_H3K4Me3_exon1",
    "6e_H3K9Me2_promoter",
    "6e_H3K9Me2_exon1"
  ),
  P_value = c(
    anova_p,
    linear_p,
    t.test(figure6a_data[figure6a_data$group == "0", "value"],
           figure6a_data[figure6a_data$group == "12", "value"])$p.value,
    t.test(figure6a_data[figure6a_data$group == "12", "value"],
           figure6a_data[figure6a_data$group == "24", "value"])$p.value,
    t.test(figure6a_data[figure6a_data$group == "24", "value"],
           figure6a_data[figure6a_data$group == "48", "value"])$p.value,
    ach3_promoter_test$p.value,
    ach3_exon_test$p.value,
    h3k27me3_promoter_test$p.value,
    h3k27me3_exon_test$p.value,
    h3k4me3_promoter_test$p.value,
    h3k4me3_exon_test$p.value,
    h3k9me2_promoter_test$p.value,
    h3k9me2_exon_test$p.value
  ),
  Significance = c(
    ifelse(anova_p < 0.001, "***", ifelse(anova_p < 0.01, "**", ifelse(anova_p < 0.05, "*", "n.s."))),
    ifelse(linear_p < 0.001, "***", ifelse(linear_p < 0.01, "**", ifelse(linear_p < 0.05, "*", "n.s."))),
    ifelse(t.test(figure6a_data[figure6a_data$group == "0", "value"],
                  figure6a_data[figure6a_data$group == "12", "value"])$p.value < 0.001, "***",
           ifelse(t.test(figure6a_data[figure6a_data$group == "0", "value"],
                         figure6a_data[figure6a_data$group == "12", "value"])$p.value < 0.01, "**",
                  ifelse(t.test(figure6a_data[figure6a_data$group == "0", "value"],
                                figure6a_data[figure6a_data$group == "12", "value"])$p.value < 0.05, "*", "n.s."))),
    ifelse(t.test(figure6a_data[figure6a_data$group == "12", "value"],
                  figure6a_data[figure6a_data$group == "24", "value"])$p.value < 0.001, "***",
           ifelse(t.test(figure6a_data[figure6a_data$group == "12", "value"],
                         figure6a_data[figure6a_data$group == "24", "value"])$p.value < 0.01, "**",
                  ifelse(t.test(figure6a_data[figure6a_data$group == "12", "value"],
                                figure6a_data[figure6a_data$group == "24", "value"])$p.value < 0.05, "*", "n.s."))),
    ifelse(t.test(figure6a_data[figure6a_data$group == "24", "value"],
                  figure6a_data[figure6a_data$group == "48", "value"])$p.value < 0.001, "***",
           ifelse(t.test(figure6a_data[figure6a_data$group == "24", "value"],
                         figure6a_data[figure6a_data$group == "48", "value"])$p.value < 0.01, "**",
                  ifelse(t.test(figure6a_data[figure6a_data$group == "24", "value"],
                                figure6a_data[figure6a_data$group == "48", "value"])$p.value < 0.05, "*", "n.s."))),
    ifelse(ach3_promoter_test$p.value < 0.001, "***", ifelse(ach3_promoter_test$p.value < 0.01, "**", ifelse(ach3_promoter_test$p.value < 0.05, "*", "n.s."))),
    ifelse(ach3_exon_test$p.value < 0.001, "***", ifelse(ach3_exon_test$p.value < 0.01, "**", ifelse(ach3_exon_test$p.value < 0.05, "*", "n.s."))),
    ifelse(h3k27me3_promoter_test$p.value < 0.001, "***", ifelse(h3k27me3_promoter_test$p.value < 0.01, "**", ifelse(h3k27me3_promoter_test$p.value < 0.05, "*", "n.s."))),
    ifelse(h3k27me3_exon_test$p.value < 0.001, "***", ifelse(h3k27me3_exon_test$p.value < 0.01, "**", ifelse(h3k27me3_exon_test$p.value < 0.05, "*", "n.s."))),
    ifelse(h3k4me3_promoter_test$p.value < 0.001, "***", ifelse(h3k4me3_promoter_test$p.value < 0.01, "**", ifelse(h3k4me3_promoter_test$p.value < 0.05, "*", "n.s."))),
    ifelse(h3k4me3_exon_test$p.value < 0.001, "***", ifelse(h3k4me3_exon_test$p.value < 0.01, "**", ifelse(h3k4me3_exon_test$p.value < 0.05, "*", "n.s."))),
    ifelse(h3k9me2_promoter_test$p.value < 0.001, "***", ifelse(h3k9me2_promoter_test$p.value < 0.01, "**", ifelse(h3k9me2_promoter_test$p.value < 0.05, "*", "n.s."))),
    ifelse(h3k9me2_exon_test$p.value < 0.001, "***", ifelse(h3k9me2_exon_test$p.value < 0.01, "**", ifelse(h3k9me2_exon_test$p.value < 0.05, "*", "n.s.")))
  )
)

# 格式化p值
summary_table$P_value_formatted <- sapply(summary_table$P_value, function(x) {
  if(x < 0.001) return("< 0.001")
  else if(x < 0.01) return(sprintf("%.3f", x))
  else return(sprintf("%.3f", x))
})

# 显示结果
print(summary_table[, c("Figure_Panel", "P_value_formatted", "Significance")])

# 保存结果
write.csv(summary_table, "Figure6_complete_pvalue_analysis.csv", row.names = FALSE)

cat("\n显著性结果总结 (p < 0.05):\n")
significant_results <- summary_table[summary_table$P_value < 0.05, ]
if(nrow(significant_results) > 0) {
  print(significant_results[, c("Figure_Panel", "P_value_formatted", "Significance")])
} else {
  cat("无显著性结果\n")
}


#########fig6h############
data <-
  data.frame(
    group = c(rep("Con siRNA", 3), rep("c-Myc siRNA", 3)),
    value = c(
      1.071955177,
      0.908595857,
      1.019448966,
      0.255259096,
      0.20464967,
      0.327144854
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Con siRNA", "c-Myc siRNA")))

plot <-
  ggbetweenstats(
    data = data,
    x = group,
    y = value,
    type = "parametric",
    pairwise.comparisons = TRUE,
    pairwise.display = "all",     # 显示所有比较
    p.adjust.method = "none",     # 不进行多重比较校正
    centrality.type = "nonparametric",
    point.args = list(
      alpha = 0.9,
      size = 3,
      color = "black",
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4)
  ) +
  theme_classic() +  # 使用theme_classic或您的theme_base
  labs(x = "", y = "c-Myc protein level") +
  theme(legend.position = "")

print(plot)


########fig6i###########
data <-
  data.frame(
    group = c(
      rep("Control", 3),
      rep("c-Myc", 3),
      rep("IL-6", 3),
      rep("IL-6+c-Myc", 3)
    ),
    value = c(
      0.903546899,
      0.982052803,
      1.126975464,
      0.416375609,
      0.716478547,
      0.602290398,
      3.072046943,
      3.032890978,
      4.548424634,
      1.186867861,
      0.94413557,
      1.472749683
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "c-Myc", "IL-6", "IL-6+c-Myc")))


plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = TRUE,
    pairwise.display = "all",
    p.adjust.method = "none",
    centrality.type = "nonparametric",
    point.args = list(
      alpha = 0.9,
      size = 3,
      color = "black",
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4)
  ) +
  theme_base +
  labs(x = "", y = "Relative ASS1 mRNA level") +
  theme(legend.position = "")

plot


# =============================================================================
# Figure 6j: AcH3 ChIP Analysis
# =============================================================================
cat("\n【FIGURE 6j - AcH3 ChIP】\n")
cat(rep("-", 50), "\n")

# AcH3 Promoter数据
ach3_promoter_data <- data.frame(
  group = c(
    rep("Con siRNA", 3), rep("c-Myc siRNA", 3),
    rep("IL-6", 3), rep("IL-6+c-Myc siRNA", 3)
  ),
  value = c(
    0.67424775, 0.453466032, 0.607918089,      # Con siRNA
    0.319569642, 0.189026627, 0.290531472,     # c-Myc siRNA
    1.189602437, 1.351410689, 1.37727331,      # IL-6
    0.735427352, 0.489868303, 0.63020071       # IL-6+c-Myc siRNA
  )
) %>%
  mutate(group = factor(group, levels = c("Con siRNA", "c-Myc siRNA", "IL-6", "IL-6+c-Myc siRNA")))

# AcH3 Exon数据
ach3_exon_data <- data.frame(
  group = c(
    rep("Con siRNA", 3), rep("c-Myc siRNA", 3),
    rep("IL-6", 3), rep("IL-6+c-Myc siRNA", 3)
  ),
  value = c(
    0.194260198, 0.252047572, 0.151702374,     # Con siRNA
    0.360854019, 0.118696385, 0.149369513,     # c-Myc siRNA
    0.223290207, 0.260127467, 0.135332277,     # IL-6
    0.345282257, 0.197095412, 0.137639843      # IL-6+c-Myc siRNA
  )
) %>%
  mutate(group = factor(group, levels = c("Con siRNA", "c-Myc siRNA", "IL-6", "IL-6+c-Myc siRNA")))

# Figure 6j统计分析
analyze_chip_data <- function(data, region_name) {
  cat(paste("\n", region_name, "Region:\n"))

  # 总体ANOVA
  anova_result <- aov(value ~ group, data = data)
  anova_p <- summary(anova_result)[[1]][["Pr(>F)"]][1]
  cat("  Overall ANOVA p-value:", format.pval(anova_p, digits = 4), "\n")

  # 关键的两两比较
  key_comparisons <- list(
    c("Con siRNA", "c-Myc siRNA"),
    c("Con siRNA", "IL-6"),
    c("IL-6", "IL-6+c-Myc siRNA"),  # 最重要的比较
    c("c-Myc siRNA", "IL-6+c-Myc siRNA")
  )

  comparison_results <- data.frame()

  for(comp in key_comparisons) {
    group1_data <- data[data$group == comp[1], "value"]
    group2_data <- data[data$group == comp[2], "value"]

    t_result <- t.test(group1_data, group2_data)

    # 计算fold change和其他统计量
    mean1 <- mean(group1_data)
    mean2 <- mean(group2_data)
    fold_change <- mean2 / mean1

    significance <- ifelse(t_result$p.value < 0.001, "***",
                           ifelse(t_result$p.value < 0.01, "**",
                                  ifelse(t_result$p.value < 0.05, "*", "n.s.")))

    cat(sprintf("  %s vs %s:\n", comp[1], comp[2]))
    cat(sprintf("    p-value: %s (%s)\n", format.pval(t_result$p.value, digits = 4), significance))
    cat(sprintf("    Fold change: %.2f\n", fold_change))
    cat(sprintf("    Means: %.3f vs %.3f\n", mean1, mean2))

    # 保存结果
    comparison_results <- rbind(comparison_results, data.frame(
      Region = region_name,
      Comparison = paste(comp[1], "vs", comp[2]),
      P_value = t_result$p.value,
      Fold_change = fold_change,
      Significance = significance
    ))
  }

  return(comparison_results)
}

# 分析AcH3数据
ach3_promoter_results <- analyze_chip_data(ach3_promoter_data, "AcH3-Promoter")
ach3_exon_results <- analyze_chip_data(ach3_exon_data, "AcH3-Exon")

# =============================================================================
# Figure 6k: H3K4Me3 ChIP Analysis
# =============================================================================
cat("\n\n【FIGURE 6k - H3K4Me3 ChIP】\n")
cat(rep("-", 50), "\n")

# H3K4Me3 Promoter数据
h3k4me3_promoter_data <- data.frame(
  group = c(
    rep("Con siRNA", 3), rep("c-Myc siRNA", 3),
    rep("IL-6", 3), rep("IL-6+c-Myc siRNA", 3)
  ),
  value = c(
    0.745170069, 0.613102799, 0.79601101,       # Con siRNA
    0.313548657, 0.232487268, 0.265758228,      # c-Myc siRNA
    1.510378811, 1.249147162, 1.393631763,      # IL-6
    0.609936822, 0.460186634, 0.77869187        # IL-6+c-Myc siRNA
  )
) %>%
  mutate(group = factor(group, levels = c("Con siRNA", "c-Myc siRNA", "IL-6", "IL-6+c-Myc siRNA")))

# H3K4Me3 Exon数据
h3k4me3_exon_data <- data.frame(
  group = c(
    rep("Con siRNA", 3), rep("c-Myc siRNA", 3),
    rep("IL-6", 3), rep("IL-6+c-Myc siRNA", 3)
  ),
  value = c(
    0.473229804, 0.336585214, 0.244170049,      # Con siRNA
    0.244635016, 0.334062719, 0.397439865,      # c-Myc siRNA
    0.668005141, 0.291169579, 0.189415098,      # IL-6
    0.350690607, 0.327359425, 0.398509109       # IL-6+c-Myc siRNA
  )
) %>%
  mutate(group = factor(group, levels = c("Con siRNA", "c-Myc siRNA", "IL-6", "IL-6+c-Myc siRNA")))

# 分析H3K4Me3数据
h3k4me3_promoter_results <- analyze_chip_data(h3k4me3_promoter_data, "H3K4Me3-Promoter")
h3k4me3_exon_results <- analyze_chip_data(h3k4me3_exon_data, "H3K4Me3-Exon")

# =============================================================================
# Figure 6l: H3K9Me2 ChIP Analysis
# =============================================================================
cat("\n\n【FIGURE 6l - H3K9Me2 ChIP】\n")
cat(rep("-", 50), "\n")

# H3K9Me2 Promoter数据
h3k9me2_promoter_data <- data.frame(
  group = c(
    rep("Con siRNA", 3), rep("c-Myc siRNA", 3),
    rep("IL-6", 3), rep("IL-6+c-Myc siRNA", 3)
  ),
  value = c(
    0.488079767, 0.340863836, 0.29555322,       # Con siRNA
    0.996505271, 0.661812939, 0.804835845,      # c-Myc siRNA
    0.12397371, 0.062068969, 0.107313614,       # IL-6
    0.287918826, 0.373381108, 0.440124035       # IL-6+c-Myc siRNA
  )
) %>%
  mutate(group = factor(group, levels = c("Con siRNA", "c-Myc siRNA", "IL-6", "IL-6+c-Myc siRNA")))

# H3K9Me2 Exon数据
h3k9me2_exon_data <- data.frame(
  group = c(
    rep("Con siRNA", 3), rep("c-Myc siRNA", 3),
    rep("IL-6", 3), rep("IL-6+c-Myc siRNA", 3)
  ),
  value = c(
    0.097559414, 0.10115572, 0.077801275,       # Con siRNA
    0.072825847, 0.064761536, 0.096237242,      # c-Myc siRNA
    0.122087492, 0.125185686, 0.084203772,      # IL-6
    0.10785539, 0.098921352, 0.066185635        # IL-6+c-Myc siRNA
  )
) %>%
  mutate(group = factor(group, levels = c("Con siRNA", "c-Myc siRNA", "IL-6", "IL-6+c-Myc siRNA")))

# 分析H3K9Me2数据
h3k9me2_promoter_results <- analyze_chip_data(h3k9me2_promoter_data, "H3K9Me2-Promoter")
h3k9me2_exon_results <- analyze_chip_data(h3k9me2_exon_data, "H3K9Me2-Exon")

# =============================================================================
# Figure 6n: Co-IP Analysis
# =============================================================================
cat("\n\n【FIGURE 6n - Co-Immunoprecipitation】\n")
cat(rep("-", 50), "\n")

# IP: P300 IB: c-Myc
coip_p300_cmyc <- data.frame(
  group = c(rep("Control", 3), rep("IL-6", 3)),
  value = c(
    0.952016353, 1.070176553, 0.977807093,      # Control
    1.905821735, 2.133104291, 2.505783915       # IL-6
  )
) %>%
  mutate(group = factor(group, levels = c("Control", "IL-6")))

# IP: KDM3A IB: c-Myc
coip_kdm3a_cmyc <- data.frame(
  group = c(rep("Control", 3), rep("IL-6", 3)),
  value = c(
    1.027955278, 1.052724924, 0.919319798,      # Control
    1.946260628, 1.914762261, 2.424039025       # IL-6
  )
) %>%
  mutate(group = factor(group, levels = c("Control", "IL-6")))

# IP: KDM3A IB: P300
coip_kdm3a_p300 <- data.frame(
  group = c(rep("Control", 3), rep("IL-6", 3)),
  value = c(
    1.061624371, 0.981292659, 0.95708297,       # Control
    1.811435397, 2.280474509, 1.964694769       # IL-6
  )
) %>%
  mutate(group = factor(group, levels = c("Control", "IL-6")))

# Co-IP统计分析函数
analyze_coip_data <- function(data, experiment_name) {
  cat(paste("\n", experiment_name, ":\n"))

  t_result <- t.test(value ~ group, data = data)

  control_mean <- mean(data[data$group == "Control", "value"])
  il6_mean <- mean(data[data$group == "IL-6", "value"])
  fold_change <- il6_mean / control_mean

  significance <- ifelse(t_result$p.value < 0.001, "***",
                         ifelse(t_result$p.value < 0.01, "**",
                                ifelse(t_result$p.value < 0.05, "*", "n.s.")))

  cat(sprintf("  t-test p-value: %s (%s)\n", format.pval(t_result$p.value, digits = 4), significance))
  cat(sprintf("  Control mean: %.3f\n", control_mean))
  cat(sprintf("  IL-6 mean: %.3f\n", il6_mean))
  cat(sprintf("  Fold change: %.2f\n", fold_change))
  cat(sprintf("  95%% CI: [%.3f, %.3f]\n", t_result$conf.int[1], t_result$conf.int[2]))

  return(data.frame(
    Experiment = experiment_name,
    P_value = t_result$p.value,
    Fold_change = fold_change,
    Significance = significance
  ))
}

# 分析所有Co-IP实验
coip_results <- rbind(
  analyze_coip_data(coip_p300_cmyc, "IP: P300, IB: c-Myc"),
  analyze_coip_data(coip_kdm3a_cmyc, "IP: KDM3A, IB: c-Myc"),
  analyze_coip_data(coip_kdm3a_p300, "IP: KDM3A, IB: P300")
)

# =============================================================================
# 汇总所有结果
# =============================================================================
cat("\n\n【COMPREHENSIVE RESULTS SUMMARY】\n")
cat(rep("=", 70), "\n")

# 合并所有ChIP结果
all_chip_results <- rbind(
  ach3_promoter_results,
  ach3_exon_results,
  h3k4me3_promoter_results,
  h3k4me3_exon_results,
  h3k9me2_promoter_results,
  h3k9me2_exon_results
)

# 添加Figure标识
all_chip_results$Figure <- c(
  rep("6j", nrow(ach3_promoter_results) + nrow(ach3_exon_results)),
  rep("6k", nrow(h3k4me3_promoter_results) + nrow(h3k4me3_exon_results)),
  rep("6l", nrow(h3k9me2_promoter_results) + nrow(h3k9me2_exon_results))
)

coip_results$Figure <- "6n"

cat("\nChIP Experiments - Key Significant Results (p < 0.05):\n")
significant_chip <- all_chip_results[all_chip_results$P_value < 0.05, ]
if(nrow(significant_chip) > 0) {
  print(significant_chip[, c("Figure", "Region", "Comparison", "P_value", "Fold_change", "Significance")])
} else {
  cat("No significant results found.\n")
}

cat("\nCo-IP Experiments - All Results:\n")
print(coip_results[, c("Figure", "Experiment", "P_value", "Fold_change", "Significance")])

# 保存完整结果
write.csv(all_chip_results, "Figure6jkl_ChIP_statistical_results.csv", row.names = FALSE)
write.csv(coip_results, "Figure6n_CoIP_statistical_results.csv", row.names = FALSE)



