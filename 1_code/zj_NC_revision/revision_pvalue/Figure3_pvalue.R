library(r4projects)
library(ggstatsplot)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

data <- readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                          sheet = 8)


# Figure 3f - IHC Score Analysis with P-values
# 数据预处理
colnames(data) <- c("Control", "UC")
data <- rbind(
  data.frame(group = "Control", value = as.numeric(as.character(data$Control))),
  data.frame(group = "UC", value = as.numeric(as.character(data$UC)))
) %>%
  filter(!is.na(value))  # 移除NA值

# 使用ggbetweenstats显示p值

library(ggstatsplot)

plot_with_stats <-
  ggbetweenstats(
    data = data,
    x = group,
    y = value,
    type = "nonparametric",       # 使用非参数检验
    pairwise.comparisons = TRUE,  # 显示两两比较
    pairwise.display = "all",     # 显示所有比较
    p.adjust.method = "none",     # 不进行多重比较校正
    results.subtitle = TRUE,      # 显示统计结果字幕
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    )
  ) +
  theme_classic() +
  labs(x = "", y = "IHC score") +
  theme(legend.position = "") +
  scale_color_manual(values = c("Control" = "#4CAF50", "UC" = "#F44336"))

print(plot_with_stats)

#########3Figure 3a###########
data <- readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                          sheet = 4)


# 数据预处理 - Hospital 1
data1 <- data[, c(1, 2)] %>%
  as.data.frame()
data1 <- data1[-c(1), ]

# 数据预处理 - Hospital 2
data2 <- data[, c(4, 5)] %>%
  as.data.frame()
data2 <- data2[-c(1), ]

# 数据预处理 - Hospital 3
data3 <- data[, c(7, 8)] %>%
  as.data.frame()
data3 <- data3[-c(1), ]

# 统一列名
colnames(data1) <- colnames(data2) <- colnames(data3) <- c("Control", "UC")

# 处理Hospital 1数据
data1 <- rbind(
  data.frame(
    sample_id = paste0("Control_", 1:nrow(data1)),
    group = "Control",
    center = "Hospital 1",
    value = data1$Control
  ),
  data.frame(
    sample_id = paste0("UC_", 1:nrow(data1)),
    group = "UC",
    center = "Hospital 1",
    value = data1$UC
  )
) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(value = as.numeric(value))

# 处理Hospital 2数据
data2 <- rbind(
  data.frame(
    sample_id = paste0("Control_", 1:nrow(data2)),
    group = "Control",
    center = "Hospital 2",
    value = data2$Control
  ),
  data.frame(
    sample_id = paste0("UC_", 1:nrow(data2)),
    group = "UC",
    center = "Hospital 2",
    value = data2$UC
  )
) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(value = as.numeric(value))

# 处理Hospital 3数据
data3 <- rbind(
  data.frame(
    sample_id = paste0("Control_", 1:nrow(data3)),
    group = "Control",
    center = "Hospital 3",
    value = data3$Control
  ),
  data.frame(
    sample_id = paste0("UC_", 1:nrow(data3)),
    group = "UC",
    center = "Hospital 3",
    value = data3$UC
  )
) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(value = as.numeric(value))

# 合并所有数据
data <- rbind(data1, data2, data3)

# Hospital 1 - 带p值的图
plot1 <- ggbetweenstats(
  data = data1,
  x = group,
  y = value,
  type = "nonparametric",
  pairwise.comparisons = TRUE,     # 改为TRUE显示p值
  pairwise.display = "all",
  p.adjust.method = "none",
  point.args = list(
    alpha = 0.9,
    size = 3,
    position = position_jitter(width = 0.1)
  )
) +
  theme_classic() +
  labs(
    x = "",
    y = "Arginine Concentration (μg/mL)",
    title = "Validation cohort 1"
  ) +
  theme(legend.position = "") +
  scale_color_manual(values = c("Control" = "#4CAF50", "UC" = "#F44336")) +
  scale_y_continuous(limits = c(12, 51), expand = c(0.1, 0.1))

print(plot1)
# ggsave(plot1, filename = "figure3a_center1.pdf", width = 5, height = 5)

# Hospital 2 - 带p值的图
plot2 <- ggbetweenstats(
  data = data2,
  x = group,
  y = value,
  type = "nonparametric",
  pairwise.comparisons = TRUE,     # 改为TRUE显示p值
  pairwise.display = "all",
  p.adjust.method = "none",
  point.args = list(
    alpha = 0.9,
    size = 3,
    position = position_jitter(width = 0.1)
  )
) +
  theme_classic() +
  labs(
    x = "",
    y = "Arginine Concentration (μg/mL)",
    title = "Validation cohort 2"
  ) +
  theme(legend.position = "") +
  scale_color_manual(values = c("Control" = "#4CAF50", "UC" = "#F44336")) +
  scale_y_continuous(limits = c(12, 51), expand = c(0.1, 0.1))

print(plot2)
# ggsave(plot2, filename = "figure3a_center2.pdf", width = 5, height = 5)

# Hospital 3 - 带p值的图
plot3 <- ggbetweenstats(
  data = data3,
  x = group,
  y = value,
  type = "nonparametric",
  pairwise.comparisons = TRUE,     # 改为TRUE显示p值
  pairwise.display = "all",
  p.adjust.method = "none",
  point.args = list(
    alpha = 0.9,
    size = 3,
    position = position_jitter(width = 0.1)
  )
) +
  theme_classic() +
  labs(
    x = "",
    y = "Arginine Concentration (μg/mL)",
    title = "Validation cohort 3"
  ) +
  theme(legend.position = "") +
  scale_color_manual(values = c("Control" = "#4CAF50", "UC" = "#F44336")) +
  scale_y_continuous(limits = c(13, 51), expand = c(0.1, 0.1))

print(plot3)
# ggsave(plot3, filename = "figure3a_center3.pdf", width = 5, height = 5)

# 详细统计分析
cat("FIGURE 3a - Arginine Validation Cohorts Statistical Analysis\n")
cat(rep("=", 70), "\n")

# 分析函数
analyze_cohort <- function(data, cohort_name) {
  cat(paste("\n", cohort_name, ":\n"))
  cat(rep("-", 30), "\n")

  # 样本量
  n_control <- sum(data$group == "Control")
  n_uc <- sum(data$group == "UC")
  cat("Sample sizes: Control n =", n_control, ", UC n =", n_uc, "\n")

  # 描述性统计
  control_values <- data[data$group == "Control", "value"]
  uc_values <- data[data$group == "UC", "value"]

  cat("Control: Median =", round(median(control_values), 2),
      " (IQR:", round(quantile(control_values, 0.25), 2), "-",
      round(quantile(control_values, 0.75), 2), ")\n")
  cat("UC: Median =", round(median(uc_values), 2),
      " (IQR:", round(quantile(uc_values, 0.25), 2), "-",
      round(quantile(uc_values, 0.75), 2), ")\n")

  # 统计检验
  wilcox_result <- wilcox.test(value ~ group, data = data)
  cat("Wilcoxon test p-value:", format.pval(wilcox_result$p.value, digits = 4), "\n")

  # 效应量和倍数变化
  fold_change <- median(uc_values) / median(control_values)
  cat("Median fold change (UC/Control):", round(fold_change, 2), "\n")

  # 显著性
  if(wilcox_result$p.value < 0.001) {
    significance <- "*** (p < 0.001)"
  } else if(wilcox_result$p.value < 0.01) {
    significance <- "** (p < 0.01)"
  } else if(wilcox_result$p.value < 0.05) {
    significance <- "* (p < 0.05)"
  } else {
    significance <- "n.s. (p ≥ 0.05)"
  }
  cat("Significance:", significance, "\n")

  return(data.frame(
    Cohort = cohort_name,
    P_value = wilcox_result$p.value,
    Fold_change = fold_change,
    Significance = significance
  ))
}

# 分析三个队列
results1 <- analyze_cohort(data1, "Validation cohort 1")
results2 <- analyze_cohort(data2, "Validation cohort 2")
results3 <- analyze_cohort(data3, "Validation cohort 3")

# 汇总结果
summary_results <- rbind(results1, results2, results3)

cat("\n\nSUMMARY OF ALL COHORTS:\n")
cat(rep("=", 50), "\n")
print(summary_results)

# 保存统计结果
# write.csv(summary_results, "Figure3a_validation_cohorts_statistics.csv", row.names = FALSE)



########Figure 3e########
data <- readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                          sheet = 7)

# 处理ASS1数据
data_ass1 <- data[, c(1, 2)]
data_ass1 <- data_ass1[-1, ]  # 移除标题行
colnames(data_ass1) <- c("Control", "UC")

data_ass1 <- rbind(
  data.frame(group = "Control", value = as.numeric(as.character(data_ass1$Control))),
  data.frame(group = "UC", value = as.numeric(as.character(data_ass1$UC)))
) %>%
  filter(!is.na(value))  # 移除NA值

# 处理ASL数据
data_asl <- data[, c(4, 5)]
data_asl <- data_asl[-1, ]  # 移除标题行
colnames(data_asl) <- c("Control", "UC")

data_asl <- rbind(
  data.frame(group = "Control", value = as.numeric(as.character(data_asl$Control))),
  data.frame(group = "UC", value = as.numeric(as.character(data_asl$UC)))
) %>%
  filter(!is.na(value))  # 移除NA值

# ASS1 mRNA图 - 带统计分析
library(ggstatsplot)

plot_ass1 <- ggbetweenstats(
  data = data_ass1,
  x = group,
  y = value,
  type = "nonparametric",
  pairwise.comparisons = TRUE,     # 改为TRUE显示p值
  pairwise.display = "all",
  p.adjust.method = "none",
  results.subtitle = TRUE,         # 显示统计结果
  point.args = list(
    alpha = 0.9,
    size = 3,
    position = position_jitter(width = 0.1)
  )
) +
  theme_classic() +
  labs(
    x = "",
    y = "Relative level",
    title = "ASS1 mRNA"
  ) +
  theme(legend.position = "") +
  scale_color_manual(values = c("Control" = "#4CAF50", "UC" = "#F44336"))

print(plot_ass1)


# ASL mRNA图 - 带统计分析
plot_asl <- ggbetweenstats(
  data = data_asl,
  x = group,
  y = value,
  type = "nonparametric",
  pairwise.comparisons = TRUE,     # 改为TRUE显示p值
  pairwise.display = "all",
  p.adjust.method = "none",
  results.subtitle = TRUE,         # 显示统计结果
  point.args = list(
    alpha = 0.9,
    size = 3,
    position = position_jitter(width = 0.1)
  )
) +
  theme_classic() +
  labs(
    x = "",
    y = "Relative level",
    title = "ASL mRNA"
  ) +
  theme(legend.position = "") +
  scale_color_manual(values = c("Control" = "#4CAF50", "UC" = "#F44336"))

print(plot_asl)



dir.create(
  "3_data_analysis/zj_NC_revision/revision_pvalue/Figure3_pvalue",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/zj_NC_revision/revision_pvalue/Figure3_pvalue")

ggsave(plot_with_stats, filename = "figure3f.pdf", width = 5, height = 5)
ggsave(plot1, filename = "figure3a_center1.pdf", width = 5, height = 5)
ggsave(plot2, filename = "figure3a_center1.pdf", width = 5, height = 5)
ggsave(plot3, filename = "figure3a_center1.pdf", width = 5, height = 5)
write.csv(summary_results, "Figure3a_validation_cohorts_statistics.csv", row.names = FALSE)

ggsave(plot_ass1, filename = "figure3e_ASS1_mRNA.pdf", width = 5, height = 5)
ggsave(plot_asl, filename = "figure3e_ASL_mRNA.pdf", width = 5, height = 5)



