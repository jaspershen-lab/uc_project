# Load required libraries
library(tidyverse)
library(tidymass)


# Create a custom theme
theme_base <- theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )

# Read the data
proteomic_data <- read.csv("/Volumes/sirius/Study/ShenLab/UC_project/NC revision0310/proteomics_results0314.csv", check.names = FALSE)

# 创建自定义主题
theme_base <- theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )

# 处理数据用于火山图
volcano_data <- proteomic_data %>%
  mutate(
    # 计算log2倍数变化
    log2fc = log2(Ratio),
    # 计算-log10 p值
    neg_log10_pval = -log10(pvalue),
    # 计算-log10校正p值(FDR)
    neg_log10_fdr = -log10(padjust),
    # 定义显著性类别
    significance = case_when(
      padjust < 0.05 & log2fc > 1 ~ "UP",
      padjust < 0.05 & log2fc < -1 ~ "DOWN",
      TRUE ~ "NO"
    )
  )

# 找到需要强调的特定基因
ass1_data <- volcano_data %>% filter(Gene.names == "ASS1")
asl_data <- volcano_data %>% filter(Gene.names == "ASL")

# 创建火山图(匹配你示例的风格)
plot <- ggplot(volcano_data, aes(x = log2fc, y = neg_log10_fdr)) +
  # 添加点
  geom_point(aes(color = significance, size = neg_log10_pval), alpha = 0.5) +
  # 添加log2FC阈值的垂直线
  geom_vline(xintercept = c(-1, 1), linetype = "dashed", color = "darkgrey") +
  # 添加p值阈值的水平线
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "darkgrey") +
  # 添加手动颜色
  scale_color_manual(values = up_down_color) +
  # 添加尺寸比例
  scale_size_continuous(range = c(1, 6)) +
  # 设置坐标轴限制
  scale_x_continuous(limits = c(-5, 5)) +
  # 标记ASS1和ASL基因
  geom_text(data = ass1_data, aes(label = Protein.names), hjust = -0.1) +
  geom_text(data = asl_data, aes(label = Protein.names), hjust = -0.1) +
  # 用圆圈强调ASS1和ASL
  geom_point(
    data = ass1_data,
    shape = 21,
    size = 10,
    color = "black"
  ) +
  geom_point(
    data = asl_data,
    shape = 21,
    size = 10,
    color = "black"
  ) +
  # 添加坐标轴标签
  labs(
    x = "log2(Fold change)",
    y = "-log(FDR, 10)",
    title = "Proteomics",
    color = "调控",
    size = "-log10(p-value)"
  ) +
  # 应用自定义主题
  theme_base

# 显示图形
print(plot)

# 保存图形
ggsave("volcano_plot_fig3c.pdf", plot, width = 6.5, height = 5)

