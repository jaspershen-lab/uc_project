# Figure 4c - H-score with p-values

library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

# 读取数据
data1 <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 13)

dir.create(
  "3_data_analysis/zj_NC_revision/revision_pvalue/Figure4_pvalue/Figure4c",
  showWarnings = FALSE,
  recursive = TRUE
)
setwd("3_data_analysis/zj_NC_revision/revision_pvalue/Figure4_pvalue/Figure4c")


# 处理H-score数据
data1 <-
  rbind(
    data.frame(group = "Control", value = data1$Control),
    data.frame(group = "DSS", value = data1$Model),
    data.frame(group = "Arg", value = data1$Arg)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))

# 方法1: 使用ggbetweenstats的参数检验
plot_parametric <-
  ggbetweenstats(
    data  = data1,
    x  = group,
    y  = value,
    type = "parametric",          # 改为参数检验 (ANOVA + t-test)
    pairwise.comparisons = TRUE,  # 启用配对比较显示p值
    pairwise.display = "all",     # 显示所有配对比较
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    results.subtitle = TRUE,      # 显示统计结果摘要
    centrality.plotting = FALSE   # 不显示中心趋势线
  ) +
  theme_base +
  labs(x = "", y = "H-score", title = "H-score") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

plot_parametric

ggsave(plot_parametric,
       filename = "H-score_parametric.pdf",
       width = 6,
       height = 5)

# 方法2: 使用ggpubr的t检验（推荐）
plot_ttest <- ggboxplot(data1,
                        x = "group",
                        y = "value",
                        color = "group",
                        add = "jitter",
                        add.params = list(size = 3, alpha = 0.9)) +
  stat_compare_means(method = "anova",
                     label.y = max(data1$value) * 1.15,
                     size = 4) +
  stat_compare_means(comparisons = list(c("Control", "DSS"),
                                        c("Control", "Arg"),
                                        c("DSS", "Arg")),
                     method = "t.test",
                     label = "p.format",
                     size = 4) +
  theme_base +
  labs(x = "", y = "H-score", title = "H-score") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

plot_ttest

ggsave(plot_ttest,
       filename = "H-score_ttest.pdf",
       width = 6,
       height = 6)

# 方法3: 使用violin plot风格（类似原图）
plot_violin <- ggviolin(data1,
                        x = "group",
                        y = "value",
                        color = "group",
                        add = "jitter",
                        add.params = list(size = 3, alpha = 0.9)) +
  stat_compare_means(method = "anova",
                     label.y = max(data1$value) * 1.15,
                     size = 4) +
  stat_compare_means(comparisons = list(c("Control", "DSS"),
                                        c("Control", "Arg"),
                                        c("DSS", "Arg")),
                     method = "t.test",
                     label = "p.format",
                     size = 4) +
  theme_base +
  labs(x = "", y = "H-score", title = "H-score") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color)

plot_violin

ggsave(plot_violin,
       filename = "H-score_violin_ttest.pdf",
       width = 6,
       height = 6)



