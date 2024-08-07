library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/old_figures/FigureS22",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/old_figures/FigureS22")


###B
data <-
  data.frame(
    group = c(rep("Control", 3), rep("IL-6", 3), rep("IL-6+MDLA", 3)),
    p_Mtor = c(
      0.945084055,
      1.037131868,
      1.017784077,
      2.165375069,
      2.532485104,
      2.646212341,
      0.976383531,
      1.016384559,
      1.186923226
    ),
    p_STAT3 = c(
      0.986433292,
      0.870491693,
      1.143075015,
      2.038813817,
      2.096782939,
      2.277045674,
      1.318262477,
      1.148619921,
      1.085587332
    ),
    COX2 = c(
      0.984430151,
      1.013772797,
      1.001797052,
      2.101539643,
      2.334469728,
      1.837224625,
      0.985448154,
      1.114710768,
      1.03638537
    ),
    ASS1 = c(
      1.068292128,
      0.865339918,
      1.066367954,
      3.249887205,
      3.313016964,
      3.860965171,
      2.951631971,
      3.432789387,
      3.786058484
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "IL-6", "IL-6+MDLA")))

data

library(ggstatsplot)

plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = p_Mtor,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      color = "black",
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "p-mTOR/mTOR", title = "p-mTOR/mTOR") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))
# scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure_s22b_1.pdf",
       width = 5,
       height = 5)









plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = p_STAT3,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      color = "black",
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "p-STAT3/STAT3", title = "p-STAT3/STAT3") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))
# scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure_s22b_2.pdf",
       width = 5,
       height = 5)





plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = COX2,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      color = "black",
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "COX2", title = "COX2") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))
# scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure_s22b_3.pdf",
       width = 5,
       height = 5)














plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = ASS1,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      color = "black",
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "ASS1", title = "ASS1") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))
# scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure_s22b_4.pdf",
       width = 5,
       height = 5)













###D
data <-
  data.frame(
    group = c(
      rep("Control", 3),
      rep("IL-6+Con siRNA", 3),
      rep("IL-6+ASS1 siRNA", 3)
    ),
    p_Mtor = c(
      0.941646903,
      1.127989264,
      0.930363834,
      2.52268064,
      2.671526319,
      2.980942916,
      1.087476413,
      1.05784885,
      1.161794906
    ),
    p_STAT3 = c(
      1.014415415,
      1.060250027,
      0.925334558,
      2.546606721,
      2.722980052,
      2.317248559,
      1.119231142,
      1.205523448,
      1.00734835
    ),
    COX2 = c(
      0.973523116,
      1.101293207,
      0.925183677,
      2.276051088,
      2.438493091,
      2.022322209,
      1.54858299,
      1.458231162,
      1.24803911
    ),
    ASS1 = c(
      0.968464649,
      1.106651094,
      0.924884258,
      2.778121111,
      2.854653303,
      3.203277306,
      1.069235841,
      1.039861873,
      0.890722699
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c(
    "Control", "IL-6+Con siRNA", "IL-6+ASS1 siRNA"
  )))

data

library(ggstatsplot)

plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = p_Mtor,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      color = "black",
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "p-mTOR/mTOR", title = "p-mTOR/mTOR") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))
# scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure_s22d_1.pdf",
       width = 5,
       height = 5)


plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = p_STAT3,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      color = "black",
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "p-STAT3/STAT3", title = "p-STAT3/STAT3") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))
# scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure_s22d_2.pdf",
       width = 5,
       height = 5)



plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = COX2,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      color = "black",
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "COX2", title = "COX2") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))
# scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure_s22d_3.pdf",
       width = 5,
       height = 5)














plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = ASS1,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      color = "black",
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "ASS1", title = "ASS1") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))
# scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "figure_s22d_4.pdf",
       width = 5,
       height = 5)
