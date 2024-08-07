library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/11_nos/2_western_blot",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/11_nos/2_western_blot")

####western blog protein level for iNOS
data1 <-
  data.frame(
    group = c(rep("Control", 4), rep("DSS", 4), rep("MDLA", 4)),
    value = c(
      0.936974488,
      1.179749642,
      0.860178405,
      1.023097464,
      1.872422194,
      2.260838788,
      2.319007371,
      2.315878897,
      2.053743961,
      1.864708353,
      2.504876855,
      2.080312581
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "MDLA")))


plot <-
  ggbetweenstats(
    data  = data1,
    x  = group,
    y  = value,
    type = "nonparametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    )
  ) +
  theme_base +
  labs(x = "", y = "iNOS protein level") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "inos_protein_level.pdf",
       width = 5,
       height = 5)


####qPCAR mRNA level for iNOS
data2 <-
  data.frame(
    group = c(rep("Control", 4), rep("DSS", 4), rep("MDLA", 4)),
    value = c(
      0.854212353,
      1.183177618,
      0.967723409,
      1.022428531,
      2.743466577,
      2.524504064,
      2.788198614,
      3.480593506,
      3.240013895,
      2.233541922,
      3.072330316,
      2.749812659
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "MDLA")))


plot <-
  ggbetweenstats(
    data  = data2,
    x  = group,
    y  = value,
    type = "nonparametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    )
  ) +
  theme_base +
  labs(x = "", y = "iNOS mRNA level") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "inos_mrna_level.pdf",
       width = 5,
       height = 5)




####western blog protein level for enos
data1 <-
  data.frame(
    group = c(rep("Control", 4), rep("DSS", 4), rep("MDLA", 4)),
    value = c(
      0.944945892,
      1.126144333,
      1.013632202,
      0.915277573,
      1.290304697,
      0.958467589,
      0.90661486,
      1.185397302,
      0.416749419,
      0.552742253,
      0.498119948,
      0.330120381
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "MDLA")))


plot <-
  ggbetweenstats(
    data  = data1,
    x  = group,
    y  = value,
    type = "nonparametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    )
  ) +
  theme_base +
  labs(x = "", y = "eNOS protein level") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "enos_protein_level.pdf",
       width = 5,
       height = 5)

####qPCAR mRNA level for enos
data2 <-
  data.frame(
    group = c(rep("Control", 4), rep("DSS", 4), rep("MDLA", 4)),
    value = c(
      1.049171119,
      0.913356509,
      0.90078206,
      1.15849354,
      1.297663809,
      1.12968196,
      1.073694332,
      0.771595517,
      0.586112699,
      0.697009391,
      0.747036169,
      0.469517228
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "MDLA")))


plot <-
  ggbetweenstats(
    data  = data2,
    x  = group,
    y  = value,
    type = "nonparametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    )
  ) +
  theme_base +
  labs(x = "", y = "eNOS mRNA level") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "enos_mrna_level.pdf",
       width = 5,
       height = 5)
