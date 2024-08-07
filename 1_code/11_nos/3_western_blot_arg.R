library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/11_nos/3_western_blot_arg",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/11_nos/3_western_blot_arg")

####western blog protein level for iNOS
data1 <-
  data.frame(
    group = c(rep("Control", 4), rep("DSS", 4), rep("Arg", 4)),
    value = c(
      1.041177681,
      1.120967716,
      0.984837565,
      0.853017038,
      1.942712259,
      2.311777859,
      1.96062448,
      1.566187613,
      3.601521081,
      4.237912496,
      4.343692989,
      3.790431577
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))


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
    group = c(rep("Control", 4), rep("DSS", 4), rep("Arg", 4)),
    value = c(
      1.021012126,
      0.882702996,
      1.074252648,
      1.032875715,
      3.490257151,
      2.969047141,
      2.324091174,
      2.560927954,
      6.758329389,
      4.856779538,
      5.464161027,
      4.637455164
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))


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
    group = c(rep("Control", 4), rep("DSS", 4), rep("Arg", 4)),
    value = c(
      1.061823677,
      0.942397644,
      0.873690586,
      1.122088092,
      0.977149038,
      1.155367685,
      0.983824584,
      1.133542085,
      1.105673069,
      0.978844599,
      1.121269249,
      1.052660475
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))


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
    group = c(rep("Control", 4), rep("DSS", 4), rep("Arg", 4)),
    value = c(
      0.861048655,
      1.087362767,
      1.136160512,
      0.940065596,
      1.243293291,
      1.206504531,
      1.087362767,
      0.699388833,
      0.995964805,
      1.094925973,
      1.332528756,
      1.052752766
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))


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
