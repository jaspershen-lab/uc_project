library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/9_deletion_of_ass1/5_western_blot",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/9_deletion_of_ass1/5_western_blot")

##ass1
ass1 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.921223127, 0.702061438, 0.852736496)
    ),
    data.frame(
      group = "Vector",
      value = c(1.316292227, 1.059194091, 1.256017455)
    ),
    data.frame(
      group = "shRNA",
      value = c(0.854155902, 0.817451134, 0.660971603)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "shRNA")))


##cox2
cox2 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.384267, 0.173545, 0.159117)
    ),
    data.frame(
      group = "Vector",
      value = c(0.857023, 1.088228, 1.160407)
    ),
    data.frame(
      group = "shRNA",
      value = c(0.255624, 0.194426, 0.204373)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "shRNA")))

###p_stat3/stat3
p_stat3_stat3 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.062965053, 0.155410901, 0.157160994)
    ),
    data.frame(
      group = "Vector",
      value = c(0.94761884, 1.216038401, 0.96554786)
    ),
    data.frame(
      group = "shRNA",
      value = c(0.520599573, 0.597126805, 0.501258006)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "shRNA")))

##p_mtor/mtro
p_mtor_mtor <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.444373321, 0.293225989, 0.601721266)
    ),
    data.frame(
      group = "Vector",
      value = c(0.938967096, 0.790558757, 1.059329091)
    ),
    data.frame(
      group = "shRNA",
      value = c(0.640625455, 0.533132979, 0.688374373)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "shRNA")))

###p_s6/s6
p_s6_s6 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.30471287, 0.347673014, 0.151352041)
    ),
    data.frame(
      group = "Vector",
      value = c(0.999214379, 1.087956702, 1.167296043)
    ),
    data.frame(
      group = "shRNA",
      value = c(0.45535544, 0.590913849, 0.276623692)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "shRNA")))

###ass1
plot <-
  ggbetweenstats(
    data  = ass1,
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
  labs(x = "", y = "ASS1") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "ass1.pdf",
       width = 6,
       height = 5)

##cox2
plot <-
  ggbetweenstats(
    data  = cox2,
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
  labs(x = "", y = "COX2") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "cox2.pdf",
       width = 6,
       height = 5)

##p_stat3/stat3
plot <-
  ggbetweenstats(
    data  = p_stat3_stat3,
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
  labs(x = "", y = "p_STAT3/STAT3") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "p_stat3_stat3.pdf",
       width = 6,
       height = 5)

##p_mtor/mtor
plot <-
  ggbetweenstats(
    data  = p_mtor_mtor,
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
  labs(x = "", y = "p_mTOR/mTOR") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "p_mtor_mtor.pdf",
       width = 6,
       height = 5)

###p_s6/s6
plot <-
  ggbetweenstats(
    data  = p_s6_s6,
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
  labs(x = "", y = "p_S6/S6") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "p_s6_s6.pdf",
       width = 6,
       height = 5)
