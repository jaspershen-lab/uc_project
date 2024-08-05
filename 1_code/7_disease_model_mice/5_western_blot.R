library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

##ass1

ass1 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.472687, 0.462967, 0.65197)
    ),
    data.frame(
      group = "DSS",
      value = c(0.970502, 1.038478, 0.940908)
    ),
    data.frame(
      group = "Arg",
      value = c(1.422338, 1.7171, 1.122251)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))

##cox2
cox2 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.501775, 0.510625, 0.388891)
    ),
    data.frame(group = "DSS", value = c(0.696029, 0.68, 0.735193)),
    data.frame(
      group = "Arg",
      value = c(1.078246, 0.957625, 1.074904)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))

###p_stat3/stat3
p_stat3_stat3 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.50265, 0.300418, 0.165834)
    ),
    data.frame(
      group = "DSS",
      value = c(0.780873, 0.770544, 0.551948)
    ),
    data.frame(
      group = "Arg",
      value = c(0.94389, 1.029203, 1.035195)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))

##p_mtor/mtro
p_mtor_mtro <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.627902, 0.664195, 0.629197)
    ),
    data.frame(
      group = "DSS",
      value = c(0.94838, 0.948402, 0.894815)
    ),
    data.frame(
      group = "Arg",
      value = c(1.199815, 1.460173, 1.044177)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))

###p_s6/s6
p_s6_s6 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.106791, 0.254717, 0.264669)
    ),
    data.frame(
      group = "DSS",
      value = c(0.824032, 0.532145, 0.578058)
    ),
    data.frame(
      group = "Arg",
      value = c(1.533274, 1.265295, 1.794139)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "Arg")))

dir.create(
  "3_data_analysis/7_disease_model_mice/5_western_blot",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/7_disease_model_mice/5_western_blot")


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
  scale_color_manual(values = disease_color)

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
  scale_color_manual(values = disease_color)

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
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "p_stat3_stat3.pdf",
       width = 6,
       height = 5)


##p_mtor/mtor
plot <-
  ggbetweenstats(
    data  = p_mtor_mtro,
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
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "p_mtor_mtro.pdf",
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
  scale_color_manual(values = disease_color)

plot

ggsave(plot,
       filename = "p_s6_s6.pdf",
       width = 6,
       height = 5)
