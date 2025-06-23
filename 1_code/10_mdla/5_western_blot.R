library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/10_mdla/5_western_blot",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/10_mdla/5_western_blot")

##ass1
ass1 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.900087996, 1.211398515, 1.260430791)
    ),
    data.frame(
      group = "DSS",
      value = c(1.101689475, 1.43116659, 1.459360862)
    ),
    data.frame(
      group = "MDLA",
      value = c(0.998222529, 1.367850446, 1.133646961)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "MDLA")))


##cox2
cox2 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(1.527164504, 0.454045375, 1.018790122)
    ),
    data.frame(
      group = "DSS",
      value = c(10.12292312, 9.474752317, 8.6264342)
    ),
    data.frame(
      group = "MDLA",
      value = c(0.750734679, 1.871352871, 2.322172585)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "MDLA")))

###p_stat3/stat3
t3 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.856897892, 0.457195455, 1.685906653)
    ),
    data.frame(
      group = "DSS",
      value = c(2.449707728, 2.716964685, 2.650601635)
    ),
    data.frame(
      group = "MDLA",
      value = c(1.65900696, 1.835296071, 1.502519025)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "MDLA")))


##p_mtor/mtro
p_mtor_mtor <-
  rbind(
    data.frame(
      group = "Control",
      value = c(1.014039768, 0.720476131, 1.265484101)
    ),
    data.frame(
      group = "DSS",
      value = c(2.088533137, 2.313158423, 2.923047638)
    ),
    data.frame(
      group = "MDLA",
      value = c(0.903140697, 0.637429175, 1.319202251)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "MDLA")))


###p_s6/s6
p_s6_s6 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(1.097327162, 0.789284781, 1.113388057)
    ),
    data.frame(
      group = "DSS",
      value = c(4.864618847, 8.09624538, 11.36424816)
    ),
    data.frame(
      group = "MDLA",
      value = c(1.29125288, 2.621969703, 2.769383016)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "MDLA")))


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
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "ASS1") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color4)

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
  scale_color_manual(values = disease_color4)

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
  scale_color_manual(values = disease_color4)

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
  scale_color_manual(values = disease_color4)

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
  scale_color_manual(values = disease_color4)

plot

ggsave(plot,
       filename = "p_s6_s6.pdf",
       width = 6,
       height = 5)
