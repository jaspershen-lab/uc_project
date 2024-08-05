library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

##ass1
ass1 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.53453176, 0.397696811, 0.483292516)
    ),
    data.frame(
      group = "Vector",
      value = c(0.813297801, 0.828871742, 0.908489192)
    ),
    data.frame(
      group = "ASS1_OE",
      value = c(1.14420917, 0.970864244, 1.154266284)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "ASS1_OE")))

##cox2
cox2 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.19193968, 0.15960863, 0.208248959)
    ),
    data.frame(
      group = "Vector",
      value = c(0.324458685, 0.404361544, 0.400871761)
    ),
    data.frame(
      group = "ASS1_OE",
      value = c(1.186206518, 0.934861035, 1.011665032)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "ASS1_OE")))


###p_stat3/stat3
p_stat3_stat3 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.023475695, 0.244773394, 0.277638313)
    ),
    data.frame(
      group = "Vector",
      value = c(0.34083203, 0.69272339, 0.538751658)
    ),
    data.frame(
      group = "ASS1_OE",
      value = c(0.956917577, 0.887418202, 0.941327513)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "ASS1_OE")))


##p_mtor/mtro
p_mtor_mtor <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.695294588, 0.636828916, 0.556887131)
    ),
    data.frame(
      group = "Vector",
      value = c(0.936676416, 0.947879098, 0.850644879)
    ),
    data.frame(
      group = "ASS1_OE",
      value = c(1.424628311, 1.067333844, 1.203624668)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "ASS1_OE")))

###p_s6/s6
p_s6_s6 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.21747534, 0.235741638, 0.284831828)
    ),
    data.frame(
      group = "Vector",
      value = c(0.356215461, 0.387966277, 0.486224979)
    ),
    data.frame(
      group = "ASS1_OE",
      value = c(1.155752219, 1.041099128, 1.167995339)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "ASS1_OE")))


dir.create(
  "3_data_analysis/8_overexpression_of_ass1/5_western_blot",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/8_overexpression_of_ass1/5_western_blot")


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
  scale_color_manual(values = disease_color2)

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
  scale_color_manual(values = disease_color2)

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
  scale_color_manual(values = disease_color2)

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
  scale_color_manual(values = disease_color2)

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
  scale_color_manual(values = disease_color2)

plot

ggsave(plot,
       filename = "p_s6_s6.pdf",
       width = 6,
       height = 5)
