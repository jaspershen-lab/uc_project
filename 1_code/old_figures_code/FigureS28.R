library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/old_figures/FigureS28",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/old_figures/FigureS28")


###A
##ass1
ass1 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.596824842, 0.460759115, 0.515849832)
    ),
    data.frame(
      group = "Control+C-01",
      value = c(0.804701257, 0.581008433, 0.888824764)
    ),
    data.frame(
      group = "Model",
      value = c(1.048538732, 0.929386739, 1.162843934)
    ),
    data.frame(
      group = "Model+C-01",
      value = c(0.92005856, 0.91472337, 1.02029324)
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("Control", "Control+C-01", "Model", "Model+C-01")
  ))



##cox2
cox2 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.565387095, 0.637209897, 0.57521189)
    ),
    data.frame(
      group = "Control+C-01",
      value = c(0.636000125, 0.651303468, 0.565807325)
    ),
    data.frame(
      group = "Model",
      value = c(1.146455884, 1.069920641, 1.060035521)
    ),
    data.frame(
      group = "Model+C-01",
      value = c(0.796750857, 0.689109421, 0.570079816)
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("Control", "Control+C-01", "Model", "Model+C-01")
  ))



###p_stat3/stat3
p_stat3_stat3 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.64637323, 0.545182742, 0.618375296)
    ),
    data.frame(
      group = "Control+C-01",
      value = c(0.764658222, 0.734576943, 0.615982105)
    ),
    data.frame(
      group = "Model",
      value = c(0.956199986, 1.075216385, 1.033274901)
    ),
    data.frame(
      group = "Model+C-01",
      value = c(0.810771826, 0.814587351, 0.772700425)
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("Control", "Control+C-01", "Model", "Model+C-01")
  ))


##p_mtor/mtro
p_mtor_mtor <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.453048581, 0.628154679, 0.721615942)
    ),
    data.frame(
      group = "Control+C-01",
      value = c(0.42426068, 0.79010669, 0.720993886)
    ),
    data.frame(
      group = "Model",
      value = c(1.206677577, 0.924393092, 1.069165189)
    ),
    data.frame(
      group = "Model+C-01",
      value = c(0.452459235, 0.709843147, 0.784017667)
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("Control", "Control+C-01", "Model", "Model+C-01")
  ))

###p_s6/s6
p_s6_s6 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.720363278, 0.647600758, 0.643420806)
    ),
    data.frame(
      group = "Control+C-01",
      value = c(0.783461573, 0.748001276, 0.775360646)
    ),
    data.frame(
      group = "Model",
      value = c(1.057008134, 1.065920724, 1.244080779)
    ),
    data.frame(
      group = "Model+C-01",
      value = c(0.746452576, 0.639539714, 0.736473902)
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("Control", "Control+C-01", "Model", "Model+C-01")
  ))

###ass1
plot <-
  ggbetweenstats(
    data  = ass1,
    x  = group,
    y  = value,
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
  labs(x = "", y = "Protein/GAPDH", title = "ASS1") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))

plot

ggsave(plot,
       filename = "figure_s28a_1.pdf",
       width = 5,
       height = 5)

##cox2
plot <-
  ggbetweenstats(
    data  = cox2,
    x  = group,
    y  = value,
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
  labs(x = "", y = "Protein/GAPDH", title = "COX2") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))
# scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "figure_s28a_2.pdf",
       width = 5,
       height = 5)

##p_stat3/stat3
plot <-
  ggbetweenstats(
    data  = p_stat3_stat3,
    x  = group,
    y  = value,
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
# scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "figure_s28a_3.pdf",
       width = 5,
       height = 5)

##p_mtor/mtor
plot <-
  ggbetweenstats(
    data  = p_mtor_mtor,
    x  = group,
    y  = value,
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
# scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "figure_s28a_4.pdf",
       width = 5,
       height = 5)

###p_s6/s6
plot <-
  ggbetweenstats(
    data  = p_s6_s6,
    x  = group,
    y  = value,
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
  labs(x = "", y = "p-S6/S6", title = "p-S6/S6") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))
# scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "figure_s28a_5.pdf",
       width = 5,
       height = 5)


###B
data <-
  data.frame(
    group = c("Control", "Control+C-01", "Model", "Model+C-01"),
    value = c(
      0.661312678,
      0.80436925,
      1.064802906,
      0.802479438,
      0.552457776,
      0.684441874,
      1.110378156,
      0.602185617,
      0.413940732,
      0.762452909,
      1.058324574,
      0.546089482
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("Control", "Control+C-01", "Model", "Model+C-01")
  ))


plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = value,
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
  labs(x = "", y = "Protein/GAPDH", title = "IL-1beta") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))
# scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "figure_s28b.pdf",
       width = 5,
       height = 5)
