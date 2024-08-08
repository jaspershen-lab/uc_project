library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/old_figures/FigureS23",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/old_figures/FigureS23")

####A
data <-
  data.frame(
    group = c(rep("Control", 10), rep("Model", 10), rep("Model+MDLA", 10)),
    value = c(
      4.90156,
      1.5827,
      5.65098,
      7.471,
      4.04508,
      3.08154,
      4.7945,
      2.22506,
      4.2592,
      5.75804,
      8.11336,
      7.36394,
      5.97216,
      5.65098,
      8.0063,
      6.9357,
      8.0063,
      9.0769,
      7.89924,
      4.2592,
      8.0063,
      6.82864,
      4.2592,
      2.118,
      3.61684,
      5.75804,
      4.04508,
      3.08154,
      3.1886,
      6.9357
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Model", "Model+MDLA")))

data

library(ggstatsplot)

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
  labs(x = "", y = "NO level (uM)") +
  theme(legend.position = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot


t.test(as.numeric(data$value[data$group == "Control"]),
       as.numeric(data$value[data$group == "Model"]))

t.test(as.numeric(data$value[data$group == "Control"]),
       as.numeric(data$value[data$group == "Model+MDLA"]))

t.test(as.numeric(data$value[data$group == "Model"]),
       as.numeric(data$value[data$group == "Model+MDLA"]))

ggsave(plot,
       filename = "figure_s23a.pdf",
       width = 5,
       height = 5)



####B
data <-
  data.frame(
    group = c(rep("Control", 10), rep("Model", 10), rep("Model + Arg", 10)),
    value = c(
      3.29566,
      2.118,
      5.75804,
      8.0063,
      4.2592,
      2.97448,
      2.203648,
      2.22506,
      7.36394,
      4.36626,
      6.9357,
      9.18396,
      6.9357,
      6.4004,
      8.0063,
      7.57806,
      5.43686,
      4.04508,
      7.25688,
      9.6122,
      14.9652,
      11.11104,
      14.32284,
      10.57574,
      8.0063,
      10.36162,
      10.6828,
      10.6828,
      5.75804,
      6.82864
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Model", "Model + Arg")))

data

library(ggstatsplot)

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
  labs(x = "", y = "NO level (uM)") +
  theme(legend.position = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot

ggsave(plot,
       filename = "figure_s23b.pdf",
       width = 5,
       height = 5)



t.test(as.numeric(data$value[data$group == "Control"]),
       as.numeric(data$value[data$group == "Model"]))

t.test(as.numeric(data$value[data$group == "Control"]),
       as.numeric(data$value[data$group == "Model + Arg"]))

t.test(as.numeric(data$value[data$group == "Model"]),
       as.numeric(data$value[data$group == "Model + Arg"]))



####c and e
data <-
  data.frame(
    group = c(rep("Control", 4), rep("Model", 4), rep("Model+MDLA", 4)),
    iNOS = c(
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
    ),
    eNOS = c(
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
  dplyr::mutate(group = factor(group, levels = c("Control", "Model", "Model+MDLA")))

data

library(ggstatsplot)

plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = iNOS,
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
  labs(x = "", y = "iNOS protein level") +
  theme(legend.position = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot

ggsave(plot,
       filename = "figure_s23c.pdf",
       width = 5,
       height = 5)

t.test(as.numeric(data$iNOS[data$group == "Control"]),
       as.numeric(data$iNOS[data$group == "Model"]))

t.test(as.numeric(data$iNOS[data$group == "Control"]),
       as.numeric(data$iNOS[data$group == "Model+MDLA"]))

t.test(as.numeric(data$iNOS[data$group == "Model"]),
       as.numeric(data$iNOS[data$group == "Model+MDLA"]))


plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = eNOS,
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
  labs(x = "", y = "eNOS protein level") +
  theme(legend.position = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot

ggsave(plot,
       filename = "figure_s23e.pdf",
       width = 5,
       height = 5)




t.test(as.numeric(data$eNOS[data$group == "Control"]),
       as.numeric(data$eNOS[data$group == "Model"]))

t.test(as.numeric(data$eNOS[data$group == "Control"]),
       as.numeric(data$eNOS[data$group == "Model+MDLA"]))

t.test(as.numeric(data$eNOS[data$group == "Model"]),
       as.numeric(data$eNOS[data$group == "Model+MDLA"]))







####d and f
data <-
  data.frame(
    group = c(rep("Control", 4), rep("Model", 4), rep("Model+MDLA", 4)),
    iNOS = c(
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
    ),
    eNOS = c(
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
  dplyr::mutate(group = factor(group, levels = c("Control", "Model", "Model+MDLA")))

data

library(ggstatsplot)

plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = iNOS,
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
  labs(x = "", y = "iNOS mRNA level") +
  theme(legend.position = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot

ggsave(plot,
       filename = "figure_s23d.pdf",
       width = 5,
       height = 5)







plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = eNOS,
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
  labs(x = "", y = "eNOS mRNA level") +
  theme(legend.position = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot

ggsave(plot,
       filename = "figure_s23f.pdf",
       width = 5,
       height = 5)



























####g and i
data <-
  data.frame(
    group = c(rep("Control", 4), rep("Model", 4), rep("Model+Arginine", 4)),
    iNOS = c(
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
    ),
    eNOS = c(
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
  dplyr::mutate(group = factor(group, levels = c("Control", "Model", "Model+Arginine")))


data

library(ggstatsplot)

plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = iNOS,
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
  labs(x = "", y = "iNOS protein level") +
  theme(legend.position = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot

ggsave(plot,
       filename = "figure_s23g.pdf",
       width = 5,
       height = 5)


t.test(as.numeric(data$iNOS[data$group == "Control"]),
       as.numeric(data$iNOS[data$group == "Model"]))

t.test(as.numeric(data$iNOS[data$group == "Control"]),
       as.numeric(data$iNOS[data$group == "Model+Arginine"]))

t.test(as.numeric(data$iNOS[data$group == "Model"]),
       as.numeric(data$iNOS[data$group == "Model+Arginine"]))




plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = eNOS,
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
  labs(x = "", y = "eNOS protein level") +
  theme(legend.position = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot

ggsave(plot,
       filename = "figure_s23i.pdf",
       width = 5,
       height = 5)



t.test(as.numeric(data$eNOS[data$group == "Control"]),
       as.numeric(data$eNOS[data$group == "Model"]))

t.test(as.numeric(data$eNOS[data$group == "Control"]),
       as.numeric(data$eNOS[data$group == "Model+Arginine"]))

t.test(as.numeric(data$eNOS[data$group == "Model"]),
       as.numeric(data$eNOS[data$group == "Model+Arginine"]))







####h and j

data <-
  data.frame(
    group = c(rep("Control", 4), rep("Model", 4), rep("Model+Arginine", 4)),
    iNOS = c(
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
    ),
    eNOS = c(
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
  dplyr::mutate(group = factor(group, levels = c("Control", "Model", "Model+Arginine")))

data

library(ggstatsplot)

plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = iNOS,
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
  labs(x = "", y = "iNOS mRNA level") +
  theme(legend.position = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot

ggsave(plot,
       filename = "figure_s23h.pdf",
       width = 5,
       height = 5)







plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = eNOS,
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
  labs(x = "", y = "eNOS mRNA level") +
  theme(legend.position = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot

ggsave(plot,
       filename = "figure_s23j.pdf",
       width = 5,
       height = 5)

