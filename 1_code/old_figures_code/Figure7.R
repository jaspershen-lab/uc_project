library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/old_figures/Figure7",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/old_figures/Figure7")

###figure 7A
##ass1
ass1 <-
  rbind(
    data.frame(
      group = "Vector",
      value = c(0.795230993, 0.413167, 0.454768)
    ),
    data.frame(
      group = "MDLA",
      value = c(0.736262999, 0.822662, 0.457999)
    ),
    data.frame(
      group = "IL6",
      value = c(1.014224153, 1.39293, 0.954376)
    ),
    data.frame(
      group = "MDLA+IL6",
      value = c(0.876580302, 1.002143, 0.995564)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Vector", "MDLA", "IL6", "MDLA+IL6")))

##cox2
cox2 <-
  rbind(
    data.frame(
      group = "Vector",
      value = c(0.656939414, 0.490783316, 0.44015891)
    ),
    data.frame(
      group = "MDLA",
      value = c(0.782533875, 0.374688937, 0.605658759)
    ),
    data.frame(
      group = "IL6",
      value = c(0.905524025, 0.942599396, 1.072875546)
    ),
    data.frame(
      group = "MDLA+IL6",
      value = c(0.614224344, 0.530797653, 0.41978175)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Vector", "MDLA", "IL6", "MDLA+IL6")))


###p_stat3/stat3

p_stat3_stat3 <-
  rbind(
    data.frame(
      group = "Vector",
      value = c(0.720697878, 0.750996857, 0.764242218)
    ),
    data.frame(
      group = "MDLA",
      value = c(0.764171855, 0.773018085, 0.835239519)
    ),
    data.frame(
      group = "IL6",
      value = c(0.940779556, 1.000157573, 1.117559934)
    ),
    data.frame(
      group = "MDLA+IL6",
      value = c(0.82387166, 0.752745645, 0.692876642)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Vector", "MDLA", "IL6", "MDLA+IL6")))


##p_mtor/mtro

p_mtor_mtor <-
  rbind(
    data.frame(
      group = "Vector",
      value = c(0.582038011, 0.755509477, 0.4654164)
    ),
    data.frame(
      group = "MDLA",
      value = c(0.851622745, 0.7602236, 0.421847585)
    ),
    data.frame(
      group = "IL6",
      value = c(1.177417121, 1.266810408, 0.917048264)
    ),
    data.frame(
      group = "MDLA+IL6",
      value = c(0.665908851, 0.700550902, 0.49236608)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Vector", "MDLA", "IL6", "MDLA+IL6")))

###p_s6/s6
p_s6_s6 <-
  rbind(
    data.frame(
      group = "Vector",
      value = c(0.238887249, 0.294591818, 0.161988197)
    ),
    data.frame(
      group = "MDLA",
      value = c(0.697552613, 0.538229419, 0.252640606)
    ),
    data.frame(
      group = "IL6",
      value = c(1.089246131, 1.246133698, 1.129604408)
    ),
    data.frame(
      group = "MDLA+IL6",
      value = c(0.67676575, 0.78155146, 0.659926941)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Vector", "MDLA", "IL6", "MDLA+IL6")))

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
  labs(x = "", y = "ASS1", title = "ASS1") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))
# scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "figure7a_1.pdf",
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
  labs(x = "", y = "COX2", title = "COX2") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))
# scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "figure7a_2.pdf",
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
       filename = "figure7a_3.pdf",
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
       filename = "figure7a_4.pdf",
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
       filename = "figure7a_5.pdf",
       width = 5,
       height = 5)


###figure 7B
data <-
  rbind(
    data.frame(
      group = "Vector",
      value = c(0.495535887, 0.641455729, 0.775977196)
    ),
    data.frame(
      group = "MDLA",
      value = c(0.411435233, 0.771040806, 0.843897704)
    ),
    data.frame(
      group = "IL6",
      value = c(1.1547415, 1.132370245, 1.082968149)
    ),
    data.frame(
      group = "MDLA+IL6",
      value = c(0.549558487, 0.484875046, 0.719952871)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Vector", "MDLA", "IL6", "MDLA+IL6")))


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
  labs(x = "", y = "IL-1beta/GAPDH", title = "IL-1beta/GAPDH") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))

plot

ggsave(plot,
       filename = "figure7b.pdf",
       width = 5,
       height = 5)


####figure 7C
##ass1
ass1 <-
  rbind(
    data.frame(
      group = "Control shRNA",
      value = c(0.718586621, 0.637959323, 0.74041697)
    ),
    data.frame(
      group = "ASS1 shRNA",
      value = c(0.352563571, 0.306535177, 0.426683097)
    ),
    data.frame(
      group = "IL6+Control shRNA",
      value = c(1.031157336, 0.963389601, 0.97899837)
    ),
    data.frame(
      group = "IL6+ASS1 shRNA",
      value = c(0.413219263, 0.517132466, 0.667674236)
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c(
      "Control shRNA",
      "ASS1 shRNA",
      "IL6+Control shRNA",
      "IL6+ASS1 shRNA"
    )
  ))

##cox2
cox2 <-
  rbind(
    data.frame(
      group = "Control shRNA",
      value = c(0.747742952, 0.9701863, 0.850169469)
    ),
    data.frame(
      group = "ASS1 shRNA",
      value = c(1.069165455, 1.080551922, 0.836004279)
    ),
    data.frame(
      group = "IL6+Control shRNA",
      value = c(1.471076487, 1.333666017, 1.186605376)
    ),
    data.frame(
      group = "IL6+ASS1 shRNA",
      value = c(0.763389618, 0.79030379, 0.757293025)
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c(
      "Control shRNA",
      "ASS1 shRNA",
      "IL6+Control shRNA",
      "IL6+ASS1 shRNA"
    )
  ))

###p_stat3/stat3
p_stat3_stat3 <-
  rbind(
    data.frame(
      group = "Control shRNA",
      value = c(0.727412758, 0.788049421, 0.501680811)
    ),
    data.frame(
      group = "ASS1 shRNA",
      value = c(0.559971691, 0.69808379, 0.402053933)
    ),
    data.frame(
      group = "IL6+Control shRNA",
      value = c(1.046771521, 1.023812681, 0.978043663)
    ),
    data.frame(
      group = "IL6+ASS1 shRNA",
      value = c(0.789473349, 0.763224893, 0.545185178)
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c(
      "Control shRNA",
      "ASS1 shRNA",
      "IL6+Control shRNA",
      "IL6+ASS1 shRNA"
    )
  ))


##p_mtor/mtro
p_mtor_mtor <-
  rbind(
    data.frame(
      group = "Control shRNA",
      value = c(0.516497815, 0.449565938, 0.311770248)
    ),
    data.frame(
      group = "ASS1 shRNA",
      value = c(0.489666129, 0.592849301, 0.515367632)
    ),
    data.frame(
      group = "IL6+Control shRNA",
      value = c(0.898071252, 0.966507404, 0.895786819)
    ),
    data.frame(
      group = "IL6+ASS1 shRNA",
      value = c(0.681377453, 0.665714248, 0.379119051)
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c(
      "Control shRNA",
      "ASS1 shRNA",
      "IL6+Control shRNA",
      "IL6+ASS1 shRNA"
    )
  ))

###p_s6/s6
p_s6_s6 <-
  rbind(
    data.frame(
      group = "Control shRNA",
      value = c(0.752602443, 0.544300422, 0.636776236)
    ),
    data.frame(
      group = "ASS1 shRNA",
      value = c(0.73597944, 0.558717315, 0.775299165)
    ),
    data.frame(
      group = "IL6+Control shRNA",
      value = c(0.969808744, 1.076075605, 1.003089395)
    ),
    data.frame(
      group = "IL6+ASS1 shRNA",
      value = c(0.800799723, 0.851391141, 0.782112979)
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c(
      "Control shRNA",
      "ASS1 shRNA",
      "IL6+Control shRNA",
      "IL6+ASS1 shRNA"
    )
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
  labs(x = "", y = "ASS1", title = "ASS1") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))
# scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "figure7c_1.pdf",
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
  labs(x = "", y = "COX2", title = "COX2") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))
# scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "figure7c_2.pdf",
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
       filename = "figure7c_3.pdf",
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
       filename = "figure7c_4.pdf",
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
       filename = "figure7c_5.pdf",
       width = 5,
       height = 5)


###figure 7D
data <-
  rbind(
    data.frame(
      group = "Control shRNA",
      value = c(0.502673487, 0.713901939, 0.677662048)
    ),
    data.frame(
      group = "ASS1 shRNA",
      value = c(0.871616448, 0.987420167, 0.688143203)
    ),
    data.frame(
      group = "IL6+Control shRNA",
      value = c(1.179770586, 1.138515012, 1.031117368)
    ),
    data.frame(
      group = "IL6+ASS1 shRNA",
      value = c(0.866989398, 0.921708754, 0.685141538)
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c(
      "Control shRNA",
      "ASS1 shRNA",
      "IL6+Control shRNA",
      "IL6+ASS1 shRNA"
    )
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
  labs(x = "", y = "IL-1beta/GAPDH", title = "IL-1beta/GAPDH") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))

plot

ggsave(plot,
       filename = "figure7d.pdf",
       width = 5,
       height = 5)
