library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/old_figures/Figure6",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/old_figures/Figure6")

###figure 6A
##ass1
ass1 <-
  rbind(
    data.frame(
      group = "Vector",
      value = c(0.489092653, 0.235837795, 0.302871213)
    ),
    data.frame(
      group = "Arg",
      value = c(0.668172941, 0.528930171, 0.557249764)
    ),
    data.frame(
      group = "IL6",
      value = c(0.921626512, 1.115860317, 0.816622319)
    ),
    data.frame(
      group = "Arg+IL6",
      value = c(1.278440945, 1.438837573, 1.115309396)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Vector", "Arg", "IL6", "Arg+IL6")))

##cox2
cox2 <-
  rbind(
    data.frame(
      group = "Vector",
      value = c(0.394493584, 0.207758711, 0.493235411)
    ),
    data.frame(
      group = "Arg",
      value = c(0.528662549, 0.352664357, 0.577426958)
    ),
    data.frame(
      group = "IL6",
      value = c(0.871393842, 0.625200346, 0.853687185)
    ),
    data.frame(
      group = "Arg+IL6",
      value = c(1.143867857, 1.050503435, 1.122453871)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Vector", "Arg", "IL6", "Arg+IL6")))


###p_stat3/stat3
p_stat3_stat3 <-
  rbind(
    data.frame(
      group = "Vector",
      value = c(0.563318838, 0.337786597, 0.376637026)
    ),
    data.frame(
      group = "Arg",
      value = c(0.600261171, 0.332442159, 0.370568925)
    ),
    data.frame(
      group = "IL6",
      value = c(0.781402893, 0.82291809, 0.731080871)
    ),
    data.frame(
      group = "Arg+IL6",
      value = c(1.045396557, 0.913511111, 1.114302526)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Vector", "Arg", "IL6", "Arg+IL6")))


##p_mtor/mtro
p_mtor_mtor <-
  rbind(
    data.frame(
      group = "Vector",
      value = c(0.638050876, 0.676162381, 0.357799521)
    ),
    data.frame(
      group = "Arg",
      value = c(0.728855078, 0.701709375, 0.425415094)
    ),
    data.frame(
      group = "IL6",
      value = c(0.979021822, 0.97611934, 0.980784859)
    ),
    data.frame(
      group = "Arg+IL6",
      value = c(1.201328451, 1.272820135, 1.453808606)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Vector", "Arg", "IL6", "Arg+IL6")))


###p_s6/s6
p_s6_s6 <-
  rbind(
    data.frame(
      group = "Vector",
      value = c(0.514528554, 0.579782413, 0.54419643)
    ),
    data.frame(
      group = "Arg",
      value = c(0.599572157, 0.558211153, 0.544072343)
    ),
    data.frame(
      group = "IL6",
      value = c(0.79088858, 0.950337554, 0.925424132)
    ),
    data.frame(
      group = "Arg+IL6",
      value = c(0.980448694, 1.096762557, 1.103231812)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Vector", "Arg", "IL6", "Arg+IL6")))


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
       filename = "figure6a_1.pdf",
       width = 5.5,
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
       filename = "figure6a_2.pdf",
       width = 5.5,
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
       filename = "figure6a_3.pdf",
       width = 5.5,
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
       filename = "figure6a_4.pdf",
       width = 5.5,
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
       filename = "figure6a_5.pdf",
       width = 5.5,
       height = 5)

###figure 6B
data <-
  rbind(
    data.frame(
      group = "Vector",
      value = c(0.30261558, 0.227894906, 0.537398928)
    ),
    data.frame(
      group = "Arg",
      value = c(0.709502811, 0.727233876, 0.47250368)
    ),
    data.frame(
      group = "IL6",
      value = c(0.908804688, 0.932742036, 0.809662868)
    ),
    data.frame(
      group = "Arg+IL6",
      value = c(1.128100156, 1.287721735, 1.080292841)
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Vector", "Arg", "IL6", "Arg+IL6")))


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
  labs(x = "", y = "IL-1β/GAPDH", title = "IL-1β") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))

plot

ggsave(plot,
       filename = "figure6b.pdf",
       width = 5.5,
       height = 5)


####figure 6C
##ass1
ass1 <-
  rbind(
    data.frame(
      group = "Vector",
      value = c(0.354592398, 0.348341799, 0.314788239)
    ),
    data.frame(
      group = "ASS1_OE",
      value = c(0.563213755, 0.805794708, 0.725897766)
    ),
    data.frame(
      group = "Vector+IL6",
      value = c(0.474927452, 0.499306956, 0.530472004)
    ),
    data.frame(
      group = "ASS1_OE+IL6",
      value = c(1.00134202, 1.073404301, 0.970780242)
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("Vector", "ASS1_OE", "Vector+IL6", "ASS1_OE+IL6")
  ))

##cox2
cox2 <-
  rbind(
    data.frame(
      group = "Vector",
      value = c(0.498768318, 0.533972112, 0.554292)
    ),
    data.frame(
      group = "ASS1_OE",
      value = c(0.652754706, 0.611520572, 0.570066)
    ),
    data.frame(
      group = "Vector+IL6",
      value = c(0.803548712, 0.958076169, 0.846553)
    ),
    data.frame(
      group = "ASS1_OE+IL6",
      value = c(1.014415536, 0.986269517, 0.993424)
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("Vector", "ASS1_OE", "Vector+IL6", "ASS1_OE+IL6")
  ))


###p_stat3/stat3
p_stat3_stat3 <-
  rbind(
    data.frame(
      group = "Vector",
      value = c(0.53546882, 0.461280632, 0.535429353)
    ),
    data.frame(
      group = "ASS1_OE",
      value = c(0.574280771, 0.512406254, 0.590807898)
    ),
    data.frame(
      group = "Vector+IL6",
      value = c(0.936360054, 0.809353796, 0.789339828)
    ),
    data.frame(
      group = "ASS1_OE+IL6",
      value = c(1.074397515, 0.995889311, 1.038153196)
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("Vector", "ASS1_OE", "Vector+IL6", "ASS1_OE+IL6")
  ))


##p_mtor/mtro
p_mtor_mtor <-
  rbind(
    data.frame(
      group = "Vector",
      value = c(0.422687699, 0.389396758, 0.48399232)
    ),
    data.frame(
      group = "ASS1_OE",
      value = c(0.561469061, 0.260999712, 0.610673855)
    ),
    data.frame(
      group = "Vector+IL6",
      value = c(0.683275641, 0.674350544, 0.764508206)
    ),
    data.frame(
      group = "ASS1_OE+IL6",
      value = c(0.920564648, 1.042755877, 0.888833446)
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("Vector", "ASS1_OE", "Vector+IL6", "ASS1_OE+IL6")
  ))


###p_s6/s6

p_s6_s6 <-
  rbind(
    data.frame(
      group = "Vector",
      value = c(0.581078314, 0.403126809, 0.600116299)
    ),
    data.frame(
      group = "ASS1_OE",
      value = c(0.787376513, 0.610206887, 0.824805325)
    ),
    data.frame(
      group = "Vector+IL6",
      value = c(0.716664117, 0.701777246, 0.847516142)
    ),
    data.frame(
      group = "ASS1_OE+IL6",
      value = c(1.009808828, 1.014697127, 0.958655469)
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("Vector", "ASS1_OE", "Vector+IL6", "ASS1_OE+IL6")
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
       filename = "figure6c_1.pdf",
       width = 5.5,
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
  labs(x = "", y = "COX2", title = 'COX2') +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))
# scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "figure6c_2.pdf",
       width = 5.5,
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
       filename = "figure6c_3.pdf",
       width = 5.5,
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
       filename = "figure6c_4.pdf",
       width = 5.5,
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
       filename = "figure6c_5.pdf",
       width = 5.5,
       height = 5)


###figure 6D
data <-
  rbind(
    data.frame(
      group = "Vector",
      value = c(0.595363554, 0.550317677, 0.574797269)
    ),
    data.frame(
      group = "ASS1_OE",
      value = c(0.668409678, 0.49595137, 0.604463068)
    ),
    data.frame(
      group = "Vector+IL6",
      value = c(0.959190357, 0.803242334, 0.958048926)
    ),
    data.frame(
      group = "ASS1_OE+IL6",
      value = c(1.067675111, 1.018646342, 1.28074917)
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("Vector", "ASS1_OE", "Vector+IL6", "ASS1_OE+IL6")
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
  labs(x = "", y = "IL-1beta/GAPDH", title = "IL-1beta") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))

plot

ggsave(plot,
       filename = "figure6d.pdf",
       width = 5.5,
       height = 5)
