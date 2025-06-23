library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/12_epigenetic/3_si_rna",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/12_epigenetic/3_si_rna")

data <-
  data.frame(
    group = c(
      rep("Control", 3),
      rep("c-Myc", 3),
      rep("IL-6", 3),
      rep("IL-6+c-Myc", 3)
    ),
    value = c(
      0.903546899,
      0.982052803,
      1.126975464,
      0.416375609,
      0.716478547,
      0.602290398,
      3.072046943,
      3.032890978,
      4.548424634,
      1.186867861,
      0.94413557,
      1.472749683
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "c-Myc", "IL-6", "IL-6+c-Myc")))


plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    centrality.type = "nonparametric",
    point.args = list(
      alpha = 0.9,
      size = 3,
      color = "black",
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4)
  ) +
  theme_base +
  labs(x = "", y = "Relative ASS1 mRNA level") +
  theme(legend.position = "")

plot

ggsave(plot,
       filename = "figure8i.pdf",
       width = 6,
       height = 5)

######Figure 8J
data1 <-
  data.frame(
    group = c(
      rep("Con siRNA", 3),
      rep("c-Myc siRNA", 3),
      rep("IL-6", 3),
      rep("IL-6+c-Myc siRNA", 3)
    ),
    value = c(
      0.67424775,
      0.453466032,
      0.607918089,
      0.319569642,
      0.189026627,
      0.290531472,
      1.189602437,
      1.351410689,
      1.37727331,
      0.735427352,
      0.489868303,
      0.63020071
    ),
    class = "Promoter-AcH3"
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("Con siRNA", "c-Myc siRNA", "IL-6", "IL-6+c-Myc siRNA")
  ))

data2 <-
  data.frame(
    group = c(
      rep("Con siRNA", 3),
      rep("c-Myc siRNA", 3),
      rep("IL-6", 3),
      rep("IL-6+c-Myc siRNA", 3)
    ),
    value = c(
      0.194260198,
      0.252047572,
      0.151702374,
      0.360854019,
      0.118696385,
      0.149369513,
      0.223290207,
      0.260127467,
      0.135332277,
      0.345282257,
      0.197095412,
      0.137639843
    ),
    class = "Exon-AcH3"
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("Con siRNA", "c-Myc siRNA", "IL-6", "IL-6+c-Myc siRNA")
  ))


data <-
  rbind(data1, data2) %>%
  dplyr::mutate(class = factor(class, levels = c("Promoter-AcH3", "Exon-AcH3")))


plot <-
  data %>%
  ggplot(aes(x = group, y = value)) +
  geom_boxplot(aes(group = group), outlier.shape = NA) +
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = 0.5) +
  facet_grid(cols = vars(class), scales = "free_y") +
  theme_base +
  labs(x = "", y = "Relative enrichment (% input)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_signif(
    comparisons = list(
      c("Con siRNA", "c-Myc siRNA"),
      c("Con siRNA", "IL-6"),
      # c("Con siRNA", "IL-6+c-Myc siRNA"),
      c("c-Myc siRNA", "IL-6"),
      c("c-Myc siRNA", "IL-6+c-Myc siRNA")
    ),
    map_signif_level = TRUE,
    textsize = 3,
    test = "t.test"
  )

plot
ggsave(plot,
       filename = "Figure8j.pdf",
       width = 6,
       height = 6)

##Figure 8K H3K4Me3
data1 <-
  data.frame(
    group = c(
      rep("Con siRNA", 3),
      rep("c-Myc siRNA", 3),
      rep("IL-6", 3),
      rep("IL-6+c-Myc siRNA", 3)
    ),
    value = c(
      0.745170069,
      0.613102799,
      0.79601101,
      0.313548657,
      0.232487268,
      0.265758228,
      1.510378811,
      1.249147162,
      1.393631763,
      0.609936822,
      0.460186634,
      0.77869187
    ),
    class = "Promoter-H3K4Me3"
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("Con siRNA", "c-Myc siRNA", "IL-6", "IL-6+c-Myc siRNA")
  ))

data2 <-
  data.frame(
    group = c(
      rep("Con siRNA", 3),
      rep("c-Myc siRNA", 3),
      rep("IL-6", 3),
      rep("IL-6+c-Myc siRNA", 3)
    ),
    value = c(
      0.473229804,
      0.336585214,
      0.244170049,
      0.244635016,
      0.334062719,
      0.397439865,
      0.668005141,
      0.291169579,
      0.189415098,
      0.350690607,
      0.327359425,
      0.398509109
    ),
    class = "Exon-H3K4Me3"
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("Con siRNA", "c-Myc siRNA", "IL-6", "IL-6+c-Myc siRNA")
  ))


data <-
  rbind(data1, data2) %>%
  dplyr::mutate(class = factor(class, levels = c("Promoter-H3K4Me3", "Exon-H3K4Me3")))

plot <-
  data %>%
  ggplot(aes(x = group, y = value)) +
  geom_boxplot(aes(group = group), outlier.shape = NA) +
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = 0.5) +
  facet_grid(cols = vars(class), scales = "free_y") +
  theme_base +
  labs(x = "", y = "Relative enrichment (% input)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_signif(
    comparisons = list(
      c("Con siRNA", "c-Myc siRNA"),
      c("Con siRNA", "IL-6"),
      c("Con siRNA", "IL-6+c-Myc siRNA"),
      c("c-Myc siRNA", "IL-6"),
      c("c-Myc siRNA", "IL-6+c-Myc siRNA")
    ),
    map_signif_level = TRUE,
    textsize = 3,
    test = "t.test"
  )

plot

ggsave(plot,
       filename = "Figure8k.pdf",
       width = 6,
       height = 6)


###Figure 8L
data1 <-
  data.frame(
    group = c(
      rep("Con siRNA", 3),
      rep("c-Myc siRNA", 3),
      rep("IL-6", 3),
      rep("IL-6+c-Myc siRNA", 3)
    ),
    value = c(
      0.488079767,
      0.340863836,
      0.29555322,
      0.996505271,
      0.661812939,
      0.804835845,
      0.12397371,
      0.062068969,
      0.107313614,
      0.287918826,
      0.373381108,
      0.440124035
    ),
    class = "Promoter-H3K9Me2"
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("Con siRNA", "c-Myc siRNA", "IL-6", "IL-6+c-Myc siRNA")
  ))

data2 <-
  data.frame(
    group = c(
      rep("Con siRNA", 3),
      rep("c-Myc siRNA", 3),
      rep("IL-6", 3),
      rep("IL-6+c-Myc siRNA", 3)
    ),
    value = c(
      0.097559414,
      0.10115572,
      0.077801275,
      0.072825847,
      0.064761536,
      0.096237242,
      0.122087492,
      0.125185686,
      0.084203772,
      0.10785539,
      0.098921352,
      0.066185635
    ),
    class = "Exon-H3K9Me2"
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("Con siRNA", "c-Myc siRNA", "IL-6", "IL-6+c-Myc siRNA")
  ))


data <-
  rbind(data1, data2) %>%
  dplyr::mutate(class = factor(class, levels = c("Promoter-H3K9Me2", "Exon-H3K9Me2")))

plot <-
  data %>%
  ggplot(aes(x = group, y = value)) +
  geom_boxplot(aes(group = group), outlier.shape = NA) +
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = 0.5) +
  facet_grid(cols = vars(class), scales = "free_y") +
  theme_base +
  labs(x = "", y = "Relative enrichment (% input)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_signif(
    comparisons = list(
      c("Con siRNA", "c-Myc siRNA"),
      c("Con siRNA", "IL-6"),
      c("Con siRNA", "IL-6+c-Myc siRNA"),
      c("c-Myc siRNA", "IL-6"),
      c("c-Myc siRNA", "IL-6+c-Myc siRNA")
    ),
    map_signif_level = TRUE,
    textsize = 3,
    test = "t.test"
  )

plot

ggsave(plot,
       filename = "Figure8l.pdf",
       width = 6,
       height = 6)



######figure 8n
data1 <-
  data.frame(
    group = c(rep("Control", 3), rep("IL-6", 3)),
    value = c(
      0.952016353,
      1.070176553,
      0.977807093,
      1.905821735,
      2.133104291,
      2.505783915
    ),
    class = "IP: P300 IB:c-Myc"
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "IL-6")))

data2 <-
  data.frame(
    group = c(rep("Control", 3), rep("IL-6", 3)),
    value = c(
      1.027955278,
      1.052724924,
      0.919319798,
      1.946260628,
      1.914762261,
      2.424039025
    ),
    class = "IP: KDM3A IB:c-Myc"
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "IL-6")))

data3 <-
  data.frame(
    group = c(rep("Control", 3), rep("IL-6", 3)),
    value = c(
      1.061624371,
      0.981292659,
      0.95708297,
      1.811435397,
      2.280474509,
      1.964694769
    ),
    class = "IP: KDM3A IB:P300"
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "IL-6")))


data <-
  rbind(data1, data2, data3) %>%
  dplyr::mutate(class = factor(
    class,
    levels = c("IP: P300 IB:c-Myc", "IP: KDM3A IB:c-Myc", "IP: KDM3A IB:P300")
  ))


plot <-
  data %>%
  ggplot(aes(x = group, y = value)) +
  geom_boxplot(aes(group = group), outlier.shape = NA) +
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = 0.5) +
  facet_grid(cols = vars(class), scales = "free_y") +
  theme_base +
  labs(x = "", y = "Relative enrichment (% input)") +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_signif(
    comparisons = list(c("Control", "IL-6")),
    map_signif_level = TRUE,
    textsize = 3,
    test = "t.test"
  )

plot

ggsave(plot,
       filename = "Figure8l.pdf",
       width = 6,
       height = 5)
