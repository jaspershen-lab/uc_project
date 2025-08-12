library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/old_figures/Figure8",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/old_figures/Figure8")

###Fiuture 8A

data <-
  data.frame(
    group = c(rep("0", 3), rep("12", 3), rep("24", 3), rep("48", 3)),
    value = c(
      0.897462978,
      0.982103094,
      1.134557154,
      2.607522585,
      1.89902566,
      2.329144563,
      2.252871076,
      2.865153215,
      4.093237609,
      2.694504772,
      2.581733203,
      3.435013528
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("0", "12", "24", "48")))

# Calculate medians
median_data <-
  data %>%
  dplyr::group_by(group) %>%
  dplyr::summarize(median_value = median(value), .groups = 'drop')

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
  geom_line(data = median_data,
            aes(x = as.numeric(group), y = median_value),
            position = position_dodge(0.35)) +
  theme_base +
  labs(x = "Time (hours)", y = "Relative ASS1 mRNA level") +
  theme(legend.position = "")

plot

ggsave(plot,
       filename = "figure8a.pdf",
       width = 6,
       height = 5)

t.test(data$value[data$group == "0"], data$value[data$group == "12"])
t.test(data$value[data$group == "12"], data$value[data$group == "24"])
t.test(data$value[data$group == "24"], data$value[data$group == "48"])



####figure 8b
data1 <-
  data.frame(
    group = c(rep("Control", 3), rep("IL-6", 3)),
    value = c(
      0.175447504,
      0.259025795,
      0.167224769,
      0.83519312,
      0.60433561,
      0.581543272
    ),
    class = "Promoter-AcH3"
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "IL-6")))

data2 <-
  data.frame(
    group = c(rep("Control", 3), rep("IL-6", 3)),
    value = c(
      0.217560139,
      0.160394373,
      0.055438869,
      0.163425975,
      0.102518817,
      0.168638125
    ),
    class = "Exon1-AcH3"
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "IL-6")))


data <-
  rbind(data1, data2) %>%
  dplyr::mutate(class = factor(class, levels = c("Promoter-AcH3", "Exon1-AcH3")))


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
  geom_signif(
    comparisons = list(c("Control", "IL-6")),
    map_signif_level = FALSE,
    textsize = 3,
    test = "t.test"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot

ggsave(plot,
       filename = "figure8b.pdf",
       width = 4,
       height = 5)


t.test(data$value[data$group == "Control" & data$class == "Promoter-AcH3"],
       data$value[data$group == "IL-6" & data$class == "Promoter-AcH3"])

t.test(data$value[data$group == "Control" & data$class == "Exon1-AcH3"],
       data$value[data$group == "IL-6" & data$class == "Exon1-AcH3"])


###Figure 8c
data1 <-
  data.frame(
    group = c(rep("Control", 3), rep("IL-6", 3)),
    value = c(
      0.167320238,
      0.109933136,
      0.21496421,
      0.096016614,
      0.133482433,
      0.175749141
    ),
    class = "Promoter-H3K27Me3"
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "IL-6")))

data2 <-
  data.frame(
    group = c(rep("Control", 3), rep("IL-6", 3)),
    value = c(
      0.071311546,
      0.041483663,
      0.050131888,
      0.073712097,
      0.014339611,
      0.079534149
    ),
    class = "Exon1-H3K27Me3"
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "IL-6")))

data <-
  rbind(data1, data2) %>%
  dplyr::mutate(class = factor(class, levels = c("Promoter-H3K27Me3", "Exon1-H3K27Me3")))

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
  geom_signif(
    comparisons = list(c("Control", "IL-6")),
    map_signif_level =FALSE,
    textsize = 3,
    test = "t.test"
  ) %>%
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot

ggsave(plot,
       filename = "figure8c.pdf",
       width = 4,
       height = 5)

t.test(data$value[data$group == "Control" & data$class == "Promoter-H3K27Me3"],
       data$value[data$group == "IL-6" & data$class == "Promoter-H3K27Me3"])

t.test(data$value[data$group == "Control" & data$class == "Exon1-H3K27Me3"],
       data$value[data$group == "IL-6" & data$class == "Exon1-H3K27Me3"])


###H3K4Me3 Figure 8d
data1 <-
  data.frame(
    group = c(rep("Control", 3), rep("IL-6", 3)),
    value = c(
      0.233593023,
      0.329008845,
      0.234458474,
      0.9741304,
      0.756954468,
      0.669553272
    ),
    class = "Promoter-H3K4Me3"
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "IL-6")))

data2 <-
  data.frame(
    group = c(rep("Control", 3), rep("IL-6", 3)),
    value = c(
      0.178469436,
      0.127347462,
      0.087809013,
      0.147869514,
      0.180611064,
      0.10535037
    ),
    class = "Exon1-H3K4Me3"
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "IL-6")))


data <-
  rbind(data1, data2) %>%
  dplyr::mutate(class = factor(class, levels = c("Promoter-H3K4Me3", "Exon1-H3K4Me3")))

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
  geom_signif(
    comparisons = list(c("Control", "IL-6")),
    map_signif_level = TRUE,
    textsize = 3,
    test = "t.test"
  ) %>%
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot

ggsave(plot,
       filename = "figure8d.pdf",
       width = 4,
       height = 5)


t.test(data$value[data$group == "Control" & data$class == "Promoter-H3K4Me3"],
       data$value[data$group == "IL-6" & data$class == "Promoter-H3K4Me3"])

t.test(data$value[data$group == "Control" & data$class == "Exon1-H3K4Me3"],
       data$value[data$group == "IL-6" & data$class == "Exon1-H3K4Me3"])


##H3K9Me2 Figure 8e
data1 <-
  data.frame(
    group = c(rep("Control", 3), rep("IL-6", 3)),
    value = c(
      0.134622751,
      0.164267967,
      0.094499494,
      0.066049115,
      0.076722804,
      0.042275803
    ),
    class = "Promoter-H3K9Me2"
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "IL-6")))

data2 <-
  data.frame(
    group = c(rep("Control", 3), rep("IL-6", 3)),
    value = c(
      0.100393676,
      0.209876351,
      0.123254144,
      0.135980258,
      0.128160587,
      0.057825048
    ),
    class = "Exon1-H3K9Me2"
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "IL-6")))

data <-
  rbind(data1, data2) %>%
  dplyr::mutate(class = factor(class, levels = c("Promoter-H3K9Me2", "Exon1-H3K9Me2")))

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
  geom_signif(
    comparisons = list(c("Control", "IL-6")),
    map_signif_level = TRUE,
    textsize = 3,
    test = "t.test"
  ) %>%
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot

ggsave(plot,
       filename = "figure8e.pdf",
       width = 4,
       height = 5)

t.test(data$value[data$group == "Control" & data$class == "Promoter-H3K9Me2"],
       data$value[data$group == "IL-6" & data$class == "Promoter-H3K9Me2"])

t.test(data$value[data$group == "Control" & data$class == "Exon1-H3K9Me2"],
       data$value[data$group == "IL-6" & data$class == "Exon1-H3K9Me2"])


###Figure 8H
data <-
  data.frame(
    group = c(rep("Con siRNA", 3), rep("c-Myc siRNA", 3)),
    value = c(
      1.071955177,
      0.908595857,
      1.019448966,
      0.255259096,
      0.20464967,
      0.327144854
    )
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Con siRNA", "c-Myc siRNA")))

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
  labs(x = "", y = "C-Myc protein level (fold change)") +
  theme(legend.position = "") %>%
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot

ggsave(plot,
       filename = "figure8h.pdf",
       width = 5,
       height = 5)



###Figure 8I
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
  theme(legend.position = "") %>%
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot

ggsave(plot,
       filename = "figure8i.pdf",
       width = 6,
       height = 5)


t.test(data$value[data$group == "Control"],
       data$value[data$group == "c-Myc"])

t.test(data$value[data$group == "Control"],
       data$value[data$group == "IL-6"])

t.test(data$value[data$group == "Control"],
       data$value[data$group == "IL-6+c-Myc"])

t.test(data$value[data$group == "c-Myc"],
       data$value[data$group == "IL-6"])

t.test(data$value[data$group == "c-Myc"],
       data$value[data$group == "IL-6+c-Myc"])

t.test(data$value[data$group == "IL-6"],
       data$value[data$group == "IL-6+c-Myc"])



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
  labs(x = "", y = "Immunoprecipitated protein (fold change)") +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_signif(
    comparisons = list(c("Control", "IL-6")),
    map_signif_level = TRUE,
    textsize = 3,
    test = "t.test"
  ) %>%
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot

ggsave(plot,
       filename = "Figure8n.pdf",
       width = 5,
       height = 5)



t.test(data$value[data$group == "Control" & data$class == "IP: P300 IB:c-Myc"],
       data$value[data$group == "IL-6" & data$class == "IP: P300 IB:c-Myc"])

t.test(data$value[data$group == "Control" & data$class == "IP: KDM3A IB:c-Myc"],
       data$value[data$group == "IL-6" & data$class == "IP: KDM3A IB:c-Myc"])

t.test(data$value[data$group == "Control" & data$class == "IP: KDM3A IB:P300"],
       data$value[data$group == "IL-6" & data$class == "IP: KDM3A IB:P300"])
