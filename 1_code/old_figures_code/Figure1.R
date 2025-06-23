library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/old_figures/Figure1",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/old_figures/Figure1")

###Figure 1D
data <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig1D")

data1 <-
  data[, c(1, 2)] %>%
  as.data.frame()

data1 <-
  data1[-c(1), ]

data2 <-
  data[, c(4, 5)] %>%
  as.data.frame()

data2 <-
  data2[-c(1), ]

data3 <-
  data[, c(7, 8)] %>%
  as.data.frame()

data3 <-
  data3[-c(1), ]

colnames(data1) <-
  colnames(data2) <-
  colnames(data3) <-
  c("Control", "UC")

data1 <-
  rbind(
    data.frame(
      sample_id = paste0("Control_", 1:nrow(data1)),
      group = "Control",
      center = "Hospital 1",
      value = data1$Control
    ),
    data.frame(
      sample_id = paste0("UC_", 1:nrow(data1)),
      group = "UC",
      center = "Hospital 1",
      value = data1$UC
    )
  ) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(value = as.numeric(value))


data2 <-
  rbind(
    data.frame(
      sample_id = paste0("Control_", 1:nrow(data2)),
      group = "Control",
      center = "Hospital 2",
      value = data2$Control
    ),
    data.frame(
      sample_id = paste0("UC_", 1:nrow(data2)),
      group = "UC",
      center = "Hospital 2",
      value = data2$UC
    )
  ) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(value = as.numeric(value))


data3 <-
  rbind(
    data.frame(
      sample_id = paste0("Control_", 1:nrow(data3)),
      group = "Control",
      center = "Hospital 3",
      value = data3$Control
    ),
    data.frame(
      sample_id = paste0("UC_", 1:nrow(data3)),
      group = "UC",
      center = "Hospital 3",
      value = data3$UC
    )
  ) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(value = as.numeric(value))


data <-
  rbind(data1, data2, data3)

library(ggstatsplot)

plot1 <-
  ggbetweenstats(
    data  = data1,
    x  = group,
    y  = value,
    type = "nonparametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4)
  ) +
  theme_base +
  labs(x = "", y = "Concentration (ug/mL)") +
  theme(legend.position = "") +
  scale_color_manual(values = group_color) +
  scale_y_continuous(limits = c(12, 51), expand = c(0.1, 0.1))

plot1


wilcox.test(data1$value[data1$group == "Control"],
            data1$value[data1$group == "UC"])

mean(data1$value[data1$group == "UC"])/
mean(data1$value[data1$group == "Control"])


ggsave(plot1,
       filename = "figure1d_1.pdf",
       width = 5,
       height = 5)

plot2 <-
  ggbetweenstats(
    data  = data2,
    x  = group,
    y  = value,
    type = "nonparametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    )
  ) +
  theme_base +
  labs(x = "", y = "Concentration (ug/mL)") +
  theme(legend.position = "") +
  scale_color_manual(values = group_color) +
  scale_y_continuous(limits = c(12, 51), expand = c(0.1, 0.1))

plot2

ggsave(plot2,
       filename = "figure1d_2.pdf",
       width = 5,
       height = 5)

wilcox.test(data2$value[data2$group == "Control"],
            data2$value[data2$group == "UC"])

mean(data2$value[data2$group == "UC"])/
  mean(data2$value[data2$group == "Control"])


plot3 <-
  ggbetweenstats(
    data  = data3,
    x  = group,
    y  = value,
    type = "nonparametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    )
  ) +
  theme_base +
  labs(x = "", y = "Concentration (ug/mL)") +
  theme(legend.position = "") +
  scale_color_manual(values = group_color) +
  scale_y_continuous(limits = c(13, 51), expand = c(0.1, 0.1))

plot3

ggsave(plot3,
       filename = "figure1d_3.pdf",
       width = 5,
       height = 5)


wilcox.test(data3$value[data3$group == "Control"],
            data3$value[data3$group == "UC"])

mean(data3$value[data3$group == "UC"])/
  mean(data3$value[data3$group == "Control"])

###Figure 1G
data <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig1G")

data_ass1 <-
  data[, c(1, 2)]

data_ass1 <-
  data_ass1[-1, ]

colnames(data_ass1) <-
  c("Control", "UC")

data_ass1 <-
  rbind(
    data.frame(group = "Control", value = as.numeric(as.character(data_ass1$Control))),
    data.frame(group = "UC", value = as.numeric(as.character(data_ass1$UC)))
  )


library(ggstatsplot)

plot <-
  ggbetweenstats(
    data  = data_ass1,
    x  = group,
    y  = value,
    type = "nonparametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    )
  ) +
  theme_base +
  labs(x = "", y = "Relative mRNA level (ASS1)") +
  theme(legend.position = "") +
  scale_color_manual(values = group_color)

plot

ggsave(plot,
       filename = "figure1g_1.pdf",
       width = 5,
       height = 5)


data_asl <-
  data[, c(4, 5)]

data_asl <-
  data_asl[-1, ]

colnames(data_asl) <-
  c("Control", "UC")

data_asl <-
  rbind(
    data.frame(group = "Control", value = as.numeric(as.character(data_asl$Control))),
    data.frame(group = "UC", value = as.numeric(as.character(data_asl$UC)))
  )

library(ggstatsplot)

plot <-
  ggbetweenstats(
    data  = data_asl,
    x  = group,
    y  = value,
    type = "nonparametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    )
  ) +
  theme_base +
  labs(x = "", y = "Relative mRNA level (ASL)") +
  theme(legend.position = "") +
  scale_color_manual(values = group_color)

plot

ggsave(plot,
       filename = "figure1g_2.pdf",
       width = 5,
       height = 5)


###Figure 1I
data <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig1I")
colnames(data) <-
  c("Control", "UC")

data <-
  rbind(
    data.frame(group = "Control", value = as.numeric(as.character(data$Control))),
    data.frame(group = "UC", value = as.numeric(as.character(data$UC)))
  )

library(ggstatsplot)

plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    )
  ) +
  theme_base +
  labs(x = "", y = "IHC score") +
  theme(legend.position = "") +
  scale_color_manual(values = group_color)

plot

ggsave(plot,
       filename = "figure1i.pdf",
       width = 5,
       height = 5)

t.test(data$value[data$group == "Control"],
       data$value[data$group == "UC"])

###Figure 1J
data2 <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig1J")

###IHC score with mayo score

test <-
  cor.test(data2$ASS1, data2$`Mayo Score`, method = "pearson")

test

plot <-
  data2 %>%
  dplyr::rename(IHC_score = ASS1, Mayo_Score = `Mayo Score`) %>%
  ggplot(aes(IHC_score, Mayo_Score)) +
  geom_point(
    size = 5,
    shape = 21,
    fill = "black",
    color = "red"
  ) +
  theme_base +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "red") +
  annotate(
    "text",
    x = 2,
    y = 5,
    label = paste0(
      "r = ",
      round(test$estimate, 2),
      "\n",
      "p = ",
      round(test$p.value, 2)
    ),
    hjust = 0,
    vjust = 1
  ) +
  labs(x = "IHC score", y = "Mayo score")

plot

ggsave(plot,
       filename = "figure1j.pdf",
       width = 5,
       height = 5)

