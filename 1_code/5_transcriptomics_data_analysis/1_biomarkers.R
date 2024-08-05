library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

data <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 7)

dir.create(
  "3_data_analysis/5_transcriptomics_data_analysis/1_biomarker",
  showWarnings = FALSE,
  recursive = TRUE
)
setwd("3_data_analysis/5_transcriptomics_data_analysis/1_biomarker")


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
       filename = "ASS1_boxplot.pdf",
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
       filename = "asl_boxplot.pdf",
       width = 5,
       height = 5)
