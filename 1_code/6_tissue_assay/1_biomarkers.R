library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

data <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 8)

data2 <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 9)

dir.create(
  "3_data_analysis/6_tissue_assay_analysis/1_biomarker",
  showWarnings = FALSE,
  recursive = TRUE
)
setwd("3_data_analysis/6_tissue_assay_analysis/1_biomarker")

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
  labs(x = "", y = "IHC score") +
  theme(legend.position = "") +
  scale_color_manual(values = group_color)

plot

ggsave(plot,
       filename = "IHC_score_boxplot.pdf",
       width = 5,
       height = 5)



###IHC score with mayo score

test <-
  cor.test(data2$ASS1, data2$`Mayo Score`, method = "pearson")

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
  geom_smooth(method = "lm", se = FALSE,
              color = "red") +
  annotate(
    "text",
    x = 2,
    y = 5,
    label = paste0("r = ", round(test$estimate, 2), "\n",
                   "p = ", round(test$p.value, 2)),
    hjust = 0,
    vjust = 1
  ) +
  labs(x = "IHC score", y = "Mayo score")

plot

ggsave(plot,
       filename = "IHC_score_vs_mayo_score.pdf",
       width = 5,
       height = 5)
