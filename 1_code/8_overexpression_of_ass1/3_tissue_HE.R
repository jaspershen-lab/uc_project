library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

data1 <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 32)

dir.create(
  "3_data_analysis/8_overexpression_of_ass1/3_tissue_HE",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/8_overexpression_of_ass1/3_tissue_HE")


data1

data1 <-
  rbind(
    data.frame(group = "Control", value = data1$Control),
    data.frame(group = "Vector", value = data1$Vector),
    data.frame(group = "ASS1_OE", value = data1$`ASS1 OE`)
  ) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "Vector", "ASS1_OE")))


plot <-
  ggbetweenstats(
    data  = data1,
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
  labs(x = "", y = "H-score") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color2)

plot

ggsave(plot,
       filename = "H-score.pdf",
       width = 5,
       height = 5)
