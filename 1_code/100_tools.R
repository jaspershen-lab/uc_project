library(tidyverse)
library(ggplot2)

class_color <-
  c("QC" = "#EFC000FF", "Subject" = "#0073C2FF")

polarity_color <-
  c("Positive" = "#CD534CFF",
    "Negative" = "#7AA6DCFF")

up_down_color <-
  c("UP" = "#CD534CFF",
    DOWN = "#003C67FF",
    NO = "#868686FF")


show_col(ggsci::pal_jco()(n = 10))

ggsci::pal_jco()(n = 10)

theme_base <-
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13)
  )
