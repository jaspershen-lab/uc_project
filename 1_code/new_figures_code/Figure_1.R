library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

subject_info <-
  readxl::read_xlsx("3_data_analysis/supplementary_data/data1/supplementary_data1.xlsx")

dir.create("3_data_analysis/new_figures/Figure_1", recursive = TRUE)

########Figure1_b
library(circlize)
##age, sex, bmi, group

df <-
  data.frame(
    factors = subject_info %>% dplyr::filter(cohort == "discovery") %>% pull(subject_id),
    x = 1,
    y = 1,
    subject_info %>% dplyr::filter(cohort == "discovery"),
    stringsAsFactors = TRUE
  ) %>%
  # dplyr::arrange(FREG8_Age) %>%
  dplyr::mutate(factors = factor(factors, levels = factors))

circos.par(
  "track.height" = 0.2,
  start.degree = 85,
  clock.wise = TRUE,
  gap.after = c(rep(0, nrow(df) - 1), 10),
  cell.padding = c(0, 0, 0, 0)
)

circos.initialize(factors = df$factors,
                  x = df$x,
                  xlim = c(0.5, 1.5))

##age
range(df$age, na.rm = TRUE)
temp_value <- df$age

circos.track(
  factors = df$factors,
  # x = df$x,
  y = temp_value,
  ylim = c(0.8 * min(temp_value), 1.1 * max(temp_value, na.rm = TRUE)),
  bg.border = "black",
  # bg.col = NA,
  track.height = 0.2,
  panel.fun = function(x, y) {
    name = get.cell.meta.data("sector.index")
    i = get.cell.meta.data("sector.numeric.index")
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")

    circos.yaxis(
      side = "left",
      at = c(0.8 * min(temp_value), round((
        min(temp_value, na.rm = TRUE) + max(temp_value, na.rm = TRUE)
      ) / 2, 2), round(max(
        temp_value, na.rm = TRUE
      ), 2)),
      sector.index = get.all.sector.index()[1],
      labels.cex = 0.4,
      labels.niceFacing = FALSE
    )

    circos.lines(
      x = mean(xlim, na.rm = TRUE),
      y =  temp_value[i],
      pch = 16,
      cex = 8,
      type = "h",
      col = ggsci::pal_aaas()(n = 10)[4],
      lwd = 2
    )
  }
)

##bmi
range(df$bmi, na.rm = TRUE)
temp_value <- df$bmi

circos.track(
  factors = df$factors,
  # x = df$x,
  y = temp_value,
  ylim = c(
    0.8 * min(temp_value, na.rm = TRUE),
    1.1 * max(temp_value, na.rm = TRUE)
  ),
  bg.border = "black",
  # bg.col = NA,
  track.height = 0.2,
  panel.fun = function(x, y) {
    name = get.cell.meta.data("sector.index")
    i = get.cell.meta.data("sector.numeric.index")
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")

    circos.yaxis(
      side = "left",
      at = c(
        0.8 * min(temp_value, na.rm = TRUE),
        round((
          min(temp_value, na.rm = TRUE) + max(temp_value, na.rm = TRUE)
        ) / 2, 2),
        round(max(temp_value, na.rm = TRUE), 2)
      ),
      sector.index = get.all.sector.index()[1],
      labels.cex = 0.4,
      labels.niceFacing = FALSE
    )

    circos.lines(
      x = mean(xlim, na.rm = TRUE),
      y =  temp_value[i],
      pch = 16,
      cex = 8,
      type = "h",
      col = ggsci::pal_tron()(n = 10)[1],
      lwd = 2
    )

    # circos.points(
    #   x = mean(xlim),
    #   y =  temp_value[i],
    #   pch = 16,
    #   cex = 0.8,
    #   col = ggsci::pal_tron()(n = 10)[1]
    # )
  }
)

## sex
temp_sex <- df$sex
temp_sex[is.na(temp_sex)] <- "grey"
temp_sex[temp_sex == "Female"] <- sex_color["Female"]
temp_sex[temp_sex == "Male"] <- sex_color["Male"]

circos.track(
  factors = df$factors,
  # x = df$x,
  y = df$y,
  ylim = c(0, 1),
  bg.border = NA,
  # bg.col = NA,
  track.height = 0.1,
  panel.fun = function(x, y) {
    name = get.cell.meta.data("sector.index")
    i = get.cell.meta.data("sector.numeric.index")
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")

    #text direction (dd) and adjusmtents (aa)
    theta = circlize(mean(xlim), 1.3)[1, 1] %% 360
    dd <-
      ifelse(theta < 90 ||
               theta > 270, "clockwise", "reverse.clockwise")
    aa = c(0.5, 1)
    # if(theta < 90 || theta > 270)  aa = c(0, 0.5)

    circos.rect(
      xleft = xlim[1],
      ybottom = ylim[1],
      xright = xlim[2],
      ytop = ylim[2],
      col = temp_sex[i],
      border = NA
    )
  }
)

## group
temp_group <- df$group
temp_group[is.na(temp_group)] <- "grey"
temp_group[temp_group == "Control"] <- group_color["Control"]
temp_group[temp_group == "UC"] <- group_color["UC"]

circos.track(
  factors = df$factors,
  # x = df$x,
  y = df$y,
  ylim = c(0, 1),
  bg.border = NA,
  # bg.col = NA,
  track.height = 0.1,
  panel.fun = function(x, y) {
    name = get.cell.meta.data("sector.index")
    i = get.cell.meta.data("sector.numeric.index")
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")

    #text direction (dd) and adjusmtents (aa)
    theta = circlize(mean(xlim), 1.3)[1, 1] %% 360
    dd <-
      ifelse(theta < 90 ||
               theta > 270, "clockwise", "reverse.clockwise")
    aa = c(0.5, 1)
    # if(theta < 90 || theta > 270)  aa = c(0, 0.5)

    circos.rect(
      xleft = xlim[1],
      ybottom = ylim[1],
      xright = xlim[2],
      ytop = ylim[2],
      col = temp_group[i],
      border = NA
    )
  }
)




####Figure1_c
library(ggpie)
plot1 <-
  subject_info %>%
  dplyr::filter(group == "Control") %>%
  dplyr::filter(cohort == "discovery") %>%
  ggdonut(
    group_key = "sex",
    count_type = "full",
    label_info = "all",
    label_type = "horizon",
    label_split = NULL,
    label_size = 4,
    label_pos = "in",
    label_threshold = 10
  ) +
  scale_fill_manual(values = sex_color)
plot1

plot2 <-
  subject_info %>%
  dplyr::filter(group == "UC") %>%
  dplyr::filter(cohort == "discovery") %>%
  ggdonut(
    group_key = "sex",
    count_type = "full",
    label_info = "all",
    label_type = "horizon",
    label_split = NULL,
    label_size = 4,
    label_pos = "in",
    label_threshold = 10
  ) +
  scale_fill_manual(values = sex_color)
plot2

library(patchwork)


####chisq test for sex in Control and UC groups

temp_data1 <-
  subject_info %>%
  dplyr::filter(cohort == "discovery") %>%
  dplyr::filter(group == "Control") %>%
  pull(sex) %>%
  table()


temp_data2 <-
  subject_info %>%
  dplyr::filter(cohort == "discovery") %>%
  dplyr::filter(group == "UC") %>%
  pull(sex) %>%
  table()

rbind(temp_data1, temp_data2) %>%
  chisq.test()


#####age
library(ggstatsplot)

plot3 <-
  ggbetweenstats(
    data  = subject_info %>%
      dplyr::filter(cohort == "discovery"),
    x  = group,
    y  = age,
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
  labs(x = "", y = "Age (years)") +
  theme(legend.position = "") +
  scale_color_manual(values = group_color)

plot3

####bmi
library(ggstatsplot)

plot4 <-
  ggbetweenstats(
    data  = subject_info %>%
      dplyr::filter(cohort == "discovery"),
    x  = group,
    y  = bmi,
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
  labs(x = "", y = "Age (years)") +
  theme(legend.position = "") +
  scale_color_manual(values = group_color)

plot4

plot <-
  plot1 + plot2 + plot3 + plot4 + plot_layout(nrow = 1)

plot

ggsave(plot,
       filename = "3_data_analysis/new_figures/Figure_1/Figure_1c.pdf",
       width = 20,
       height = 5)
