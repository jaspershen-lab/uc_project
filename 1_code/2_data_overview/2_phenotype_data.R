library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

load("3_data_analysis/1_metabolomics_data/peaks/metabolomics_object.RData")

dir.create("3_data_analysis/2_data_overview/2_phenotype_data")
setwd("3_data_analysis/2_data_overview/2_phenotype_data")

sample_info <-
  metabolomics_object %>%
  activate_mass_dataset(what = "sample_info") %>%
  dplyr::filter(class == "Subject") %>%
  extract_sample_info()

library(ggpie)
plot <-
  sample_info %>%
  dplyr::filter(group == "Control") %>%
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

ggsave(filename = "control_sex_pie.pdf",
       width = 5,
       height = 5)


plot <-
  sample_info %>%
  dplyr::filter(group == "UC") %>%
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
plot
ggsave(filename = "uc_sex_pie.pdf",
       width = 5,
       height = 5)


#####age
library(ggstatsplot)

plot <-
  ggbetweenstats(
    data  = sample_info,
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

plot

ggsave(plot,
       filename = "age_boxplot.pdf",
       width = 5,
       height = 5)



####BMI
library(ggstatsplot)

plot <-
  ggbetweenstats(
    data  = sample_info,
    x  = group,
    y  = BMI,
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

plot

ggsave(plot,
       filename = "bmi_boxplot.pdf",
       width = 5,
       height = 5)




#####circos plot
#####circos plot
library(circlize)
##age, sex, BMI, group

df <-
  data.frame(
    factors = sample_info$subject_id,
    x = 1,
    y = 1,
    sample_info,
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

##BMI
range(df$BMI, na.rm = TRUE)
temp_value <- df$BMI

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


