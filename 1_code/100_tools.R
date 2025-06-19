library(tidyverse)
library(ggplot2)
library(scales)

class_color <-
  c("QC" = "#EFC000FF", "Subject" = "#0073C2FF")

polarity_color <-
  c("Positive" = "#CD534CFF",
    "Negative" = "#7AA6DCFF")

up_down_color <-
  c(
    "UP" = "#CD534CFF",
    DOWN = "#003C67FF",
    NO = "#868686FF"
  )


group_color <-
  c("Control" = "#8F7700FF", "UC" = "#A73030FF")

sex_color <-
  c(Male = "#003C67FF", Female = "#A73030FF")


disease_color <-
  c(
    "Control" = "#8F7700FF",
    "DSS" = "#A73030FF",
    "Arg" = "#0073C2FF"
  )

disease_color2 <-
  c(
    "Control" = "#8F7700FF",
    "Vector" = "#A73030FF",
    "ASS1_OE" = "#631879FF"
  )

disease_color3 <-
  c(
    "Control" = "#8F7700FF",
    "Vector" = "#A73030FF",
    "shRNA" = "#EFC000FF"
  )

disease_color4 <-
  c(
    "Control" = "#8F7700FF",
    "DSS" = "#A73030FF",
    "MDLA" = "#008280FF",
    "5_ASA" = "#7AA6DCFF"
  )

disease_color5 <-
  c(
    "Control" = "#8F7700FF",
    "DSS" = "#A73030FF",
    "MDLA" = "#008280FF",
    "C_01" = "#7AA6DCFF"
  )


disease_color6 <-
  c(
    "Control" = "#8F7700FF",
    "Arg" = "#0073C2FF"
  )

disease_color7 <-
  c(
    "Control" = "#8F7700FF",
    "Model" = "#A73030FF",
    "Arg" = "#0073C2FF"
  )

show_col(ggsci::pal_jco()(n = 10))

ggsci::pal_jco()(n = 10)


show_col(ggsci::pal_aaas()(n = 10))

ggsci::pal_aaas()(n = 10)

theme_base <-
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    strip.text = element_text(size = 13)
  )

volcano_plot2 <-
  function(object,
           fc_column_name = "fc",
           log2_fc = TRUE,
           p_value_column_name = "p_value_adjust",
           labs_x = "log2(Fold change)",
           labs_y = "-log(p-adjust, 10)",
           fc_up_cutoff = 2,
           fc_down_cutoff = 0.5,
           p_value_cutoff = 0.05,
           line_color = "red",
           up_color = "#EE0000FF",
           down_color = "#3B4992FF",
           no_color = "#808180FF",
           point_size = 2,
           point_alpha = 1,
           point_size_scale,
           line_type = 1,
           add_text = FALSE,
           text_for = c("marker", "UP", "DOWM"),
           text_from = "variable_id") {
    text_for = match.arg(text_for)
    # massdataset::check_object_class(object = object, class = "mass_dataset")

    object <- object@variable_info

    if (all(colnames(object) != fc_column_name)) {
      stop(paste("no", fc_column_name, "in variable_info.\n"))
    }

    if (all(colnames(object) != p_value_column_name)) {
      stop(paste("no", p_value_column_name, "in variable_info.\n"))
    }

    variable_info <-
      object

    if (log2_fc) {
      variable_info <-
        variable_info %>%
        dplyr::mutate(log2_fc = log(get(fc_column_name), 2)) %>%
        dplyr::mutate(log10_p = -log(get(p_value_column_name), 10)) %>%
        dplyr::mutate(
          marker = case_when(
            log2_fc > log(fc_up_cutoff, 2) &
              log10_p > -log(p_value_cutoff, 10) & VIP > 1 ~ "UP",
            log2_fc < log(fc_down_cutoff, 2) &
              log10_p > -log(p_value_cutoff, 10) & VIP > 1 ~ "DOWN",
            TRUE ~ "NO"
          )
        )
    } else{
      variable_info <-
        variable_info %>%
        dplyr::mutate(log2_fc = get(fc_column_name)) %>%
        dplyr::mutate(log10_p = -log(get(p_value_column_name), 10)) %>%
        dplyr::mutate(
          marker = case_when(
            log2_fc > fc_up_cutoff &
              log10_p > -log(p_value_cutoff, 10) & VIP > 1 ~ "UP",
            log2_fc < fc_down_cutoff &
              log10_p > -log(p_value_cutoff, 10) & VIP > 1 ~ "DOWN",
            TRUE ~ "NO"
          )
        )
    }

    variable_info <-
      variable_info %>%
      dplyr::mutate(marker = factor(marker, levels = c("NO", "UP", "DOWN")))

    # browser()

    if (log2_fc) {
      plot =
        variable_info %>%
        ggplot(aes(log2_fc, log10_p)) +
        theme_bw() +
        labs(x = labs_x, y = labs_y) +
        theme(panel.grid.major = element_blank()) +
        geom_vline(
          xintercept = log(fc_up_cutoff, 2),
          color = line_color,
          linetype = line_type
        ) +
        geom_vline(
          xintercept = log(fc_down_cutoff, 2),
          color = line_color,
          linetype = line_type
        ) +
        geom_hline(
          yintercept = -log(p_value_cutoff, 10),
          color = line_color,
          linetype = line_type
        )
    } else{
      plot =
        variable_info %>%
        ggplot(aes(log2_fc, log10_p)) +
        theme_bw() +
        labs(x = labs_x, y = labs_y) +
        theme(panel.grid.major = element_blank()) +
        geom_vline(xintercept = fc_up_cutoff,
                   color = line_color,
                   linetype = line_type) +
        geom_vline(xintercept = fc_down_cutoff,
                   color = line_color,
                   linetype = line_type) +
        geom_hline(
          yintercept = -log(p_value_cutoff, 10),
          color = line_color,
          linetype = line_type
        )
    }

    if (!missing(point_size_scale)) {
      if (all(colnames(variable_info) != point_size_scale)) {
        stop(paste("no", point_size_scale, "in variable_info.\n"))
      } else{
        if (length(grep("p_value", point_size_scale)) > 0) {
          point_size_scale = "log10_p"
        }
        plot =
          plot +
          geom_point(aes(color = marker, size = get(point_size_scale)), alpha = point_alpha) +
          scale_color_manual(values = c(
            "UP" = up_color,
            "DOWN" = down_color,
            "NO" = no_color
          )) +
          guides(size = guide_legend(title = point_size_scale))
      }
    } else{
      plot =
        plot +
        geom_point(aes(color = marker), size = point_size, alpha = point_alpha) +
        scale_color_manual(values = c(
          "UP" = up_color,
          "DOWN" = down_color,
          "NO" = no_color
        ))
    }


    if (add_text) {
      if (all(colnames(variable_info) != text_from)) {
        stop(paste("no", text_from, "in variable_info.\n"))
      }

      if (text_for == "marker") {
        text_data =
          variable_info %>%
          dplyr::filter(marker %in% c("UP", "DOWN"))
      } else{
        text_data =
          variable_info %>%
          dplyr::filter(marker %in% text_for)
      }

      # browser()

      if (requireNamespace("ggrepel", quietly = TRUE)) {
        plot =
          plot +
          ggrepel::geom_text_repel(aes(log2_fc, log10_p, label = get(text_from)),
                                   data = text_data %>% dplyr::filter(!is.na(get(text_from))))
      } else{
        message("Please install ggrepel package first.")
      }

    }

    return(plot)
  }



volcano_plot3 <-
  function(object,
           fc_column_name = "fc",
           log2_fc = TRUE,
           p_value_column_name = "p_value_adjust",
           labs_x = "log2(Fold change)",
           labs_y = "-log(p-adjust, 10)",
           fc_up_cutoff = 2,
           fc_down_cutoff = 0.5,
           p_value_cutoff = 0.05,
           line_color = "red",
           up_color = "#EE0000FF",
           down_color = "#3B4992FF",
           no_color = "#808180FF",
           point_size = 2,
           point_alpha = 1,
           point_size_scale,
           line_type = 1,
           add_text = FALSE,
           text_for = c("marker", "UP", "DOWM"),
           text_from = "variable_id") {
    text_for = match.arg(text_for)
    # massdataset::check_object_class(object = object, class = "mass_dataset")

    object <- object@variable_info

    if (all(colnames(object) != fc_column_name)) {
      stop(paste("no", fc_column_name, "in variable_info.\n"))
    }

    if (all(colnames(object) != p_value_column_name)) {
      stop(paste("no", p_value_column_name, "in variable_info.\n"))
    }

    variable_info <-
      object

    if (log2_fc) {
      variable_info <-
        variable_info %>%
        dplyr::mutate(log2_fc = log(get(fc_column_name), 2)) %>%
        dplyr::mutate(log10_p = -log(get(p_value_column_name), 10)) %>%
        dplyr::mutate(
          marker = case_when(
            log2_fc > log(fc_up_cutoff, 2) &
              log10_p > -log(p_value_cutoff, 10) ~ "UP",
            log2_fc < log(fc_down_cutoff, 2) &
              log10_p > -log(p_value_cutoff, 10) ~ "DOWN",
            TRUE ~ "NO"
          )
        )
    } else{
      variable_info <-
        variable_info %>%
        dplyr::mutate(log2_fc = get(fc_column_name)) %>%
        dplyr::mutate(log10_p = -log(get(p_value_column_name), 10)) %>%
        dplyr::mutate(
          marker = case_when(
            log2_fc > fc_up_cutoff & log10_p > -log(p_value_cutoff, 10) ~ "UP",
            log2_fc < fc_down_cutoff &
              log10_p > -log(p_value_cutoff, 10) ~ "DOWN",
            TRUE ~ "NO"
          )
        )
    }

    variable_info <-
      variable_info %>%
      dplyr::mutate(marker = factor(marker, levels = c("NO", "UP", "DOWN")))

    # browser()

    if (log2_fc) {
      plot =
        variable_info %>%
        ggplot(aes(log2_fc, log10_p)) +
        theme_bw() +
        labs(x = labs_x, y = labs_y) +
        theme(panel.grid.major = element_blank()) +
        geom_vline(
          xintercept = log(fc_up_cutoff, 2),
          color = line_color,
          linetype = line_type
        ) +
        geom_vline(
          xintercept = log(fc_down_cutoff, 2),
          color = line_color,
          linetype = line_type
        ) +
        geom_hline(
          yintercept = -log(p_value_cutoff, 10),
          color = line_color,
          linetype = line_type
        )
    } else{
      plot =
        variable_info %>%
        ggplot(aes(log2_fc, log10_p)) +
        theme_bw() +
        labs(x = labs_x, y = labs_y) +
        theme(panel.grid.major = element_blank()) +
        geom_vline(xintercept = fc_up_cutoff,
                   color = line_color,
                   linetype = line_type) +
        geom_vline(xintercept = fc_down_cutoff,
                   color = line_color,
                   linetype = line_type) +
        geom_hline(
          yintercept = -log(p_value_cutoff, 10),
          color = line_color,
          linetype = line_type
        )
    }

    if (!missing(point_size_scale)) {
      if (all(colnames(variable_info) != point_size_scale)) {
        stop(paste("no", point_size_scale, "in variable_info.\n"))
      } else{
        if (length(grep("p_value", point_size_scale)) > 0) {
          point_size_scale = "log10_p"
        }
        plot =
          plot +
          geom_point(aes(color = marker, size = get(point_size_scale)), alpha = point_alpha) +
          scale_color_manual(values = c(
            "UP" = up_color,
            "DOWN" = down_color,
            "NO" = no_color
          )) +
          guides(size = guide_legend(title = point_size_scale))
      }
    } else{
      plot =
        plot +
        geom_point(aes(color = marker), size = point_size, alpha = point_alpha) +
        scale_color_manual(values = c(
          "UP" = up_color,
          "DOWN" = down_color,
          "NO" = no_color
        ))
    }


    if (add_text) {
      if (all(colnames(variable_info) != text_from)) {
        stop(paste("no", text_from, "in variable_info.\n"))
      }

      if (text_for == "marker") {
        text_data =
          variable_info %>%
          dplyr::filter(marker %in% c("UP", "DOWN"))
      } else{
        text_data =
          variable_info %>%
          dplyr::filter(marker %in% text_for)
      }

      # browser()

      if (requireNamespace("ggrepel", quietly = TRUE)) {
        plot =
          plot +
          ggrepel::geom_text_repel(aes(log2_fc, log10_p, label = get(text_from)),
                                   data = text_data %>% dplyr::filter(!is.na(get(text_from))))
      } else{
        message("Please install ggrepel package first.")
      }

    }

    return(plot)
  }
