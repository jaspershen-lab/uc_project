library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create(
  "3_data_analysis/old_figures/Figure9",
  showWarnings = TRUE,
  recursive = TRUE
)
setwd("3_data_analysis/old_figures/Figure9")


###Figure 9D
data <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig9D")

colnames(data)[1] <- "time"

colnames(data) <-
  colnames(data) %>%
  stringr::str_replace("Cycle\\: [0-9]{1,2}  0673 ", "") %>%
  stringr::str_replace("μM_Y", "") %>%
  stringr::str_trim()

colnames(data)[-1] <-
paste0(as.character(as.numeric(colnames(data)[-1]) * 1000), "_nM")


plot <-
data %>%
  pivot_longer(cols = -time,
               names_to = "concentration",
               values_to = "value") %>%
  dplyr::mutate(concentration = factor(concentration, levels = unique(concentration))) %>%
  ggplot(aes(x = time, y = value, color = concentration)) +
  geom_line(aes(group = concentration,
                color = concentration)) +
  theme_base +
  ggsci::scale_color_jco() +
  labs(x = "Time (seconds)")
plot

ggsave(plot,
       filename = "figure9d.pdf",
       width = 5,
       height = 7)

###Figure 9E
data <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig9E")

colnames(data)[1] <- "Day"


data <-
  data %>%
  as.data.frame() %>%
  rename_with( ~ gsub("\\.\\.\\.", "_", .)) %>%
  tidyr::pivot_longer(cols = -Day,
                      names_to = "sample_id",
                      values_to = "value") %>%
  dplyr::mutate(group = case_when(
    grepl("Control", sample_id) ~ "Control",
    grepl("Model", sample_id) ~ "DSS",
    grepl("MDLA", sample_id) ~ "MDLA",
    grepl("C-01", sample_id) ~ "C_01"
  )) %>%
  dplyr::mutate(Day = as.character(Day)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "C_01")))

# Calculate medians
median_data <-
  data %>%
  dplyr::group_by(Day, group) %>%
  dplyr::summarize(median_value = median(value), .groups = 'drop')

plot <-
  data %>%
  ggplot(aes(x = as.character(Day), y = value, color = group)) +
  geom_point(
    aes(x = Day, y = value, color = group),
    position = position_dodge(0.35),
    size = 2,
    alpha = 0.7
  ) +
  geom_boxplot(
    aes(color = group),
    outlier.shape = NA,
    fill = "transparent",
    position = position_dodge(0.35),
    width = 0.3
  ) +
  geom_line(
    data = median_data,
    aes(
      x = as.numeric(Day) + 1,
      y = median_value,
      group = group
    ),
    position = position_dodge(0.35)
  ) +
  theme_base +
  scale_color_manual(values = disease_color5) +
  labs(x = "Day", y = "Disease activity index (DAI)")
plot

ggsave(plot,
       filename = "figure9e.pdf",
       width = 7,
       height = 5)


library(ez)


ezANOVA(
  data = data %>% dplyr::filter(group %in% c("Control", "DSS")),
  dv = value,
  wid = sample_id,
  within = Day,
  between = group,
  type = 1
) %>%
  print()

ezANOVA(
  data = data %>% dplyr::filter(group %in% c("Control", "C_01")),
  dv = value,
  wid = sample_id,
  within = Day,
  between = group,
  type = 1
) %>%
  print()

ezANOVA(
  data = data %>% dplyr::filter(group %in% c("DSS", "C_01")),
  dv = value,
  wid = sample_id,
  within = Day,
  between = group,
  type = 1
) %>%
  print()



###Figure 9F
data <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig9F")

data <-
  rbind(
    data.frame(group = "Control", value = as.numeric(as.character(data$Control))),
    data.frame(group = "DSS", value = as.numeric(as.character(data$Model))),
    data.frame(group = "C_01", value = as.numeric(as.character(data$`C-01`)))
  ) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "C_01")))

plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    # centrality.plotting = FALSE,
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "Colon length (cm)") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = disease_color5)

plot

ggsave(plot,
       filename = "figure9f.pdf",
       width = 5,
       height = 5)

##Figure 9H
data <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig9H")
data <-
  rbind(
    data.frame(group = "Control", value = as.numeric(as.character(data$Control))),
    data.frame(group = "DSS", value = as.numeric(as.character(data$Model))),
    data.frame(group = "C_01", value = as.numeric(as.character(data$`C-01`)))
  ) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "C_01")))

plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    # centrality.plotting = FALSE,
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "H-score") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = disease_color5)

plot

ggsave(plot,
       filename = "figure9h.pdf",
       width = 5,
       height = 5)

##Figure 9I
data <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig9I")

data <-
  rbind(
    data.frame(group = "Control", value = as.numeric(as.character(data$Control))),
    data.frame(group = "DSS", value = as.numeric(as.character(data$Model))),
    data.frame(group = "C_01", value = as.numeric(as.character(data$`C-01`)))
  ) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "C_01")))


plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "ug/g (Colon tissue)") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = disease_color5)

plot

ggsave(plot,
       filename = "figure9i.pdf",
       width = 5,
       height = 5)

##Figure 9J
data <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig9J")

data <-
  rbind(
    data.frame(group = "Control", value = as.numeric(as.character(data$Control))),
    data.frame(group = "DSS", value = as.numeric(as.character(data$Model))),
    data.frame(group = "C_01", value = as.numeric(as.character(data$`C-01`)))
  ) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "C_01")))

plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "ug/g (colon tissue)") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = disease_color5)

plot

ggsave(plot,
       filename = "figure9j.pdf",
       width = 5,
       height = 5)


##Figure 9K
data <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig9K")

data <-
  rbind(
    data.frame(group = "Control", value = as.numeric(as.character(data$Control))),
    data.frame(group = "DSS", value = as.numeric(as.character(data$Model))),
    data.frame(group = "C_01", value = as.numeric(as.character(data$`C-01`)))
  ) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "C_01")))

plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "ug/g (colon tissue)") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = disease_color5)

plot

ggsave(plot,
       filename = "figure9k.pdf",
       width = 5,
       height = 5)

###Figure 9L
##ass1
ass1 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.932496003, 0.945467085, 0.85711974)
    ),
    data.frame(
      group = "Control+C-01",
      value = c(0.843728177, 0.767791352, 0.71317981)
    ),
    data.frame(
      group = "Model",
      value = c(1.137810984, 1.119667318, 1.049997849)
    ),
    data.frame(
      group = "Model+C-01",
      value = c(1.033551823, 1.03744504, 0.790372832)
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("Control", "Control+C-01", "Model", "Model+C-01")
  ))


##cox2
cox2 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.24216383, 0.359695875, 0.446936391)
    ),
    data.frame(
      group = "Control+C-01",
      value = c(0.209963008, 0.264762331, 0.433598685)
    ),
    data.frame(
      group = "Model",
      value = c(0.99362222, 1.068268923, 0.94746889)
    ),
    data.frame(
      group = "Model+C-01",
      value = c(0.508863567, 0.450994016, 0.635114066)
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("Control", "Control+C-01", "Model", "Model+C-01")
  ))

###p_stat3/stat3
p_stat3_stat3 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.454174061, 0.373788725, 0.16910512)
    ),
    data.frame(
      group = "Control+C-01",
      value = c(0.491016867, 0.566680744, 0.349992236)
    ),
    data.frame(
      group = "Model",
      value = c(1.346611532, 1.041260504, 0.990059299)
    ),
    data.frame(
      group = "Model+C-01",
      value = c(0.391008949, 0.481233638, 0.272249052)
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("Control", "Control+C-01", "Model", "Model+C-01")
  ))


##p_mtor/mtro
p_mtor_mtor <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.424938745, 0.346677812, 0.789497748)
    ),
    data.frame(
      group = "Control+C-01",
      value = c(0.580617642, 0.602262815, 0.67246608)
    ),
    data.frame(
      group = "Model",
      value = c(1.265325664, 0.903722882, 1.487364037)
    ),
    data.frame(
      group = "Model+C-01",
      value = c(0.563522038, 0.722959039, 0.869416008)
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("Control", "Control+C-01", "Model", "Model+C-01")
  ))


###p_s6/s6
p_s6_s6 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.223036828, 0.168153833, 0.26598216)
    ),
    data.frame(
      group = "Control+C-01",
      value = c(0.271514135, 0.291666223, 0.366328349)
    ),
    data.frame(
      group = "Model",
      value = c(1.24010449, 1.206885064, 1.147673926)
    ),
    data.frame(
      group = "Model+C-01",
      value = c(0.472225336, 0.627325837, 0.569366437)
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c("Control", "Control+C-01", "Model", "Model+C-01")
  ))

###ass1
plot <-
  ggbetweenstats(
    data  = ass1,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      color = "black",
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "ASS1") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))
# scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "figure9l_1.pdf",
       width = 5,
       height = 5)


t.test(ass1$value[ass1$group == "Control"], ass1$value[ass1$group == "Control+C-01"])

t.test(ass1$value[ass1$group == "Model"], ass1$value[ass1$group == "Model+C-01"])

t.test(ass1$value[ass1$group == "Control"], ass1$value[ass1$group == "Model"])


##cox2
plot <-
  ggbetweenstats(
    data  = cox2,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      color = "black",
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "COX2") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))
# scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "figure9l_2.pdf",
       width = 5,
       height = 5)

t.test(cox2$value[cox2$group == "Control"], cox2$value[cox2$group == "Control+C-01"])

t.test(cox2$value[cox2$group == "Model"], cox2$value[cox2$group == "Model+C-01"])

t.test(cox2$value[cox2$group == "Control"], cox2$value[cox2$group == "Model"])

##p_stat3/stat3
plot <-
  ggbetweenstats(
    data  = p_stat3_stat3,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      color = "black",
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "p-STAT3/STAT3") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))
# scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "figure9l_3.pdf",
       width = 5,
       height = 5)


t.test(p_stat3_stat3$value[p_stat3_stat3$group == "Control"], p_stat3_stat3$value[p_stat3_stat3$group == "Control+C-01"])

t.test(p_stat3_stat3$value[p_stat3_stat3$group == "Model"], p_stat3_stat3$value[p_stat3_stat3$group == "Model+C-01"])

t.test(p_stat3_stat3$value[p_stat3_stat3$group == "Control"], p_stat3_stat3$value[p_stat3_stat3$group == "Model"])

##p_mtor/mtor
plot <-
  ggbetweenstats(
    data  = p_mtor_mtor,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      color = "black",
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "p-mTOR/mTOR") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))
# scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "figure9l_4.pdf",
       width = 5,
       height = 5)

t.test(p_mtor_mtor$value[p_mtor_mtor$group == "Control"], p_mtor_mtor$value[p_mtor_mtor$group == "Control+C-01"])

t.test(p_mtor_mtor$value[p_mtor_mtor$group == "Control"], p_mtor_mtor$value[p_mtor_mtor$group == "Model"])

t.test(p_mtor_mtor$value[p_mtor_mtor$group == "Model"], p_mtor_mtor$value[p_mtor_mtor$group == "Model+C-01"])



###p_s6/s6
plot <-
  ggbetweenstats(
    data  = p_s6_s6,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      color = "black",
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "p-S6/S6") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))
# scale_color_manual(values = disease_color3)

plot

ggsave(plot,
       filename = "figure9l_5.pdf",
       width = 5,
       height = 5)


t.test(p_s6_s6$value[p_s6_s6$group == "Control"], p_s6_s6$value[p_s6_s6$group == "Control+C-01"])

t.test(p_s6_s6$value[p_s6_s6$group == "Control"], p_s6_s6$value[p_s6_s6$group == "Model"])

t.test(p_s6_s6$value[p_s6_s6$group == "Model"], p_s6_s6$value[p_s6_s6$group == "Model+C-01"])


###Figure 9M
data <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig9M")


data <-
  rbind(
    data.frame(group = "Control", value = as.numeric(as.character(data$Control))),
    data.frame(group = "DSS", value = as.numeric(as.character(data$Model))),
    data.frame(group = "C_01", value = as.numeric(as.character(data$`C-01`)))
  ) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "C_01")))


plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "Relative abundance (fold of change)") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = disease_color5)

plot

ggsave(plot,
       filename = "figure9m.pdf",
       width = 5,
       height = 5)

###Figure 9N
data <-
  readxl::read_xlsx("../../../2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = "Fig9N")

data <-
  rbind(
    data.frame(group = "Control", value = as.numeric(as.character(data$Control))),
    data.frame(group = "DSS", value = as.numeric(as.character(data$Model))),
    data.frame(group = "C_01", value = as.numeric(as.character(data$`C-01`)))
  ) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(group = factor(group, levels = c("Control", "DSS", "C_01")))

plot <-
  ggbetweenstats(
    data  = data,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = FALSE,
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      position = position_jitter(width = 0.1)
    ),
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_base +
  labs(x = "", y = "Relative abundance (fold of change)") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = disease_color5)

plot

ggsave(plot,
       filename = "figure9n.pdf",
       width = 5,
       height = 5)
