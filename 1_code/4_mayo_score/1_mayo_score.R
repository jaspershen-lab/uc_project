library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

load("3_data_analysis/1_metabolomics_data/metabolites/metabolomics_object.RData")

dir.create("3_data_analysis/4_mayo_score")
setwd("3_data_analysis/4_mayo_score/")

temp_data <-
  metabolomics_object %>%
  activate_mass_dataset(what = "variable_info") %>%
  dplyr::filter(p_value < 0.05 &
                  VIP > 1) %>%
  dplyr::filter(!is.na(Compound.name)) %>%
  activate_mass_dataset(what = "sample_info") %>%
  dplyr::filter(!is.na(mayo_score))

library(plyr)

temp_variable_info <-
  temp_data@variable_info[, c("variable_id", "Compound.name", "fc", "p_value")] %>%
  plyr::dlply(.variables = .(Compound.name)) %>%
  purrr::map(function(x) {
    x %>%
      dplyr::filter(p_value == min(p_value))
  }) %>%
  do.call(rbind, .) %>%
  as.data.frame()

temp_data <-
  temp_data %>%
  activate_mass_dataset(what = "variable_info") %>%
  dplyr::filter(variable_id %in% temp_variable_info$variable_id)

plot(unlist(temp_data[1, , drop = TRUE]), temp_data$mayo_score)

cor_data <-
  1:nrow(temp_data) %>%
  purrr::map(function(i) {
    test_result <-
      cor.test(unlist(temp_data@expression_data[i, , drop = TRUE]),
               temp_data@sample_info$mayo_score,
               method = "pearson")
    data.frame(
      variable_id = temp_data@variable_info$variable_id[i],
      compound = temp_data@variable_info$Compound.name[i],
      rho = test_result$estimate,
      p_value = test_result$p.value
    )
  }) %>%
  do.call(rbind, .) %>%
  as.data.frame()

cor_data$fdr <- p.adjust(cor_data$p_value, method = "fdr")

cor_data <-
  cor_data %>%
  dplyr::distinct(compound, .keep_all = TRUE) %>%
  dplyr::arrange(desc(rho)) %>%
  dplyr::mutate(compound = factor(compound, levels = compound))


plot1 <-
  cor_data %>%
  dplyr::mutate(class = case_when(rho > 0 &
                                    fdr < 0.05 ~ "UP", rho < 0 &
                                    fdr < 0.05 ~ "DOWN", TRUE ~ "NO")) %>%
  ggplot(aes(compound, rho)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_segment(aes(
    xend = compound,
    y = rho,
    yend = 0,
    color = class
  )) +
  geom_point(aes(size = -log(fdr, 10), fill = class), shape = 21, ) +
  geom_text(aes(
    label = compound,
    vjust = 0.5,
    hjust = ifelse(rho > 0, -0.3, 1.3)
  ), angle = 90) +
  scale_color_manual(values = up_down_color) +
  scale_fill_manual(values = up_down_color) +
  scale_size_continuous(range = c(1, 8)) +
  theme_base +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(x = "", y = "Pearson correlation coefficient") +
  scale_y_continuous(expand = c(0.1, 0.1))


plot1

library(plyr)

plot2 <-
  cor_data %>%
  dplyr::select(-p_value) %>%
  dplyr::left_join(temp_data@variable_info[, c("variable_id", "Compound.name", "fc", "p_value")], by = "variable_id") %>%
  dplyr::mutate(class = case_when(fc > 1 &
                                    p_value < 0.05 ~ "UP", fc < 1 &
                                    p_value < 0.05 ~ "DOWN", TRUE ~ "NO")) %>%
  ggplot(aes(x = compound)) +
  geom_point(aes(
    x = compound,
    y = "0",
    size = -log(p_value, 10),
    fill = log(fc, 2)
  ), shape = 21) +
  scale_fill_gradientn(colours = c(unname(up_down_color["DOWN"]), "white", unname(up_down_color["UP"]))) +
  scale_color_manual(values = up_down_color) +
  scale_size_continuous(range = c(1, 8)) +
  theme_base +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(x = "", y = "") +
  scale_y_discrete(expand = c(0, 0))

plot2

library(patchwork)


plot <-
  plot1 + plot2 + plot_layout(ncol = 1, heights = c(14, 1))

plot

ggsave(plot, filename = "mayo_score.pdf", width = 10, height = 5)


temp_data %>%
  activate_mass_dataset(what = "variable_info") %>%
  dplyr::filter(Compound.name == "L-Arginine") %>%
  pivot_longer() %>%
  dplyr::left_join(temp_data@sample_info, by = "sample_id") %>%
  ggplot(aes(mayo_score, value)) +
  geom_point()

temp_data %>%
  activate_mass_dataset(what = "variable_info") %>%
  dplyr::filter(Compound.name == "L-Arginine") %>%
  log(10) %>% 
  pivot_longer() %>%
  dplyr::left_join(temp_data@sample_info, by = "sample_id") %>%
  ggplot(aes(mayo_score, value)) +
  geom_point()
