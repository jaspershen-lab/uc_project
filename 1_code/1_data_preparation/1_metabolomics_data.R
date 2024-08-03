library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

data_pos <- readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                              sheet = 1)
data_neg <- readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                              sheet = 2)
###positive
variable_info_pos <-
  data_pos %>%
  dplyr::select(name:"rt(s)") %>%
  dplyr::rename(
    variable_id = name,
    Compound.name = description,
    VIP = VIP,
    fc = "Fold change",
    p_value = "p-value",
    mz = "m/z",
    rt = "rt(s)"
  ) %>%
  dplyr::mutate(variable_id = paste(variable_id, "POS", sep = "_")) %>%
  as.data.frame()

expression_data_pos <-
  data_pos %>%
  dplyr::select(-c(name:"rt(s)")) %>%
  rename_all( ~ str_replace_all(., "-", "_")) %>%
  as.data.frame()

rownames(expression_data_pos) <- variable_info_pos$variable_id

sample_info_pos <-
  data.frame(sample_id = colnames(expression_data_pos)) %>%
  dplyr::mutate(group = case_when(
    grepl("Ctr", sample_id) ~ "Control",
    grepl("UC", sample_id) ~ "UC",
    grepl("QC", sample_id) ~ "QC",
    TRUE ~ "Unknown"
  )) %>%
  dplyr::mutate(class = case_when(group == "QC" ~ "QC", TRUE ~ "Subject"))


###negative
variable_info_neg <-
  data_neg %>%
  dplyr::select(name:"rt(s)") %>%
  dplyr::rename(
    variable_id = name,
    Compound.name = description,
    VIP = VIP,
    fc = "Fold change",
    p_value = "p-value",
    mz = "m/z",
    rt = "rt(s)"
  ) %>%
  dplyr::mutate(variable_id = paste(variable_id, "NEG", sep = "_")) %>%
  as.data.frame()

expression_data_neg <-
  data_neg %>%
  dplyr::select(-c(name:"rt(s)")) %>%
  rename_all( ~ str_replace_all(., "-", "_")) %>%
  as.data.frame()

sample_info_neg <-
  data.frame(sample_id = colnames(expression_data_neg)) %>%
  dplyr::mutate(group = case_when(
    grepl("Ctr", sample_id) ~ "Control",
    grepl("UC", sample_id) ~ "UC",
    grepl("QC", sample_id) ~ "QC",
    TRUE ~ "Unknown"
  )) %>%
  dplyr::mutate(class = case_when(group == "QC" ~ "QC", TRUE ~ "Subject"))


rownames(expression_data_neg) <- variable_info_neg$variable_id

dim(sample_info_pos)
dim(sample_info_neg)
sum(sample_info_pos$sample_id == sample_info_neg$sample_id)

sample_info <-
  sample_info_pos

variable_info <-
  rbind(variable_info_pos %>% dplyr::mutate(polarity = "Positive"),
        variable_info_neg %>% dplyr::mutate(polarity = "Negative"))

expression_data <-
  rbind(expression_data_pos, expression_data_neg)

rownames(expression_data) == variable_info$variable_id

dir.create("3_data_analysis/1_metabolomics_data", showWarnings = TRUE)
setwd("3_data_analysis/1_metabolomics_data/")

dir.create("peaks")
dir.create("metabolites")

library(tidymass)

metabolomics_object <-
  create_mass_dataset(
    sample_info = sample_info,
    variable_info = variable_info,
    expression_data = expression_data
  )

save(metabolomics_object, file = "peaks/metabolomics_object.RData")

####metabolites
variable_info <-
  variable_info %>%
  dplyr::filter(!is.na(Compound.name))


expression_data <-
  expression_data[variable_info$variable_id, ]

metabolomics_object <-
  create_mass_dataset(
    sample_info = sample_info,
    variable_info = variable_info,
    expression_data = expression_data
  )

save(metabolomics_object, file = "metabolites/metabolomics_object.RData")
