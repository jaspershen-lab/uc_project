library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

data <-
  readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                    sheet = 5)

dir.create(
  "3_data_analysis/1_data_preparation/2_proteomics_data",
  showWarnings = FALSE,
  recursive = TRUE
)

setwd("3_data_analysis/1_data_preparation/2_proteomics_data/")

###positive
variable_info <-
  data %>%
  dplyr::select("Protein IDs":"Score") %>%
  dplyr::rename(
    variable_id = "Protein IDs",
    protein_name = "Protein names",
    gene_name = "Gene names",
    Unique_peptides_H1 = "Unique peptides H1",
    Unique_peptides_H2 = "Unique peptides H2",
    Unique_peptides_H3 = "Unique peptides H3",
    Unique_peptides_UC1 = "Unique peptides UC1",
    Unique_peptides_UC2 = "Unique peptides UC2",
    Unique_peptides_UC3 = "Unique peptides UC3",
    Sequence_coverage = "Sequence coverage [%]",
    Mol_weight = "Mol. weight [kDa]"
  ) %>%
  as.data.frame() %>%
  dplyr::mutate(UNIPROT_ID = variable_id)

expression_data <-
  data %>%
  dplyr::select(-c("Protein IDs":"Score")) %>%
  rename_all( ~ str_replace_all(., "LFQ intensity ", "")) %>%
  as.data.frame()

rownames(expression_data) <- variable_info$variable_id

sample_info <-
  data.frame(sample_id = colnames(expression_data)) %>%
  dplyr::mutate(group = case_when(
    grepl("H", sample_id) ~ "Control",
    grepl("UC", sample_id) ~ "UC",
    TRUE ~ "Unknown"
  )) %>%
  dplyr::mutate(class = case_when(group == "QC" ~ "QC", TRUE ~ "Subject"))

proteomics_object <-
  create_mass_dataset(
    sample_info = sample_info,
    variable_info = variable_info,
    expression_data = expression_data
  )

save(proteomics_object, file = "proteomics_object.RData")
