library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

data_pos <- readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                              sheet = 1)
data_neg <- readxl::read_xlsx("2_data/Source Data file for NCOMMS-22-46768-update-2.xlsx",
                              sheet = 2)

subject1 <- readxl::read_xlsx("2_data/participant_info.xlsx", sheet = 1)
subject2 <- readxl::read_xlsx("2_data/participant_info.xlsx", sheet = 2)

colnames(subject1) <- c(
  "subject_id",
  "number",
  "participant_name",
  "case_id",
  "date",
  "mayo_score",
  "sex",
  "age",
  "GWDB",
  "ESR",
  "CPR",
  "HGB",
  "TP",
  "ALB",
  "height",
  "weight",
  "BMI",
  "diagnosis"
)
colnames(subject2) <- c("subject_id", "number", "participant_name", "sex", "age", "BMI")


subject1$subject_id <- paste("UC", subject1$subject_id, sep = "_")
subject2$subject_id <- paste("Ctr", subject2$subject_id, sep = "_")

dim(subject1)
dim(subject2)

subject_info <-
  subject1 %>%
  dplyr::full_join(subject2, by = colnames(subject2))

subject_info <-
  subject_info %>%
  dplyr::mutate(sex = case_when(sex == "男" ~ "Male", sex == "女" ~ "Female")) %>%
  dplyr::mutate(sample_id = subject_id)

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

#####positive
object_pos <-
  create_mass_dataset(
    sample_info = sample_info_pos,
    variable_info = variable_info_pos,
    expression_data = expression_data_pos
  )


object_pos <-
  object_pos %>%
  impute_mv(method = "knn")


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

object_neg <-
  create_mass_dataset(
    sample_info = sample_info_neg,
    variable_info = variable_info_neg,
    expression_data = expression_data_neg
  )

object_neg <-
  object_neg %>%
  impute_mv(method = "knn")

expression_data_pos <-
  object_pos %>%
  extract_expression_data()

expression_data_neg <-
  object_neg %>%
  extract_expression_data()

sample_info <-
  sample_info_pos

variable_info <-
  rbind(
    variable_info_pos %>% dplyr::mutate(polarity = "Positive"),
    variable_info_neg %>% dplyr::mutate(polarity = "Negative")
  )

expression_data <-
  rbind(expression_data_pos, expression_data_neg)

rownames(expression_data) == variable_info$variable_id

dir.create("3_data_analysis/1_metabolomics_data", showWarnings = TRUE)
setwd("3_data_analysis/1_metabolomics_data/")

dir.create("peaks")
dir.create("metabolites")

library(tidymass)

sample_info <-
  sample_info %>%
  dplyr::left_join(subject_info, by = "sample_id")

metabolite_info <-
  variable_info %>%
  dplyr::select(variable_id, Compound.name) %>%
  dplyr::filter(!is.na(Compound.name))


# kegg_id <-
#   metabolite_info$Compound.name %>%
#   purrr::map(function(x){
#     cat(x, " ")
#     trans_ID(
#       query = x,
#       from = "Chemical Name",
#       to = "KEGG",
#       top = 1,
#       server = "cts.fiehnlab"
#     )
#   })
#
# kegg_id <-
#   kegg_id %>%
#   do.call(rbind, .) %>%
#   as.data.frame()
#
#
# hmdb_id <-
#   kegg_id$KEGG %>%
#   purrr::map(function(x) {
#     cat(x, " ")
#     trans_ID(
#       query = x,
#       from = "KEGG",
#       to = "Human Metabolome Database",
#       top = 1,
#       server = "cts.fiehnlab"
#     )
#   })
#
# hmdb_id <-
#   hmdb_id %>%
#   do.call(rbind, .) %>%
#   as.data.frame()
#
# id <-
#   data.frame(variable_id = metabolite_info$variable_id,
#              Compound.name = metabolite_info$Compound.name,
#              HMDB = hmdb_id$`Human Metabolome Database`,
#              KEGG = kegg_id$KEGG)
#
# write.csv(id, "id.csv", row.names = FALSE)

id <-
  readr::read_csv("id.csv")

variable_info <-
  variable_info %>%
  dplyr::left_join(id[, c("variable_id", "HMDB", "KEGG")], by = "variable_id")

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
